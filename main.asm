exhirom
math pri on
math round off

incsrc "macros.asm"
incsrc "snes.asm"

; 32 khz streaming
!SAMPLERATE = 32000
!BLOCK_SIZE = 315
!BURST_SIZE = 80
!SPC_DIVIDER = 140    ; Sets SPC Timer 0 divider to 140 (8000/140) = 57.142857142857146hz
                      ; This means sending one 315 bytes block 57.142857142857146 times per second, = ((315*57.142857142857146)/9)*16 = 32000hz

; 24 khz streaming
; !SAMPLERATE = 24000
; !BLOCK_SIZE = 270
; !BURST_SIZE = 80
; !SPC_DIVIDER = 160    ; 50hz timer -> ((270*50)/9)*16 = 24000hz

; 16 khz streaming
; !SAMPLERATE = 16000
; !BLOCK_SIZE = 180
; !BURST_SIZE = 80
; !SPC_DIVIDER = 160      ; 50hz timer -> ((180*50)/9)*16 = 16000hz


org $c08000
base $808000
init:
    phk : plb
    ; Code jumps here after boot

    %ai16()
    ldx #$0000
    lda.w #brr_data_left
    sta $00
    lda.w #(brr_data_left>>8)
    sta $01
    lda #$0000
    sta $03

    lda.w #brr_data_right
    sta $04
    lda.w #(brr_data_right>>8)
    sta $05

    lda #$0000
    sta $07

    jsr init_buffer
    jsr update_buffer
    jsr spc_init_driver

    %a8()
    lda #%01000100
    sta $4370
    lda #$40
    sta $4371

    ldx #hdma_table
    stx $4372
    lda.b #(hdma_table>>16)
    sta $4374
    lda #$7f
    sta $4377

    lda #$80
    sta $420c

    lda #$0f
    sta $2100

    lda #$80
    sta $4200

-   jml -

init_buffer:
    pha : phy : phx : phb : php
    %ai16()

    lda #$0000
    sta.l $7f0000
    sta.l $7f0002
    lda #$ffff
    sta.l $7f0004
    sta.l $7f0006

    ; initialize some DMA values
    %a8()
    lda #$00
    sta $4300
    lda #$80
    sta $4301

    plp : plb : plx : ply : pla
    rts   

; Copies one frame worth of BRR chunks into SRAM buffer
update_buffer:
    pha : phy : phx : phb : php
    pea $0000 : plb : plb
        
    %a8()
    lda $2143         ; Does the APU want some data?
    bne +

    lda #$00          ; Guess not...
    sta $420c         ; Disable H-DMA for the upcoming frame
    jmp .end

+
    %a8()
    lda #$00
    sta $420c       ; Enable H-DMA for the upcoming frame

    %ai16()
    lda #$1000
    sta $2181
    lda #$0001
    sta $2183

    lda $00
    clc : adc.w #!BLOCK_SIZE
    bcc .nowrap

    ; This transfer will wrap around, so let's do two transfers
    sta $0a ; save overflow bytes
    lda.w #!BLOCK_SIZE
    sec : sbc $0a
    ldy $00
    ldx $02
    jsr dma_block
    lda $0a
    ldy #$0000
    inx
    jsr dma_block

    lda.w #!BLOCK_SIZE
    sec : sbc $0a
    ldy $04
    ldx $06
    jsr dma_block
    lda $0a
    ldy #$0000
    inx
    jsr dma_block
    bra .next

.nowrap
    lda.w #!BLOCK_SIZE
    ldy $00
    ldx $02
    jsr dma_block
    
    lda.w #!BLOCK_SIZE
    ldy $04
    ldx $06
    jsr dma_block

.next

    %a16()

    lda $00
    clc : adc.w #!BLOCK_SIZE
    sta $00
    bcc +
    inc $02
+

    lda $04
    clc : adc.w #!BLOCK_SIZE
    sta $04
    bcc +
    inc $06
+

    %a8()
    lda #$80
    sta $420c       ; Enable H-DMA for the upcoming frame

.end
    plp : plb : plx : ply : pla
    rts

dma_block:
    ; source
    sty $4302
    stx $4304

    ;size 
    sta $4305

    ; start dma
    lda #$0100 : sta $420a

    rts

stream_nmi:
    jsr update_buffer
    rti

; Generate a HDMA table that transfer from RAM.
; This requires the values $0000 at $0004 and $FFFF at $0000
; as well as the main transfer buffer being at $1000

macro write_hdma_table(burst_size, block_size)
    !transfer_size #= ceil(<block_size>/4.0)*4
    !counter #= !transfer_size
    while !counter > 0
        db $01 : dw $0004
        if <burst_size> < !counter
            db ($80+(<burst_size>/4)) : dw $1000+(!transfer_size-!counter)
        else
            db ($80+(!counter/4)) : dw $1000+(!transfer_size-!counter)
        endif
        db $01 : dw $0000
        !counter #= !counter-<burst_size>
    endif
        
    db $02 : dw $0000

    !counter = !transfer_size
    while !counter > 0
        db $01 : dw $0004
        if <burst_size> < !counter
            db ($80+(<burst_size>/4)) : dw $1000+<block_size>+(!transfer_size-!counter)
        else
            db ($80+(!counter/4)) : dw $1000+<block_size>+(!transfer_size-!counter)
        endif
        db $01 : dw $0000
        !counter #= !counter-<burst_size>
    endif
    db $00   
endmacro

hdma_table:
%write_hdma_table(!BURST_SIZE, !BLOCK_SIZE)

; Include the SPC driver and transfer functions
incsrc "spc.asm"

; BRR data for left and right channels
check bankcross off
org $c10000
brr_data_left:
incbin "left.brr":0-3CFFFF

org $410000
brr_data_right:
incbin "right.brr":0-3CFFFF

; pad rom
org $ffffff
    db $00