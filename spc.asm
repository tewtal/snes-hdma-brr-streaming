spc_init_driver:
    %a8()
    jsr spc_wait_boot

    ldx #spc_driver_end-spc_driver      ; Get spc driver size in bytes
    ldy #$0400
    jsr spc_begin_upload   

-
    phx : tyx
    lda.l spc_driver+$4, x
    plx
    jsr spc_upload_byte
    dex
    bne -

    %a8()
    %i16()

    ldy #$0400
    jsr spc_execute
    rts

spc_wait_boot:
    lda #$AA
    -   cmp $2140
        bne -

    ; Clear in case it already has $CC in it
    ; (this actually occurred in testing)
    sta $2140

    lda #$BB
    -   cmp $2141
        bne -

    rts

spc_begin_upload:
    sty $2142

    ; Send command
    lda $2140
    clc
    adc #$22
    bne +       ; special case fully verified
        inc
    +
    sta $2141
    sta $2140

    ; Wait for acknowledgement
    -   cmp $2140
        bne -

    ; Initialize index
    ldy.w #0

    rts

spc_upload_byte:
    sta $2141

    ; Signal that it's ready
    tya
    sta $2140
    iny

    ; Wait for acknowledgement
    -   cmp $2140
        bne -

    rts

spc_execute:
    sty $2142

    stz $2141

    lda $2140
    clc
    adc #$22
    sta $2140

    ; Wait for acknowledgement
    -   cmp $2140
        bne -

    ldy #$0000
    sty $2142
    rts

spc_driver:
arch spc700-inline
!SAMPLEHDR = $0200
!SAMPLEBUF = $1000
!WAIT = $70

!SAMPLEBUFSIZE #= !BLOCK_SIZE*83     ;$6621 
!SAMPLEBUFLEFT = $1000
!SAMPLEBUFRIGHT = $8000
!SAMPLEBLOCKSIZE = !BLOCK_SIZE
!PITCH #= $1000*(!SAMPLERATE/32000)

!SAMPLEBUFEND_LEFT #= ($1000+!SAMPLEBUFSIZE)
!SAMPLEBUFEND_RIGHT #= ($8000+!SAMPLEBUFSIZE)

!BUFPTR = $10
!PREVTIMER = $12
!BUFPTRRIGHT = $14

!FFFF = $28

!SAMPLEBLOCKSIZE_ZP = $30
!SAMPLEBUFEND_LEFT_ZP = $32

!KEYON_WAIT = $40
!TRANSFER_SIZE = $42
!TRANSFER_PTR = $44
!BLOCKS = $46

!ZEROES = $ee
!BLOCKSIZE = $e4
!BUFEND = $e6
!BUFENDRIGHT = $e8
!BUFTMP = $ea

; 16-bit moves to zero page
macro movwiz(addr, val)
    mov <addr>, #<val>&$ff
    mov <addr>+1, #<val>>>8
endmacro

macro movwmz(addr, mem)
    mov <addr>, <mem>
    mov <addr>+1, <mem>+1
endmacro

; 16-bit moves
macro movwi(addr, val)
    mov a, #<val>&$ff
    mov <addr>, a
    mov a, #<val>>>8
    mov <addr>+1, a
endmacro

macro movwm(addr, mem)
    mov ya, mem
    mov <addr>, a
    mov <addr>+1, y
endmacro

; DPS writes
macro movdsp(reg, val)
    mov $f2, #<reg>
    mov $f3, #(<val>&$ff)
endmacro

; Generate a block of H-DMA transfer bursts depending on burst size and block size parameters
macro write_transfer_block(burst_size, block_size)
    !transfer_size #= (ceil(<block_size>/4.0)*4)
    !transfer_extra #= !transfer_size-<block_size>
    !counter #= !transfer_size

    while !counter > 0
        if <burst_size> < !counter
            mov !TRANSFER_SIZE, #<burst_size>
            call transfer_block
        else
            mov !TRANSFER_SIZE, #!counter
            call transfer_block
        endif
        !counter #= !counter-<burst_size>
    endif
    
    !counter = !transfer_extra
    while !counter > 0
        decw !TRANSFER_PTR
        !counter #= !counter-1
    endif
endmacro

org $0400
spc_init:
    ; write some useful zero page things
    %movwi(!SAMPLEHDR, $1000)
    %movwi(!SAMPLEHDR+2, $1000)
    %movwi(!SAMPLEHDR+4, $8000)
    %movwi(!SAMPLEHDR+6, $8000)
    %movwi(!FFFF, $FFFF)
    %movwi(!SAMPLEBLOCKSIZE_ZP, !SAMPLEBLOCKSIZE)
    %movwi(!SAMPLEBUFEND_LEFT_ZP, !SAMPLEBUFEND_LEFT)
    %movwi(!BLOCKSIZE, !SAMPLEBLOCKSIZE)
    %movwi(!BUFEND, !SAMPLEBUF+!SAMPLEBUFSIZE)
    %movwi(!BUFENDRIGHT, !SAMPLEBUFRIGHT+!SAMPLEBUFSIZE)
    %movwi(!BUFPTR, !SAMPLEBUF)
    %movwi(!BUFPTRRIGHT, !SAMPLEBUFRIGHT)

    ; Disable timers
    mov $f0, #$0a
    mov $f1, #$00

    %movwi($10, $1000)
    %movwi($12, $10ff)

    %movwi($14, $8000)
    %movwi($16, $80ff)
    mov x, $00

    %movdsp($6c, $20) ; FLG - Unmute and disable echo
    %movdsp($5c, $ff) ; Key-off all voices
    %movdsp($5c, $00) ; Key-off all voices
    %movdsp($4c, $00) ; Key-off all voices
    %movdsp($4d, $00) ; Clear EON
    %movdsp($3d, $00) ; Clear noise enable
    %movdsp($7d, $00) ; Echo buffer size to $00 (4 bytes)
    %movdsp($6d, $03) ; Echo buffer to $0300
    %movdsp($5d, $02) ; DIR - Set sample table to $0200
    %movdsp($2c, $00) ; EVOLL - Set left echo volume
    %movdsp($3c, $00) ; EVOLR - Set right echo volume
    %movdsp($2d, $00) ; PMON - Set pitch modulation off
    %movdsp($0d, $00) ; EFB - Set echo feedback volume
    %movdsp($0c, $7f) ; MVOLL - Set left master volume
    %movdsp($1c, $7f) ; MOVLR - Set right master volume

    %movdsp($00, $7f) ; VxVOLL - Set left channel 0 volume
    %movdsp($01, $00) ; VxVOLR - Set right channel 0 volume
    %movdsp($02, !PITCH)     ; VxPITCHL - Set channel 0 pitch (low byte)
    %movdsp($03, !PITCH>>8)  ; VxPTTCHH - Set channel 0 pitch (hi byte)
    %movdsp($05, $00) ; VxADSR1 - Set channel 0 to use direct gain (No ADSR)
    %movdsp($06, $00) ; VxADSR2 - Set channel 0 ADSR parameters
    %movdsp($07, $7f) ; VxGAIN - Set channel 0 gain
    %movdsp($04, $00) ; VxOUTX - Set channel 0 sample

    %movdsp($10, $00) ; VxVOLL - Set left channel 1 volume
    %movdsp($11, $7f) ; VxVOLR - Set right channel 1 volume
    %movdsp($12, !PITCH)       ; VxPITCHL - Set channel 1 pitch (low byte)
    %movdsp($13, !PITCH>>8)  ; VxPTTCHH - Set channel 1 pitch (hi byte)
    %movdsp($15, $00) ; VxADSR1 - Set channel 1 to use direct gain (No ADSR)
    %movdsp($16, $00) ; VxADSR2 - Set channel 1 ADSR parameters
    %movdsp($17, $7f) ; VxGAIN - Set channel 1 gain
    %movdsp($14, $01) ; VxOUTX - Set channel 1 sample

    mov $fa, #!SPC_DIVIDER       
    mov $f1, #$01                ; Enable Timer 0

    ; Wait for 8 frames of data before keying on channels
    mov !KEYON_WAIT, #$08
    mov a, $fd
    mov $f7, #$00       ; Ask for initial data

    mov !BLOCKS, #$10   ; Initial blocks to request    

.mainloop
    ; Check how many frames left before keying on
    cmp !KEYON_WAIT, #$00
    beq +
    dec !KEYON_WAIT
    bne +
    %movdsp($4c, $03) ; Key-on channel 0 and 1
+
    
    cmp !BLOCKS, #$00   ; Check if the block transfer queue is empty or not
    beq .skipTransfer   ; Skip if no blocks queued
    dec !BLOCKS         ; Remove one block from the queue
    mov $f7, #$01       ; Ask the SNES to transfer a block
    call transfer_frame ; Transfer a block
    jmp .end
.skipTransfer
    mov $f7, #$00       ; Clear transfer request flag to SNES

.end
    mov a, $fd          ; Get timer value and add to the block queue
    clrc : adc a, !BLOCKS
    mov !BLOCKS, a      

    jmp .mainloop

transfer_frame:

    movw ya, $10
    movw !TRANSFER_PTR, ya
    
    %write_transfer_block(!BURST_SIZE, !BLOCK_SIZE)

    movw ya, !TRANSFER_PTR      ; Update left channel pointer
    movw $10, ya

    movw ya, $14
    movw !TRANSFER_PTR, ya
    
    %write_transfer_block(!BURST_SIZE, !BLOCK_SIZE)

    movw ya, !TRANSFER_PTR      ; Update right channel pointer
    movw $14, ya

    movw ya, $10
    cmpw ya, !SAMPLEBUFEND_LEFT_ZP ; At end of buffer?
    bne .not_end

.adjust_loop_point                  
    ; We're at the end of the buffer, reset pointers and write loop flag in last BRR block
    mov a, (!SAMPLEBUFEND_LEFT-9)
    or a, #$03
    mov (!SAMPLEBUFEND_LEFT-9), a

    mov a, (!SAMPLEBUFEND_RIGHT-9)
    or a, #$03
    mov (!SAMPLEBUFEND_RIGHT-9), a

    %movwi($10, $1000)
    %movwi($14, $8000)

.not_end
    ret


; Wait for $FFFF sync marker, then cycle times the rest of the data burst
transfer_block:

; Wait for sync marker
-
    movw ya, !FFFF
    cmpw ya, $f4
    bne -
    mov y, #$00

; Align the next read to be as close as possible to the next HDMA update for timing reasons
    nop : nop : nop : nop : nop : nop
    nop : nop : nop : nop : nop : nop
    nop : nop : nop : nop : nop : nop
    nop : nop : nop : nop : nop : nop
    nop : nop : nop : nop : nop : nop
    nop : nop : nop : nop : nop : nop

; Transfer data each scanline from H-DMA writes
.burst_scanline    
    mov a, $f4
    mov (!TRANSFER_PTR)+y, a
    inc y

    mov a, $f5
    mov (!TRANSFER_PTR)+y, a
    inc y

    mov a, $f6
    mov (!TRANSFER_PTR)+y, a
    inc y

    mov a, $f7
    mov (!TRANSFER_PTR)+y, a
    inc y

    nop : nop : nop : nop : nop

    cmp y, !TRANSFER_SIZE
    bne .burst_scanline

; Update buffer pointer
    movw ya, !TRANSFER_PTR
    addw ya, !TRANSFER_SIZE
    movw !TRANSFER_PTR, ya

    ret

arch 65816
spc_driver_end:

