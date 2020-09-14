org $c0ffc0
    ;   0              f01234
    db "BRR STREAMING TEST   "
    db $35, $00, $0D, $00, $00, $01, $00, $B1, $BC, $4E, $43

org $c0ffe4
	dw brk
	dw brk
	dw brk
	dw nmi
	dw reset
	dw irq
	
org $c0fff4
	dw brk
	dw brk
	dw brk
	dw nmi
	dw reset
	dw irq

org $00ffc0
    ;   0              f01234
    db "BRR STREAMING TEST   "
    db $35, $00, $0D, $00, $00, $01, $00, $B1, $BC, $4E, $43

org $00ffe4
	dw brk
	dw brk
	dw brk
	dw nmi
	dw reset
	dw irq
	
org $00fff4
	dw brk
	dw brk
	dw brk
	dw nmi
	dw reset
	dw irq

org $00f000
brk:
    jml brk

nmi:
    jml stream_nmi

irq:
    rti

reset:
    ; Set up a consistent SNES state
    sei
    clc
    xce
    %ai16()
    ldx #$1fff
    txs
    pea $0000 : plb : plb

    ; Direct page to 2100 for HW regs
    lda #$2100
    tcd

    %a8()

    lda #$01
    sta $420d   ; enable FastROM

    ; Disable NMI and H-DMA
    lda #$00
    sta $4200
    sta $420c

    ; Turn on forced blanking
    lda #$8f
    sta $00    

    ; Clear all PPU registers
	ldx #$0001
-           	; loop over ppu registers $2101-$2133
	stz $00,x   ; write 0 to direct page ($2100) + x
	stz $00,x   ; some registers need to be written twice
	inx
	cpx #$0033    ; while x < $33 ($2133)
	bne -

    ; Restore zero page
    %ai16()
    lda #$0000
    tcd

    jml init