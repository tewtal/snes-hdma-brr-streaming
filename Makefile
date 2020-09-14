AS=build/asar
ASFLAGS=--symbols=wla

main:
	$(AS) $(ASFLAGS) main.asm build/hdmabrr.sfc