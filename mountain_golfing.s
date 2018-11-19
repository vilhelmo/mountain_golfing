
; === Includes ===

.include "nes.inc"
.include "nesdefs.inc"
.include "helpers.inc"

; === iNES header ===

; https://wiki.nesdev.com/w/index.php/INES
.segment "INESHDR"
	.byt "NES",$1A
	.byt 1 			; 1 x 16kB PRG block
	.byt 1			; 1 x 8kB CHR block
	.byt %00000001	; Flags 6 - only enable vertical mirroring
	; Rest is zero filled

; === Zero-page RAM ===

.segment "ZEROPAGE"

seed:
	.res 2			; initialize 16-bit seed to any value except 0

nmi_counter:
	.res 1			; Counts DOWN for each NMI.

col_pointer:
	.res 1			; Which column to update next

tile_selection:
	.res 32			; Which tile to use as the ground on each level

tile_position:
	.res 32			; Where on the screen each ground tile is located


; === General RAM ===

.bss

; TODO

; === Code ===

.code

; --- Main program start
.proc reset

	basic_init
	clear_wram
	ack_interrupts
	init_apu
	ppu_wakeup

	; We're in VBLANK for a short while, so do video prep now...

	load_palettes palette_data

	; Clear all 4 nametables (i.e. start at nametable 0, and clear 4 nametables):
	clear_vram 0, 4

	; Fill attribute tables for nametable 0 and 1
	lda #%00011011
	fill_attribute_table 0
	fill_attribute_table 1

	enable_vblank_nmi

	; Now wait until nmi_counter increments, to indicate the next VBLANK.
	wait_for_nmi
	; By this point, we're in the 3rd VBLANK.

	init_sprites
	trigger_ppu_dma

	; Set X & Y scrolling positions (which have ranges of 0-255 and 0-239 respectively):
	ppu_scroll 0, 0

	; Configure PPU parameters/behaviour/table selection:
	lda #VBLANK_NMI|BG_0|SPR_1|NT_0|NT_1|VRAM_DOWN
	sta PPU_CTRL

	; Turn the screen on, by activating background and sprites:
	lda #BG_ON|SPR_ON
	sta PPU_MASK

	; Wait until the screen refreshes.
	;wait_for_nmi
	; OK, at this point we know the screen is visible, ready, and waiting.

	; Write seed
	ldx #$BE
	stx seed
	ldx #$EF
	stx seed + 1

	; -- Write values to our tables
	ldx #0
	lda #$0F ; Mid screen
    : 
    sta tile_position, x
	inx
	cpx #32
	bcc :-

	ldx #0
	lda #1
	: sta tile_selection, x
	inx
	cpx #32
	bcc :-

	jsr generate_map

	; Reset the row_pointer
	ldx #0
	stx col_pointer

	jmp runloop
.endproc


; prng
;
; Returns a random 8-bit number in A (0-255), clobbers X (0).
;
; Requires a 2-byte value on the zero page called "seed".
; Initialize seed to any value except 0 before the first call to prng.
; (A seed value of 0 will cause prng to always return 0.)
;
; This is a 16-bit Galois linear feedback shift register with polynomial $002D.
; The sequence of numbers it generates will repeat after 65535 calls.
;
; Execution time is an average of 125 cycles (excluding jsr and rts)
.proc prng
	ldx #8     ; iteration count (generates 8 bits)
	lda seed+0

	:
		asl 		; shift the register
		rol seed+1
		bcc :+
		eor #$2D	; apply XOR feedback whenever a 1 bit is shifted out
	:
		dex
		bne :--
	sta seed+0
	cmp #0     		; reload flags
	rts
.endproc


; --- Generate a new map into tile_position, tile_selection
.proc generate_map
	
	jsr prng	; generate a random number into A
	ldx #$0F	; Previous tile position
	ldy #0
	:
		clc 						; clear the carry bit
		sbc #85						; subtract 255/3 = 85 = $55
		bcs down
		sbc #85						; subtract again
		bcs up

		flat:
			lda #1
			jmp next
		up:
			lda #2
			dex 					; Decrease X to go up TODO: Bounds check
			jmp next
		down:
			lda #3
			inx 					; Increase X to go down TODO: Bounds check
		next:
			sta tile_selection, y 	; Store the tile selection for this column from A
			stx tile_position, y 	; Store the tile position for this column from X
			jsr prng				; Generate the next random number into A
			ldx tile_position, y 	; Restore the previous tile position into X
		iny
		cpy #32
		bcc :-

	rts
	
.endproc


; --- Increment the col_pointer with wrap-around
.proc next_column
	ldx col_pointer
	inx
	cpx #32
	bcc done
	ldx #0

	done:
		stx col_pointer
		rts
.endproc


; --- Set the PPU_ADDR to the column specified in X
.proc ppu_addr_X
	ldy #$20
	sty PPU_ADDR
	stx PPU_ADDR
	rts
.endproc


; --- Update the column specified by X to PPU_DATA
.proc update_col_X
	ldy #0
	row:
		tya
		cmp tile_position, x
		beq tile
		bcs ground
		sky:
			lda #0
			jmp write_cell
		tile:
			lda tile_selection, x
			jmp write_cell
		ground:
			lda #4
		write_cell:
			sta PPU_DATA
		iny
		cpy #30
		bcc row
	
	done:
		rts
.endproc


; --- Main runloop
.proc runloop

	; Load the column into X
	ldx col_pointer
	
	; Wait fo vsync
	wait_for_nmi
	bit PPU_STATUS

	; Update PPU address
	jsr ppu_addr_X
	; Update nametable column
	jsr update_col_X

	; Fix scroll position:
	ppu_scroll $0, 0

	jsr next_column

	jmp runloop
.endproc


; --- NMI handler
.proc nmi_handler
	dec nmi_counter
	rti
.endproc

; --- IRQ/BRK handler
.proc irq_handler
	; Handle IRQ/BRK here.
	rti
.endproc

; === ROM data (read-only) ===

.segment "RODATA"

palette_data:
	; Background palette data
	pal $21, $09, $10, $20
	pal $00, $07, $17, $27
	pal $00, $09, $19, $29
	pal $00, $0c, $1c, $2c
	; Sprite palette data
	pal $21, $04, $14, $24	; First byte is a mirror of $3F00 above
	pal $00, $04, $14, $24
	pal $00, $04, $14, $24
	pal $00, $04, $14, $24


; === Interrupt vectors ===

.segment "VECTORS"
	.addr nmi_handler, reset, irq_handler

; === CHR-ROM Pattern table ===

.segment "PATTERN0"
	.incbin "bg.chr"

.segment "PATTERN1"
	.incbin "sprites.chr"


