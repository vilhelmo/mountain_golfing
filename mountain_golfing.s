
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

scratch:
	.res 8

current_ppu_addr:
	.res 2

nmi_counter:
	.res 1			; Counts DOWN for each NMI.

col_pointer:
	.res 1			; Which column to update next

row_pointer:
	.res 1			; Which row to update next

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

	; Reset the row_pointer
	ldx #0
	stx row_pointer
	stx col_pointer

	jmp runloop
.endproc


; --- Increments the row_pointer with wrap-around
.proc next_row
	ldx row_pointer
	inx
	cpx #30
	bcc done
	ldx #0

	done:
		stx row_pointer
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


; --- Computes a nametable address into $00, $01
;		based on the row-index passed in through X
.proc compute_ppu_addr
	lda #$20
	sta current_ppu_addr
	lda #$00
	sta current_ppu_addr + 1
	:
		clc
		lda current_ppu_addr + 1
		adc #32
		sta current_ppu_addr + 1
		lda current_ppu_addr
		adc #0 						; plus carry
		sta current_ppu_addr
		dex
		bne :-
	
	done:
		rts
.endproc


; --- Set the PPU_ADDR based on current_ppu_addr
.proc set_ppu_addr

	;bit PPU_STATUS
	ldx current_ppu_addr
	stx PPU_ADDR
	ldx current_ppu_addr + 1
	stx PPU_ADDR
	
	done:
		rts
.endproc


; --- Set the PPU_ADDR to the column specified in X
.proc ppu_addr_X
	ldy #$20
	sty PPU_ADDR
	stx PPU_ADDR
	rts
.endproc

; --- Write the row specified by row_pointer to PPU_DATA
.proc write_current_row
	jsr set_ppu_addr

	ldx #0
	col:
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
		inx
		cpx #2; $0F; #32
		bcc col

	; Fix scroll position:
	ppu_scroll $0, 0
	
	done:
		rts
.endproc


; --- Main runloop
.proc runloop

	;wait_for_nmi
	;ldx row_pointer
	;jsr compute_ppu_addr

	;ldy row_pointer
	;wait_for_nmi
	;jsr write_current_row
	
	ldx col_pointer
	wait_for_nmi
	bit PPU_STATUS
	jsr ppu_addr_X

	ldx #0
	row:
		txa
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
		inx
		cpx #30
		bcc row

	ppu_scroll $0, 0


	;jsr next_row

	;nmi_delay 60		; 1s delay

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


