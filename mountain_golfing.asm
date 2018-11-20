
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
	.byt %1			; Flags 6 - only enable vertical mirroring
	; Rest is zero filled

; === Zero-page RAM ===

.segment "ZEROPAGE"

nmi_counter:
	.res 1			; Counts DOWN for each NMI.

col_pointer:
	.res 1			; Which column to update next

scroll:
	.res 1			; Current scroll position within the current nametable

nametable:
	.res 1			; Current nametable (0 or 1)

level_pointer:
	.res 2			; Which level to use next
	.res 1			; Level index (for now 0-$FF)

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

	jsr setup_data

	enable_vblank_nmi

	; Now wait until nmi_counter increments, to indicate the next VBLANK.
	wait_for_nmi
	; By this point, we're in the 3rd VBLANK.

	init_sprites
	trigger_ppu_dma

	; Set X & Y scrolling positions (which have ranges of 0-255 and 0-239 respectively):
	ppu_scroll 0, 0

	; Configure PPU parameters/behaviour/table selection:
	lda #VBLANK_NMI|BG_0|SPR_1|VRAM_DOWN|NT_1
	sta PPU_CTRL

	; Turn the screen on, by activating background and sprites:
	lda #BG_ON|SPR_ON|SHOW_BG_LHS|SHOW_SPR_LHS
	sta PPU_MASK

	; Wait until the screen refreshes.
	wait_for_nmi
	; OK, at this point we know the screen is visible, ready, and waiting.


	; Reset the col_pointer
	ldx #0
	stx col_pointer

	; Reset the scroll and nametable
	stx scroll
	ldx #1
	stx nametable

	; Reset the level pointer to the first level
	ldx #<levels
	stx level_pointer
	ldx #>levels
	stx level_pointer + 1
	; Reset the level index
	ldx #0
	stx level_pointer + 2

	
	; Load the first level
	jsr load_next_level

	jmp runloop
.endproc


.proc setup_data

	done:
		rts
.endproc

.proc load_next_level
	
	ldx #0
	ldy #0
	:
		lda (level_pointer), y
		sta tile_position, x
		iny 
		
		lda (level_pointer), y
		sta tile_selection, x
		iny
		
		inx
		cpx #32
		bcc :-

	; Move the level pointer to the next level
	clc
	tya
	adc level_pointer
	sta level_pointer
	lda level_pointer + 1
	adc #0
	sta level_pointer + 1

	; Update the level index
	inc level_pointer + 2

	rts

.endproc


; --- Increment the col_pointer with wrap-around
.proc next_column
	ldx col_pointer
	inx
	cpx #32
	bcc done
	ldx #0
	; wrapped around; next level
	inc level_pointer + 2

	done:
		stx col_pointer
		rts
.endproc


; --- Load next column from level data
.proc load_level_column
	lda level_pointer + 2
	cmp #2
	bcs done

	ldx col_pointer
	ldy #0
	
	lda (level_pointer), y
	sta tile_position, x
	iny
	
	lda (level_pointer), y
	sta tile_selection, x
	iny

	; Increment the level pointer by two (value in Y)
	clc
	tya
	adc level_pointer
	sta level_pointer
	lda level_pointer + 1
	adc #0
	sta level_pointer + 1

	done:
		rts
.endproc


; --- Set the PPU_ADDR to the column specified in X and the inverse of the nametable specified in Y
.proc ppu_addr_X
	bit PPU_STATUS
	cpy #0
	beq nt1 			; write to nametable 1 if we're currently in nametable 0
	nt0:
		ldy #$20
		jmp write
	nt1:
		ldy #$24

	write:
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


.proc scroll_right
	; Scroll right one pixel
	inc scroll
	bne done

	; Swap nametable (0 <-> 1)
	lda nametable
	eor #$01
	sta nametable

	done:
		rts
.endproc


; --- Main runloop
.proc runloop

	jmp runloop
.endproc


; --- Vsync graphics updates should happen here
.proc graphics_update

	; Load the current column into X
	ldx col_pointer
	; Load the current nametable into Y
	ldy nametable

	; Update PPU address to the column specified by X
	; and nametable specified by Y
	jsr ppu_addr_X
	
	; Update data for one column (X) of the nametable
	jsr update_col_X

	; clean up PPU address registers
	lda #$00
  	sta PPU_ADDR
  	sta PPU_ADDR

	; Set scroll position:
	ppu_scroll_x scroll
	
	; This is the PPU clean up section, so rendering the next frame starts properly.
	lda #VBLANK_NMI|BG_0|SPR_1|VRAM_DOWN|NT_0
	; Select correct nametable for bit 0
	ora nametable
	sta PPU_CTRL
	  
	lda #BG_ON|SPR_ON|SHOW_BG_LHS|SHOW_SPR_LHS
	sta PPU_MASK
	
	; Load the next slice of level data into the column we just wrote
	jsr load_level_column

	; Process next column next frame
	jsr next_column

	; Update scroll for next frame
	jsr scroll_right

	rts
.endproc

; --- NMI handler
.proc nmi_handler
	
	jsr graphics_update

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

levels:
	.incbin "levels.bin"

; === Interrupt vectors ===

.segment "VECTORS"
	.addr nmi_handler, reset, irq_handler

; === CHR-ROM Pattern table ===

.segment "PATTERN0"
	.incbin "bg.chr"

.segment "PATTERN1"
	.incbin "sprites.chr"

