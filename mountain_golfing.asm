
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

setup_complete:
	.res 1			; Set to non-zero when setup has completed

col_pointer:
	.res 1			; Which column to update next

col_ppu_pointer:
	.res 1			; Which column we've written to the PPU

scroll:
	.res 1			; Current scroll position within the current nametable

col_nametable:
	.res 1			; Current nametable for writing

scroll_nametable:
	.res 1			; Current nametable for display

ball_pos:
	.res 4			; Ball position X(lo/hi), Y(lo/hi)

ball_vel:
	.res 4			; Ball velocity X(lo/hi), Y(lo/hi)

cursor:
	.res 2			; Cursor position, offset from the ball position

shot_counter:
	.res 1			; Keep track of the number of shots made

buttons:
	.res 8			; Storing data that's read from the controller

level_pointer:
	.res 2			; Which level to use next
	.res 1			; Level index (for now 0-$FF)
	.res 1			; How many tiles wide the level is
	.res 1			; How many pixels wide the level is

level_size_pointer:
	.res 2			; pointer to level size data

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

	lda #0
	sta setup_complete
	jsr setup_data

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

	jsr setup_graphics

	enable_vblank_nmi

	; Now wait until nmi_counter increments, to indicate the next VBLANK.
	wait_for_nmi
	; By this point, we're in the 3rd VBLANK.

	init_sprites
	trigger_ppu_dma

	; Set X & Y scrolling positions (which have ranges of 0-255 and 0-239 respectively):
	ppu_scroll 0, 0

	; Configure PPU parameters/behaviour/table selection:
	lda #VBLANK_NMI|BG_0|SPR_1|VRAM_DOWN|NT_0
	; Select correct nametable for bit 0
	ora scroll_nametable
	sta PPU_CTRL

	; Turn the screen on, by activating background and sprites:
	lda #BG_ON|SPR_ON|SHOW_BG_LHS|SHOW_SPR_LHS
	sta PPU_MASK
	
	lda #1
	sta setup_complete
	; Wait until the screen refreshes.
	wait_for_nmi
	; OK, at this point we know the screen is visible, ready, and waiting.

	jmp runloop
.endproc


.proc setup_data
	; Reset the col_pointer
	ldx #0
	stx col_pointer
	stx col_ppu_pointer

	; Reset the scroll
	ldx #0
	stx scroll

	; Reset the ball and cursor positions
	ldx #0
	stx ball_pos
	stx ball_pos + 1
	stx ball_pos + 2
	stx ball_pos + 3
	ldx #0
	stx ball_vel		; Velocity x low
	stx ball_vel + 1	; Velocity x high
	stx ball_vel + 2	; Y low
	stx ball_vel + 3	; Y high
	
	ldx #10			; Cursor offset X
	stx cursor
	ldx #$EF		; Cursor offset Y
	stx cursor + 1

	; Reset the shot counter
	ldx #0
	stx shot_counter
		
	; Reset the current nametable
	ldx #0
	stx scroll_nametable
	ldx #1
	stx col_nametable
	
	; Reset the level pointer to the first level
	ldx #<levels
	stx level_pointer
	ldx #>levels
	stx level_pointer + 1
	; Reset the level index to 0
	ldx #0
	stx level_pointer + 2

	ldx #<level_sizes
	stx level_size_pointer
	ldx #>level_sizes
	stx level_size_pointer + 1

	; Load the first level into the cache
	jsr load_full_level
	jsr load_first_screen

	rts
.endproc

.proc setup_graphics
	; Write the first level to the PPU
	ppu_addr $2000	; write into the first nametable
	ldy #0
	row:
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
			cpx #32; level_pointer + 3 
			bne col
		iny
		cpy #30
		bne row

	rts
.endproc



.macro add_wrap_32  memory
	.scope
	clc
	adc memory 	; Add the value to A
	sec		; Set the carry before subtraction
	sbc #32		; Subtract 32
	bcs done	; If carry is still set then we wrapped around 32 (still positive)
	; Carry not set, so value became negative, or original value was < 32.
	; Need to add 32 back and clear the carry flag afterwards.
	adc #32
	clc
	
	done:
	.endscope
.endmacro


; --- Increase the value in X, but wrapping around at 32
; 		The zero bit, and carry flag will be set if wrapping around
.macro inx_wrap_32
	.scope
	inx
	cpx #32
	bcc done

	; Wrap around
	ldx #0

	done:
	.endscope
.endmacro

.proc load_first_screen

	ldx col_pointer
	; Skip next level size, just fill the data
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

	ldx #0
	stx col_pointer

	; Move the level pointer to the next level
	clc
	tya
	adc level_pointer
	sta level_pointer
	lda level_pointer + 1
	adc #0
	sta level_pointer + 1
	
	rts

.endproc

.proc load_full_level
	
	; We only have 4 levels of data at the moment
	ldy level_pointer + 2
	cpy #4
	bcs done
	
	; Store the level size, in tiles
	lda (level_size_pointer), y
	iny
	sta level_pointer + 3
	
	; Compute the level width in pixels
	tax
	lda #0
	:
		adc #8	
		dex
		bne :-
	sta level_pointer + 4
		
	; Restore the level size in tiles to A
	lda level_pointer + 3	
	; Load the current col_pointer
	ldx col_pointer
	
	; Compute the end column of this level
	add_wrap_32 col_pointer
	; Store the next column pointer
	sta col_pointer
	
	
	ldy #0
	:
		lda (level_pointer), y
		sta tile_position, x
		iny 
		
		lda (level_pointer), y
		sta tile_selection, x
		iny
		
		inx_wrap_32
		cpx col_pointer ;level_pointer + 3; #32
		bne :-
	
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

	done:
		rts
.endproc

; --- Set the PPU_ADDR to the column specified in X and the inverse of the nametable specified in Y
.proc ppu_addr_colX_ntY
	bit PPU_STATUS
	cpy #0
	bne nt1
 			; write to nametable 1 if we're currently in nametable 0
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


; --- Update the next unwritten column to PPU_DATA
.proc write_col_ppu
	ldx col_ppu_pointer
	; Skip writing if we're caught up
	cpx col_pointer
	beq done

	ldy col_nametable
	jsr ppu_addr_colX_ntY

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
	
	inx_wrap_32
	bcc finalize
	
	; Wrapped around, time to swap nametables (0 <-> 1)
	lda col_nametable
	eor #$01
	sta col_nametable

	finalize:
		stx col_ppu_pointer
	done:
		rts
.endproc

.macro wait_for_data_upload
	ldx col_pointer
	:
		cpx col_ppu_pointer
		bne :-
.endmacro

.proc scroll_right
	; Scroll right one pixel
	inc scroll
	bne done

	; Swap nametable (0 <-> 1)
	lda scroll_nametable
	eor #$01
	sta scroll_nametable

	done:
		rts
.endproc

.macro read_button button_idx
	lda APU_PAD1
	and #%00000001
	sta buttons + button_idx
.endmacro


.proc read_buttons
	; Latch buttons
	lda #$01
	sta APU_PAD1
	lda #$00
	sta APU_PAD1

	; A
	read_button 0

	; B
	read_button 1

	; Select
	read_button 2
	
	; Start
	read_button 3
	
	; Up
	read_button 4

	; Down
	read_button 5
	
	; Left
	read_button 6
	
	; Right
	read_button 7

	rts
.endproc


.macro update_sprites
	; Update ball position first
	lda ball_pos + 3	; Y position, high bit
	sta OAM_RAM + 0 	; Ball Y
	adc cursor + 1
	sta OAM_RAM + 4 	; Cursor Y

	lda #1			; ball uses sprite 1
	sta OAM_RAM + 1
	lda #0			; cursor uses sprite 0
	sta OAM_RAM + 5

	lda #%00000000 
	sta OAM_RAM + 2
	sta OAM_RAM + 6

	lda ball_pos + 1	; Ball X position, high bit
	sta OAM_RAM + 3
	adc cursor + 0		; Cursor X
	sta OAM_RAM + 7
.endmacro

.macro physics_step
	.scope
	clc
	lda ball_vel + 2	; Ball Y velocity, low bit
	; Add gravity
	adc #10			; ? Figure out good constant
	sta ball_vel + 2	; Update Y velocity low bit
	lda ball_vel + 3
	adc #0
	sta ball_vel + 3	; Update Y velocity high bit
	
	add_eq16 ball_pos + 2, ball_vel + 2
	
	add_eq16 ball_pos, ball_vel

	lda #0
	; Reset if carry flag was set (wrap around screen in X)
	bcc done

	reset:
		lda #1
	done:
	.endscope
.endmacro

; --- Main runloop
.proc runloop
	
	jmp reset_level
		
	aim:
		wait_for_nmi
		jsr read_buttons
		up:
			lda buttons + 4
			cmp #1
			bne down
			dec cursor + 1
		down:
			lda buttons + 5
			cmp #1
			bne left
			inc cursor + 1	
		left:
			lda buttons + 6
			cmp #1
			bne right
			dec cursor
		right:
			lda buttons + 7
			cmp #1
			bne next
			inc cursor
		next:
			update_sprites
			lda buttons + 0
			cmp #1
			bne aim
	
	.proc perform_shot
		inc shot_counter
		lda cursor + 0
		tax
		is_neg
		bmi neg_x
		pos_x:
			txa
			Repeat 4, lsr
			sta ball_vel + 1
			jmp lower_byte_x
		neg_x:
			txa
			eor #$FF
			Repeat 4, lsr
			eor #$FF
			sta ball_vel + 1
		lower_byte_x:
			txa
			Repeat 4, asl
			sta ball_vel + 0	; Ball velocity X

		lda cursor + 1
		tax
		is_neg
		bmi neg_y
		pos_y:
			txa
			Repeat 4, lsr
			sta ball_vel + 3
			jmp lower_byte_y
		neg_y:
			txa
			eor #$FF	; Flip bits
			Repeat 4, lsr
			eor #$FF
			sta ball_vel + 3
		lower_byte_y:
			txa
			Repeat 4, asl
			sta ball_vel + 2
	.endproc

	simulate:
		update_sprites
		wait_for_nmi
		physics_step
		cmp #2			; Level complete
		beq goto_next_level
		cmp #1			; Level failed
		beq reset_level
		jmp simulate		; Continue sim	
	
	reset_level:
		; Reset ball velocty
		lda #0
		sta ball_vel + 0
		sta ball_vel + 1
		sta ball_vel + 2
		sta ball_vel + 3
		; Reset ball position
		lda #8
		sta ball_pos + 1	; Reset X pos, high bit
		lda #0
		sta ball_pos + 0	; Reset X pos, low bit
		; Reset position in Y
		ldy col_pointer
		iny
		lda #0
		ldx #8
		:
			adc tile_position, y
			dex
			bne :-
		sta ball_pos + 3	; Reset Y pos, high bit
		lda #0
		sta ball_pos + 2	; Reset Y pos, low bit
		jmp aim
				
	goto_next_level:	
		jsr load_full_level
		wait_for_data_upload

		ldx level_pointer + 4
		r:
			jsr scroll_right
			wait_for_nmi
			dex
			bne r
	
	jmp aim

.endproc


; --- Vsync graphics updates should happen here
.proc graphics_update

	; Update data for one column (X) of the nametable if needed
	jsr write_col_ppu

	; clean up PPU address registers
	lda #$00
  	sta PPU_ADDR
  	sta PPU_ADDR

	; Set scroll position:
	ppu_scroll_x scroll

	; Update sprites	
	trigger_ppu_dma

	; This is the PPU clean up section, so rendering the next frame starts properly.
	lda #VBLANK_NMI|BG_0|SPR_1|VRAM_DOWN|NT_0
	; Select correct nametable for bit 0
	ora scroll_nametable
	sta PPU_CTRL
	  
	lda #BG_ON|SPR_ON|SHOW_BG_LHS|SHOW_SPR_LHS
	sta PPU_MASK
	
	rts
.endproc

; --- NMI handler
.proc nmi_handler
	; Push registers to the stack
	pha
	txa
	pha
	tya
	pha
	php

	lda setup_complete
	cmp #0
	beq done
	jsr graphics_update

	done:
		; Restore the registers
		plp
		pla
		tay
		pla
		tax
		pla
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

level_sizes:
	.incbin "level_sizes.bin"

; === Interrupt vectors ===

.segment "VECTORS"
	.addr nmi_handler, reset, irq_handler

; === CHR-ROM Pattern table ===

.segment "PATTERN0"
	.incbin "bg.chr"

.segment "PATTERN1"
	.incbin "sprites.chr"


