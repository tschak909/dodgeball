;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;      _           _            _           _ _
;;;     | |         | |          | |         | | | 
;;;   __| | ___   __| | __ _  ___| |__   __ _| | |
;;;  / _` |/ _ \ / _` |/ _` |/ _ \ '_ \ / _` | | |
;;; | (_| | (_) | (_| | (_| |  __/ |_) | (_| | | |
;;;  \__,_|\___/ \__,_|\__, |\___|_.__/ \__,_|_|_|
;;;                     __/ |
;;;                    |___/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Author: Thomas Cherryhomes
;;; 	<thom.cherryhomes@gmail.com>

;;; Originally Started: 2016-06-01
;;; Suspended: 		2016-07-30
;;; Restarted:		2016-08-26
	
	PROCESSOR 6502

	include vcs.h
	include macro.h

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Constants
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

VBLANK_WAIT_TIME = 43		; 40 (37+3) vertical sync/blank lines
KERNEL_WAIT_TIME = 237		; 200 visible lines
OVERSCAN_WAIT_TIME = 27		; 22 overscan lines
	
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Variables
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	SEG.U VARS
	ORG $80

	;; Bit 7 = Ice Dodgeball or Regular Dodgeball?
GAMVAR:			ds 1	; Game Variation (0 indexed)
GAMPFMODE:		ds 1	; Game PF mode
TEMP:			ds 1	; temp variable.
CYCLE:			ds 1	; Color cycle.
GAMESTATE:		ds 1	; Game State (D7 = Game On/Off)
SCORE:			ds 2	; Two player scores (BCD)
SCOREGFX:		ds 2	; Current Score graphics for a given scanline.
DIGITONES:		ds 2	; Holder for ones digit for p1/p0
DIGITTENS:		ds 2	; holder for tens digit for p1/p0
PLAYERX0:		ds 1	; Player 0 X
PLAYERX1:		ds 1	; Player 1 X
PLAYERY0:		ds 1	; Player 0 Y
PLAYERY1:		ds 1	; Player 1 Y

	echo "----", [$FA-*]d, "bytes before end of RAM"
	
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Code Segment
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	SEG CODE
	ORG $F800		; 2K Cartridge

ColdStart:
	CLEAN_START		; defined in macro.h
	
MLOOP:	JSR VCNTRL		; Generate VSYNC; Enter VBLANK
	JSR VBLNK		; Vertical Blank routines
	JSR KERNEL		; Visible Display
	JSR OSCAN		; Overscan area
	JMP MLOOP		; Go back to main loop

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Vertical control
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

VCNTRL:	LDA #$02		; D1 = 1
	STA WSYNC		; Make sure we're at the beginning of a scanline, and...
	STA VSYNC		; Turn on the Vertical sync
	STA WSYNC		; And do it for one...
	STA WSYNC		; ...two...
	LDA #$00		; D1 = 0
	STA WSYNC		; ...three lines...
	STA VSYNC		; turn off the VSYNC
	LDA #VBLANK_WAIT_TIME	; set TIM64T to vblank wait time
	STA TIM64T		; ...
	STA WSYNC		; wait a line...
	RTS

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Vertical Blank Routines
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

VBLNK:  LDA #$02		; D1 = 1
	STA VBLANK		; Start VBLANK
	LDX GAMVAR		; Get Game Variation #
	LDA VARTBL,X		; Get variation from table
	STA GAMPFMODE		; And store it in Game PF mode
	JSR SetTIA
	JSR PrepScore		; Prepare score for kernel display.
VBLANKWait:
	LDA INTIM		; Poll the timer
	BNE VBLANKWait		; and if not ready, loop back to wait.
Sleep12:
	RTS

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Set TIA Registers
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SetTIA:	LDX #$03		; X = current pos in color table
	LDY #$03		; Y = Loop iterator
	BIT GAMPFMODE		; Get game PF mode
	BPL SetTIACheckBW	; Are we in ice dodgeball?
	LDX #$07		; Ice Dodgeball.
SetTIACheckBW:	
	LDA SWCHB		; Get Game switches
	AND #$08		; mask off the B/W switch
	BNE SetTIAColorLoop	; if color, skip to loop
SetBWBit:
	TXA			; A = X
	ORA #$08		; Turn on D3 to get B/W colors from table.
	TAX			; X = A
SetTIAColorLoop:
	LDA COLRTBL,X
	STA COLUP0,Y
	DEX
	DEY
	BPL SetTIAColorLoop
	RTS
	
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Prep score for kernel display
;;; ;; essentially a multiply by 5 routine to get
;;; ;; graphic offsets for each digit.
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PrepScore:
	ldx #$01		; Go through loop, twice for each score.
PSLoop:
	lda SCORE,x		; Get BCD score (first P1, then P0)
	and #$0F		; Mask off the upper nybble
	sta TEMP		; and store the result temporarily.
	asl			; * 2
	asl			; * 4
	adc TEMP		; and add the temp value back to get * 5
	sta DIGITONES,x		; store the result into 
	lda SCORE,x		; Get BCD score again (first P1, then P0)
	and #$F0		; This time, mask off the lower nibble.
	lsr			; / 2
	lsr			; / 4
	adc TEMP		; and now 5
	sta DIGITTENS,x		; Store the result into the tens spot for (P1, then P0)
	dex			; decrement X to do the P0 in the second pass
	bpl PSLoop		; Only return if X < 0
	rts			; if we're done? return.
	
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Visible Screen Kernel
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

KERNEL: STA WSYNC
	LDA #$00
	STA VBLANK
	LDA #KERNEL_WAIT_TIME
	STA TIM64T
	STA HMOVE
	STA CXCLR
	LDA #$02
	STA CTRLPF 		; flip on SCORE mode to get ready.
	STA WSYNC
	;;
	;; Score kernel
	;;

	ldx #$05	 ; 5 2LK lines (10 scanlines)
	;; ------------------------------------------
ScoreLoop:	
	ldy DIGITTENS		; 3	46	Load tens digit
	lda DigitGFX,y		; 5	51	Load digit graphics for tens digit 
	and #$F0		; 2	53	Mask off the top digit graphic
	sta SCOREGFX		; 3	56	store this into score graphics for P0
	ldy DIGITONES		; 3 	59	Load ones digit
	lda DigitGFX,y		; 5	64	get graphics for ones digit
	and #$0F		; 2	66	mask off high nibble
	ora SCOREGFX		; 3	69	turn on the new bits for the ones
	sta SCOREGFX		; 3	72	store the composite graphic line
	sta WSYNC		; 3	75	End of Line, next line.
	;; -----------------------------------------
	sta PF1			; 3	3	Store PF1
	ldy DIGITTENS+1		; 3 	6	get the left digit offset for score+1
	lda DigitGFX,y		; 5	11	and load the digit graphics
	and #$F0		; 2	13	Mask off upper nibble
	sta SCOREGFX+1		; 3	16	and store it.
	ldy DIGITONES+1		; 3 	19	get the ones offset for score + 1
	lda DigitGFX,y		; 5	24	load its digit graphics
	and #$0F		; 2	26	mask off the high nibble.
	ora SCOREGFX+1		; 3 	29	merge with the score graphics.
	sta SCOREGFX+1		; 3 	32	and store it into memory.
	jsr Sleep12		; 12	44	waste come cycles
	sta PF1			; 3	47	so we can update P1's score
	ldy SCOREGFX 		; 3	50	preload for next scanline
	sta WSYNC		; 3	53	wait for next scanline
	;; ----------------------------------------
	sty PF1			; 3	3	Update Playfield for score
	inc DIGITTENS		; 5 	8 	Advance for next scanline of graphic data
	inc DIGITTENS+1		; 5 	13	...
	inc DIGITONES		; 5 	18	...
	inc DIGITONES+1		; 5 	23	...
	jsr Sleep12		; 12	35	waste some cycles so we can update other side
	dex			; 2	37	decrease loop counter
	sta PF1			; 3	40	update right score (P1)
	bne ScoreLoop		; 2	42	(3 43) if x != 0, then loop back
	sta WSYNC		; 3 	45	wait for end of scanline
	;; ----------------------------------------
	stx PF1			; 3	3	x = 0 so this blanks the playfield for this line
	sta WSYNC		; 3	6	wait for scanline
	;; ----------------------------------------
	lda #$11		; 2	2	a = 0
	STA CTRLPF		; 3	5	which turns off SCORE

	LDY #$16		; Playfield counter
	STA WSYNC
	
ArenaLoop:
	LDA PF0_0,Y		; 5	5	Load PF0
	STA PF0			; 3	8	Store PF0 
	LDA PF1_0,Y		; 5	13	Load PF1
	STA PF1			; 3	16	Store PF1
	LDA PF2_0,Y		; 5	21	Load PF2
	STA PF2			; 3	24	Store PF2
	STA WSYNC		; ... some padding to make it all look right, for now.
	STA WSYNC
	STA WSYNC
	STA WSYNC
	STA WSYNC
	STA WSYNC
	STA WSYNC
	STA WSYNC
	DEY			; Decrement playfield counter, and 
	BPL ArenaLoop		; loop around. 
	
	LDA #$00
	STA PF0
	STA PF1
	STA PF2
	
KERNWait:
	LDA INTIM
	BNE KERNWait
	RTS

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Overscan Kernel
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

OSCAN:	LDA #$02
	STA WSYNC
	STA VBLANK
	LDA #OVERSCAN_WAIT_TIME
	STA TIM64T
OSCANWait:
	LDA INTIM
	BNE OSCANWait
	RTS

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; PosObject subroutine
;;; ;; A = X Position of Object
;;; ;; X = Which object to position
;;; ;;
;;; ;; Position is 0-159 for players
;;; ;;             1-160 for missiles/ball
;;; ;;
;;; ;; Object is 0 = Player 0
;;; ;;	 	 1 = Player 1
;;; ;; 		 2 = Missile 0
;;; ;;		 3 = Missile 1
;;; ;;		 4 = Ball
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PosObject:
	sec
	sta WSYNC		; Ensure that we are at the beginning of a scanline
DivideLoop:
	sbc #$0F		; 2	2 - Each time through this loop, takes 5 cycles
	bcs DivideLoop		; 2	4 - Or the same amount of time it takes to do 15 pixels
	eor #$07		; 2	6 - EOR and ASL statements convert modulus to the
	asl			; 2	8 - horizontal motion position register value
	asl			; 2	10
	asl			; 2 	12
	asl			; 2	14
	sta.wx HMP0,X		; 5	19 - Store fine tuning of X (wx used bc it takes 5 cycles)
	sta RESP0,X		; 4	23 - Set X position of object
	rts			; 6 	29 - and done.
	
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Graphics and Tables
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	if (* & $FF)
	  echo "----", [(>.+1)*256 - .]d, "bytes free before DigitGFX"
	  align 256
	endif

	;;
	;; digit graphics
	;;

	align 256
DigitGFX:
        .byte %01110111
        .byte %01010101
        .byte %01010101
        .byte %01010101
        .byte %01110111
        
        .byte %00010001
        .byte %00010001
        .byte %00010001
        .byte %00010001        
        .byte %00010001
        
        .byte %01110111
        .byte %00010001
        .byte %01110111
        .byte %01000100
        .byte %01110111
        
        .byte %01110111
        .byte %00010001
        .byte %00110011
        .byte %00010001
        .byte %01110111
        
        .byte %01010101
        .byte %01010101
        .byte %01110111
        .byte %00010001
        .byte %00010001
        
        .byte %01110111
        .byte %01000100
        .byte %01110111
        .byte %00010001
        .byte %01110111
           
        .byte %01110111
        .byte %01000100
        .byte %01110111
        .byte %01010101
        .byte %01110111
        
        .byte %01110111
        .byte %00010001
        .byte %00010001
        .byte %00010001
        .byte %00010001
        
        .byte %01110111
        .byte %01010101
        .byte %01110111
        .byte %01010101
        .byte %01110111
        
        .byte %01110111
        .byte %01010101
        .byte %01110111
        .byte %00010001
        .byte %01110111

GRP0_0:
	.byte %00000000
	.byte %00000000
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00000000
	.byte %00000000
	
	;;
	;; Playfield Data
	;;
PF0_0:
	.byte %11110000
        .byte %00010000
        .byte %00010000
        .byte %00010000
        .byte %00010000
        .byte %00010000
        .byte %00010000
        .byte %00010000
        .byte %00010000
        .byte %00010000
        .byte %00010000
        .byte %00010000
        .byte %00010000
        .byte %00010000
        .byte %00010000
        .byte %00010000
        .byte %00010000
        .byte %00010000
        .byte %00010000
        .byte %00010000
        .byte %00010000
        .byte %00010000
        .byte %11110000       

PF1_0:
	.byte %11111111
        .byte %00000000
        .byte %00000000
        .byte %00111000
        .byte %00000000
        .byte %00000000
        .byte %00000000
        .byte %11000000
        .byte %01000000
        .byte %01000000
        .byte %01000001
        .byte %01000001
        .byte %01000000
        .byte %01000000
        .byte %11000000
        .byte %00000000
        .byte %00000000
        .byte %00000000
        .byte %00111000
        .byte %00000000
        .byte %00000000
        .byte %00000000	
        .byte %11111111       

PF2_0:
        .byte %11111111
        .byte %10000000
        .byte %00000000
        .byte %00000000
        .byte %00000000
        .byte %00000000
        .byte %00011100
        .byte %00000100
        .byte %00000000
        .byte %00000000
        .byte %00000000
        .byte %00000000
        .byte %00000000
        .byte %00000000
        .byte %00000100
        .byte %00011100
        .byte %00000000
        .byte %00000000
        .byte %00000000
        .byte %00000000
        .byte %00000000
        .byte %10000000
        .byte %11111111       
	
	;;
	;; Game Variation Table
	;;
VARTBL:	.byte %00000000		; GAME 1 - Dodgeball
	.byte %10000000		; GAME 2 - Ice Dodgeball
	.byte %11111111		; End of Variation Table

COLRTBL:
	.byte $DA, $8A, $2C, $32 ; Regular Dodgeball
	.byte $3A, $DA, $9C, $80 ; Ice Dodgeball
	.byte $0E, $00, $04, $08 ; B/W Regular Dodgeball
	.byte $0E, $00, $04, $08 ; B/W Ice Dodgeball
	
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; System Vectors
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	if (* & $FF)
		echo "----", [$FFFA-*]d, "bytes free before end of cart."
		align 256
	endif
	
	ORG $FFFA
	.WORD ColdStart
	.WORD ColdStart
	.WORD ColdStart
