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
FIRST_ARENA_SCANLINE = 49	; First Arena Scanline
KERNEL_SCANLINE_BIAS = 6	; scanline bias
KERNEL_SCANLINE_MAX = 232	; Max kernel scanline (minus overscan)
PLAYFIELD_HEIGHT = 91

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Variables
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	SEG.U VARS
	ORG $80

	;; Bit 7 = Ice Dodgeball or Regular Dodgeball
	
	;; 
	;; global variables/state
	;; 
FRAME			ds 1	; Frame counter
SCANLINE:		ds 1	; temporarily stored scanline
STOREDSTACKPTR:		ds 1	; Stored SP for kernel.
GAMVAR:			ds 1	; Game Variation (0 indexed)
GAMPFMODE:		ds 1	; Game PF mode
TEMP:			ds 1	; temp variable.
TEMP1:			ds 1	; Temp Variable 2.
CYCLE:			ds 1	; Color cycle.
GAMESTATE:		ds 1	; Game State (00 = Not Playing, FF = Game on)

	;;
	;; score variables
	;; 
SCORE:			ds 2	; Two player scores (BCD)
SCOREGFX:		ds 2	; Current Score graphics for a given scanline.
DIGITONES:		ds 2	; Holder for ones digit for p1/p0
DIGITTENS:		ds 2	; holder for tens digit for p1/p0

	;;
	;; position variables
	;; 
PLAYERX0:		ds 1	; Player 0 X
PLAYERX1:		ds 1	; Player 1 X
BALLX0:			ds 1	; Ball 0 X
BALLX1:			ds 1	; Ball 1 X
BALLX2:			ds 1	; Ball 2 X
PLAYERY0:		ds 1	; Player 0 Y
PLAYERY1:		ds 1	; Player 1 Y
BALLY0:			ds 1	; Ball 0 Y (M0)
BALLY1:			ds 1	; Ball 1 Y (M1)
BALLY2:			ds 1	; Ball 2 Y (BL) Computer Ball

	;;
	;; position variables (last known good)
	;; 
PLAYERX0S:		ds 1	; Player 0 X
PLAYERX1S:		ds 1	; Player 1 X
BALLX0S:		ds 1	; Ball 0 X
BALLX1S:		ds 1	; Ball 1 X
BALLX2S:		ds 1	; Ball 2 X
PLAYERY0S:		ds 1	; Player 0 Y
PLAYERY1S:		ds 1	; Player 1 Y
BALLY0S:		ds 1	; Ball 0 Y (M0)
BALLY1S:		ds 1	; Ball 1 Y (M1)
BALLY2S:		ds 1	; Ball 2 Y (BL) Computer Ball

BALLD0:			ds 1	; Ball 0 Direction
	
	echo "----", [$FA-*]d, "bytes before end of RAM"
	
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Code Segment
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	SEG CODE
	ORG $F800		; 2K Cartridge

ColdStart:
	CLEAN_START		; defined in macro.h
	
	JSR GameReset		; Call Game Reset after cold start.

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Main Loop
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

MLOOP:	JSR VCNTRL		; Generate VSYNC; Enter VBLANK
	JSR VBLNK		; Vertical Blank routines
	JSR KERNEL		; Visible Display
	JSR OSCAN		; Overscan area
	JMP MLOOP		; Go back to main loop

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Vertical control
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

VCNTRL: SUBROUTINE
	INC FRAME		; Increment master frame counter.
	STA HMCLR		; Clear motion registers.
	LDA #$02		; D1 = 1
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

VBLNK:	SUBROUTINE
	LDA #$02		; D1 = 1
	STA VBLANK		; Start VBLANK
	LDX GAMVAR		; Get Game Variation #
	LDA VARTBL,X		; Get variation from table	
	STA GAMPFMODE		; And store it in Game PF mode
	JSR SetTIA		; Set TIA Registers
	JSR ProcessJoysticks	; Process Joysticks
	LDX #$01
	JSR BallDirection	; Compute ball direction
	JSR PositionObjects	; And Position Objects
	JSR PrepScore		; Prepare score for kernel display.
.waitUntilDone:
	LDA INTIM		; Poll the timer
	BNE .waitUntilDone	; and if not ready, loop back to wait.
	RTS
	
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Set TIA Registers
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SetTIA:	SUBROUTINE
	LDA #$01		; XXX TEMPORARY
	STA VDELP0		; XXX TEMPORARY (puts both players on same line)
	LDA #$10
	STA NUSIZ0
	STA NUSIZ1
	LDX #$03		; X = current pos in color table
	LDY #$03		; Y = Loop iterator
	BIT GAMPFMODE		; Get game PF mode
	BPL .checkBW		; Are we in ice dodgeball?
	LDX #$07		; Ice Dodgeball.
.checkBW:	
	LDA SWCHB		; Get Game switches
	AND #$08		; mask off the B/W switch
	BNE .setNextColor	; if color, skip to loop
.setBW:
	TXA			; A = X
	ORA #$08		; Turn on D3 to get B/W colors from table.
	TAX			; X = A
.setNextColor:		
	LDA COLRTBL,X		; Get next entry from color table
	STA COLUP0,Y		; set it.
	DEX			; decrement table index
	DEY			; decrement register index
	BPL .setNextColor	; loop around, until done
	RTS			; then return.

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Process Joysticks
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ProcessJoysticks: SUBROUTINE
	LDX #$01
	LDA SWCHA
	
up:	LSR
	BCS down
	DEC PLAYERY0,X
	
down:	LSR
	BCS left
	INC PLAYERY0,X

left:	LSR
	BCS right
	DEC PLAYERX0,X

right:	LSR
	BCS next
	INC PLAYERX0,X

next:	DEX
	BPL up
	
	RTS
	
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Position Objects
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	;;
	;; Do X positions
	;; 
PositionObjects: SUBROUTINE
	LDX #$04
XLoop:	LDA PLAYERX0,X
	JSR PosObject
	DEX
	BPL XLoop
	STA WSYNC
	STA HMOVE
	RTS			; otherwise, return.
	
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Prep score for kernel display
;;; ;; essentially a multiply by 5 routine to get
;;; ;; graphic offsets for each digit.
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PrepScore: SUBROUTINE
	ldx #$01		; Go through loop, twice for each score.
.loop:
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
	bpl .loop		; Only return if X < 0
	rts			; if we're done? return.
	
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; GameReset - Reset players to initial pos
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

GameReset: SUBROUTINE
	LDA #$08
	STA PLAYERX0
	LDA #$8F
	STA PLAYERX1
	LDA #$90
	STA PLAYERY0
	STA PLAYERY1

	LDA #$3F
	STA BALLX1
	STA BALLY1
	
	RTS			; and return.
	
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Visible Screen Kernel
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	;;
	;; after the WSYNC, we will be on Scanline 36
	;; 
KERNEL:	SUBROUTINE
	STA WSYNC
	LDA #$00
	STA VBLANK
	LDA #KERNEL_WAIT_TIME
	STA TIM64T
	STA CXCLR
	LDA #$02
	STA CTRLPF 		; flip on SCORE mode to get ready.
	;;
	;; Score kernel
	;;
	ldx #$05	 ; 5 2LK lines (10 scanlines)
	;; ------------------------------------------
.renderScore:	
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
	bne .renderScore	; 2	42	(3 43) if x != 0, then loop back
	sta WSYNC		; 3 	45	wait for end of scanline
	;; ----------------------------------------
	stx PF1			; 3	3	x = 0 so this blanks the playfield for this line
	sta WSYNC		; 3	6	wait for scanline
	;; ----------------------------------------
	lda #$11		; 2	2	a = 0
	STA CTRLPF		; 3	5	which turns off SCORE

	LDA #FIRST_ARENA_SCANLINE
	STA SCANLINE

.saveStack:
	TSX
	STX STOREDSTACKPTR
	STA WSYNC

.mainLoop:			; X	33
	LDX #ENABL		; 2	35
	TXS			; 2	37
	LDA SCANLINE		; 3	40
	LSR			; 2	42
	LSR			; 2	44
	LSR			; 2	46
	TAY			; 2	48
	LDA PLAYERY0		; 3	51
	SBC SCANLINE		; 3	54
	AND #$FE		; 2	56
	TAX			; 2	58
	AND #$F0		; 2 	60
	BEQ .doP0		; 2	62
	LDA #$00		; 2	64
	BEQ .noP0		; 3 jmp 67
.doP0:				; X	63
	LDA GRP,X		; 4	67
.noP0:				; X	67
	LDX SCANLINE		; 3	70
	STA WSYNC		; 3	--
	STA GRP0		; 3	3
	TXA			; 2	5
	EOR BALLY2		; 3	8
	AND #$FE		; 2	10
	PHP			; 3	13
	LDA PF0_0-6,Y		; 4	17
	STA PF0			; 3	20
	LDA PF1_0-6,Y		; 4	24
	STA PF1			; 3	27
	LDA PF2_0-6,Y		; 4	31
	STA PF2			; 3	34
	STY TEMP		; 3	37
	INX			; 2	39
	LDA PLAYERY1		; 3	42
	SBC SCANLINE		; 3	45
	AND #$FE		; 2 	47
	TAY			; 2 	49
	AND #$F0		; 2	51
	BEQ .doP1		; 2	53
	LDA #$00		; 2	55
	BEQ .noP1		; 3	58

.doP1:				; X	54
	LDA GRP,Y		; 4	58
.noP1:
	STA WSYNC		; 3	--
	STA GRP1		; 3	3
	TXA			; 2	5
	EOR BALLY1		; 3	8
	AND #$FE		; 2	10
	PHP			; 3	13
	TXA			; 2	15
	EOR BALLY0		; 3	18
	AND #$FE		; 2	20
	PHP			; 3	23
	INX			; 2	25
	STX SCANLINE		; 3	28
	CPX #232		; 2	30
	BCC .mainLoop
	
.cleanup:
	STA WSYNC
	LDX STOREDSTACKPTR
	TXS
	LDA #$00
	STA PF0
	STA PF1
	STA PF2
	STA GRP0
	STA GRP1
	STA ENABL
	STA ENAM0
	STA ENAM1
	
.waitUntilDone:
	LDA INTIM
	BNE .waitUntilDone
	RTS

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Overscan Period
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

OSCAN:	SUBROUTINE
	LDA #$02
	STA WSYNC
	STA VBLANK
	LDA #OVERSCAN_WAIT_TIME
	STA TIM64T
	JSR ProcessCollisions
.waitUntilDone:
	LDA INTIM
	BNE .waitUntilDone
	RTS

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; ProcessCollisions
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ProcessCollisions: SUBROUTINE
	;;
	;; Handle Player to Playfield collisions
	;; 
	LDX #$01
PLtoPF:
	LDA CXP0FB,X
	BPL nextPLtoPF
PLtoPFCollide:
	JSR RecallPlayerPosition
nextPLtoPF:
	JSR SavePlayerPosition
	DEX
	BPL PLtoPF
	RTS

	
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; RecallPosition
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
RecallPlayerPosition:	SUBROUTINE
	LDA PLAYERX0S,X
	STA PLAYERX0,X
	LDA PLAYERY0S,X
	STA PLAYERY0,X
	RTS

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; SavePosition
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SavePlayerPosition:	SUBROUTINE
	LDA PLAYERX0,X
	STA PLAYERX0S,X
	LDA PLAYERY0,X
	STA PLAYERY0S,X	
	RTS

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; BallDirection
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BallDirection:	SUBROUTINE

	LDA BALLD0
	
D01:	CMP #$01
	BNE D02
	TAY
	LDA BALLX0,X
	CLC
	ADC #$02
	STA BALLX0,X
	TYA

D02:	CMP #$02
	BNE D03
	TAY
	LDA BALLX0,X
	CLC
	ADC #$02
	STA BALLX0,X
	LDA BALLY0,X
	SEC
	SBC #$01
	STA BALLY0,X
	TYA

D03:	CMP #$03
	BNE D04
	TAY
	LDA BALLX0,X
	CLC
	ADC #$02
	STA BALLX0,X
	LDA BALLY0,X
	SEC
	SBC #$02
	STA BALLY0,X
	TYA

D04:	CMP #$04
	BNE D05
	TAY
	LDA BALLY0,X
	SBC #$02
	STA BALLY0,X
	TYA

D05:	CMP #$05
	BNE D06
	TAY
	LDA BALLX0,X
	SEC
	SBC #$01
	STA BALLX0,X
	LDA BALLY0,X
	SEC
	SBC #$02
	STA BALLY0,X
	TYA

D06:	CMP #$06
	BNE D07
	TAY
	LDA BALLX0,X
	SEC
	SBC #$02
	STA BALLX0,X
	LDA BALLY0,X
	SEC
	SBC #$02
	STA BALLY0,X
	TYA

D07:	CMP #$07
	BNE D08
	TAY
	LDA BALLX0,X
	SEC
	SBC #$02
	STA BALLX0,X
	LDA BALLY0,X
	SEC
	SBC #$01
	STA BALLY0,X
	TYA

D08:	CMP #$08
	BNE D09
	TAY
	LDA BALLX0,X
	SEC
	SBC #$01
	STA BALLX0,X
	TYA

D09:	CMP #$09
	BNE D0A
	TAY
	LDA BALLX0,X
	SEC
	SBC #$02
	STA BALLX0,X
	LDA BALLY0,X
	CLC
	ADC #$01
	STA BALLY0,X
	TYA

D0A:	CMP #$0A
	BNE D0B
	TAY
	LDA BALLX0,X
	SEC
	SBC #$02
	STA BALLX0,X
	LDA BALLY0,X
	CLC
	ADC #$02
	STA BALLY0,X
	TYA

D0B:	CMP #$0B
	BNE D0C
	TAY
	LDA BALLX0,X
	SEC
	SBC #$01
	STA BALLX0,X
	LDA BALLY0,X
	CLC
	ADC #$02
	STA BALLY0,X
	TYA

D0C:	CMP #$0C
	BNE D0D
	TAY
	LDA BALLY0,X
	CLC
	ADC #$02
	STA BALLY0,X
	TYA

D0D:	CMP #$0D
	BNE D0E
	TAY
	LDA BALLX0,X
	CLC
	ADC #$01
	STA BALLX0,X
	LDA BALLY0,X
	CLC
	ADC #$02
	STA BALLY0,X
	TYA

D0E:	CMP #$0E
	BNE D0F
	TAY
	LDA BALLX0,X
	CLC
	ADC #$02
	STA BALLX0,X
	LDA BALLY0,X
	CLC
	ADC #$02
	STA BALLY0,X
	TYA

D0F:	CMP #$0F
	BNE done
	TAY
	LDA BALLX0,X
	CLC
	ADC #$02
	STA BALLX0,X
	LDA BALLY0,X
	CLC
	ADC #$01
	STA BALLY0,X
	TYA
done:	RTS

	
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
;;; ;; Sleep 12 cycles (JSR & RTS)
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Sleep12:
	RTS
	
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

GRP:
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000

	
	;;
	;; Playfield Data
	;;

PF0_0:   ; PF0 is drawn in reverse order, and only the upper nybble
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

PF1_0:   ; PF1 is drawn in expected order       
        .byte %11111111 ; Arena 1
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

PF2_0:   ; PF2 is drawn in reverse order
        .byte %11111111 ; Arena 1
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

PFOFFSET:
	.byte $00
	
	;;
	;; Game Variation Table
	;;
VARTBL:	.byte %00000000		; GAME 1 - Dodgeball
	.byte %10000000		; GAME 2 - Ice Dodgeball
	.byte %11111111		; End of Variation Table

	;;
	;; Color Table
	;;
	;; COLUP0, COLUP1, COLUPF, COLUBK
	;; 
COLRTBL:
	.byte $DA, $8A, $2C, $32 ; Regular Dodgeball
	.byte $3A, $DA, $9C, $80 ; Ice Dodgeball
	.byte $0E, $00, $04, $08 ; B/W Regular Dodgeball
	.byte $0E, $00, $04, $08 ; B/W Ice Dodgeball

	;; Initial Position Table
	;; X0, X1, Y0, Y1
InitialPosTbl:
	.byte $20, $80, $3F, $3F

DirectnToXY:
	.byte $01, $F1, $F0, $FF, $0F, $1F, $10, $11
	
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
