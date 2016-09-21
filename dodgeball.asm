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
;;; First Playable:	2016-09-18
	
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
TEMP2:			ds 1	; Temp Variable 3.
TEMP3:			ds 1	; Temp Variable 4.
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

	;;
	;; Player/Ball direction vector, 0 is right, goes counterclockwise
	;; in 22.5 degree increments
	;; 
PLAYERD0:		ds 1	; Player 0 Direction
PLAYERD1:		ds 1	; Player 1 Direction
BALLD0:			ds 1	; Ball 0 Direction
BALLD1:			ds 1	; Ball 1 Direction
BALLD2:			ds 1	; Ball 2 Direction

	;;
	;; Decay variables (00 means stop)
	;; 
DECAY0:			ds 1	; Ball 0 Decay
DECAY1:			ds 1	; Ball 1 Decay (computer ball 2 has no decay)

	;;
	;; # of balls in hands
	;; 
BIH0:			ds 1	; # of balls in hands of P0	(0, 1, or 2)
BIH1:			ds 1	; # of balls in hands of P1	(0, 1, or 2)
	
	
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
	LDA #$42		; D1 = 1
	STA VBLANK		; Start VBLANK
	LDX GAMVAR		; Get Game Variation #
	LDA VARTBL,X		; Get variation from table	
	STA GAMPFMODE		; And store it in Game PF mode
	JSR SetTIA		; Set TIA Registers
	JSR ProcessJoysticks	; Process Joysticks
	JSR BallDirection	; Compute ball directions
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
	LDX #$01		; Start with Player 1
	LDA SWCHA		; and scan the joysticks.
	EOR #$FF		; flip the bits.
	
loop:	STA TEMP3		; now contains sticks, pre-mask
	AND #$0F		; only deal with bottom 4 bits
	TAY			; transfer to the Y for index
	LDA JoyToDirTable,Y	; grab the desired vector
	STA TEMP2		; now contains desired vector
	LDA INPT4,X		; scan fire button.
	AND #$80
	CMP #$80
	BPL nofire		; if fire wasn't pressed, go to nofire
fire:	LDA BIH0,X		; Check if at least one ball in hand
	CMP #$00		; no balls in hand?
	BEQ playerStill		; no balls, don't throw.
	LDA DECAY0,X		; Check decay.
	CMP #$00		; is it still?
	BNE playerStill		; Player stay still.
	CMP #$3F		; is it in motion?
	BCS playerStill		; player is still still ,but do not reset decay.
	LDA #$00
	STA RESMP0,X		; Turn off missile center reset
	LDA PLAYERX0,X		; load player X
	CLC
	ADC #$03
	STA BALLX0,X		; make ball X
	LDA PLAYERY0,X		; load player Y
	SEC
	SBC #$08
	STA BALLY0,X		; make ball Y
	LDA TEMP2		; if fire pressed, load the desired velocity
	STA BALLD0,X		; store in the ball's desired motion vector.
	LDA #$3F		; Ball Decay now $3F
	STA DECAY0,X		; ...
	DEC BIH0,X		; one less ball in hand. (or maybe zero)
playerStill:
	LDA #$FF		; ... stop the player in their tracks.
	STA PLAYERD0,X		; ...
	BVC next		; and go to next player.
nofire:	LDA TEMP2		; else, load the stored desired velocity.
	STA PLAYERD0,X		; store the new vector into player's direction.
	LDA DECAY0,X		; Load decay
	CMP #$00		; is ball dead?
	BEQ next		; if so, go to next bal.
	SEC			; otherwise...
	SBC #$01		; subtract one from ball's decay counter.
	STA DECAY0,X		; and store.
	CMP #$00		; did we just die?
	BNE next		; no, go to next ball.
	LDA #$FF		; make ball still
	STA BALLD0,X	  	; and store.
next:	LDA TEMP3		; Re-grab saved SWCHA
	LSR			; and shift out the already processed bytes
	LSR			;
	LSR			;
	LSR			; 
	DEX			; decrement player joystick index
	BPL loop		; if we haven't processed 0, loop around
	
	RTS			; else, return.
	
JoyToDirTable:
	.BYTE $FF, $04, $0C, $FF, $08, $06, $0A, $FF, $00, $02, $0E, $FF, $FF, $FF, $FF, $FF
	
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
        lda SCORE,x     ; LoaD A with SCORE+1(first pass) or SCORE(second pass)
        and #$0F        ; remove the tens digit
        sta TEMP        ; Store A into TEMP
        asl             ; Accumulator Shift Left (# * 2)
        asl             ; Accumulator Shift Left (# * 4)
        adc TEMP        ; ADd with Carry value in TEMP (# * 5)
        sta DIGITONES,x  ; STore A in DIGITONES+1(first pass) or DIGITONES(second pass)
        lda SCORE,x     ; LoaD A with SCORE+1(first pass) or SCORE(second pass)
        and #$F0        ; remove the ones digit
        lsr             ; Logical Shift Right (# / 2)
        lsr             ; Logical Shift Right (# / 4)
        sta TEMP        ; Store A into TEMP
        lsr             ; Logical Shift Right (# / 8)
        lsr             ; Logical Shift Right (# / 16)
        adc TEMP        ; ADd with Carry value in TEMP ((# / 16) * 5)
        sta DIGITTENS,x ; STore A in DIGITTENS+1(first pass) or DIGITTENS(second pass)
        dex             ; DEcrement X by 1
        bpl .loop    	; Branch PLus (positive) to PSFDloop
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
	STA BALLX0
	STA BALLY0

	LDA #$75
	STA BALLX1
	STA BALLY1
	
	LDA #$70
	STA BALLX2
	STA BALLY2

	LDA #$FF
	STA PLAYERD0
	STA PLAYERD1
	STA BALLD0
	STA BALLD1
	
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
	AND #$FC		; 2	10
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
	AND #$FC		; 2	10
	PHP			; 3	13
	TXA			; 2	15
	EOR BALLY0		; 3	18
	AND #$FC		; 2	20
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
	LDX #$01		; Start with Player 1
PLtoPF:
	LDA CXP0FB,X		; Did player collide with playfield?
	BPL nextPLtoPF		; No.
PLtoPFCollide:
	JSR RecallPlayerPosition ; Yes, recall the player position.
nextPLtoPF:
	JSR SavePlayerPosition	; And subsequently save it.
	DEX			; decrement to next player
	BPL PLtoPF		; if we're not done with player 0, loop around.

	;;
	;; Handle Ball collisions (PONG), currently no decay.
	;; Assumes carry is set.
	;; 
	LDX #$02		; Start with computer ball
BLtoPF:
	LDA CXM0FB,X		; Read collision
	BPL nextBLtoPF		; if collision didn't happen, skip to next ball.
BLtoPFCollide:
	JSR RecallBallPosition	; otherwise, recall the previous ball position
	LDA BALLD0,X		; Load the requested direction vector
	ADC #$08		; reflect it (the carry will add an additional 22.5 deg)
	AND #$0F		; and make sure we stay within 16 possible directions.
	STA BALLD0,X		; store the new vector
	AND #$03		; check if it is N, S, E, W?
	BNE nextBLtoPF		; if not, go to next ball.
	INC BALLD0,X		; if so, add an additional 22.5 deg to keep it diagonal.
nextBLtoPF:
	JSR SaveBallPosition	; Save ball position.
	DEX			; Decrement to next ball.
	BPL BLtoPF		; if we're not at ball 0 yet, go back.

	;;
	;; Handle case when player collides with his own ball. Automatically grab it.
	;;
	
	LDX #$01		; start with player ball
MxtoPL:	LDA CXM0P,X		; scan missile collision registers
	AND #$40
	CMP #$40
	BNE nextMxtoPL		; go to next ball, if player didn't collide with ball.
	LDA DECAY0,X
	CMP #$00
	BNE nextMxtoPL
	LDA #$02		; we collided, set bit 2
	STA RESMP0,X		; and strobe RESMP0 to reset missile to center of player.
	INC BIH0,X		; and one more ball into the hand.
nextMxtoPL:
	DEX			; decrement player/missile counter
	BPL MxtoPL		; and branch back if we haven't taken care of P0/M0.

	;;
	;; Handle case when ball hits opposing player
	;;

	LDX #$01		; Start with second player
MxtoOP:	LDA CXM0P,X		; Check bit 7 of CXM0P (optimize)
	AND #$80		; mask off bit 7
	CMP #$80		; is it set?
	BNE nextMxtoOP		; if not, check next ball.
	LDA DECAY0,X		; if a legal collision, check decay.
	CMP #$00		; is it 0?
	BEQ nextMxtoOP		; if it's 0, then the ball is still, no points awarded.
	SED			; else it is a legal hit, turn on decimal mode.
	CLC			; clear the carry
	LDA SCORE,X		; get current player's score.
	ADC #$01		; add one to it.
	STA SCORE,X		; store it back
	CLD			; clear the decimal flag
	SEC			; set the carry back (for the ball direction routines)
	LDA #$08		; set the ball decay to the ball hit decay value
	STA DECAY0,X		; store it so the ball will come to a halt, away from the player.
	JSR RecallBallPosition	; PONG logic, recall ball position pre-collsion
	LDA BALLD0,X		; get current ball direction
	ADC #$08		; reflect it
	AND #$0F		; mask it off to a legal direction
	STA BALLD0,X		; store the new ball vector
	AND #$03		; is it N/S/E/W?
	BNE nextMxtoOP		; no? do next ball
	INC BALLD0,X		; otherwise, add 22.5 deg to prevent lateral bounce.
nextMxtoOP:
	JSR SaveBallPosition	; save the now current ball position.
	DEX			; decrement X for next player
	BPL MxtoOP		; if >=0 then loop back around for the next player.

	;;
	;; When M0 and M1 balls hit each other.
	;; 
	
MxToMxCollide:
	BIT CXPPMM		; Check P/P M/M collision
	BVC NoMxToMxCollide	; If not, skip this whole routine.
	LDX #$01		; if yes, set up to update both M1 and M0
MxToMxCollideLoop:
	JSR RecallBallPosition	; Recall the original ball position
	LDA #$08		; decay now set to 8 frames
	STA DECAY0,X		; for the current ball in the loop
	LDA BALLD0,X		; get the ball vector
	ADC #$08		; reflect it
	AND #$0F		; mask off to a legal direction
	STA BALLD0,X		; store the vector
	AND #$03		; check for N/S/E/W...
	BNE NextMxToMxCollide	; if not, skip past direction increment
	INC BALLD0,X		; add an additional 22.5 degress of counterclockwise direction.
NextMxToMxCollide:
	JSR SaveBallPosition	; Save the resulting ball position
	DEX			; decrement X
	BPL MxToMxCollideLoop	; and if X >= 0, loop back around for the next ball.
NoMxToMxCollide:	

	;;
	;; When BL collides with P0 or P1
	;;

	LDX #$01		; Start with P1
BLtoPL:	LDA CXP0FB,X		; load next P/FB collision register
	AND #$40		; Mask off only bit 6
	CMP #$40		; is bit 6 set? (did we collide?)
	BNE nextBLtoPL		; if not, skip to next player
BLtoPLCollide:
	LDA SCORE,X		; Load current player's score.
	CMP #$00		; is it already 0?
	BEQ BLtoPLCollide0	; yes, just handle the collision
	SED			; otherwise, set decimal mode
	LDA SCORE,X		; load current score 
	SBC #$01		; subtract 1
	STA SCORE,X		; store it back
	CLD			; clear decimal mode
BLtoPLCollide0:
	STX TEMP		; temporarily store the X register
	LDX #$02		; 02 = computer ball
	JSR RecallBallPosition	; recall computer ball's last pre-collision position
	LDX TEMP		; restore X from TEMP
	LDA BALLD2		; get computer ball's direction.
	ADC #$08		; reflect it
	AND #$0F		; mask it off to legal direction
	STA BALLD2		; and set the new direction.
	AND #$03		; is it N/S/E/W ?
	BNE nextBLtoPL		; if not, skip over next increment
	INC BALLD2		; add 22.5 degrees counterclockwise to avoid lateral direction
nextBLtoPL:
	STX TEMP		; temporarily store X register
	LDX #$02		; set computer ball 
	JSR SaveBallPosition	; save its position.
	LDX TEMP		; re-load X with temp
	DEX			; decrement it
	BPL BLtoPL		; and go back around if we haven't done the other ball.

	;;
	;; M0/M1 collides with BL
	;;

	LDX #$01		; start with M1 (second ball)
MxToBL:	LDA CXM0FB,X		; read collision register
	AND #$40		; mask off all but bit 6
	CMP #$40		; is bit 6 set?
	BNE nextMxToBL		; no? loop around to check M0
MxToBLCollide:
	LDA BALLD2		; Load vector for computer ball
	ADC #$08		; reflect it
	AND #$0F		; mask into a legal direction.
	STA BALLD2		; store the vector
	AND #$03		; is it N/S/E/W ?
	BNE MxToBLCollide0	; if not, don't increment the direction vector. deal with Mx ball
	INC BALLD2		; it is, increment the computer's direction vector by 22.5 deg.
MxToBLCollide0:	
	LDA #$08		; set Mx's ball to decay 8 frames from now.
	STA DECAY0,X		; store it.
	JSR RecallBallPosition	; recall pre-collision ball position
	LDA BALLD0,X		; load current ball direction vector
	ADC #$08		; reflect it.
	AND #$0F		; mask to a legal value.
	STA BALLD0,X		; and store it back.
	AND #$03		; are we N/S/E/W ?
	BNE nextMxToBL		; if not, don't increment ball vector
	INC BALLD0,X		; otherwise, increment ball vector by 22.5 degrees.
nextMxToBL:
	JSR SaveBallPosition	; Save the ball position
	DEX			; decrement X
	BPL MxToBL		; if X >= 0, go back around to the first ball.
	
colDone:
	RTS			; else, return.

	
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; RecallPosition
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	;;
	;; TODO: Refactor this to rename RecallPlayerPosition to RecallPosition
	;; 
RecallPlayerPosition:	SUBROUTINE
	LDA PLAYERX0S,X		; Load last saved X
	STA PLAYERX0,X		; store to current X
	LDA PLAYERY0S,X		; Load last saved Y
	STA PLAYERY0,X		; store to current Y
	RTS			; return

RecallBallPosition:	SUBROUTINE
	LDA BALLX0S,X		; Load last saved X
	STA BALLX0,X		; store to current X
	LDA BALLY0S,X		; load last saved Y
	STA BALLY0,X		; store to current Y
	RTS			; return
	
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; SavePosition
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SavePlayerPosition:	SUBROUTINE

	;;
	;; TODO: Refactor this to make it simply SavePosition, get rid of redundancy.
	;; 
	
	LDA PLAYERX0,X		; Get Player X current position
	STA PLAYERX0S,X		; store to last saved X position
	LDA PLAYERY0,X		; Get Player Y current position
	STA PLAYERY0S,X		; store to last saved Y position
	RTS			; and return

SaveBallPosition:	SUBROUTINE
	LDA BALLX0,X		; Get Ball X current Position
	STA BALLX0S,X		; store to last saved X position
	LDA BALLY0,X		; Get Ball Y current position
	STA BALLY0S,X		; store to last saved Y position
	RTS			; return
	
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; BallDirection
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BallDirection:	SUBROUTINE

	LDX #$04		; Start with computer ball
doBall:
	LDY PLAYERD0,X		; Get requested ball vector

	;;
	;; First, we deal with Special case $FF, which means
	;; do not move the ball.
	;;

	CPY #$FF		; $FF = do not move, stay still.
	BEQ nextBall		; If so, skip this ball.

	;;
	;; Apply Velocity delay if player.
	;;
	CPX #$02
	BPL moveBall
playerDelay:
	LDA FRAME
	AND #$01
	BNE nextBall
		
	;;
	;; Deal with Ball X
	;; 
moveBall:
	LDA PLAYERX0,X		; Get current ball X
	CLC			; clear carry
	ADC BallVectorX,Y	; Add the new vector difference
	STA PLAYERX0,X		; store it back into Ball X

	;;
	;; Deal with Ball Y
	;;
	LDA PLAYERY0,X		; Get current ball Y
	CLC			; clear carry
	ADC BallVectorY,Y	; Add the new vector difference
	STA PLAYERY0,X		; store it back into Ball Y

	;;
	;; If not done, do the next ball
	;; 
nextBall:
	DEX			; Decrement X (which ball)
	BPL doBall		; If >= 0, do the next ball.
	
	RTS

;
	;; This table is obviously shifted, come back here and collapse this.
	;; 
BallVectorX:
	.byte $02, $02, $02, $01, $00, $FF, $FE, $FE, $FE, $FE, $FE, $FF, $00, $01, $02, $02

BallVectorY:
	.byte $00, $FF, $FE, $FE, $FE, $FE, $FE, $FF, $00, $01, $02, $02, $02, $02, $02, $01
	
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; PObject subroutine
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
        .byte %00000111
        .byte %00000101
        .byte %00000101
        .byte %00000101
        .byte %00000111
        
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
        
        .byte %00000000     ; used to blank out right score in 1 player games
        .byte %00000000
        .byte %00000000
        .byte %00000000
        .byte %00000000

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
