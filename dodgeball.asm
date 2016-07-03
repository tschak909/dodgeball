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

	

	PROCESSOR 6502

	include vcs.h
	include macro.h

	SEG.U VARS
	ORG $80

Frame:		ds 1	        ; Frame counter.
ScanLine:	ds 1		; scanline counter
PlayerY0:	ds 1		; Player Y0
PlayerY1:	ds 1		; Player Y1
BallY0:		ds 1		; ball y0
BallY1:		ds 1		; missile y0
BallY2:		ds 1		; missile y1
Score:		ds 1		; Score
Timer:		ds 1		; Timer
DigitOnes:	ds 2		; Player 0 and 1 digit graphics
DigitTens:	ds 2		; Player 0 and 1 digit graphics
ScoreGfx:	ds 1		; pointers
TimerGfx:	ds 1		; pointers
Temp:		ds 1		; Temp
TempStackPtr:	ds 1		; Temporary Stack Pointer
GameState:	ds 1		; store game state (BIT tested)
Temp2:		ds 1		; another temp value
ColorCycle:	ds 1		; Color cycling temp value (attract mode)
JoystickStates:	ds 1		; Joystick states.
P0XVelocity:	ds 1		; P0 X Velocity
P1XVelocity:	ds 1		; P0 Y Velocity
P0YVelocity:	ds 1		; P1 X Velocity
P1YVelocity:	ds 1		; P1 Y Velocity
P0XDirection:	ds 1		; P0 X Direction (Bit 7 Left, Bit 6 Right)
P1XDirection:	ds 1		; P1 X Direction
P0YDirection:	ds 1		; P0 Y Direction (Bit 7 Up, Bit 6 Down)
P1YDirection:	ds 1		; P1 Y Direction
P0XIsWaiting:	ds 1		; P0 X is Waiting (BIT 7)
P1XIsWaiting:	ds 1		; P1 X is Waiting (Bit 7)
P0YIsWaiting:	ds 1		; P0 Y is Waiting (BIT 7)
P1YIsWaiting:	ds 1		; P1 Y is Waiting (BIT 7)
VelocityTemp:	ds 1		; used for velocity unpacking.
VelocityFlip:	ds 1		; used to flip velocity values.
WaitingTemp:	ds 1		; Waiting index temp between frames.

	
	SEG CODE
	ORG $F800

ColdStart:
	CLEAN_START
	;;
	;; all this is temporary
	;;
	;; lda #$32
	;; sta PlayerY0
	;; sta RESM0
	;; lda #$50
	;; sta PlayerY1
	;; lda #$80
	;; sta BallY0
	;; lda #$90
	;; sta BallY1
	;; lda #$A0
	;; sta BallY2
	;; sta RESBL
	;; sta RESM1
	;; lda #$10
	;; sta NUSIZ0
	;; sta NUSIZ1

	jsr InitialPosition

MainLoop:
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Vertical Sync
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

VerticalSync:
	lda #$02		; Get ready to start VSYNC
	ldx #44
	sta WSYNC		; Next line
	sta VSYNC		; Start VSYNC
	stx TIM64T
	sta CTRLPF
	lda Frame		; (8 bit frame counter)
	and #$3F		; (we only want to act every 64 frames)
	bne ColorSkip		; don't update color cycle if not @64 frames

Every128Frames:	
	inc ColorCycle		; Increment the color cycle counter.
	bit GameState		; check gamestate
	bpl ColorSkip		; if we're not playing, don't increment timer.
	inc GameState		; otherwise increment the game timer.

ColorSkip:
	inc Frame		; otherwise increment the frame counter.
	sta HMCLR
	sta WSYNC
	sta WSYNC
	lda #$00
	sta PF0
	sta PF1
	sta PF2
	sta WSYNC
	sta VSYNC
	
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Vertical Blank routines.
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

VerticalBlank:

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Process console switches.
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ProcessSwitches:
	;; Process start switch
	lda SWCHB
	lsr
	bcs NoNuGam	      ; go to no nu game if not pressed.
	;;  Reset/Start pressed.
	jsr InitialPosition
	ldx #$80		; Put 128 into gamestate
	stx GameState
NoNuGam:
	lsr			; get select value
	bcs SetTia		; select not pressed.
	;;
	;;  select pressed.
	;; 
	ldx #$00		; put $00 into
	stx GameState		; into GameState, re-enabling attract.
	
	;;  Start/select  switch not pressed
SetTia:
	lda #$FF
	sta Temp2		; default color mask
	and ColorCycle		; color cycle
	bit GameState
	bpl GameOver
	lda #$00		; if game is active, no color cycle.
GameOver:
	sta Temp
	ldx #$03		; Color register offset
	ldy #$03		; color table offset.
	lda SWCHB		; check B/W switch
	and #$08		; pressed?
	bne SOCloop		; if not pressed, offset is still 0 (or the color table)
	lda #$0f
	ldy #$07
	sta Temp2
SOCloop:
	lda Colors,Y		; Get next color
	eor Temp
	and Temp2
	sta COLUP0,x		; set color into register
	dey
	dex
	bpl SOCloop		; branch if not end of table.

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; motion/velocity code for players.
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	ldx #$00		; Start with player 0

	;;
	;; Horizontal Motion
	;;
	
HorizVelocityParse:
	lda P0XVelocity,x	; grab X velocity index
	cmp #$00		; is it 0? (going left)
	beq VertVelocityParse	; yes, skip to vertical motion.
	cmp #$F0		; is it F0? (going right)
	beq VertVelocityParse	; yes, skip to vertical motion.
	lda #$80		; assume waiting, until we're proven not.
	sta P0XIsWaiting,x	;
	ldy P0XVelocity,x	; Get velocity index
	bpl HorizVelocityLeft	; Go left if we're not (Fx)
HorizVelocityRight:
	lda #$FF		; flip bits for a two's compliment subtraction, later
	sta VelocityFlip	;
	tya
	and #$0F		; mask off twop bits, getting rid of sign.
	tay
HorizVelocityLeft:
	lda HORIZ_VELOCITY,y	; now, get the actual velocity value.
	and #$0F		; mask off HMOVE componoent, leaving delay.
	sta VelocityTemp	; Store temporarily in velocity temp.
	lda Frame		; get current frame
	and VelocityTemp	; mask against requested velocity delay.
	bne VertVelocityParse	; if we do not need to move, go to vertical.
	lda #$00		; we're not waiting, clear the waiting flag.
	sta P0XIsWaiting,x	; and store it.
	lda HORIZ_VELOCITY,y	; grab horizontal velocity again.
	sec			; clear carry to prepare for...
	eor VelocityFlip	; a two's compliment bit flip.
	adc #00			; ....yeah.
	sta Temp		; store into temp.
PerformHorizMotion:
	lda Temp		; bring back temp
	sta HMP0,X		; slam it into Horizontal motion vector.

	;;
	;; Vertical Motion
	;;

VertVelocityParse:
	lda #$00		; clear the velocity flip
	sta VelocityFlip	;
	lda #$80		; set Y is waiting until proven otherwise.
	sta P0YIsWaiting,x	;
	ldy P0YVelocity,x	; Get the requested player Y velocity index.
	bpl VertVelocityDown	; Go down if Y velocity index isn't (Fx)
VertVelocityUp:
	tya			; do a little register gymnastics
	and #$0f		; to get rid of the sign.
	tay			; and back again.
	lda VERT_VELOCITY,y	; get actual velocity/delay value from index.
	and #$0F		; mask of vertical delta value.
	sta VelocityTemp	; and store temporarily into temp
	lda Frame		; get current frame
	and VelocityTemp	; and mask against it.
	bne DoNextPlayer	; if we're not ready, go to bottom and loop to next player.
	lda #$00		; clear P0 Y is waiting, because we're not.
	sta P0YIsWaiting,y	; ...
	lda VERT_VELOCITY,y	; get the requested Y movement amount.
	lsr			; ...
	lsr			; shift it over 4 bits, to get requested
	lsr			; movement amount...
	lsr			; ...
	sta Temp		; and store it temporarily.
PerformVertUp:
	lda PlayerY0,X		; get current Y pos
	sec			; set carry to prepare for subtract
	sbc Temp		; subtract velocity delta we calculated.
	sta PlayerY0,X		; and store it back.
	bvc DoNextPlayer	; and go to next player.
VertVelocityDown:
	lda VERT_VELOCITY,y	; get requested vertical velocity index
	and #$0f		; mask off the delta, leaving delay.
	sta VelocityTemp	; store away.
	lda Frame		; get current frame
	and VelocityTemp	; and mask against it.
	bne DoNextPlayer	; if we're waiting, just move along to the next player.
	lda #$00		; otherwise, clear waiting.
	sta P0YIsWaiting,y	; because we're not waiting.
	lda VERT_VELOCITY,y	; get actual velocity nibbles for this player's Y
	lsr			; ...
	lsr			; shift it over 4 bits, to get to the good stuff.
	lsr			; for the # of units to move down
	lsr			; ...
	sta Temp
PerformVertDown:
	lda PlayerY0,x		; get Player x's Y coordinate
	clc			; clear carry for add
	adc Temp		; add the player delta
	sta PlayerY0,x		; and store it back.

	;;
	;; Do next player
	;;

DoNextPlayer:	
	inx			; increment X (which contains current player)
	cpx #$02		; are we done?
	beq PrepScoreForDisplay	; We're done, go calculate store.
	jmp HorizVelocityParse	; go back.
	
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Calculate digit graphic offsets from score variables
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
PrepScoreForDisplay:
	inc Timer		; Increment frame timer.
	bne PSFDskip
	inc Score		; for now, we increment score.

PSFDskip:
	ldx #1
PSFDloop:
	lda Score,X		; Get next score offset
	and #$0F		; mask off right digit
	sta Temp		; store in temp
	asl			; shift it over
	asl
	adc Temp		; add to temp
	sta DigitOnes,x		; store digit result into digitones for kernel line
	lda Score,x		; get the score offset again
	and #$F0		; mask off the left digit
	lsr			; shift over
	lsr
	sta Temp		; store into temp
	lsr			; 
	lsr			; shift it over
	adc Temp		; store into temp
	sta DigitTens,x		; take the result and store into the tens.
	dex			; next score line to calculate
	bpl PSFDloop		; and loop back around.

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; The kernel for the visible screen.
;;; ;; at this point, the vblank timer is running out
;;; ;; and the kernel waits for the timer to lapse, bringing it
;;; ;; to the visible section of the screen, and then we start
;;; ;; slamming stuff into registers to clock out the visible
;;; ;; screen, starting with the score, and then the playfield.
;;; ;; The visible kernel is divided into:
;;; ;; * Score
;;; ;; * Top border (PF0, PF1, PF2)
;;; ;; * Playfield (PF1, PF2, P0, P1, M0, M1, BL)
;;; ;; * Bottom Border (PF0, PF1, PF2)
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Kernel:
	lda #$00
	sta CXCLR
KernelWait:	
	;; wait for timer to run out
	lda INTIM
	bne KernelWait

	;; a is now 0, so turn off vblank, so we can see stuff.
	sta VBLANK
	
	;; Assume scanline 35 once HMOVE happens.
	lda #$19
	sta ScanLine

	;; go to next scanline and thwack HMOVE, to commit the X motion changes.
	sta WSYNC
	sta HMOVE

	;; grab the stack pointer and temporarily store it, for later.
	tsx
	stx TempStackPtr

	;; Add a dash of padding. :)
	sta WSYNC
	
	;;
	;; two digit score for p0 and p1
	;;

	ldx #$05		; score is 5 lines tall
ScoreLoop:
	ldy DigitTens		; P0: Get the y offset for the tens digit next score line
	lda DigitGfx,y		; load accumulator with contents of score line
	and #$f0		; mask off left digit
	sta ScoreGfx		; store into score gfx
	ldy DigitOnes		; do the same for the ones
	lda DigitGfx,y		;
	and #$0F		;
	ora ScoreGfx		; merge the ones into the tens
	sta ScoreGfx		; and store into the store graphics
	sta WSYNC		; next line, on your mark... get set...
	;;
	sta PF1			; slam it into PF1
	ldy DigitTens+1		; Now calculate P1 same thing as above.
	lda DigitGfx,y		;
	and #$F0		;
	sta TimerGfx		;
	ldy DigitOnes+1		;
	lda DigitGfx,y		;
	and #$0F		;
	ora TimerGfx		;
	sta TimerGfx		;
	jsr Sleep12		; wait 12 cycles, to put it just before the reflected PF1
	sta PF1			; and slam it out.
	ldy ScoreGfx		;
	sta WSYNC		; next line
	sty PF1			; slam it out.
	inc DigitTens		; TEMP: increment the digits for the next frame.
	inc DigitTens+1		;
	inc DigitOnes		;
	inc DigitOnes+1		;
	jsr Sleep12		; sleep 12 more cycles
	dex
	sta PF1			; at this point the A is 0, clearing out PF1
	bne ScoreLoop		; if we're not done, loop back around.

	;;
	;; Clear the playfield
	;;
ScoreDone:
	sta WSYNC
	lda #$00
	sta PF0
	sta PF1
	sta PF2
	lda ScanLine
	adc #$05
	sta ScanLine
	sta WSYNC
	sta WSYNC

	;;
	;; Set CTRLPF to do a 2 clock ball, and reflective playfield.
	;;
PFReady:	
	lda #$15
	sta CTRLPF

	;;
	;; the inner playfield loop
	;; 
vfield:	ldx #$1F		; Place SP over ENABL
	txs			; for the stack trick to be used in a moment.
	;;
	;; handle Player 0
	;; 
	sec			;
	lda PlayerY0		; Where is player0's Y?
	sbc ScanLine		; subtract current scanline
	and #$FE		; quantise to two scanlines
	tax			; move into x register for graphic lookup
	and #$F0		; mask upper 4 bits
	beq vdoplayer		; if we're 0, we need to do the player.
	lda #$00		; else don't draw a player, and....
	beq vnoplayer		; continue onward.
vdoplayer:
	lda PLAYER,X		; load the next player graphic line
vnoplayer:
	sta WSYNC		; go to next scanline
	STA GRP0		; slam player0's graphic into place.
	;;
	;; the combat stack trick. Utilize the fact that the missile and ball enables
	;; are in bit D1, and thus can be strobed in O(1) time if we're on the right
	;; scanline, because A = 0 = Z
	;;
	;; First, do the ball., ENABL
	;; 
	lda BallY2		; Get ball's Y coord
	eor ScanLine		; eor with scanline to flip bits if needed
	and #$FC		; quantise to 4 lines.
	php			; transfer processor flags (we are interested in Z) to where SP is
	;;
	;; Then, missile 1, php caused SP to decrement over to ENAM1
	;; 
	lda BallY1		; Same thing again, just against missile 1
	eor ScanLine		;
	and #$FC		;
	php			;
	;;
	;; Then, missile 0, php caused SP to decrement to ENAM0
	;; 
	lda BallY0		; same thing again, just against missile 0
	eor ScanLine		;
	and #$FC		;
	php			;
	;;
	;; Ok, we're now done with p0, bl, m1, m0, now to precalculate playfield offset.
	;; 
	lda ScanLine		; Get current scanline
	bpl vvrefl		; if we're < 127, then don't reflect.
	eor #$F8		; otherwise flip bits to reflect.
vvrefl:	cmp #$19		; figure out if we're at last playfield line, if so, done.
	bcc vfdone		;
	lsr			; divide the scanline counter by 8.
	lsr			;
	lsr			;
	tay			; transfer to Y for table lookup.
	;;
	;; now deal with Player 1
	;; 
vfdone:	lda PlayerY1		; get player 1's Y coord
	sec			; 
	sbc ScanLine		; subtract it against current scanline
	inc ScanLine		; go ahead and increment scanline in memory (to correct bias)
	nop			; wait a tick...
	ora #$01		; get the next odd bit in player (p0 and p1 graphics are interleaved!)
	tax			; convert to table lookup
	;;
	and #$F0		; mask off top bits (we have 16 possible entries)
	beq vdot1		; do player 1 graphics if we need to, else...
	lda #$00		; write 0's
	beq vnot1		; and slam it to GRP0
vdot1:	lda PLAYER,X		; load next graphic line
vnot1:	sta GRP1		; slam it into GRP1
	lda PF0_0,Y
	sta PF0
	lda PF1_0,Y		; now we have some time to slam PF1 into place
	sta PF1			;
	lda PF2_0,Y		; and PF2.
	sta PF2			;
	inc ScanLine		; increment to next scanline
	lda ScanLine		; get current scanline
	eor #$E6		; are we done?
	bne vfield		; if not, loop around.

	;;
	;; go to next line, to keep things clean.
	;;
	sta WSYNC

	;;
	;; Bottom of the visible kernel
	;; clean up the visible regsters to prevent messes.
	;;
KernelCleanup:

	;;
	;; put the stack back the way it was.
	;;
	ldx TempStackPtr
	txs
	ldx #$08
wloop	sta WSYNC
	dex
	bpl wloop
	lda #$00
		
	jmp MainLoop		; and go back to the top!

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Ancilliary subroutines
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Sleep12:
	rts			; this takes 6 cycles, the jsr takes 6 cycles.

InitialPosition:
	;; 
	;;  Reset player positions.
	;;
	sta WSYNC
	SLEEP 11
	ldx #$87
	stx PlayerY0
	stx PlayerY1
	nop
	nop
	stx RESP0		; Put us somewhere close to left of screen.
	SLEEP 38
	stx RESP1		; Put P1 on the right side of screen.
	rts			; and return.
	
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Tables
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; color table, first 4 bytes are color, next 4 are b/w
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Colors:
	.byte $DA
	.byte $8A
	.byte $0E
	.byte $32
	.byte $0e
	.byte $00
	.byte $08
	.byte $04


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Playfield data
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PF0_0
	.byte $00
	.byte $00
	.byte $00
	.byte $00

	.byte $F0
	.byte $10
	.byte $10
	.byte $10
	.byte $10
	.byte $10
	.byte $10
	.byte $10
	.byte $10
	.byte $10
	.byte $10
	.byte $10
	
PF1_0
	.byte $00
	.byte $00
	.byte $00
	.byte $00

	.byte $FF
	.byte $00
	.byte $00
	.byte $00
	.byte $38
	.byte $00
	.byte $00
	.byte $00
	.byte $60
	.byte $20
	.byte $20
	.byte $23
	
PF2_0
	.byte $00
	.byte $00
	.byte $00
	.byte $00

	.byte $FF
	.byte $80
	.byte $80
	.byte $00
	.byte $00
	.byte $00
	.byte $1C
	.byte $04
	.byte $00
	.byte $00
	.byte $00
	.byte $00

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Player data, p0 and p1 are interleaved.
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PLAYER:	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $3C
	.byte $3C
	.byte $3C
	.byte $3C
	.byte $3C
	.byte $3C
	.byte $3C
	.byte $3C
	.byte $00
	.byte $00
	.byte $00
	.byte $00

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Score Digit Graphics
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	;; Right now, we have hex digits here too to make things easy.
	
DigitGfx:
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
        
        .byte %00100010
        .byte %01010101
        .byte %01110111
        .byte %01010101
        .byte %01010101
         
        .byte %01100110
        .byte %01010101
        .byte %01100110
        .byte %01010101
        .byte %01100110
        
        .byte %00110011
        .byte %01000100
        .byte %01000100
        .byte %01000100
        .byte %00110011
        
        .byte %01100110
        .byte %01010101
        .byte %01010101
        .byte %01010101
        .byte %01100110
        
        .byte %01110111
        .byte %01000100
        .byte %01100110
        .byte %01000100
        .byte %01110111
        
        .byte %01110111
        .byte %01000100
        .byte %01100110
        .byte %01000100
        .byte %01000100

	;; upper nibble is delay, lower nibble is HMOVE delta.
HORIZ_VELOCITY:	.byte $00,$1F,$1F,$1F,$13,$13,$10,$10,$20
VERT_VELOCITY:	.byte $00,$17,$17,$17,$13,$13,$10,$10,$20
	
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Function to check for free space at end of cart.
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	echo "------", [$FFFA - *]d, "bytes free before End of Cartridge"
	
	ORG $FFFA
	.WORD ColdStart
	.WORD ColdStart
	.WORD ColdStart
