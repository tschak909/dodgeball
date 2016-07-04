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
PlayerX0:	ds 1		; Player X0
PlayerX1:	ds 1		; Player X1
PlayerY0:	ds 1		; Player Y0
PlayerY1:	ds 1		; Player Y1
PlayerX0S:	ds 1		; Player X0 Saved
PlayerX1S:	ds 1		; Player X1 Saved 
PlayerY0S:	ds 1		; Player Y0 Saved
PlayerY1S:	ds 1		; Player Y1 Saved
BallX0:		ds 1		; Ball X0
BallX1:		ds 1		; Ball X1
BallX2:		ds 1		; Ball X2
BallY0:		ds 1		; ball y0
BallY1:		ds 1		; Ball y1
BallY2:		ds 1		; Ball y2
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
P0Velocity:	ds 1		; P0 Velocity
P1Velocity:	ds 1		; P1 Velocity
	
	SEG CODE
	ORG $F800

ColdStart:
	CLEAN_START
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
;;; ;; Yet another thwack at motion. No really, this
;;; ;; is getting really old. I feel like I'm Bill Murray
;;; ;; at this point. Actually, wait, maybe I'm schizophrenic
;;; ;; and I am both Bill Murray, and Stephen Tobolowsky?
;;; ;; Don't do drugs, kids...
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	;;
	;;  Persist last known good coordinates if not colliding
	;;  with anything.
	;;

	ldx #$00

PersistLNG:
	lda CXP0FB,x
	bmi MotionPlayer
	lda PlayerX0,x
	sta PlayerX0S,x
	lda PlayerY0,x
	sta PlayerY0S,x
PersistNext:
	inx
	cpx #$02
	bne PersistLNG

	;;
	;;  start with first player.
	;; 
MotionPlayer:
	ldx #$00

	;;
	;; Apply the appropriate pre-motion delay in frames
	;; 
MotionPDelay:
	lda P0Velocity,x
	cmp #$00
	beq MotionNext
	and #$0F
	sec
	sbc #$01
	sta Temp
	lda Frame
	and Temp
	bne MotionNext

	;;
	;; Next grab the requested direction(s),
	;; and flip them for easy carry set shifting.
	;; 
MotionPDirection:
	lda P0Velocity,x
	eor #$FF

	;;
	;; Iterate through each direction, and applying the appropriate
	;; coordinate change
	;; 
CheckPDirections:
	asl
	bcs CheckLeft
DoRight:
	tay
	lda PlayerX0,x
	clc
	adc #$01
	cmp #$9F
	bne DoRight1
	lda #$00
DoRight1:
	sta PlayerX0,x
	tya
CheckLeft:	
	asl
	bcs CheckDown
DoLeft:
	tay
	lda PlayerX0,x
	sec
	sbc #$01
	cmp #$00
	bne DoLeft1
	lda #$9F
DoLeft1:
	sta PlayerX0,x
	tya
CheckDown:
	asl
	bcs CheckUp
DoDown:
	tay
	lda PlayerY0,x
	clc
	adc #$01
	bne DoDown1
	lda #$00
DoDown1:
	sta PlayerY0,x
	tya
CheckUp:
	asl
	bcs MotionNext
DoUp:
	tay
	lda PlayerY0,x
	sec
	sbc #$01
	bne DoUp1
	lda #$00
DoUp1:
	sta PlayerY0,x
	tya

	;;
	;; Finally, move on to the next player.
	;; 
MotionNext:
	inx
	cpx #$02
	bne MotionPDelay

	;;
	;; Check each player for collision, if it has occurred, reset player
	;; coordintes back to last known good coordinates.
	;;

	ldx #$00
	
PlayerColCheck:
	lda CXP0FB,x
	bpl NextColCheck
	lda PlayerX0S,x
	sta PlayerX0,x
	lda PlayerY0S,x
	sta PlayerY0,x
NextColCheck:
	inx
	cpx #$02
	bne PlayerColCheck

	;;
	;; Apply X motion vectors.
	;; 
	ldx #$00
	
ApplyMotion:	
	lda PlayerX0,x
	jsr PosObject
	inx
	cpx #$02
	bne ApplyMotion
	
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
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Kernel:
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

	;; 
	;; sleep for 12 cycles
	;; 
Sleep12:
	rts			; this takes 6 cycles, the jsr takes 6 cycles.

	;;
	;; set Initial player position
	;; 
InitialPosition:
	lda #$0E
	sta PlayerX0
	lda #$8A
	sta PlayerX1
	lda #$87
	sta PlayerY0
	sta PlayerY1
	rts			; and return.

	;;
	;;  Position object's X coordinate.
	;;  A register = X position (0-159 for P0, P1, 1-160 for M0,M1,BL)
	;;  X Register = Object to position (0,1 = P0/P1, 2,3,4 = M0, M1, BL)
	;;
	;;  Yeah, ok, this routine is everywhere, and I didn't feel like reinventing it.
	;;  
PosObject:
        sec
        sta WSYNC
DivideLoop
        sbc #15        ; 2  2 - each time thru this loop takes 5 cycles, which is 
        bcs DivideLoop ; 2  4 - the same amount of time it takes to draw 15 pixels
        eor #7         ; 2  6 - The EOR & ASL statements convert the remainder
        asl            ; 2  8 - of position/15 to the value needed to fine tune
        asl            ; 2 10 - the X position
        asl            ; 2 12
        asl            ; 2 14
        sta.wx HMP0,X  ; 5 19 - store fine tuning of X
        sta RESP0,X    ; 4 23 - set coarse X position of object
        rts            ; 6 29

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

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Function to check for free space at end of cart.
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	echo "------", [$FFFA - *]d, "bytes free before End of Cartridge"
	
	ORG $FFFA
	.WORD ColdStart
	.WORD ColdStart
	.WORD ColdStart
