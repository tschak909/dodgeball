	;;
	;; Temporary scratchpad for code bits being rethought or refactored
	;;

                                                             |
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Parse velocity variables, and adjust player motion
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;;
;;; ;; x = current player
;;; ;; y = velocity table offset
;;; ;;
DoMotion:
	ldx #$00		; start with player 0
DoHorizMotion:
	lda P0XVelocity,x
	cmp #$00
	beq DoVertMotion	; no horiz velocity == no horiz collision checks.
	lda #$00		; clear velocity flip variable
	sta VelocityFlip	;
	lda #$80		; Assume waiting until proven not.
	sta P0XIsWaiting,x	;
	ldy P0XVelocity,x	; Get velocity index
	bpl HorizLeft		; if it's not signed, hmove goes left.
HorizRight:
	lda #$FF		; otherwise, set the VelocityFlip to $FF for eor later.
	sta VelocityFlip	; 
	tya			; move Y to A so that we can get rid of the sign.
	and #$0F		; mask off top 4 bits. getting rid of sign.
	tay			; move A back to Y.
HorizLeft:
	lda HORIZ_VELOCITY,y	; Get value from index
	and #$0F		; mask off the HMOVE component, leaving initial delay
	sta VelocityTemp	; store in temp
	lda Frame		; get current frame
	and VelocityTemp	; mask against initial delay
	bne DoVertMotion	; and skip to vertical motion if we aren't ready to move.
	lda #$00		; not waiting, clear the waiting bit.
	sta P0XIsWaiting,x	;
	lda HORIZ_VELOCITY,y	; otherwise, grab the horizontal velocity
	sec			; If we need to complement, flip it over.
	eor VelocityFlip	;
	adc #$00		;
	sta Temp		; Hold onto calculated velocity value, for a moment.
	lda CXP0FB,x		; Check P0/P1 to PF collision.
	bpl ExHorizMotion	; If we haven't collided with anything, execute the motion.
HorizCollided:
	lda P0XVelocity,x
	and #$F0
	cmp #$F0
	bne HorizCollided1
	lda #$50
	sta Temp
	bvc ExHorizMotion
HorizCollided1:
	lda #$A0
	sta Temp
ExHorizMotion:
	lda Temp		; Bring back velocity.
	sta HMP0,X		; and plop it into the right HMOVE register (lower 4 bits ignored)
DoVertMotion:
	lda #$00		; clear the velocity flip
	sta VelocityFlip	;
	lda #$80		; Set Y is waiting, until proven otherwise.
	sta P0YIsWaiting,x	;
	ldy P0YVelocity,x	; Get the requested player velocity index.
	bpl VertDown		; sign bit = go up
VertUp:
	tya			; do a little register gymnastics, to get rid of the sign
	and #$0F		; ...mask it off.
	tay			; and flip it back into the Y.
	lda VERT_VELOCITY,y	; Get the requested velocity value from computed index
	and #$0F		; mask off the after delay (AND MASK) move Y
	sta VelocityTemp	; store in velocity temp
	lda Frame		; get current frame
	and VelocityTemp	; and the delay
	bne DoNextPlayer	; if we're not ready to move the Y axis for this player, skip ahead.
	lda #$00		; clear P0 Y is waiting, we're not...
	sta P0YIsWaiting,x	; 
	lda VERT_VELOCITY,y	; else get the requested Y movement amount
	lsr			; shift it over into the lower four bits
	lsr			;
	lsr			;
	lsr			;
	sta VelocityTemp	; keep it in Velocity temp
	lda CXP0FB,X		; Check collision register for P0/P1->PF
	bpl DoVertUp		; If we haven't collided, do normal vertical up
VertUpCollided:
	lda P0YVelocity,x
	sec
	eor VelocityFlip
	adc #$00
	cmp #$00
	beq DoNextPlayer
	lda PlayerY0,X		; get player's current Y
	sec			; clear carry for add
	adc #$06		; add the requested Y amount
	sta PlayerY0,X		; store it back in player's current Y
	jmp DoNextPlayer	; do the next player.	
DoVertUp:	
	lda PlayerY0,X		; get player's current Y
	sec			; set carry for subtract
	sbc VelocityTemp	; subtract the requested Y amount
	sta PlayerY0,X		; store it back in player's current Y
	jmp DoNextPlayer	; do the next player.
VertDown:
	lda VERT_VELOCITY,y	; get requested vertical velocity index
	and #$0F		; lop off the after delay movement value
	sta VelocityTemp	; store in velocity temp
	lda Frame		; get current frame #
	and VelocityTemp	; and against stored delay
	bne DoNextPlayer	; if we're not ready to move, jump already to next player.
	lda #$00		; Clear player y is waiting, if we're not.
	sta P0YIsWaiting,y      ; 
	lda VERT_VELOCITY,y	; otherwise, get the requested velocity
	and #$F0		; shift it over to get the # of Y units to move down
	lsr			;
	lsr			;
	lsr			;
	lsr			; ...four times
	sta Temp
	lda CXP0FB,X
	bpl DoVertDown
VertDownCollided:
	lda P0YVelocity,x
	sec
	eor VelocityFlip
	adc #$00
	cmp #$00
	beq DoNextPlayer
	lda PlayerY0,X		; get player's current Y
	sec			; set carry for subtract
	sbc #$06		; subtract the requested Y amount
	sta PlayerY0,X		; store it back in player's current Y
	jmp DoNextPlayer	; do the next player.	
DoVertDown:
	lda Temp
	clc			; clear carry to prepare for add
	adc PlayerY0,X		; add Y value to player0
	sta PlayerY0,X		; and store it right back.
DoNextPlayer:
	inx			; increment player value
	cpx #$02		; if player=2 we're done, fall through.
	beq PrepScoreForDisplay		; otherwise, go back and handle the next player.
	jmp DoHorizMotion	; (we kinda went over the branch, ugh.)
















	
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Motion code
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Motion:
       ldx #$00                ; Start with player 0
       lda SWCHA               ; get player switches

CheckRight:
       asl
       bcs CheckLeft
       tay
       lda P0XVelocity,x
       cmp #$F8
       beq RightMaxVelocity
       clc
       adc #$01
       ora #$F0
       sta P0XVelocity,x
RightMaxVelocity:
       tya
CheckLeft:
       asl
       bcs CheckDown
       tay
       lda P0XVelocity,x
       cmp #$08
       beq LeftMaxVelocity
       clc
       adc #$01
       and #$0F
       sta P0XVelocity,x
LeftMaxVelocity:
       tya
CheckDown:
       asl
       bcs CheckUp
       tay
       lda P0YVelocity,x
       cmp #$08
       beq DownMaxVelocity
       clc
       adc #$01
       and #$0F
       sta P0YVelocity,x
DownMaxVelocity:
       tya
CheckUp:
       asl
       bcs StickEnd
       tay
       lda P0YVelocity,x
       cmp #$F8
       beq UpMaxVelocity
       clc
       adc #$01
       ora #$F0
       sta P0YVelocity,x
UpMaxVelocity:
       tya
StickEnd:
       inx
       cpx #$02
       bne CheckRight

VelocityDecay:
       lda SWCHA               ; Read the sticks again.
       cmp #$FF
       bne PrepScoreForDisplay         ; Do not do velocity decay if joystick isn't still

       ldx #$00                ; Start with player 0 X, end with 1 Y (4 values)

DoDecay:
       lda P0XVelocity,x
       and #$F0
       sta VelocityTemp
       lda P0XVelocity,x
       and #$0F
       cmp #$00
       beq DecayNext
       sec
       sbc #$01
	ora VelocityTemp
	cmp #$F0
	bne StoreDecay
	lda #$00
StoreDecay:	
       sta P0XVelocity,x
DecayNext:
       inx
       cpx #$05
       bne DoDecay
