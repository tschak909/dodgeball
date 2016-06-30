	;;
	;; Temporary scratchpad for code bits being rethought or refactored
	;;

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
