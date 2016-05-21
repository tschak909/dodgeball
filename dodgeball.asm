	PROCESSOR 6502

	include "vcs.h"

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Variables.
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	SEG.U VARIABLES
	
	ORG $80

BINVAR:		ds 1		; Master game variation control
BCDVAR:		ds 1		; BCD format
CLOCK:		ds 1		; Master frame clock
SHOWSCR:	ds 1		; Show/Hide Right Player score
GAMEON:		ds 1		; $00 = Attract, $FF = game on
SELECTDEBOUNCE:	ds 1		; Select switch debounce flag.
DIRECTION:	ds 3		; M0, M1, BL direction.
BOUNCECOUNT:	ds 3		; M0, M1, BL bounce count.
MXPFCOUNT:	ds 3		; M0, M1, BL collision bounce counting
SCORE:		ds 2		; Player scores in BCD
GAMVAR:		ds 1		; Game Variation bitwise descriptor
P0YPOS:		ds 1		; Player 0 Y POS
P1YPOS:		ds 1		; Player 1 Y POS
M0YPOS:		ds 1		; Missile 0 Y POS
M1YPOS:		ds 1		; Missile 1 Y POS
XOFFSET:	ds 1		; X Offset for pending HMOVE
XOFFBASE:	ds 1		; $0, $10, $20, $30 offset into X Offset Table
OLDBALLDIR:	ds 3		; Ball bearing before a pong bounce begins.
SCANLINE:	ds 1		; The current scanline on playfield.
LORES:		ds 6		; lo-res indirect pointers. Three of them.
SHAPES:		ds 1		; Pointer to player sprites.
HIRES:		ds 16		; Hi-res player sprites, 8 bytes * 2
TEMP1:		ds 1		; Temp storage for math ops
TEMP:		ds 1		; Temp storage for score.
TEMPSTACK:	ds 1		; Temp storage for stack pointer
DIFSWITCH:	ds 1		; Temp storage for console switches
COLOR0:		ds 1		; Colors loaded from color table for 0 and 1
COLOR1:		ds 1		;
XCOLOR0:	ds 1		;
XCOLOR1:	ds 1		;
COLORPF:	ds 1		;
COLORBK:	ds 1		;
KLSKIP:		ds 1		; Kernel lines to skip before score
GAMETIMER:	ds 1		; Game timer may use..may not.
NUMG0:		ds 1		; Storage for current score graphics
NUMG1:		ds 1		; Digit 1
SCROFF:		ds 4		; Score pattern offsets (4 bytes)
	
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Code
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	SEG CODE

	;; 2K Cartridge.
	ORG $F800

cold:
	SEI
	CLD
	LDX #$FF
	TXS
clrlp	STA #$00,X
	DEX
	BNE clrlp

	LDA #$3C
	STA HIRES+2
	STA HIRES+3
	STA HIRES+4
	STA HIRES+5
	
MLOOP:	JSR VCNTRL
	JSR GSGRCK
	JSR LDSTEL
	JSR ROT
	JSR SCROT
	JSR VOUT
	JMP MLOOP

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Vertical Control - Start VBLANK
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
VCNTRL  INC  CLOCK            ; Master frame count timer
	STA  HMCLR            ; Clear horizontal move registers.
	LDA  #2	              ; Get this ready...
	STA  WSYNC            ; for start of next line...
	STA  VBLANK           ; Start vertical blank.
	STA  WSYNC 
	STA  WSYNC            ; and do three lines
	STA  WSYNC
	STA  VSYNC            ; Now start vertical sync
	STA  WSYNC
	STA  WSYNC            ; and do three lines
	LDA  #0               ; get this ready
	STA  WSYNC
	STA  VSYNC            ; End of vertical sync pulse
	LDA  #43              ; And set VBLANK timer
	STA  TIM64T           ; with 64 clock interval.
	RTS  

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Game Select Game Reset Check
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
GSGRCK:	
	LDA  SWCHB              ; Start/Reset button....
	LSR                     ; Shove bit 0 into carry flag,
	BCS  NoNewGM            ; and if it's pushed...
	;
	; Start a new game.
	;
	LDA  #$0F
	STA  SHOWSCR            ; Show right score.
	LDA  #$FF               ; Set all bits
	STA  GAMEON             ; in GAMEON.  
	LDA  #$80   
	STA  GAMETIMER          ; and bit 7 of GAMETIMER (this is not too
	                        ; significant, as GAMETIMER rollover is
	                        ; only checked if GAMEON<>$00)
	LDX  #$E6
	JSR  ClearMem           ; zero out $89 thru $A2
	BEQ  ResetField         ; Unconditional branch
	;
NoNewGM LDY  #$02               ; Assume score to be drawn
	LDA  GAMETIMER          ; If game in play (GAMEON=$FF) AND
	AND  GAMEON             ; GAMETIMER < 7/8 finished @ $F0,
	CMP  #$F0               ; draw the score unconditionally.
	BCC  SCdrawn
	LDA  CLOCK              ; CLOCK used to flash score near end
	AND  #$30               ; of play, note the peripheral synchronization
	BNE  SCdrawn            ; with GAMETIMER's timing of the game, which
	                        ; always ends when CLOCK & $3F = 0.  CLOCK
	                        ; is used here because the score blink
	                        ; off duty cycle is a too quick for
	                        ; GAMETIMER to handle, being about 1/3 sec.
	LDY  #$0E               ; Set this for no score
SCdrawn STY  KLSKIP             ; where the Kernal will find it
	LDA  CLOCK
	AND  #$3F               ; CLOCK also used to slow debounce reset
	BNE  ChkSel
	;
	; GAMETIMER is incremented and SELECTDEBOUNCE reset when
	; CLOCK & $3F = 0.  This occurs 1 frame out of 64 or
	; about once/second.  Thus the game is 128*64 frames
	; or about 2 minutes long.
	;
	STA  SELECTDEBOUNCE           ; Reset Select Debounce Flag.  This is
	                        ; what keeps incrementing the selection
	                        ; if you hold Select down for a long time.
	INC  GAMETIMER          ; increment the Main Game ~1-sec Timer.
	BNE  ChkSel             ; if GAMETIMER rolls over,
	STA  GAMEON             ; zero GAMEON -- game over
	;
ChkSel  LDA  SWCHB              ; Select button???
	AND  #$02
	BEQ  SelDown
	STA  SELECTDEBOUNCE           ; Set flag: Sel has not been down
	BNE  CS_RTS             ; Unconditional branch
	;
SelDown BIT  SELECTDEBOUNCE           ; If Sel has been down,
	BMI  CS_RTS		; don't select a new game.
	;
	INC  BINVAR             ; SELECT: Go to next game.
ClrGam  LDX  #$DF               ; Clear data from current game ($82-$A2)
ClrGRST JSR  ClearMem
	LDA  #$FF
	STA  SELECTDEBOUNCE           ; Set flag: Sel has been down.
	LDY  BINVAR
	LDA  VARMAP,Y           ; Get feature bits for this variation.
	STA  GAMVAR
	EOR  #$FF               ; #$FF signifies end of variations
	BNE  SelGO              ; Not at end yet, set up new game
	LDX  #$DD		; Clear $80-$A2; resets BINVAR, BCDVAR
	BNE  ClrGRST            ; so we start over. BNE is unconditional.
	;
SelGO	LDA  BCDVAR		; Since we have incremented BINVAR, we
	SED                     ; must increment BCDVAR in BCD to keep
	CLC                     ; it in sync. Note BCDVAR is actually
	ADC  #1                 ; BinVar+1, since it's incremented when
	STA  BCDVAR             ; we reset but don't increment BINVAR.
	STA  SCORE              ; Display variation as score 0
	CLD
	;; BIT  GAMVAR             ; GAMSHP was reset at ClrGam...
	;; BPL  ResetField         ; if this is a plane game,
	;; INC  GAMSHP             ; increase GAMSHP.
	;; BVC  ResetField         ; if this is a jet game,
	;; INC  GAMSHP             ; increase GAMSHP further still.

	;
	; Branches here when game is started, too.
	;
ResetField 
	JSR  InitPF
	;
	; Assuming plane game for now, we set the right player
	; at a slightly higher position than the left player,
	; and the position of the right player is irrelevant.
	;
	LDA  #50
	STA  P1YPOS
	LDA  #134
	STA  P0YPOS
	BIT  GAMVAR             ; Check to see if it is a tank game.
	BMI  CS_RTS             ; Nope, bail.
	                        ; It is a tank game, so
	STA  P1YPOS             ; Right tank has same Y value,
	STA  RESP1              ; and tank is at opposite side.
	LDA  #$00
	STA  DIRECTION+1	; and right player faces left.
	LDA  #$20
	STA  HMP0
	STA  HMP1
	STA  WSYNC
	STA  HMOVE		
CS_RTS	RTS  

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Initialize Playfield
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
InitPF	LDX  #$00               ; 0=tank, 1=biplane, 2=jet
	LDA  SPRLO,X            ; Set up base pointer to all
	STA  SHAPES             ; sprite shapes which will
	LDA  SPRHI,X            ; be used in this game.
	STA  SHAPES+1
	;
	LDA  GAMVAR             ; Now set up PF_PONG and playfield type
	LSR   
	LSR   
	AND  #$03               ; bits 0,1=maze (playfield) type.
	TAX                     ; send it to X.
	LDA  GAMVAR
	BPL  IFgo               ; Branch not plane game, PF_PONG=GAMVAR
	AND  #$08               ; Test for clouds
	BEQ  IF80               ; Branch if no clouds
	LDX  #$03               ; change "maze type" in X to 3 ("clouds")
	BPL  IFskip             ; Unconditional skip to next test,
	                        ; leaving PF_PONG set to 0.
IF80    LDA  #$80               ; Change PF_PONG to #$80
	                        ; (enable playfield, no Pong)
IFgo    NOP
	NOP			; store GAMVAR or #$80 in PF_PONG.
IFskip  LDA  GAMVAR             ; Next test..
	ASL   
	ASL                     ; Do this again....
	BIT  GAMVAR 
	BMI  IFnoPlane          ; Branch if a plane game.
	STA  WSYNC              ; This MUST be something that dropped
	                        ; through the cracks, there is NO reason!
	;; STA  BILLIARD	 	; Store GAMVAR*4 in 84 (bit 6 = Billiard Hit)
	AND  #$80               ; IF it's a tank game.
IFnoPlane
	;; STA  GUIDED	 ; set guided missile flag.
	;
	; GUIDED is ZERO if a tank game
	; it is negative if a guided missile game,
	; it is overflowed if a machine gun game.
	; (Inapplicable in tank games, hence the
	; previous branch trick)
	;
	LDA  #>PF0_0            ; Store page of first PF map
	STA  LORES+1            ; as high order byte
	STA  LORES+3            ; for all of these pointers,
	STA  LORES+5            ; 'cause that's where it is.
	;
	; Store the proper offsets for each column of
	; playfield from the vectors given
	;
	LDA  PLFPNT,X
	STA  RESP0              ; Reset player 0 while we're at it.
	STA  LORES 
	LDA  PLFPNT+4,X
	STA  LORES+2
	LDA  PLFPNT+8,X
	STA  LORES+4
	RTS  

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Load Stella Registers
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
LDSTEL	LDA  GAMVAR
	AND  #$87
	BMI  LDmult
	;
	; If bit 7 is set, we are playing with one or more
	; planes.  If not, well, we can only have one tank,
	; so...
	;
	LDA  #$00
LDmult	ASL   
	TAX  
	LDA  WIDTHS,X           ; The TIA's NUSIZ registers make
	STA  NUSIZ0             ; it as easy to play with two or
	LDA  WIDTHS+1,X         ; three planes as it is for one
	STA  NUSIZ1             ; freakin' huge bomber.
	LDA  GAMVAR
	AND  #$C0
	LSR   
	LSR   
	LSR   
	LSR                     ; Our hardware is now in bits 3 and 2.
	TAY                     ; Of the Y-register.
	;
	; Render joysticks immobile if game not in play, and
	; select player and field colors according to Y
	;
	LDA  GAMEON             ; Enable joysticks via bit 1
	STA  SWCHB              ; of $FF game-on value
	EOR  #$FF               ; now $FF=no game, $00=game on
	AND  GAMETIMER          ; Cycle tank colors only when NO
	STA  TEMP1              ; game on (attract mode)
	LDX  #$FF
	LDA  SWCHB
	AND  #$08               ; Color/BW switch
	BNE  LDcolor            ; Branch if set to Color
	LDY  #$04               ; Force B&W colors
	LDX  #$0F
LDcolor	STX  TEMP
	LDX  #$03               ; We loop 3 times to get 4 values
LDcol0	LDA  ColorTable,Y
	EOR  TEMP1              ; Apply color-cycle if no game on
	AND  TEMP               ; Apply B&W massage 
	STA  COLUP0,X           ; Color the real item.
	STA  COLOR0,X           ; Color the virtual item.  This can
	                        ; be changd, e.g. invisible tanks
	STA  XCOLOR0,X          ; Color the deep virtual item. This
	                        ; is used to restore ColorX.
	INY  
	DEX  
	BPL  LDcol0
	RTS  

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Score offset calculation, aka
;;; ;; How I learned to love multiplying by 5
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SCROT	LDX  #$01
SCROT0	LDA  SCORE,X
	AND  #$0F               ; Lo nibble
	STA  TEMP
	ASL                     ; *2
	ASL                     ; *4
	CLC
	ADC  TEMP               ; + original * 1 = original * 5
	STA  SCROFF,X
	LDA  SCORE,X
	AND  #$F0               ; Repeat for hi nibble.  Starts *16
	LSR                     ; *8
	LSR                     ; *4
	STA  TEMP
	LSR                     ; *2
	LSR                     ; *1
	CLC  
	ADC  TEMP               ; + (*4) = original * 5
	STA  SCROFF+2,X
	DEX  
	BPL  SCROT0             ;Decrement & repeat once for P0
	RTS  

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Calculate sprite offsets.
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ROT	LDA  #$01               ; The LO byte of CLOCK used to
	AND  CLOCK              ; select alternate players on
	TAX                     ; alternate frames
	LDA  #$00
	STA  REFP0,X            ; Step 1 taken care of.
	AND  #$0F		;
	TAY                     ; Y = DIRECTN[X] & 0x0F.
	;; BIT  GUIDED     
	BPL  ROTnoGM            ; If it's a guided missile game,
	;; STY  DIRECTN+2,X        ; copy player bearings to missile
ROTnoGM TXA                     ; X ^= $0E,
	EOR  #$0E
	TAX 
	TYA  
	ASL   
	ASL   
	ASL   
	CMP  #$3F               ; And so step 2 begins...
	CLC  
	BMI  ROTnoFlip          ; Branch if <180 deg.
	SEC
	EOR  #$47    ;The EOR sets bits 0-2, and clears bit 4
	;             to subtract 180 degrees from the memory
	;             pointer, too.
ROTnoFlip TAY  
	;
	;Put all the shapes where they ought to be.
	;
ROTnext	LDA  (SHAPES),Y
	STA  HIRES,X
	BCC  ROTinc 
	DEY                     ; Decrement instead of increment
	DEY                     ; plus cancel the upcoming INY.
ROTinc	INY                     ; More of step 2.
	DEX  
	DEX                     ; X-=2.
	BPL  ROTnext            ; Do for both, 1 then 0 then stop.
	RTS  
	
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; vout - The kernel, where stuff
;;; ;; gets banged out to the visible.
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

VOUT	LDA  #$20
	STA  SCANLINE           ; We're assuming scanline $20.
	STA  WSYNC
	STA  HMOVE              ; Move sprites horizontally.
VOUT_VB	LDA  INTIM
	BNE  VOUT_VB            ; Wait for INTIM to time-out.
	STA  WSYNC
	STA  CXCLR              ; Clear collision latches
	STA  VBLANK             ; End vertical blank
	TSX  
	STX  TEMPSTACK             ; Save stack pointer
	LDA  #$02
	STA  CTRLPF             ; Double, instead of reflect.
	LDX  KLSKIP
Vskip1  STA  WSYNC              ; Skip a few scanlines...
	DEX  
	BNE  Vskip1
	LDA  KLSKIP
	CMP  #$0E               ; "No Score" value of KLSKIP
	BEQ  Vmain
	;
	; KLSKIP is set as such so that when the score is
	; to be displayed, it waits for just the right time
	; to start drawing the score, but if the score is
	; not to be displayed, as when the score flashes
	; signifying "time's almost up", it waits for just
	; the right time to start drawing the rest of the
	; screen.
	;
	; Draw the score:
	;
	LDX  #$05      	        ; Score is five bytes high.
	LDA  #$00               ; Clear number graphics.
	STA  NUMG0              ; They won't be calculated yet,
	STA  NUMG1              ; but first time through the loop
		                ; the game will try to draw with
		                ; them anyway.
VSCOR	STA  WSYNC              ; Start with a fresh scanline.
	LDA  NUMG0              ; Take last scanline's left score,
	STA  PF1                ; and recycle it,
	;
	; Here, we begin drawing the next scanline's
	; left score, as the electron beam moves towards
	; the right score's position in this scanline.
	;
	LDY  SCROFF+2       
	LDA  NUMBERS,Y          ; Get left digit.
	AND  #$F0       
	STA  NUMG0     
	LDY  SCROFF
	LDA  NUMBERS,Y          ; Get right digit.
	AND  #$0F
	ORA  NUMG0
	STA  NUMG0              ; Left score is ready to ship.
	LDA  NUMG1              ; Take last scanline's right score,
	STA  PF1                ; and recycle it.
	LDY  SCROFF+3
	LDA  NUMBERS,Y          ; Left digit...
	AND  #$F0
	STA  NUMG1
	LDY  SCROFF+1
	LDA  NUMBERS,Y          ; right digit...
	AND  SHOWSCR
	;
	; Now, we use our fresh, new score graphics in this next scanline.
	;
	STA  WSYNC                         ; *COUNT*
	ORA  NUMG1   ;Finish calculating     (0) +3
	STA  NUMG1   ;right score.           (3) +3
	LDA  NUMG0                         ; (6) +3
	STA  PF1                           ; *9* +3
	;
	; We use this time to check whether we're at the end of our loop.
	;
	DEX                                ; (12)+2
	BMI  Vmain                         ; (14)+2 No Branch
	;
	; If so, we're out of here.  Don't worry, the score will be
	; cleared immediately, so nobody will know that we've gone
	; past five bytes and are displaying garbage.
	;
	INC  SCROFF                        ; (16)+5
	INC  SCROFF+2           ; Get ready to draw the next
	INC  SCROFF+1           ; line of the byte.
	INC  SCROFF+3
	LDA  NUMG1
	STA  PF1                ; Right score is in place.
	JMP  VSCOR              ; Go to next scanline,
	;
	; Main Kernal Display loop for the game itself
	;
Vmain	LDA  #$00               ; Inner Display Loop
	STA  PF1                ; Clear the score.
	STA  WSYNC              
	LDA  #$01
	STA  CTRLPF             ; Reflecting playfield.
	LDA  COLOR0
	STA  COLUP0             ; How often must THIS be done?
	LDA  COLOR1
	STA  COLUP1
Vfield	LDX  #$1E               ; Very Sneaky -
	TXS                     ; Set stack to missile registers
	SEC
	;  
	; This yields which line of player 0 to draw.
	;
	LDA  P0YPOS
	SBC  SCANLINE           ; A=P0YPOS-SCANLINE
	AND  #$FE               ; Force an even number
	TAX                     ; Only sixteen bytes of 
	AND  #$F0               ; sprite memory, so...
	BEQ  VdoTank            ; If not valid,
	LDA  #$00               ; blank the tank.
	BEQ  VnoTank            ;         (unconditional branch)
VdoTank LDA  HIRES,X            ; Else, load the appropriate byte.
VnoTank STA  WSYNC              ; ----END OF ONE LINE----
	STA  GRP0               ; Just for player 0.
	;
	; The infamous Combat Stack Trick:
	;
	; Keep in mind that at this point, the stack pointer
	; is set to the missile registers, and the "zero-result"
	; bit of the P register is the same at the bit ENAM0/1
	; looks at.
	;
	LDA  M1YPOS
	EOR  SCANLINE
	AND  #$FE
	PHP                     ; This turns the missle 1 on/off
	LDA  M0YPOS
	EOR  SCANLINE  
	AND  #$FE
	PHP                     ; This turns the missle 0 on/off
	;
	; We've got the missile taken care of.
	; Now let's see which line of the playfield to draw.
	;
	LDA  SCANLINE
	BPL  VvRefl             ; If on the bottom half of the screen,
	EOR  #$F8               ; reverse direction so we can mirror.
VvRefl	CMP  #$20
	BCC  VfDone             ; Branch if at bottom.
	LSR   
	LSR   
	LSR                     ; Divide by eight,
	TAY                     ; and stow it in the Y-register.
	;
	; By now, the electron beam is already at the next
	; scanline, so we don't have to do a STA WSYNC.
	;
	; This yields which line of Tank 1 to draw.
	;
VfDone	LDA  P1YPOS             ; P1YPOS is other player's position.
	SEC  
	SBC  SCANLINE           ; A=P1YPOS - SCANLINE
	INC  SCANLINE           ; Increment the loop.
	NOP
	ORA  #$01               ; Add bit 0, force odd number.
	TAX
	;
	AND  #$F0               ; There are only sixteen bytes of
	BEQ  VdoT1              ; sprite memory, so...
	LDA  #$00               ; If tank is not ready, blank it.
	BEQ  VnoT1
VdoT1   LDA  HIRES,X            ; Else, draw the tank
VnoT1   NOP
	NOP
	STA  GRP1
	NOP
	NOP			; If PF_PONG bit 7 set, don't write PF
	LDA  (LORES),Y          ; (this means game variation has blank
	STA  PF0                ; background)
	LDA  (LORES+2),Y
	STA  PF1
	LDA  (LORES+4),Y
	STA  PF2
VnoPF	INC  SCANLINE           ; One more up in the loop.
	LDA  SCANLINE
	EOR  #$EC               ; When we've reached the $ECth line,
	BNE  Vfield             ; we've had enough.
	LDX  TEMPSTACK             ; Restore stack pointer, which is
	TXS                     ; is used for calls in main game loop
	STA  ENAM0              ; Clear a bunch of registers.
	STA  ENAM1
	STA  GRP0
	STA  GRP1
	STA  GRP0               ; In case GRP0 isn't COMPLETELY zeroed.
	STA  PF0
	STA  PF1
	STA  PF2
	RTS  
	
ClearMem
	LDA  #$00
ClrLoop	INX  
	STA  $A2,X
	BNE  ClrLoop  ;Continue until X rolls over.
	RTS  

	
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Tables
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

WIDTHS	.BYTE  $00 ,$00  ;1 vs. 1
	.BYTE  $01 ,$01  ;2 vs. 2
	.BYTE  $00 ,$03  ;1 vs. 3
	.BYTE  $27 ,$03  ;Bomber vs. 3

	
	;        Complex   ,    None 
	;        Simple    ,   Clouds
PLFPNT
	.BYTE  <(PF0_0-4) ,<(PF0_0-4)
	.BYTE  <(PF0_0-4) ,<(PF0_3-4)   ;PF0
	.BYTE  <(PF1_0-4) ,<(PF1_1-4)
	.BYTE  <(PF1_2-4) ,<(PF1_3-4)   ;PF1
	.BYTE  <(PF2_0-4) ,<(PF1_1-4)
	.BYTE  <(PF2_2-4) ,<(PF1_3-4)   ;PF2

	
ColorTable:
	.byte $32, $2C, $8A, $DA ; Color
	.byte $08, $04, $00, $0E ; Black and White.

SPRLO	.byte <SHAPE
SPRHI	.byte >SHAPE
	
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Graphics data
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PF0_0  .byte $F0 ; |XXXX    | $F779
       .byte $10 ; |   X    | $F77A
       .byte $10 ; |   X    | $F77B
       .byte $10 ; |   X    | $F77C
       .byte $10 ; |   X    | $F77D
       .byte $10 ; |   X    | $F77E
       .byte $10 ; |   X    | $F77F
       .byte $10 ; |   X    | $F780
       .byte $10 ; |   X    | $F781
       .byte $10 ; |   X    | $F782
       .byte $10 ; |   X    | $F783
       .byte $10 ; |   X    | $F784
PF1_0  .byte $FF ; |XXXXXXXX| $F785
       .byte $00 ; |        | $F786
       .byte $00 ; |        | $F787
       .byte $00 ; |        | $F788
       .byte $38 ; |  XXX   | $F789
       .byte $00 ; |        | $F78A
       .byte $00 ; |        | $F78B
       .byte $00 ; |        | $F78C
       .byte $60 ; | XX     | $F78D
       .byte $20 ; |  X     | $F78E
       .byte $20 ; |  X     | $F78F
       .byte $23 ; |  X   XX| $F790
PF2_0  .byte $FF ; |XXXXXXXX| $F791
       .byte $80 ; |X       | $F792
       .byte $80 ; |X       | $F793
       .byte $00 ; |        | $F794
       .byte $00 ; |        | $F795
       .byte $00 ; |        | $F796
       .byte $1C ; |   XXX  | $F797
       .byte $04 ; |     X  | $F798
       .byte $00 ; |        | $F799
       .byte $00 ; |        | $F79A
       .byte $00 ; |        | $F79B
       .byte $00 ; |        | $F79C
PF1_1  .byte $FF ; |XXXXXXXX| $F79D
PF0_3  .byte $00 ; |        | $F79E
       .byte $00 ; |        | $F79F
       .byte $00 ; |        | $F7A0
PF1_3  .byte $00 ; |        | $F7A1
       .byte $00 ; |        | $F7A2
       .byte $00 ; |        | $F7A3
       .byte $00 ; |        | $F7A4
       .byte $00 ; |        | $F7A5
       .byte $00 ; |        | $F7A6
       .byte $00 ; |        | $F7A7
       .byte $00 ; |        | $F7A8
       .byte $00 ; |        | $F7A9
       .byte $07 ; |     XXX| $F7AA
       .byte $1F ; |   XXXXX| $F7AB
       .byte $3F ; |  XXXXXX| $F7AC
       .byte $7F ; | XXXXXXX| $F7AD
PF1_2  .byte $FF ; |XXXXXXXX| $F7AE
       .byte $00 ; |        | $F7AF
       .byte $00 ; |        | $F7B0
       .byte $00 ; |        | $F7B1
       .byte $00 ; |        | $F7B2
       .byte $00 ; |        | $F7B3
       .byte $00 ; |        | $F7B4
       .byte $00 ; |        | $F7B5
       .byte $00 ; |        | $F7B6
       .byte $60 ; | XX     | $F7B7
       .byte $20 ; |  X     | $F7B8
       .byte $21 ; |  X    X| $F7B9
PF2_2  .byte $FF ; |XXXXXXXX| $F7BA
       .byte $00 ; |        | $F7BB
       .byte $00 ; |        | $F7BC
       .byte $00 ; |        | $F7BD
       .byte $80 ; |X       | $F7BE
       .byte $80 ; |X       | $F7BF
       .byte $80 ; |X       | $F7C0
       .byte $80 ; |X       | $F7C1
       .byte $00 ; |        | $F7C2
       .byte $00 ; |        | $F7C3
       .byte $00 ; |        | $F7C4
       .byte $07 ; |     XXX| $F7C5
	
NUMBERS:
	.byte $0E ; |    XXX |    Leading zero is not drawn
	.byte $0A ; |    X X |    because it's never used.
	.byte $0A ; |    X X |      
	.byte $0A ; |    X X |      
	.byte $0E ; |    XXX |      
	
	.byte $22 ; |  X   X |      
	.byte $22 ; |  X   X |      
	.byte $22 ; |  X   X |      
	.byte $22 ; |  X   X |      
	.byte $22 ; |  X   X |      
	
	.byte $EE ; |XXX XXX |      
	.byte $22 ; |  X   X |      
	.byte $EE ; |XXX XXX |      
	.byte $88 ; |X   X   |      
	.byte $EE ; |XXX XXX |      
	
	.byte $EE ; |XXX XXX |      
	.byte $22 ; |  X   X |      
	.byte $66 ; | XX  XX |      
	.byte $22 ; |  X   X |      
	.byte $EE ; |XXX XXX |      
	
	.byte $AA ; |X X X X |      
	.byte $AA ; |X X X X |      
	.byte $EE ; |XXX XXX |      
	.byte $22 ; |  X   X |      
	.byte $22 ; |  X   X |      
	
	.byte $EE ; |XXX XXX |      
	.byte $88 ; |X   X   |      
	.byte $EE ; |XXX XXX |      
	.byte $22 ; |  X   X |      
	.byte $EE ; |XXX XXX |      
	
	.byte $EE ; |XXX XXX |      
	.byte $88 ; |X   X   |       
	.byte $EE ; |XXX XXX |      
	.byte $AA ; |X X X X |       
	.byte $EE ; |XXX XXX |      
	
	.byte $EE ; |XXX XXX |      
	.byte $22 ; |  X   X |      
	.byte $22 ; |  X   X |      
	.byte $22 ; |  X   X |      
	.byte $22 ; |  X   X |      
	
	.byte $EE ; |XXX XXX |      
	.byte $AA ; |X X X X |      
	.byte $EE ; |XXX XXX |      
	.byte $AA ; |X X X X |      
	.byte $EE ; |XXX XXX |      
	
	.byte $EE ; |XXX XXX |      
	.byte $AA ; |X X X X |      
	.byte $EE ; |XXX XXX |      
	.byte $22 ; |  X   X |      
	.byte $EE ; |XXX XXX |      

SHAPE:	

	.byte $00
	.byte $00
	.byte $3C
	.byte $3C
	.byte $3C
	.byte $3C
	.byte $00
	.byte $00
	
VARMAP:
	.byte $06
	.byte $04
	.byte $02
	.byte $01
	
	;; .BYTE  $48 ;Game 6:  0100 1000  TANK PONG
	;; .BYTE  $40 ;Game 7:  0100 0000
	;; .BYTE  $54 ;Game 8:  0101 0100
	;; .BYTE  $58 ;Game 9:  0101 1000
	;; .BYTE  $25 ;Game 10: 0010 0101  INVISIBLE TANK
	;; .BYTE  $29 ;Game 11: 0010 1001
	;; .BYTE  $49 ;Game 12: 0100 1001  INVISIBLE TANK-PONG
	;; .BYTE  $55 ;Game 13: 0101 0101
	;; .BYTE  $59 ;Game 14: 0101 1001
	;; .BYTE  $A8 ;Game 15: 1010 1000  BIPLANE
	;; .BYTE  $88 ;Game 16: 1000 1000
	;; .BYTE  $98 ;Game 17: 1001 1000
	;; .BYTE  $90 ;Game 18: 1001 0000
	;; .BYTE  $A1 ;Game 19: 1010 0001
	;; .BYTE  $83 ;Game 20: 1000 0011
	;; .BYTE  $E8 ;Game 21: 1110 1000  JET FIGHTER
	;; .BYTE  $C8 ;Game 22: 1100 1000
	;; .BYTE  $E0 ;Game 23: 1110 0000
	;; .BYTE  $C0 ;Game 24: 1100 0000
	;; .BYTE  $E9 ;Game 25: 1110 1001
	;; .BYTE  $E2 ;Game 26: 1110 0010
	;; .BYTE  $C1 ;Game 27: 1100 0001
	;; ;
	;; ; $FF to signify end of game variations.
	;; ;
	.BYTE  $FF
	
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Processor start vectors
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	ORG $FFFA
	.WORD cold
	.WORD cold
	.WORD cold

