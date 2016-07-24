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

;;; Contains code from Combat (CX-2601) by
;;; Joe Decuir and Larry Wagner

	PROCESSOR 6502

	include vcs.h
	include macro.h

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Constants
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

VBLANK_WAIT_TIME	EQU	43 ; TIM64T based vblank waiting time.
	
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Variables
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	SEG.U VARS
	ORG $80

BINvar:		ds 1
BCDvar:		ds 1
JMove:		ds 2
NU3:		ds 1
NU4:		ds 1
CLOCK:		ds 1
SHOWSCR:	ds 1
GameOn:		ds 1
SelDbnce:	ds 1
NU5:		ds 1
Vtemp:		ds 1
FwdTimer:	ds 4
LastTimer:	ds 1
LastTimer2:	ds 1
LastTurn:	ds 1
LastTurn2:	ds 1
DIRECTN:	ds 5
MisLife:	ds 3
BounceCount:	ds 3
MxPFcount:	ds 3
NU10:		ds 1
SCORE:		ds 2
GAMVAR:		ds 1
PlayerY0:	ds 1
PlayerY1:	ds 1
BallY0:		ds 1
BallY1:		ds 1
BallY2:		ds 1
MVadjA:		ds 2
MVadjB:		ds 2
MPace:		ds 1
XOFFS:		ds 1
XoffBase:	ds 1
OldMisDir:	ds 3
ScanLine:	ds 1
LORES:		ds 6
SHAPES:		ds 2
HIRES:		ds 16
TEMP1:		ds 1
TEMP:		ds 1
TMPSTK:		ds 1
TEMP2:		ds 1
DIFSWCH:	ds 1
Color0:		ds 1
Color1:		ds 1
XColor0:	ds 1
XColor1:	ds 1
ColorPF:	ds 1
ColorBK:	ds 1
KLskip:		ds 1
GameTimer:	ds 1
NUMG0:		ds 1
NUMG1:		ds 1
SCROFF:		ds 4
COLcount:	ds 3
	

	
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Code Segment
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	SEG CODE
	ORG $F800		; 2K Cartridge

ColdStart:
	CLEAN_START		; defined in macro.h
	LDA #$10
	STA SWCHB+1
	STA GameOn
	JSR ClrGam
	
MLOOP:	JSR VCNTRL		; Generate VSYNC; Enter VBLANK
	;;
	;; VBLANK Logic
	;;
	JSR CONSWT		; Parse Console Switches
	JSR LDSTEL		; Load Stella Registers
	JSR JOYSTK		; Check Joystick Switches
	JSR COLIS		; Check Collision Registers
	JSR SETMOT		; Setup Motion of Objects
	JSR SETPLR		; Rotate players (This will be simplified.)
	JSR SCROT		; Calculate Score Offsets
	;;
	;; Kernel Code
	;;
	JSR VOUT		; Clock out the screen

	JMP MLOOP

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Vertical control
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

VCNTRL:	INC CLOCK		; Master frame count timer.
	STA HMCLR		; Clear Horizontal Move Registers.
	LDA #$02		; Get ready to set VBLANK by setting bit 2
	STA WSYNC		; Wait until the next line.
	STA VBLANK		; and flip on VBLANK (no drawing of anything but black)
	STA WSYNC		;
	STA WSYNC		; do 3 lines of vblank, before...
	STA WSYNC		;
	STA VSYNC		; we latch on the VSYNC (ultra black)
	STA WSYNC		;
	STA WSYNC		; Do 3 lines of VSYNC, before...
	LDA #$00		;
	STA WSYNC		;
	STA VSYNC		; We flip off the VSYNC
	LDA #VBLANK_WAIT_TIME	; Set vblank waiting time
	STA TIM64T		; and we're off!
	RTS

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; VOUT - The Kernel
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	;;
	;; Strobe HMOVE and wait for the end of the Vertical Blank
	;; Also clear collision registers.
	;; 
VOUT:	LDA #$20		; assume scanline 32
	STA ScanLine		;
	STA WSYNC		; wait until next line...
	STA HMOVE		; strobe HMOVE
VOUTW:	LDA INTIM		;
	BNE VOUTW		; Wait for VBLANK to finish.
	STA WSYNC		;
	STA CXCLR		; Clear Collision Latches
	STA VBLANK		; We are now out of vertical blank
	;;
	;; We are now in the visible portion of the screen.
	;; Save the stack pointer.
	;;
SAVSTK:	TSX			; Move SP into X
	STX TMPSTK		; Save SP for later.
	;;
	;; Draw the score
	;; 
SCRSTA:	LDA #$02
	STA CTRLPF		; Double instead of Reflect.
	LDX KLskip		;
Vskip1:	STA WSYNC		; Skip a few scanlines
	DEX			;
	BNE Vskip1		;
	LDA KLskip
	CMP #$0E		; No score version of KLskip
	beq Vmain		; if so, skip to the main playfield.
	;;
	;; Prepare to draw score.
	;; 
Vscdis:	LDX #$05		; Score is 5 lines high.
	LDA #$00		; Clear graphics
	STA NUMG0
	STA NUMG1
Vscor:	STA WSYNC		; Start with a new scanline
	LDA NUMG0		; Take last scanline's left score
	STA PF1			; and recycle it.
	LDY SCROFF+2
	LDA NUMBERS,Y
	AND #$F0		; left digit.
	STA NUMG0
	LDY SCROFF
	LDA NUMBERS,Y
	AND #$0F		; and right digit.
	ORA NUMG0
	STA NUMG0
	LDA NUMG1
	STA PF1
	LDY SCROFF+3
	LDA NUMBERS,Y
	AND #$F0
	STA NUMG1
	LDY SCROFF+1
	LDA NUMBERS,Y
	AND SHOWSCR		; Mask off the right digits if not needed.
	STA WSYNC		;
	ORA NUMG1		; (0) +3
	STA NUMG1		; (3) +3
	LDA NUMG0		; (6) +3
	STA PF1			; *9* +3
	DEX
	BMI Vmain
	INC SCROFF		; (16)+5
	INC SCROFF+2
	INC SCROFF+1
	INC SCROFF+3
	LDA NUMG1
	STA PF1
	JMP Vscor
	;;
	;; Main kernel
	;;
Vmain:	LDA #$00		; Inner Display Loop
	STA PF1			; clear Score.
	STA WSYNC		; Next line
	LDA #$15		; Set a Reflecting Playfield, 2 clock ball.
	STA CTRLPF
	LDA Color0
	STA COLUP0
	LDA Color1
	STA COLUP1
Vfield:	LDX #$1F
	TXS			; Place stack pointer over ENABL
	;;
	;; Draw Player 0
	;; 
	SEC
	LDA PlayerY0		; Get Player Y coordinate
	SBC ScanLine		; subtract scanline to determine if we're there, yet.
	AND #$FE
	TAX
	AND #$F0
	BEQ VdoPlr		; If 0 we start drawing player.
	LDA #$00		; Otherwise, blank the player data.
	BEQ VnoPlr		; (unconditional branch due to preceeding LDA)
VdoPlr:	LDA HIRES,X		; Load appropriate player byte
VnoPlr:	STA WSYNC		; next line
	STA GRP0		; Draw Player 0 sprite
	;;
	;; Draw the two of the balls.
	;; 
VdrBls:	LDA BallY2		; Get Computer ball coordinate
	EOR ScanLine
	AND #$FC
	PHP
	LDA BallY1
	EOR ScanLine
	AND #$FC
	PHP
	;;
	;; Now do the playfield
	;;
	LDA ScanLine
	BPL Vvrefl		; If we're not past scanline 128, skip ahead
	EOR #$F8		; otherwise, flip bits around to reflect.
Vvrefl:	CMP #$20		;
	BCC VfDone		; Branch if @ Bottom.
	LSR
	LSR
	LSR			; Divide by 8
	TAY			; stow it in Y register.
VfDone	LDA BallY0
	EOR ScanLine
	AND #$FC
	PHP
	LDA PlayerY1
	SEC
	SBC ScanLine
	INC ScanLine
	NOP
	ORA #$01
	TAX
	;;
	AND #$F0
	BEQ VdoP1
	LDA #$00
	BEQ VnoP1
VdoP1:	LDA HIRES,X
VnoP1:	STA GRP1
	LDA (LORES),Y
	STA PF0
	LDA (LORES+2),Y
	STA PF1
	LDA (LORES+4),Y
	STA PF2
VnoPF:	INC ScanLine
	LDA ScanLine
	EOR #$EC
	BNE Vfield
	LDX TMPSTK
	TXS
	STA ENAM0
	STA ENAM1
	STA ENABL
	STA GRP0
	STA GRP1
	STA GRP0
	STA PF0
	STA PF1
	STA PF2
	RTS

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Parse Console Switches
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CONSWT: LDA SWCHB
	LSR
	BCS NoNewGM
	;;
	;; start new game
	;;
	LDA #$0F
	STA SHOWSCR
	LDA #$FF
	STA GameOn
	LDA #$80
	STA GameTimer
	LDX #$E4
	JSR ClearMem
	BEQ ResetField
NoNewGM	LDY #$02
	LDA GameTimer
	AND GameOn
	CMP #$F0
	BCC SCdrawn
	LDA CLOCK
	AND #$30
	BNE SCdrawn
	LDY #$0E
SCdrawn	STY KLskip
	LDA CLOCK
	AND #$3F
	BNE ChkSel
	;; 
	STA SelDbnce
	INC GameTimer
	BNE ChkSel
	STA GameOn
	;;
ChkSel	LDA SWCHB
	AND #$02
	BEQ SelDown
	STA SelDbnce
	BNE CS_RTS
	;;
SelDown	BIT SelDbnce
	BMI CS_RTS
	;;
	INC BINvar
ClrGam	LDX #$DE
ClrGRST	JSR ClearMem
	LDA #$FF
	STA SelDbnce
	LDY BINvar
	LDA VARMAP,y
	STA GAMVAR
	EOR #$FF
	BNE SelGO
	LDX #$DA
	BNE ClrGRST
	;;
SelGO	LDA BCDvar
	SED
	CLC
	ADC #$01
	STA BCDvar
	STA SCORE
	CLD
	BIT GAMVAR
	BPL ResetField
	NOP
	BVC ResetField
	NOP
ResetField:
	JSR InitPF
	LDA #50
	STA PlayerY1
	LDA #134
	STA PlayerY0
	BIT GAMVAR
	STA RESP1
	BMI CS_RTS
	STA PlayerY1
	LDA #$08
	STA DIRECTN+1
	LDA #$20
	STA HMP0
	STA HMP1
	STA WSYNC
	STA HMOVE
CS_RTS	RTS

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Load Stella Registers
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

LDSTEL:	LDA GAMVAR
	AND #$87
	BMI LDmult
	LDA #$00
LDmult:	ASL
	TAX
	LDA WIDTHS,X
	STA NUSIZ0
	LDA WIDTHS+1,X
	STA NUSIZ1
	LDA GAMVAR
	AND #$C0
	LSR
	LSR
	LSR
	LSR
	TAY
	LDA GameOn		; Enable joysticks via bit 1 of $FF
	STA SWCHB
	EOR #$FF		; flip value
	AND GameTimer		; Cycle player colors only when no game on
	STA TEMP1		; (attract mode)
	LDX #$FF
	LDA SWCHB
	AND #$08		; Check Color/BW switch
	BNE LDcolor		; not flipped? do color.
	LDY #$04
	LDX #$0F
LDcolor STX TEMP
	LDX #$03
LDcol0	LDA ColorTbl,Y
	EOR TEMP1
	AND TEMP
	STA COLUP0,X
	STA Color0,X
	STA XColor0,X
	INY
	DEX
	BPL LDcol0
	RTS

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Check Joystick Motion
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

JOYSTK:
	BIT GameOn
	BMI JS1
	RTS
JS1:	LDA #$FF
	STA JMove
	LDA SWCHA
	EOR #$FF
	LDX #$00
JSNxt:	LDY #$EE
	STY JMove
JRight:	CMP #$80
	BNE JLeft
	LDY #$00
	JMP JSDone
JLeft:	CMP #$40
	BNE JDown
	LDY #$08
	JMP JSDone
JDown:	CMP #$20
	BNE JUp
	LDY #$0C
	JMP JSDone
JUp:	CMP #$10
	BNE JRUp
	LDY #$04
	JMP JSDone
JRUp:	CMP #$90
	BNE JRDown
	LDY #$02
	JMP JSDone
JRDown:	CMP #$A0
	BNE JLUp
	LDY #$0D
	JMP JSDone
JLUp:	CMP #$50
	BNE JLDown
	LDY #$05
	JMP JSDone
JLDown:	CMP #$60
	BNE JLNone
	LDY #$0A
	JMP JSDone
JLNone:	LDY #$00
	STY JMove
JSDone:
	STA TEMP
	LDA INPT4,X
	BMI JSOnly
JSMsl:	
	SEC
	LDA PlayerY0,X
	SBC #$06
	STA BallY0,X
	LDA DIRECTN,X
	STA DIRECTN+2,X
	LDA #$02
	STA RESM0,X
	LDA #$00
	STA MxPFcount,X
	LDA TEMP
	JMP JSInc
JSOnly: STY DIRECTN,X
	LDY JMove
	STY MVadjA,X
JSInc:	
	ASL
	ASL
	ASL
	ASL
	INX
	CPX #$02
	BNE JSNxt0
	RTS

JSNxt0:	JMP JSNxt
	
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Check for collisions
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

COLIS:	LDX #$02
COLnext:
	CPX #$02
	BEQ COLnoHit
	LDA CXM0P,X
	BPL COLnoHit
	;;
	;; billiard code was here, removed.
	;;
COLDET:	INC DIRECTN,X
	INC DIRECTN+2,X
	SED
	LDA SCORE,X
	CLC
	ADC #$01
	STA SCORE,X
	CLD
	TXA
	CLC
	;;
	;; stirtimer code, removed
	;;
	LDA #$FF
	STA RESMP0
	STA RESMP1
	STA RESBL
	LDA #$00
	STA AUDV0,X
	STA MisLife
	STA MisLife+1
	RTS
COLnoHit:
	NOP
COLTNK:	NOP
	LDA CXM0FB,X
	BMI COLMPF
	LDA #$00
	STA MxPFcount,x
	JMP COLTCK
	;;
COLMPF:	LDA MxPFcount,X
	BnE COLMPFX
	NOP
	DEC BounceCount,x
	LDA DIRECTN+2,X
	STA OldMisDir,X
	EOR #$FF
	STA DIRECTN+2,X
	INC DIRECTN+2,X
	LDA DIRECTN+2,X
	AND #$03
	BNE COLXY0
	INC DIRECTN+2,X
COLXY0:	JMP COLMPFdone
COLMPFX:
	CMP #$01
	BEQ Rev180
	CMP #$03
	BCC COLMPFdone
	BNE COLMPFdone
	LDA OldMisDir,X
	JMP Bump180
Rev180:	LDA DIRECTN+2,X
Bump180:
	CLC
	ADC #$08
	STA DIRECTN+2,X
	JMP COLMPFdone
	;;
COLMPFdone:
	INC MxPFcount,x
	;;
COLTCK: CPX #$02
	BEQ COLjmp
	LDA CXP0FB,X
	BMI COLTW
	LDA CXPPMM
	BPL COLTCLR
COLTW:  BCC COLTnk1
COLTCLR:
	LDA #$03
	STA COLcount,x
	BNE COLPD
	;;
COLTnk1:
	DEC COLcount,X
	BMI COLbonk
	LDA Vtemp,X
	BEQ COLPD
	BNE COLreverse
	;;
COLbonk:
	INC DIRECTN,X		
COLreverse:
	LDA DIRECTN,X
	CLC
	ADC #$08
	JSR BumpTank
COLPD:	DEX
	BMI COLrts
	JMP COLnext
COLrts:	RTS
COLjmp: DEX
	JMP COLnext
	;;
BumpTank:
	AND #$0F
	TAY
	LDA HDGTBL,Y
	JSR PhMove
	LDA #$00
	STA MVadjA,X
	STA MVadjB,X
	STA FwdTimer,X
	LDA XColor0,X
	STA Color0,X
	RTS
	

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Setup Motion for Players/Missiles/Ball
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SETMOT:	NOP
	LDA #$20
	STA XoffBase
	LDX #$04
	JSR STPM
	DEX
	JSR STPM
	DEX
	JSR STPM
				;
	DEX
STPnext LDA FwdTimer,X
	AND #$08
	LSR
	LSR
	STX TEMP1
	CLC
	ADC TEMP1
	TAY
	LDA MVadjA,Y
	;;
	SEC
	BMI STP7set
	CLC
STP7set ROL
	;;
	STA MVadjA,y
	BCC STPnoV
	;;
	LDA MPace,X
	AND #$01
	ASL
	ASL
	ASL
	ASL
	STA XoffBase
	JSR STPM
STPnoV:	DEX
	BEQ STPnext
	RTS

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Calculate Score Offsets
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SCROT:	LDX #$01
SCROT0:	LDA SCORE,X
	AND #$0F
	STA TEMP
	ASL
	ASL
	CLC
	ADC TEMP
	STA SCROFF,X
	LDA SCORE,X
	AND #$F0
	LSR
	LSR
	STA TEMP
	LSR
	LSR
	CLC
	ADC TEMP
	STA SCROFF+2,X
	DEX
	BPL SCROT0
	RTS

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Additional motion code.
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
STPM:	INC MPace,X
	LDA DIRECTN,X
	AND #$0F
	CLC
	ADC XoffBase
	TAY
	LDA Xoffsets,Y
	STA XOFFS
	BVS STPgo
	LDA DIRECTN,X
	SEC
	SBC #$02
	AND #$03
	BNE STPgo
	LDA MPace,x
	AND #$03
	BNE STPgo
	LDA #$08
	STA XOFFS
STPgo:	LDA XOFFS
	;;
PhMove:	STA HMP0,X
	AND #$0F
	SEC
	SBC #$08
	STA TEMP2
	CLC
	ADC PlayerY0,X
	BIT GAMVAR
	BMI PhNoPlayer
	CPX #$02
	BCS PhNoWrap
PhNoPlayer:
	CMP #$DB
	BCS PhNoWrapTop
	CMP #$25
	BCS PhNoWrap
PhNoWrapTop:
	LDA #$D9
	BIT TEMP2
	BMI PhNoWrap
	LDA #$28
PhNoWrap:
	STA PlayerY0,X
	CPX #$02
	BCS PhNoVD
	STA VDELP0,X
PhNoVD:	RTS 

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Setup player data
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SETPLR:	LDA #$01
	AND CLOCK
	TAX
	TXA
	EOR #$0E
	TAX
	LDY #$00		; always one shape
	TYA
	ASL
	ASL
	ASL
	CMP #$3F
	CLC
	BMI ROTnoFlip
	SEC
	EOR #$47
ROTnoFlip:
	TAY
ROTnext:
	LDA (SHAPES),Y
	STA HIRES,X
	BCC ROTinc
	DEY
	DEY
ROTinc:	INY
	DEX
	DEX
	BPL ROTnext
	RTS
	
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Clear RAM, mod $0100
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
; ------------------------------------------------------------
;
; Zero out zero-page memory starting with ($A3+X) MOD $100,
; through $A2 wrapping around at $100.
;
; Calling with:
; X=$5D will clear $00-$A2
; X=$DD will clear $80-$A2
; X=$DF will clear $82-$A2
; X=$E6 will clear $89-$A2
;
; Returns with zero bit set.
;
ClearMem
	LDA  #$00
ClrLoop	INX  
	STA  SCORE+1,X
	BNE  ClrLoop  ;Continue until X rolls over.
	RTS  

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Init Playfield subroutine
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

InitPF	LDX #$01
	LDA SPRLO,x
	STA SHAPES
	LDA SPRHI,x
	STA SHAPES+1
	;;
	LDA GAMVAR
	LSR
	LSR
	AND #$03
	TAX
	LDA GAMVAR
	BPL IFgo
	AND #$08
	BEQ IF80
	LDX #$03
	BPL IFskip
IF80:	LDA #$80
IFgo:	NOP
IFskip:	LDA GAMVAR
	ASL
	ASL
	BIT GAMVAR
	ASL
	ASL
	BIT GAMVAR
	BMI IFnoPlane
	STA WSYNC
	AND #$80
IFnoPlane:
	NOP
	LDA #>PF0_0
	STA LORES+1
	STA LORES+3
	STA LORES+5
	;;
	LDA PLFPNT,X
	NOP
	STA RESP0
	STA RESBL
	STA LORES
	LDA PLFPNT+3,X
	STA LORES+2
	LDA PLFPNT+6,X
	STA LORES+4
	LDA #$30
	STA BallY2
	RTS
	
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Tables
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

WIDTHS:	
	.BYTE  $10 ,$10  ;1 vs. 1

ColorTbl:	
	byte $80 ,$9C ,$DA ,$3A      ; 00 = Regular Tanks
       .byte $08 ,$04 ,$00 ,$0E      ; special B&W
	
PlayerShape:
	.byte $00
	.byte $00
	.byte $3C
	.byte $3C
	.byte $3C
	.byte $3C
	.byte $00
	.byte $00


	align 100
PF0_0:
	.byte $F0 ; |XXXX    | $F779
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
PF1_0:	.byte $FF ; |XXXXXXXX| $F785
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
PF2_0:
	.byte $FF ; |XXXXXXXX| $F791
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
PF1_1:
	.byte $FF ; |XXXXXXXX| $F79D
PF0_3:	.byte $00 ; |        | $F79E
       .byte $00 ; |        | $F79F
	.byte $00 ; |        | $F7A0
PF1_3:	.byte $00 ; |        | $F7A1
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
PF1_2:	.byte $FF ; |XXXXXXXX| $F7AE
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
PF2_2:	.byte $FF ; |XXXXXXXX| $F7BA
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
	
;	Patterns for numbers
;
NUMBERS:	.byte $0E ; |    XXX | $F5C5   Leading zero is not drawn
	.byte $0A ; |    X X | $F5C6   because it's never used.
	.byte $0A ; |    X X | $F5C7
	.byte $0A ; |    X X | $F5C8
	.byte $0E ; |    XXX | $F5C9
	
	.byte $22 ; |  X   X | $F5CA
	.byte $22 ; |  X   X | $F5CB
	.byte $22 ; |  X   X | $F5CC
	.byte $22 ; |  X   X | $F5CD
	.byte $22 ; |  X   X | $F5CE
	
	.byte $EE ; |XXX XXX | $F5CF
	.byte $22 ; |  X   X | $F5D0
	.byte $EE ; |XXX XXX | $F5D1
	.byte $88 ; |X   X   | $F5D2
	.byte $EE ; |XXX XXX | $F5D3
	
	.byte $EE ; |XXX XXX | $F5D4
	.byte $22 ; |  X   X | $F5D5
	.byte $66 ; | XX  XX | $F5D6
	.byte $22 ; |  X   X | $F5D7
	.byte $EE ; |XXX XXX | $F5D8
	
	.byte $AA ; |X X X X | $F5D9
	.byte $AA ; |X X X X | $F5DA
	.byte $EE ; |XXX XXX | $F5DB
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

; Addresses for Sprite Graphics

SPRLO   .BYTE  #<PlayerShape, #<PlayerShape, #<PlayerShape
SPRHI   .BYTE  #>PlayerShape, #>PlayerShape, #>PlayerShape


; Playfield address data.  Kernal timing requires that
; these addresses point 4 bytes before the real start
; of data.
;
	;        Complex   ,    None 
	;        Simple    ,   Clouds
PLFPNT	.BYTE  #<(PF0_0-4) ,#<(PF0_0-4)
	.BYTE  #<(PF0_0-4)   ;PF0
	.BYTE  #<(PF1_0-4) ,#<(PF1_1-4)
	.BYTE  #<(PF1_2-4) ;PF1
	.BYTE  #<(PF2_0-4) ,#<(PF1_1-4)
	.BYTE  #<(PF2_2-4) ;PF2

	
VARMAP:
	.BYTE $28
	.BYTE $20
	
	;
	; $FF to signify end of game variations.
	;
	.BYTE  $FF

; Horizontal and vertical offsets for movement by orientation.
; Basic table is $10 bytes long (22.5-degree increments), but
; XoffBase is added to it to alter for game options.  High
; nibble is raw HMPx value for horizontal offset, low nibble
; is vertical offset in scan lines.
;
Xoffsets

	.BYTE  $F8 ,$F7 ,$F6 ,$06    ;XoffBase=0
	.BYTE  $06 ,$06 ,$16 ,$17
	.BYTE  $18 ,$19 ,$1A ,$0A
	.BYTE  $0A ,$0A ,$FA ,$F9

	.BYTE  $F8 ,$F7 ,$F6 ,$F6    ;XoffBase=$10
	.BYTE  $06 ,$16 ,$16 ,$17
	.BYTE  $18 ,$19 ,$1A ,$1A
	.BYTE  $0A ,$FA ,$FA ,$F9

	.BYTE  $E8 ,$E6 ,$E4 ,$F4    ;XoffBase=$20
	.BYTE  $04 ,$14 ,$24 ,$26    ;normal missiles
	.BYTE  $28 ,$2A ,$2C ,$1C
	.BYTE  $0C ,$FC ,$EC ,$EA

; This Xoffsets entry is also used directly for "bumping"
; a player after a hit or to back away from playfield collision
;
HDGTBL	.BYTE  $C8 ,$C4 ,$C0 ,$E0    ;XoffBase=$30
	.BYTE  $00 ,$20 ,$40 ,$44    ;machine guns, "bump"
	.BYTE  $48 ,$4C ,$4F ,$2F
	.BYTE  $0F ,$EF ,$CF ,$CC
;
; Player velocity momentum adjustments.  Table of two-byte
; entries, indexed by player's desired final velocity.  Even
; locations go to MVadjA to be applied during the first half of
; the FwdTimer cycle, and odd locations goe to MVadjB to be
; applied during the second half.
;
; During each half, the byte is rotated left one bit; if
; the bit which emerges is 1, XoffBase is tweaked by $10 
; to adjust the velocity for that frame only.  Since FwdTimer
; goes through 16 cycles or 2 8-bit halves in its course from,
; $F0 to $00, this gives us a bitwise "adjust this frame" flag
; for each frame in the course of FwdTimer's run.  This is
; used to obscure the suddenness of transition from one
; velocity to another.
;
; The adjustment is only done once for each two ON bits
; since the MPace 1 bit is used for the adjustment, and
; MPace is INCed in the same code block that does the
; tweak.  The tweak consists of replacing whatever XoffBase
; the final velocity calls for with $10, an intermediate value.
;
MVtable	.BYTE  $00 ,$00
	.BYTE  $80 ,$80
	.BYTE  $84 ,$20
	.BYTE  $88 ,$88
	.BYTE  $92 ,$48
	.BYTE  $A4 ,$A4
	.BYTE  $A9 ,$52
	.BYTE  $AA ,$AA
	.BYTE  $D5 ,$AA
	.BYTE  $DA ,$DA
	.BYTE  $DB ,$6D
	.BYTE  $EE ,$EE

	
	
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; System Vectors
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	echo "------", [$FFFA - *]d, "bytes free before End of Cartridge"
	
	ORG $FFFA
	.WORD ColdStart
	.WORD ColdStart
	.WORD ColdStart
