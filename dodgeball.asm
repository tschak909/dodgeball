	PROCESSOR 6502

	include "vcs.h"

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Variables.
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	SEG.U VARIABLES
	
	ORG $80

TEMP:		ds 1		; Temporary variable
TEMP1:		ds 1		; ...
TEMPSTACK:	ds 1		; Temporary variable for stack.
KLSKIP:		ds 1		; Number of kernel lines to skip for score.
NUMG0:		ds 1		; Score graphic for first digit
NUMG1:		ds 1		; Score graphic for second digit
SCROFF:		ds 4		; Score offsets (BCD biased) 
SHOWSCR:	ds 1		; Show score mask
SCORE:		ds 2		; Player Scores (BCD)

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

;;; Temporary code.
	LDA #$00
	STA TEMP
	STA TEMP1

main:	JSR vertical_control
	JSR vertical_blank
	JSR vout
	JSR overscan
	JMP main

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Vertical control
;;; ;; Generate the needed sync signals and
;;; ;; vertical blank time
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

vertical_control:
	LDA #$02		; 2 = Assert vertical sync. wsync doesn't care.
	STA WSYNC		; wait until next line.
	STA VSYNC		; Turn on vertical sync.
	LDA #47			; Set the 64-cycle timer for 47*64 cycles.
	STA TIM64T		;
	STA WSYNC		; Wait another two lines
	STA WSYNC		;
	LDA #$00		; Then while we're on the last line of the vsync
	STA GRP0		; use the time to clear some registers.
	STA GRP1		; why the hell not?
	STA ENAM0		;
	STA ENAM1		;
	STA ENABL		;
	STA WSYNC		;
	STA VSYNC		; vertical sync signal is done. We are now in vblank.
	RTS
	
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Vertical blank
;;; ;; While we're off the visible screen
;;; ;; do the game logic.
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
vertical_blank:
	JSR game_switches
	JSR load_stella		; Load stella registers
	JSR calculate_score_offsets
	STA WSYNC		; Wait until next line
	BIT TIMINT		; Clever way to check if timer ends
	BPL vertical_blank	; without altering accumulator
	LDA #$0			; Turn off vblank
	STA VBLANK		; and get ready for kernel.
	RTS

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Process Game Switches
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

game_switches:
	LDA #$02		; For now, just set score skip to 2
	STA KLSKIP

calculate_score_offsets:
	LDX #$01
scrot0	LDA SCORE,X
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
	CLC
	ADC TEMP
	STA SCROFF+2,X
	DEX
	BPL scrot0
	RTS
	
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Load Stella registers
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
load_stella:
	LDY #$00
	LDA SWCHB		; Get B/W switch
	AND #$08		; mask it off
	BNE set_colors		; Jump to B/W colors if BW switch is set
	LDY #$04		
	LDX #$0F
set_colors:
	STX TEMP
	LDX #$03
ldcol0	LDA ColorTable,Y
	EOR TEMP1
	AND TEMP
	STA COLUP0,X
	INY
	DEX
	BPL ldcol0
	RTS
	
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; vout - Video Out - The Kernel
;;; ;; Race down the display, setting
;;; ;; TIA registers as appropriate
;;; ;; to generate the display.
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
vout:	LDY #200		; Start at scanline 200
	STY WSYNC		; blow a line
	STY HMOVE		; Strobe HMOVE.
	DEY			; 199...
	TSX			; Move stack into X Register 
	STX TEMPSTACK		; Save stack pointer, as it is about to be changed.
	LDA #$02		; Set playfield control to double
	STA CTRLPF		;
	LDX KLSKIP		; see how many kernel lines to skip
vskip1:	STA WSYNC		; skip X lines
	DEX
	BNE vskip1
	LDA KLSKIP
	CMP #$0E		; No score value
	BEQ vmain		; Skip to main kernel if no score to be drawn.
	;;
	;; Draw the score...
	;;
	LDX #$05		; Score is 6 bytes high.
	LDA #$00
	STA NUMG0		; Clear score graphics
	STA NUMG1		;
vscor	STA WSYNC
	LDA NUMG0		; Load score graphic
	STA PF1			; Slam into PF1
	;;
	;; Get score offsets from table, mask them, and put them in.
	;;
	LDY SCROFF+2
	LDA NUMBERS,Y		; Get left digit
	AND #$F0		
	STA NUMG0
	LDY SCROFF
	LDA NUMBERS,Y		; Right digit
	AND #$0F
	ORA NUMG0
	STA NUMG0		; Left score ready 
	LDA NUMG1
	STA PF1
	LDY SCROFF+3
	LDA NUMBERS,Y
	AND #$F0
	STA NUMG1
	LDY SCROFF+1
	LDA NUMBERS,Y
	AND SHOWSCR		; Whether to show right digits or not.
	;;
	STA WSYNC
	ORA NUMG1
	STA NUMG1
	LDA NUMG0
	STA PF1
	DEX
	BMI vmain		; If we're done, go straigt to playfield
	;;
	;; Otherwise, get the next set of graphics
	;;
	INC SCROFF
	INC SCROFF+2
	INC SCROFF+1
	INC SCROFF+3
	LDA NUMG1
	STA PF1
	JMP vscor		; Go back to score list.

vmain	RTS
	
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Logic to be done during the
;;; ;; overscan period.
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
overscan:
	STA WSYNC		; Wait for next scanline
	LDA #$02		; 2 = Turn on VBLANK
	STA VBLANK
	LDA #22
	STA TIM64T		; wait 22*64 cycles.

oswait:
	STA WSYNC		; count down the scanlines
	BIT TIMINT		; until the timer ends.
	BPL oswait
	RTS



	
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Tables
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ColorTable:
	.byte $32, $2C, $8A, $DA ; Color
	.byte $08, $04, $00, $0E ; Black and White.
	
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Graphics data
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Processor start vectors
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	ORG $FFFA
	.WORD cold
	.WORD cold
	.WORD cold

