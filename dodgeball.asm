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
VBLANKWait:
	LDA INTIM		; Poll the timer
	BNE VBLANKWait		; and if not ready, loop back to wait.
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
;;; ;; Visible Screen Kernel
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

KERNEL: STA WSYNC
	LDA #$00
	STA VBLANK
	LDA #KERNEL_WAIT_TIME
	STA TIM64T
	STA HMOVE
	sta CXCLR
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
;;; ;; Graphics and Tables
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

	echo "------", [$FFFA - *]d, "bytes free before End of Cartridge"
	
	ORG $FFFA
	.WORD ColdStart
	.WORD ColdStart
	.WORD ColdStart
