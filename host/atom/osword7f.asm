	sector 		= $80
	
	IOFlag		= $84
	IOAddr		= $86
	
	SDDOSClientId   = $DE
	
;;; OSWORD A=7F8 271 command level disk access
osword7f:

.if (debug_osw_7f = 1)	
        LDY #$00
	LDA TubeCtrl + 5	; number of parameters
	CLC
	ADC #$06		; command at +6, params after that
	TAX
word7Fdebug:
        LDA TubeCtrl, Y         ; command block
        JSR DebugHexOut
	INY
	DEX
        BPL word7Fdebug
        JSR DebugNewline
.endif
	
        LDY #6
        LDA TubeCtrl, Y
        CMP #$53                ; read sector multi
        BEQ word7F_53

word7Freturn:
        LDY TubeCtrl + 5        ; number of parameters
        LDA #$00
        STA TubeCtrl + 7, Y     ; Results byte stored after the params
	RTS

	
word7F_53:
	; Check if it's an read to host memory FFFFxxxx
	
	LDA TubeCtrl + 1
	STA IOAddr

	LDA TubeCtrl + 2
	STA IOAddr + 1
	
	LDA TubeCtrl + 3
	AND TubeCtrl + 4
	CLC
	ADC #1
	STA IOFlag		; 0 = transfer to IO
	BEQ word7F_skipclaim

        ;; Claim Tube
        LDA #SDDOSClientId
        JSR L0406
        
        ;; Setup Data Transfer
	LDA #1
        LDX #<(TubeCtrl + 1)
        LDY #>(TubeCtrl + 1)
        JSR L0406

word7F_skipclaim:
	LDA TubeCtrl + 8	; sector number
	STA sector

	LDA #0
	STA sector+1
	STA sector+2
	STA sector+3	

	LDA TubeCtrl		; drive 2 = second side
	AND #$02
	BEQ word7Fdrive0

	;; Add 10 sectors to access drive 2 tracks
	LDA sector
	CLC
	ADC #10
	STA sector
	
word7Fdrive0:
	
	;; Add 20 sectors for each track (because the image is an track-interleaved DSD)
	LDX TubeCtrl + 7	; track number
	BEQ word7F_track_done
word7F_tracks:
	CLC
	LDA sector
	ADC #20
	STA sector
	BCC word7F_track_dec
	INC sector + 1
	BNE word7F_track_dec
	INC sector + 2
	BNE word7F_track_dec
	INC sector + 3
word7F_track_dec:
	DEX
	BNE word7F_tracks

word7F_track_done:
	LDA TubeCtrl + 9		; MS 3 bits = sector size; LS 5 bits = num sectors
	AND #$1F
	TAX

word7F_read_sectors:
	JSR sd_sector_r

	INC sector
	BNE word7F_read_dec
	INC sector + 1
	BNE word7F_read_dec
	INC sector + 2
	BNE word7F_read_dec
	INC sector + 3

word7F_read_dec:
	DEX
	BNE word7F_read_sectors

	LDA IOFlag
	BEQ word7F_skiprelease

        ;; Release Tube
        LDA #SDDOSClientId - $40
        JSR L0406

word7F_skiprelease:
	
        jmp word7Freturn
	        

;;; Sector level disk access (from SDDOS)

sd_sector_r:
        JSR read_start            ; Set SDDOS drive + sector 

        LDY #0                    ; Copy globalbuffer to Atom memory

	LDA IOFlag
	BEQ rx2

;;; Destination is a parasite address
	
rx1:
	readportFAST AREAD_DATA_REG
        STA $BEE5
        INY
        BNE rx1
        RTS


;;; Destination is a host address
	
rx2:
	readportFAST AREAD_DATA_REG
        STA (IOAddr), Y
        INY
        BNE rx2
	INC IOAddr + 1
        RTS
	
read_start:
        JSR PREPPUTTOB407         ; Reset globalbufferpointer
	
        LDA #0                    ; Assume disk is in drive 0
        writeportFAST AWRITE_DATA_REG             
	JSR interwritedelay
        LDA sector                ; Send sectornr LB
        writeportFAST AWRITE_DATA_REG
	JSR interwritedelay
        LDA sector+1              ; Send sectornr
        writeportFAST AWRITE_DATA_REG
	JSR interwritedelay
        LDA sector+2              ; Send sectornr
        writeportFAST AWRITE_DATA_REG
	JSR interwritedelay
        LDA sector+3              ; Send sectornr HB
        writeportFAST AWRITE_DATA_REG
	JSR interwritedelay
        NOP                       ; Wait, until command finished!!!
	
        FASTCMDI CMD_LOAD_PARAM   ; Command = load SDDOS parameters
        SLOWCMDI CMD_READ_IMG_SEC ; Command = read SDDOS sector

        JMP PREPGETFRB406         ; Reset datapointer


PREPGETFRB406:
        LDA #CMD_INIT_READ
        writeportFAST ACMD_REG
        JMP  interwritedelay
	
PREPPUTTOB407:
        LDA #CMD_INIT_WRITE
        writeportFAST ACMD_REG
        JMP interwritedelay

