        sector          = $80
        drive           = $84
        IOFlag          = $85
        IOAddr          = $86

        SDDOSClientId   = $DE

;;; OSWORD A=7F8 271 command level disk access
osword7f:

.if (debug_osw_7f = 1)
        LDY #$00
        LDA TubeCtrl + 5        ; number of parameters
        CLC
        ADC #$06                ; command at +6, params after that
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
        CMP #$4B                ; write sector multi
        BEQ word7F_4B
        CMP #$53                ; read sector multi
        BEQ word7F_53

        PHA
        JSR STROUT
        .byte "UNSUPPORTED OSWORD 7F:"
        NOP
        PLA
        JSR HEXOUT
        JSR OSNEWL

word7Freturn:
        LDY TubeCtrl + 5        ; number of parameters
        LDA #$00
        STA TubeCtrl + 7, Y     ; Results byte stored after the params
        RTS

word7F_4B:
        LDX #0
        JSR word7f_process_params

word7F_write_sectors:
        JSR sd_sector_w

        INC sector
        BNE word7F_write_dec
        INC sector + 1
        BNE word7F_write_dec
        INC sector + 2
        BNE word7F_write_dec
        INC sector + 3

word7F_write_dec:
        DEX
        BNE word7F_write_sectors
        BEQ word7F_release

word7F_53:
        LDX #1
        JSR word7f_process_params

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


word7F_release:
        LDA IOFlag
        BEQ word7F_skiprelease

        ;; Release Tube
        LDA #SDDOSClientId - $40
        JSR L0406

word7F_skiprelease:

        jmp word7Freturn

;;; Common code for read/write sector param parocessing
;;;
;;; On Entry
;;; - OSWORD 7F parameter block in TubeCtrl
;;; - X = 1 for read sector and X = 0 for write sector
;;;
;;; Actions:
;;; - Processes memory address from TubeCtrl + 1
;;; - Claims the tube if the memory address in the parasite
;;; - Sets the IOFlag to 0 = host, 1 = parasite
;;; - Calculate the physical SDDOS sector number from drive, track and sector
;;;
;;; On Exit:
;;; - Returns the number of sectors to read in X

word7f_process_params:

        LDA TubeCtrl + 1
        STA IOAddr

        LDA TubeCtrl + 2
        STA IOAddr + 1

        LDA TubeCtrl + 3
        AND TubeCtrl + 4
        CLC
        ADC #1
        STA IOFlag              ; 0 = transfer to host
        BEQ word7F_skipclaim

        ;; Claim Tube
        LDA #SDDOSClientId
        JSR L0406

        ;; Setup Data Transfer
        TXA                     ;  X = 1 for read sector and X = 0 for write sector
        LDX #<(TubeCtrl + 1)
        LDY #>(TubeCtrl + 1)
        JSR L0406

word7F_skipclaim:

        LDA TubeCtrl + 8        ; sector number
        STA sector

        LDA #0
        STA sector+1
        STA sector+2
        STA sector+3

        LDA TubeCtrl            ; drive number (0=A; 1=B)
        AND #$01
        STA drive

        LDA TubeCtrl            ; drive 2 = second side
        AND #$02
        BEQ word7Fdrive0

        ;; Add 10 sectors to access drive 2 tracks
        LDA sector
        CLC
        ADC #10
        STA sector

word7Fdrive0:

        ;; Add 20 sectors for each track (because the image is an track-interleaved DSD)
        LDX TubeCtrl + 7        ; track number
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
        LDA TubeCtrl + 9                ; MS 3 bits = sector size; LS 5 bits = num sectors
        AND #$1F
        TAX
        RTS



;;; Sector level disk access (from SDDOS)

;;; Read Sector Entry Point
sd_sector_r:
        JSR send_drive_and_sector ; Set SDDOS drive + sector
        SLOWCMDI CMD_READ_IMG_SEC ; Command = read SDDOS sector
        JSR send_cmd_init_read    ; Reset datapointer
        LDY #0
        LDA IOFlag
        BEQ rd2

;;; Copy block of 256 from AtoMMC to parasite memory via tube
rd1:
        readportFAST AREAD_DATA_REG
        STA TubeR3
.if (debug_sddos = 1)
        STA debug_data, Y
.endif
        INY
        BNE rd1
        BEQ sd_sector_r_end

;;; Copy block of 256 from AtoMMC to host memory
rd2:
        readportFAST AREAD_DATA_REG
        STA (IOAddr), Y
.if (debug_sddos = 1)
        STA debug_data, Y
.endif
        INY
        BNE rd2
        INC IOAddr + 1

sd_sector_r_end:
.if (debug_sddos = 1)
        JSR dump_debug_data
.endif
        RTS

;;; Write Sector Entry Point
sd_sector_w:
        JSR send_drive_and_sector ; Set SDDOS drive + sector
        JSR send_cmd_init_write   ; Reset globalbufferpointer
        LDY #0
        LDA IOFlag
        BEQ wr2

;;; Copy block of 256 from AtoMMC to parasite memory via tube
wr1:
        LDA TubeR3
        writeportFAST AWRITE_DATA_REG
.if (debug_sddos = 1)
        STA debug_data, Y
.endif
        INY
        BNE wr1
        BEQ sd_sector_w_end

;;; Copy block of 256 from AtoMMC to host memory
wr2:
        LDA (IOAddr), Y
        writeportFAST AWRITE_DATA_REG
.if (debug_sddos = 1)
        STA debug_data, Y
.endif
        INY
        BNE wr2
        INC IOAddr + 1

sd_sector_w_end:
        SLOWCMDI CMD_WRITE_IMG_SEC   ; Command = write SDDOS sector
.if (debug_sddos = 1)
        JSR dump_debug_data
.endif
        RTS

send_drive_and_sector:
        JSR send_cmd_init_write   ; Reset globalbufferpointer
        LDA drive                 ; Send drive 0 or 1
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
        FASTCMDI CMD_LOAD_PARAM   ; Command = load SDDOS parameters
        JMP  interwritedelay

send_cmd_init_read:
        LDA #CMD_INIT_READ
        writeportFAST ACMD_REG
        JMP  interwritedelay

send_cmd_init_write:
        LDA #CMD_INIT_WRITE
        writeportFAST ACMD_REG
        JMP interwritedelay

.if (debug_sddos = 1)
dump_debug_data:
        PHA
        TXA
        PHA
        TYA
        PHA
        JSR OSNEWL
        LDY #0
dump_debug_loop:
        TYA
        PHA
        JSR HEXOUT
        LDA #' '
        JSR OSWRCH
        LDA #':'
        JSR OSWRCH
        LDA #' '
        JSR OSWRCH
        LDX #16
dump_debug_loop1:
        LDA debug_data, Y
        JSR HEXOUT
        INY
        DEX
        BNE dump_debug_loop1
        PLA
        TAY
        LDA #' '
        JSR OSWRCH
        LDX #16
dump_debug_loop2:
        LDA debug_data, Y
        JSR dump_debug_char
        INY
        DEX
        BNE dump_debug_loop2
        JSR OSNEWL
        CPY #0
        BNE dump_debug_loop
        PLA
        TAY
        PLA
        TAX
        PLA
        RTS

dump_debug_char:
        CMP #$20
        BCC dump_debug_char1
        CMP #$7F
        BCC dump_debug_char2
dump_debug_char1:
        LDA #'.'
dump_debug_char2:
        JMP OSWRCH

debug_data:
        .word 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
        .word 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
        .word 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
        .word 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
        .word 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
        .word 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
        .word 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
        .word 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.endif

