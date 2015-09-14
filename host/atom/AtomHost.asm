;;;  AtomHost/src
;;; Source for Atom Tube Host
;;; J.G.Harston and D.M.Banks

        load     = $4000        ; Load address of the host code

        atmhdr   = 1            ; Whether to include an ARM header (form AtoMMC2)
        debug    = 0            ; Whether to include debugging of R2 commands

;;; MOS entry addresses
;;; -------------------
        osshut   = $FFCB
        osfind   = $FFCE
        osbput   = $FFD1
        osbget   = $FFD4
        osstar   = $FFD7
        osrdar   = $FFDA
        ossave   = $FFDD
        osload   = $FFE0
        osrdch   = $FFE3
        osecho   = $FFE6
        osasci   = $FFE9
        osnewl   = $FFED
        oswrcr   = $FFF2
        oswrch   = $FFF4
        oscli    = $FFF7

        oshex    = $F802

;;; Vectors
;;; -------
        nmiv     = $200
        brkv     = $202
        irq1v    = $204
        cliv     = $206
        wrchv    = $208
        rdchv    = $20A
        loadv    = $20C
        savev    = $20E
        rdarv    = $210
        starv    = $212
        bgetv    = $214
        bputv    = $216
        findv    = $218
        shutv    = $21A

;;; System: $0E21 - keypress, b7=1 if nothing pressed
;;; Atom:   $B001 - b5=0 if Escape pressed

;;;I/O addresses
;;;-------------
        TubeIO   = $BC00

        TubeS1=TubeIO+0         ; VDU
        TubeR1=TubeIO+1
        TubeS2=TubeIO+2         ; Command
        TubeR2=TubeIO+3
        TubeS3=TubeIO+4         ; DATA
        TubeR3=TubeIO+5
        TubeS4=TubeIO+6         ; Interrupts
        TubeR4=TubeIO+7

;;; Workspace in zero page
;;; ----------------------

        TubeCtrl   = $80        ; Control block for MOS calls
        TubeSrc    = $92        ; Pointer to Tube transfer block
        TubeStatus = $94        ; Tube status
        TubeOwner  = $95        ; Tube owner
        R2Cmd      = $96        ; Computed address of R2 Command Handler

;;; Optional 22-byte ATM Header
;;; --------------------------

        org     load - 22
        
.AtmHeader

IF (atmhdr = 1)
        EQUS    "TUBE"
        org     AtmHeader + 16
        EQUW    StartAddr
        EQUW    StartAddr
        EQUW    EndAddr - StartAddr
ENDIF

;;; Main Entry Point
;;; ----------------

.StartAddr
        JMP TubeStartup         ; Start Tube system
        JMP TubeEscape          ; Copy Escape state across Tube
        RTS                     ; Data transfer
        NOP
        NOP

;;; pointers to R2 commands
;;; -----------------------

.R2CmdHandlers
        EQUW rdch               ; A=00
        EQUW cli                ; A=02
        EQUW bytelo             ; A=04
        EQUW bytehi             ; A=06
        EQUW word               ; A=08
        EQUW rdline             ; A=0A
        EQUW args               ; A=0C
        EQUW bget               ; A=0E
        EQUW bput               ; A=10
        EQUW find               ; A=12
        EQUW file               ; A=14
        EQUW gbpb               ; A=16

;;; UNUSED
;;; Tube data transfer flags
;;; ------------------------
        EQUB $86                ; CoPro->I/O bytes
        EQUB $88                ; I/O->CoPro bytes
        EQUB $96                ; CoPro->I/O words
        EQUB $98                ; I/O->CoPro words
        EQUB $18                ; Set Execute Address in CoPro
        EQUB $18                ; Release Tube
        EQUB $82                ; CoPro->I/O 256 bytes
        EQUB $18                ; I/O->CoPro 256 bytes

;;; BRK handler
;;; -----------

.TubeBRK
        LDA #$FF
        JSR TubeSendR4
        LDA TubeR2              ; Get ACK byte from CoPro
        LDA #$00
        JSR TubeSendR2          ; Send $00 to R2 to specify ERROR
        TAY
        LDA TubeError,Y
        JSR TubeSendR2          ;  Send via R2
.TubeBRKlp
        INY
        LDA TubeError,Y
        JSR TubeSendR2          ; Send via R2
        TAX
        BNE TubeBRKlp           ; Loop until terminating $00 sent

;;; Tube Idle startup
;;;  -----------------

        ;; Clear stack, enable IRQs
        LDX #$FF
        TXS
        CLI

;;;  Tube idle loop
;;;  --------------

.TubeIdle
        LDA $B001               ; Read keyboard hardware
        AND #$20                ; was escape pressed?
        BNE TubeIdle1
        JSR TubeEscape          ; Escape pressed, pass to Client
.TubeIdle1
        BIT TubeS1
        BPL TubeIdle2           ; Nothing in VDU port, jump to check Command port
.TubeWRCH
        LDA TubeR1
        JSR oswrch              ; Get character and send to OSWRCH
.TubeIdle2
        BIT TubeS2
        BPL TubeIdle            ; Nothing in Command port, loop back
        BIT TubeS1
        BMI TubeWRCH            ; Check VDU port again
        LDX TubeR2              ; Get command
IF (debug = 1)
        JSR DebugNewline
        TXA
        JSR DebugHexOut
ENDIF
        LDA R2CmdHandlers, X    ; Read command handler
        STA R2Cmd
        LDA R2CmdHandlers + 1, X
        STA R2Cmd + 1
        JMP (R2Cmd)             ; index into jump table

.TubeAddr
        EQUD $00800000

        ;;; Start up Tube system
        ;;;  --------------------
.TubeStartup
        LDA #12
        JSR oswrch              ; Clear screen, ready for startup banner
        LDA #$C0
        STA TubeS1              ; Clear all Tube Regs
        LDA #$40
        STA TubeS1
        LDA #$A0
        STA TubeS1              ; Reset client
        LDA #$20
        STA TubeS1
.StartupLp1
        BIT TubeS1
        BPL StartupLp1          ; Loop until VDU data present
        LDA TubeR1
        BEQ Startup2            ; Get it, if CHR$0, finished
        JSR oswrch
        JMP StartupLp1          ; Print character, loop for more
.Startup2
        LDA #(TubeBRK AND 255)
        STA brkv+0              ; Claim BRKV
        LDA #(TubeBRK DIV 256)
        STA brkv+1
        LDA #$8E
        STA TubeS1              ; Enable NMI on R1, IRQ on R4, IRQ on R1
        JSR TubeFree            ; Set Tube 'free' and no owner
        JMP TubeSendAck         ; Send $7F ack and enter idle loop

;;;  Clear Tube status and owner
;;;  ---------------------------
.TubeFree
        LDA #$80
        STA TubeStatus          ; Set Tube ID to 'unclaimed'
        STA TubeOwner           ; Set Tube status to 'free'
.TubeEscape
        RTS

.TubeError
        EQUB 255
        EQUS "HOST ERROR"
        BRK

;;; UNUSED
.AtomCtrl
        EQUW 0,0,0,0,0,0,0,0    ; Atom control block


;;; *****************
;;; TUBE MOS ROUTINES
;;; *****************

;;; CHARACTER I/O CALLS
;;; ===================

;;; OSRDCH
;;; ------
.rdch
        JSR AtomRDCH            ; Wait for a character
.SendCarryA
        ROR A
        JSR TubeSendR2          ; Move Carry into b7 and send it
        ROL A
        JMP TubeSendIdle        ; Restore A and send it, return to idle loop

;;; WORD0 - Read a line
;;; -------------------
.rdline
        LDX #$05
        JSR TubeWaitBlock       ; Fetch 5-byte control block 
        ;;
        ;; We have to do a RDLINE manually, as Atom doesn't provide it
        ;;
        LDY #0
.RdLineLp1
        JSR AtomRDCH
        BCS RdLineEsc           ; Escape pressed, exit
        CMP #127
        BNE RdLineChar
        CPY #0
        BEQ RdLineLp1           ; Nothing to delete
        JSR oswrch
        DEY
        JMP RdLineLp1           ; Delete one character
.RdLineChar
        STA $100,Y              ; Store in string buffer
        CMP #13
        BEQ RdLineCR            ; Repeat until <cr>
        JSR oswrch
        INY
        BNE RdLineLp1           ; Echo character, loop for more
.RdLineCR
        JSR osnewl              ; Print <newline>
        LDA #$7F
        JSR TubeSendR2          ; Send $7F via R2 to indicate no Escape
        LDY #0                  ; Point to start of string buffer
.RdLineLp2
        LDA $0100,Y
        JSR TubeSendR2          ; Send byte via R2
        INY
        CMP #13
        BNE RdLineLp2           ; Loop until <cr> sent
        JMP TubeIdle
.RdLineEsc
        LDA #$FF
        BNE TubeSendIdle        ; Return $FF for Escape, return to Tube idle loop


.bytelo
        LDX #$02
        JSR TubeWaitBlock       ; Fetch 2-byte control block
        LDA #0
        JMP TubeSendIdle

.bytehi
        LDX #$03
        JSR TubeWaitBlock       ; Fetch 3-byte control block
        LDA #0
        JSR TubeSendR2
        JSR TubeSendR2
        JMP TubeSendIdle

.word
        JSR TubeWaitR2          ; Get A
IF (debug = 1)  
        JSR DebugHexOut
ENDIF
        JSR TubeWaitR2          ; Get in-length
IF (debug = 1)  
        JSR DebugHexOut
ENDIF
        TAX
        BEQ wordNoRequest
        JSR TubeWaitBlock
.wordNoRequest
        JSR TubeWaitR2          ; Get out-length
IF (debug = 1)  
        JSR DebugHexOut
ENDIF
        TAX
        BEQ wordNoResponse
        JSR TubeSendBlock
.wordNoResponse
        JMP TubeIdle


;;; OSCLI
;;; =====
.cli

        JSR ReadString          ; Read string to $0100

        LDA $100                ; Test for a zero-length string
        CMP #$0d
        BEQ TubeSendAck         ; Skip it
        
        JSR oscli               ; Execute it

        ;; If the command returns here, the CoPro will get $7F as an acknowledgement.
        ;; The CoPro also gets sent a $7F byte if there is no language available on
        ;; Break. If calling OSCLI results in code being run in the CoPro or a language
        ;; being copied over and entered, the CoPro will get an $80 acknowledgement
        ;; elsewhere.

        ;; Will have to hook into Atom's OSLOAD and OSSAVE to intercept *DELETEACS and *DELETEBGET
        ;; commands to redirect to manual implementations.

;;; Send $7F acknowledgement byte via R2 and return to idle loop
;;; ------------------------------------------------------------
.TubeSendAck
        LDA #$7F                ; Send $7F to CoPro

;;; Send byte in A via R2 and return to Tube idle loop
;;; --------------------------------------------------
.TubeSendIdle
        BIT TubeS2
        BVC TubeSendIdle        ; Loop until Command port free
        STA TubeR2
        JMP TubeIdle            ; Send byte and jump to Tube idle loop


;;; FILING LOADATNTEM CALLS
;;; ===================

;;; OSBPUT
;;; ------
.bput
        JSR TubeWaitR2
        TAY                     ; Wait for a handle
        JSR TubeWaitR2          ; Wait for a data byte
        JSR osbput              ; Write to file
        JMP TubeSendAck         ; Send $7F ack, return to idle loop

;;; OSBGET
;;; ------
.bget
        JSR TubeWaitR2
        TAY                     ; Wait for a handle
        JSR osbget              ; Read from file
        JMP SendCarryA          ; Send Carry and A, return to idle loop

;;; OSFIND
;;; ------
.find
        JSR TubeWaitR2
        BEQ close               ; Zero - jump to do CLOSE
        PHA
        JSR ReadString
        PLA
        ;; Get filename string via R2
        ;;
        ;; Atom OSFIND needs

        ;; X=>zero page, (X+0,1)=>filename
        ;; CS=OPENIN, CC=OPENOUT
        ;;
        STX TubeCtrl+0          ; Store address of filename
        STY TubeCtrl+1
        LDX #TubeCtrl           ; X=>address of filename
        ASL A
        ASL A                   ; Move b6 into Cy, CS=OPENIN, CC=OPENOUT
        JSR osfind              ; Do the OPEN
        JMP TubeSendIdle        ; Send handle back, return to idle loop

;;; CLOSE
;;; -----
.close
        JSR TubeWaitR2
        TAY                     ; Wait for a handle
        JSR osshut              ; Do the CLOSE
        JMP TubeSendAck         ; Send $7F ack and jump to idle loop

;;; OSARGS
;;; ------
.args
        JSR TubeWaitR2
        TAY                     ; Wait for a handle
        LDX #$04
        JSR TubeWaitBlock       ; Fetch 4-byte data block
        JSR TubeWaitR2          ; Wait for action; ;;; Atom needs
;;;
;;;OSRDAR #FFDA Read file's arguments
;;;On entry,  X=>zero page location
;;;           Y=handle
;;;           A=0 - read PTR
;;;           A=1 - read EXT
;;;           A=2 - read allocation
;;;On exit,   X+0-X+3 = returned argument
;;;           A,X,Y preserved
;;;
;;;
;;;OSSTAR #FFD7 Set file's arguments
;;;On entry,  X=>zero page location
;;;           Y=handle
;;;           A=0 - write PTR
;;;           X+0-X+3 = argument
;;;On exit,   A,X,Y preserved;  JSR TubeSendR2             ; Send result back
        LDX #4
        JSR TubeSendBlock       ; Send 4-byte data black
        JMP TubeIdle


;;; OSGBPB
;;; ------
.gbpb
        RTS

;;; OSFILE
;;; ------
.file
        RTS

;;; Atom OSLOAD and OSSAVE can't be used, as will have to pass data across Tube
;;; manually. Not sure how to set load/exec addresses without doing a DELETEBGET.
;;; Maybe do a zero-length DELETEBGET, then OPENOUT it.


;;; TUBE COMMAND TRANSFERS
;;; **********************

;;; Read a string via R2 into string buffer at $0100
;;; ------------------------------------------------
.ReadString
        LDY #$00
.ReadStrLp
        JSR TubeWaitR2
        STA $0100,Y             ; Wait for byte and store in string buffer
        INY
        BEQ ReadStrFull         ; Buffer full, end loop
        CMP #$0D
        BNE ReadStrLp           ; Loop until <cr> received
.ReadStrFull
        LDX #0
        LDY #1
        RTS                     ; Return XY pointing to $0100

;;; Get control block to 0,X
;;; ------------------------
.TubeWaitBlock
        JSR TubeWaitR2
        STA $FF,X
IF (debug = 1)  
        JSR DebugHexOut
ENDIF
        DEX
        BNE TubeWaitBlock
        RTS

;;; Get X and A from Tube R2
;;; ------------------------
.TubeWaitXA
        JSR TubeWaitR2
        TAX

;;; Wait for data from Tube R2
;;; --------------------------
.TubeWaitR2
        BIT TubeS2
        BPL TubeWaitR2          ; Loop until data present
        LDA TubeR2
        RTS                     ; Get byte


;;; Send control block from 0,X
;;; ---------------------------
.TubeSendBlock
        LDA $FF,X
        JSR TubeSendR2
        DEX
        BNE TubeSendBlock
        RTS

;;; Send byte in A via Tube R2
;;; --------------------------
.TubeSendR2
        BIT TubeS2
        BVC TubeSendR2          ; Loop until port free
        STA TubeR2
        RTS
;;; Send byte

;;; Send byte in A via Tube R4
;;; --------------------------
.TubeSendR4
        BIT TubeS4
        BVC TubeSendR4          ; Loop until port free
        STA TubeR4
        RTS                     ; Send byte


;;; ***************************
;;; INTERFACE TO ATOM MOS CALLS
;;; ***************************

;;; Interface to Atom OSRDCH
;;; ------------------------
.AtomRDCH
        JSR osrdch
        PHA
        CLC                     ; Wait for a character
        LDA $B001
        AND #$20                ; Read keyboard hardware
        BNE AtomRDCHok
        SEC                     ; SEC as Escape key pressed
.AtomRDCHok
        PLA
        RTS

;;; Debugging output, avoid trashing A
;;;

.DebugNewline
        PHA
        JSR osnewl
        PLA
        RTS
        
.DebugHexOut
        PHA
        JSR oshex
        PLA
        RTS

.EndAddr

IF (atmhdr = 1)
        SAVE "TUBE",AtmHeader,EndAddr
ELSE
        SAVE "TUBE",StartAddr,EndAddr
ENDIF
