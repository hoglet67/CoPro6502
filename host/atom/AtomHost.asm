;;;  AtomHost/src
;;; Source for Atom Tube Host
;;; J.G.Harston and D.M.Banks

        load     = $3000        ; Load address of the host code

        atmhdr   = 1            ; Whether to include an ARM header (form AtoMMC2)
        debug    = 0            ; Whether to include debugging of R2 commands

        LangStart = $4000       ; start of the language in host memory
        LangEnd   = $8000       ; end of the language in host memory

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

;;; VIA Addresses
	ViaBase       = $B800
	ViaT1CounterL = ViaBase + 4
	ViaT1CounterH = ViaBase + 5
	ViaACR        = ViaBase + 11
	ViaIER        = ViaBase + 14
	
;;; Workspace in zero page
;;; ----------------------

        TubeCtrl   = $80        ; Control block for MOS calls
        TubeSrc    = $92        ; Pointer to Tube transfer block
        TubeStatus = $94        ; Tube status
        TubeOwner  = $95        ; Tube owner
        R2Cmd      = $96        ; Computed address of R2 Command Handler
        LangFlag   = $98

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

.StartAddr

        ;;; Start up the Atom Tube system
        ;;; ----------------------------

.TubeStartup
        LDA #$01
        STA LangFlag
        LDA #12
        JSR oswrch              ; Clear screen, ready for startup banner
	JSR ViaInit             ; Initialize 50Hz interrupts
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
        CMP #$60
        BCC UpperCase
        AND #$DF
.UpperCase
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

        LDA LangFlag            ; Skip language transfer if flag 0
        BEQ Startup3

        SEC                     ; Transfer the language
        JSR L0400

        LDA #$80
        JMP TubeSendIdle        ;

.Startup3
        JMP TubeSendAck         ; Send $7f ack and enter idle loop

;;; Main Entry Point Block
;;; ----------------------

.L0400
        JMP LanguageStartup         ; Copy Language and Start Tube system

.L0403
        JMP TubeEscape          ; Copy Escape state across Tube


;;; Tube Transfer/Claim/Release
;;; ---------------------------

.L0406
        CMP #&80                ; Claim/Release/Action via Tube
        BCC TubeTransfer        ; If <&80, data transfer action
        CMP #&C0                ; Is it claim or release?
        BCS TubeClaim           ; &C0-&FF - jump to claim Tube
        ORA #&40                ; Ensure release ID same as claim ID
        CMP TubeOwner           ; Is the the same as the claim ID?
        BNE TubeExit            ; No, exit.

.TubeRelease
        PHP                     ; Save IRQ state
        SEI                     ; Disable IRQs
        LDA #&05                ; Send &05 to R4 to interupt CoPro
        JSR TubeSendR4
        LDA TubeOwner           ; Send Tube ID to notify a Tube release
        JSR TubeSendR4
        PLP                     ; Get IRQ state back

        ;; Clear Tube status and owner
.TubeFree
        LDA #&80
        STA TubeOwner           ; Set Tube ID to 'unclaimed'
        STA TubeStatus          ; Set Tube status to 'free'
        RTS

;;; Claim Tube
;;; ----------

.TubeClaim
        ASL TubeStatus          ; Is Tube free?
        BCS TubeClaim1          ; Yes, jump to claim it
        CMP TubeOwner           ; Is Tube ID same as claimer?
        BEQ TubeExit            ; Yes, exit as we already own it
        CLC                     ; Signal 'can't claim Tube'
        RTS                     ; And exit

.TubeClaim1
        STA TubeOwner           ; Store Tube ID

.TubeExit
        RTS

;;; Tube data transfer
;;; ------------------

.TubeTransfer
        PHP                     ; Save IRQ status
        SEI                     ; Disable IRQs
        STY TubeSrc + 1         ; Store pointer to control block
        STX TubeSrc             ; Send action code to R4 to
        JSR TubeSendR4          ; interrupt CoPro
        TAX                     ; Save action code in X
        LDY #&03                ; Prepare to send 4 byte control block
        LDA TubeOwner           ; Send Tube ID via R4, interupting
        JSR TubeSendR4          ; CoPro

.TubeTransfer1
        LDA (TubeSrc),Y         ; Get byte from Tube control block
        JSR TubeSendR4          ; Send via R4
        DEY
        BPL TubeTransfer1       ; Loop for whole block
        LDY #&18
        STY TubeS1              ; Disable FIFO on R3, and NMI on R3 by default
        LDA TransferFlags,X     ; Get Tube I/O setting according to
        STA TubeS1              ; action code and set Tube
        LSR A
        LSR A                   ; Move b1 to Carry (b1 set = Copro->I/O)
        BCC TubeTransfer2       ; If no pre-delay needed, jump past
        BIT TubeR3              ; Read R3 twice to delay & empty FIFO
        BIT TubeR3

.TubeTransfer2
        JSR TubeSendR4          ; Send flag via R4 to synchronise

.TubeTransfer3
        BIT TubeS4              ; Check R4 status
        BVC TubeTransfer3       ; Loop until data has left R4
        BCS TubeTransfer5       ; Carry still indicates direction
        CPX #&04                ; Is action 'execute code'?
        BNE TubeTransfer6       ; No, jump to finish

.TubeTransfer4
        JSR TubeRelease         ; Release Tube
        JSR TubeSendR2          ; Send &80 via R2
        JMP TubeIdleStartup     ; Jump to Tube Idle loop

.TubeTransfer5
        LSR A                   ; Move Tube I/O setting b2 into Carry (b2 set = NMI required)
        BCC TubeTransfer6       ; It was clear, jump to exit
        LDY #&88                ; Set Tube I/O to NMI on R3
        STY TubeS1

.TubeTransfer6
        PLP                     ; Restore IRQ status
        RTS                     ; And exit

;;; Copy language across Tube
;;; -------------------------
;;;     On entry, A=1 - enter language, CLC=Break, SEC=OSBYTE 142
;;;               A=0 - no language found at Break

.LanguageStartup

        CLI                     ; Enable IRQs
        BCS LanguageEnter       ; Branch if selected with *fx142
        BNE TestLastBreak       ; A<>0, jump to enter language
        JMP TubeSendAck         ; A=0, jump to enter Tube Idle loop

;;; Language entered at BREAK
;;; -------------------------

.TestLastBreak

;;; The Atom does not have different break types
;;; So always handles a for hard bread

;;;     LDX &028D               ; Get last break type
;;;     BEQ TubeTransfer4       ; If Soft Break, release Tube, send &80
                                ; via R2 and enter Idle loop

;;; The current language is not copied across the Tube on soft Break, only on
;;; Power-On Break and Hard Break, or when entered explicitly with OSBYTE 142.

;;; Language entered with OSBYTE 142, or on Hard Break
;;; --------------------------------------------------

.LanguageEnter
        LDA #&FF
        JSR L0406               ; Claim Tube with ID=&3F
        BCC LanguageEnter       ; Loop until Tube available
        JSR FindLanguageAddr    ; Find address to copy language to

;;; Send language ROM via Tube 256 bytes at a time
;;; ----------------------------------------------

.TransferLanguage
        PHP                     ; Save IRQ status
        SEI                     ; Disable IRQs
        LDA #&07                ; Start I/O->CoPro transfer 256 bytes
        JSR StartTransfer       ; Use Tube address at .TubeAddr
        LDY #&00
        STY TubeCtrl            ; Start copying from &8000

.TransferBlock
        LDA (TubeCtrl),Y        ; Get byte from ROM
        STA TubeR3              ; Send to CoPro via R3
        NOP                     ; Delay for a while
        NOP
        NOP
        INY
        BNE TransferBlock       ; Loop for 256 bytes
        PLP                     ; Restore IRQs
        INC TubeAddr + 1        ; Update Tube address
        BNE TransferIncSrc
        INC TubeAddr + 2
        BNE TransferIncSrc
        INC TubeAddr + 3

.TransferIncSrc
        INC TubeCtrl + 1        ; Update source address
        LDA TubeCtrl + 1        ; Check b6 of source high byte
        CMP #>LangEnd
        BCC TransferLanguage    ; Loop until end of language
        JSR FindLanguageAddr    ; Find start address language copied to
        LDA #&04                ; Execute code in CoPro, finished by
                                ; sending &80 to Copro in R2

;;; Start a Tube transfer with address block at &0053
;;; -------------------------------------------------

.StartTransfer
        LDY #>TubeAddr
        LDX #<TubeAddr          ; Point to Tube control block
        JMP L0406               ; Jump to do a data transfer

;;; Set Tube address to destination to copy language to
;;; ---------------------------------------------------
;;; Also sets source address at &00/&01 to &80xx

.FindLanguageAddr
        LDA #&80
        STA TubeAddr + 1        ; Set Tube address to &xxxx80xx
        LDA #>LangStart
        STA TubeCtrl + 1        ; Set source address to language
        LDA #&20
        AND LangStart + 6        ; Check relocation bit in ROM type
        TAY                     ; If no relocation address, A=0, Y=0
        STY TubeAddr            ; Set Tube address to &xxxx8000
        BEQ FindLanguageAddr2   ; Jump forward with no relocation

        LDX LangStart + 7        ; Get offset to ROM copyright
.FindLanguageAddr1
        INX
        LDA LangStart, X         ; Skip past copyright message
        BNE FindLanguageAddr1   ; Loop until terminating zero byte
        LDA LangStart + 1, X     ; Get relocation address from after
        STA TubeAddr            ; copyright message
        LDA LangStart + 2, X
        STA TubeAddr + 1
        LDY LangStart + 3, X     ; Get two high bytes to Y and A
        LDA LangStart + 4, X

;;; Set Tube address high bytes
;;; ---------------------------

.FindLanguageAddr2
        STA TubeAddr + 3        ; Set Tube address high bytes
        STY TubeAddr + 2
        RTS

;;; Tube data transfer flags
;;; ------------------------

.TransferFlags
        EQUB $86                ; CoPro->I/O bytes
        EQUB $88                ; I/O->CoPro bytes
        EQUB $96                ; CoPro->I/O words
        EQUB $98                ; I/O->CoPro words
        EQUB $18                ; Set Execute Address in CoPro
        EQUB $18                ; Release Tube
        EQUB $82                ; CoPro->I/O 256 bytes
        EQUB $18                ; I/O->CoPro 256 bytes

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
;;; -----------------

.TubeIdleStartup
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
	PHA			; Stack the osword number	
	JSR TubeWaitR2          ; Get in-length
IF (debug = 1)
        JSR DebugHexOut
ENDIF
        TAX
        JSR TubeWaitBlock
        JSR TubeWaitR2          ; Get out-length
IF (debug = 1)
        JSR DebugHexOut
ENDIF
        TAX
	PLA			; Restore osword number
	CMP #$01		; Read System Clock
	BEQ word01ReadSys
	CMP #$02		; Write System Clock
	BEQ word02WriteSys

;;; Default OSWORD HANDLER
.wordSendBlock
        JSR TubeSendBlock	; length of block in X
        JMP TubeIdle

;;; OSWORD A=1 Read System Clock
.word01ReadSys
	LDY #4			; Copy the 5 byte time value
.word01ReadSysLoop		; to the Tube Control block
	LDA ViaTime, Y	
	STA TubeCtrl, Y
	DEY
	BPL word01ReadSysLoop
	BMI wordSendBlock
	
;;; OSWORD A=2 Write System Clock
.word02WriteSys
	LDY #4			; Copy the 5 byte time value
.word02WriteSysLoop		; to the Via Time
	LDA TubeCtrl, Y
	STA ViaTime, Y	
	DEY
	BPL word02WriteSysLoop
	BMI wordSendBlock
	

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

;;; Get control block to TubeCtrl, X
;;; --------------------------------
.TubeWaitBlock
	DEX
	BMI TubeWaitBlockExit
        JSR TubeWaitR2 
IF (debug = 1)
        JSR DebugHexOut
ENDIF
        STA TubeCtrl, X
        JMP TubeWaitBlock
.TubeWaitBlockExit
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


;;; Send control block from TubeCtrl, X
;;; -----------------------------------
.TubeSendBlock
	DEX
	BMI TubeSendBlockExit
        LDA TubeCtrl, X
        JSR TubeSendR2
        JMP TubeSendBlock
.TubeSendBlockExit
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
IF (debug = 1)
        JSR DebugHexOut
ENDIF
        RTS                     ; Send byte

;;; Copy Escape state across Tube
;;; -----------------------------

;;; TODO

.TubeEscape
        RTS

.TubeError
        EQUB 255
        EQUS "HOST ERROR"
        BRK

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


.ViaInit
	LDA #<ViaISR		; Setup the interrupt handler
	STA irq1v
	LDA #>ViaISR
	STA irq1v+1
	LDA #$00		; Clear the timer
	STA ViaTime
	STA ViaTime + 1
	STA ViaTime + 2
	STA ViaTime + 3
	STA ViaTime + 4
	LDA #<9999		; 10ms timer interrupts
	STA ViaT1CounterL
	LDA #>9999
	STA ViaT1CounterH
	LDA #$40		; Enable T1 continuous interrupts
	STA ViaACR  		; Disable everything else
	LDA #$7F		; Disable all interrupts
	STA ViaIER
	LDA #$C0		; Enable T1 interrupts
	STA ViaIER
	RTS

.ViaISR
	LDA ViaT1CounterL	; Clear the interrupts flag
	INC ViaTime
	BNE ViaExit
	INC ViaTime + 1
	BNE ViaExit
	INC ViaTime + 2
	BNE ViaExit
	INC ViaTime + 3
	BNE ViaExit
	INC ViaTime + 4
.ViaExit
	PLA			; the Atom stacks A for us
	RTI

.ViaTime
	EQUB 0,0,0,0,0
	
;;; Debugging output, avoid trashing A
;;;

.DebugNewline
	PHP
        PHA
        JSR osnewl
        PLA
	PLP
        RTS

.DebugHexOut
	PHP
        PHA
        JSR oshex
        PLA
	PLP
        RTS

.EndAddr

	
IF (atmhdr = 1)
        SAVE "TUBE",AtmHeader,EndAddr
ELSE
        SAVE "TUBE",StartAddr,EndAddr
ENDIF
