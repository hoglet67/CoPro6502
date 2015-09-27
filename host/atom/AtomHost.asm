;;;  AtomHost/src
;;; Source for Atom Tube Host
;;; J.G.Harston and D.M.Banks

        load     = $3000        ; Load address of the host code

        atmhdr   = 1            ; Whether to include an ARM header (form AtoMMC2)

        atommc   = 0            ; Whether to include a local copy of AtomMMC2 load/save

        debug_r1 = 0            ; Whether to include debugging of R1 commands
        debug_r2 = 0            ; Whether to include debugging of R2 commands
        debug_r4 = 0            ; Whether to include debugging of R4 commands

        debug_osw_7f = 0        ; Whether to include debugging of OSWORD 7F commands
        debug_osw_ff = 0        ; Whether to include debugging of OSWORD FF commands

        debug_sddos = 0         ; Whether to include debugging of SDDOS data blocks

        debug_unsupp = 0        ; Whether to include debugging of unsupported osbyte/osword commands

        buffered_kbd = 1        ; Whether to include a buffered keyboard routine

        LangStart = $4000       ; start of the language in host memory
        LangEnd   = $8000       ; end of the language in host memory

.include "macros.asm"
.include "atmmc2def.asm"


;;; MOS entry addresses
;;; -------------------
        OSSHUT   = $FFCB
        OSFIND   = $FFCE
        OSBPUT   = $FFD1
        OSBGET   = $FFD4
        OSSTAR   = $FFD7
        OSRDAR   = $FFDA
        OSSAVE   = $FFDD
        OSLOAD   = $FFE0
        OSECHO   = $FFE6
        OSASCI   = $FFE9
        OSNEWL   = $FFED
        OSWRCR   = $FFF2
        OSCLI    = $FFF7

        ;; These are already defined in atmmc2def.asm
        ;; OSRDCH   = $FFE3
        ;; OSWRCH   = $FFF4
        ;; HEXOUT   = $F802
        ;; STROUT   = $F7D1

;;; Atom OS addresses
        ERRPTR    = $D5


;;; Vectors
;;; -------
        NMIV     = $200
        BRKV     = $202
        IRQ1V    = $204
        CLIV     = $206
        WRCHV    = $208
        RDCHV    = $20A
        LOADV    = $20C
        SAVEV    = $20E
        RDARV    = $210
        STARV    = $212
        BGETV    = $214
        BPUTV    = $216
        FINDV    = $218
        SHUTV    = $21A

;;; System: $0E21 - keypress, b7=1 if nothing pressed
;;; Atom:   $B001 - b5=0 if Escape pressed

;;;I/O addresses
;;;-------------

        GODIL    = $BDE0
        GodilModeExtension = GODIL + 0
        GodilVersion = GODIL + 15


        TubeIO   = $BEE0

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

        TubeCtrl   = $60        ; Control block for MOS calls
        TubeSrc    = $72        ; Pointer to Tube transfer block
        TubeStatus = $74        ; Tube status
        TubeOwner  = $75        ; Tube owner
        R2Cmd      = $76        ; Computed address of R2 Command Handler
        LangFlag   = $78
        EscapeFlag = $79
        GodilFlag  = $7A
.if (buffered_kbd = 1)
        KeyBuf     = $7B        ; one character keyboard buffer
.endif
        AtomCmd    = $c9        ; used by osfile; this could be anywhere
        AtomStr    = $140       ; used by osfile; atommc assumes 140

        TubeFlag   = $3CF       ; tube enabled flag, set by atom tube host
        TubeEna    = $5A        ; tube enable magic value

;;; Optional 22-byte ATM Header
;;; --------------------------

AtmHeader:

        .byte    "TUBE"
        .word    0,0,0,0,0,0
        .word    StartAddr
        .word    StartAddr
        .word    EndAddr - StartAddr

StartAddr:

;;; Main Entry Point Block
;;; ----------------------

L0400:
        JMP TubeStartup         ; Copy Language and Start Tube system
L0403:
        JMP EscapeCopy          ; Copy Escape state across Tube
L0406:
        JMP TubeClaimTransferRelease
L0409:
        JMP TubeError

;;; Start up the Atom Tube system
;;; ----------------------------

TubeStartup:
        LDA #$00                ; non zero means transfer language
        STA LangFlag
        LDA #$00                ; B5 tracks escape key, B6 tracks escape state
        STA EscapeFlag
.if (buffered_kbd = 1)
        LDA #$80                ; bit 7 is the KeyFlag,
        STA KeyBuf              ; bits 6-0 are the ASCII value, or zero if a key wasn't pressed
.endif
        LDA #TubeEna            ; Enable tube transfers in AtoMMC
        STA TubeFlag
        LDA GodilVersion        ; Test GODIL version is 1x
        AND #$F0
        CMP #$10
        BNE NoGodil
        LDA GodilModeExtension  ; Test GODIL 80x40 mode
        BPL NoGodil
        LDA #$80		; Allow lower case characters to be output
        BNE UpdateGodilFlag
NoGodil:
        LDA #$00
UpdateGodilFlag:
        STA GodilFlag
        LDA #12
        JSR AtomWRCH            ; Clear screen, ready for startup banner
        JSR ViaInit             ; Initialize 50Hz interrupts
        LDA #$C0
        STA TubeS1              ; Clear all Tube Regs
        LDA #$40
        STA TubeS1
        LDA #$A0
        STA TubeS1              ; Reset client
        LDA #$20
        STA TubeS1
StartupLp1:
        BIT TubeS1
        BPL StartupLp1          ; Loop until VDU data present
        LDA TubeR1
        BEQ Startup2            ; Get it, if CHR$0, finished
        JSR AtomWRCH
        JMP StartupLp1          ; Print character, loop for more
Startup2:
        LDA #<TubeBRK
        STA BRKV+0              ; Claim BRKV
        LDA #>TubeBRK
        STA BRKV+1

.if (atommc = 1)
        LDA #<osloadcode        ; Claim OSLOAD
        STA LOADV
        LDA #>osloadcode
        STA LOADV+1
        LDA #<ossavecode        ; Claim OSSAVE
        STA SAVEV
        LDA #>ossavecode
        STA SAVEV+1
.endif
        LDA #$8E
        STA TubeS1              ; Enable NMI on R1, IRQ on R4, IRQ on R1
        JSR TubeFree            ; Set Tube 'free' and no owner

        LDA LangFlag            ; Skip language transfer if flag 0
        BEQ Startup3

        SEC                     ; Transfer the language
        JSR L0400

        LDA #$80
        JMP TubeSendIdle        ;

Startup3:
        JMP TubeSendAck         ; Send $7f ack and enter idle loop


;;; Tube Transfer/Claim/Release
;;; ---------------------------

TubeClaimTransferRelease:
        CMP #$80                ; Claim/Release/Action via Tube
        BCC TubeTransfer        ; If <$80, data transfer action
        CMP #$C0                ; Is it claim or release?
        BCS TubeClaim           ; $C0-$FF - jump to claim Tube
        ORA #$40                ; Ensure release ID same as claim ID
        CMP TubeOwner           ; Is the the same as the claim ID?
        BNE TubeExit            ; No, exit

TubeRelease:
        PHP                     ; Save IRQ state
        SEI                     ; Disable IRQs
        LDA #$05                ; Send $05 to R4 to interupt CoPro
        JSR TubeSendR4
        LDA TubeOwner           ; Send Tube ID to notify a Tube release
        JSR TubeSendR4
.if (debug_r4 = 1)
        JSR DebugNewline
.endif
        PLP                     ; Get IRQ state back

        ;; Clear Tube status and owner
TubeFree:
        LDA #$80
        STA TubeOwner           ; Set Tube ID to 'unclaimed'
        STA TubeStatus          ; Set Tube status to 'free'
        RTS

;;; Claim Tube
;;; ----------

TubeClaim:
        ASL TubeStatus          ; Is Tube free?
        BCS TubeClaim1          ; Yes, jump to claim it
        CMP TubeOwner           ; Is Tube ID same as claimer?
        BEQ TubeExit            ; Yes, exit as we already own it
        CLC                     ; Signal 'can't claim Tube'
        RTS                     ; And exit

TubeClaim1:
        STA TubeOwner           ; Store Tube ID

TubeExit:
        RTS

;;; Tube data transfer
;;; ------------------

TubeTransfer:
        PHP                     ; Save IRQ status
        SEI                     ; Disable IRQs
        STY TubeSrc + 1         ; Store pointer to control block
        STX TubeSrc             ; Send action code to R4 to
        JSR TubeSendR4          ; interrupt CoPro
        TAX                     ; Save action code in X
        LDY #$03                ; Prepare to send 4 byte control block
        LDA TubeOwner           ; Send Tube ID via R4, interupting
        JSR TubeSendR4          ; CoPro

TubeTransfer1:
        LDA (TubeSrc),Y         ; Get byte from Tube control block
        JSR TubeSendR4          ; Send via R4
        DEY
        BPL TubeTransfer1       ; Loop for whole block
        LDY #$18
        STY TubeS1              ; Disable FIFO on R3, and NMI on R3 by default
        LDA TransferFlags,X     ; Get Tube I/O setting according to
        STA TubeS1              ; action code and set Tube
        LSR A
        LSR A                   ; Move b1 to Carry (b1 set = Copro->I/O)
        BCC TubeTransfer2       ; If no pre-delay needed, jump past
        BIT TubeR3              ; Read R3 twice to delay & empty FIFO
        BIT TubeR3

TubeTransfer2:
        JSR TubeSendR4          ; Send flag via R4 to synchronise

TubeTransfer3:
        BIT TubeS4              ; Check R4 status
        BVC TubeTransfer3       ; Loop until data has left R4
        BCS TubeTransfer5       ; Carry still indicates direction
        CPX #$04                ; Is action 'execute code'?
        BNE TubeTransfer6       ; No, jump to finish

TubeTransfer4:
        JSR TubeRelease         ; Release Tube
        JSR TubeSendR2          ; Send $80 via R2
        JMP TubeIdleStartup     ; Jump to Tube Idle loop

TubeTransfer5:
        LSR A                   ; Move Tube I/O setting b2 into Carry (b2 set = NMI required)
        BCC TubeTransfer6       ; It was clear, jump to exit
        LDY #$88                ; Set Tube I/O to NMI on R3
        STY TubeS1

TubeTransfer6:
        PLP                     ; Restore IRQ status
        RTS                     ; And exit

;;; Copy language across Tube
;;; -------------------------
;;;     On entry, A=1 - enter language, CLC=Break, SEC=OSBYTE 142
;;;               A=0 - no language found at Break

LanguageStartup:

        CLI                     ; Enable IRQs
        BCS LanguageEnter       ; Branch if selected with *fx142
        BNE TestLastBreak       ; A<>0, jump to enter language
        JMP TubeSendAck         ; A=0, jump to enter Tube Idle loop

;;; Language entered at BREAK
;;; -------------------------

TestLastBreak:

;;; The Atom does not have different break types
;;; So always handles a for hard bread

;;;     LDX $028D               ; Get last break type
;;;     BEQ TubeTransfer4       ; If Soft Break, release Tube, send $80
                                ; via R2 and enter Idle loop

;;; The current language is not copied across the Tube on soft Break, only on
;;; Power-On Break and Hard Break, or when entered explicitly with OSBYTE 142

;;; Language entered with OSBYTE 142, or on Hard Break
;;; --------------------------------------------------

LanguageEnter:
        LDA #$FF
        JSR L0406               ; Claim Tube with ID=$3F
        BCC LanguageEnter       ; Loop until Tube available
        JSR FindLanguageAddr    ; Find address to copy language to

;;; Send language ROM via Tube 256 bytes at a time
;;; ----------------------------------------------

TransferLanguage:
        PHP                     ; Save IRQ status
        SEI                     ; Disable IRQs
        LDA #$07                ; Start I/O->CoPro transfer 256 bytes
        JSR StartTransfer       ; Use Tube address at TubeAddr
        LDY #$00
        STY TubeCtrl            ; Start copying from $8000

TransferBlock:
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

TransferIncSrc:
        INC TubeCtrl + 1        ; Update source address
        LDA TubeCtrl + 1        ; Check b6 of source high byte
        CMP #>LangEnd
        BCC TransferLanguage    ; Loop until end of language
        JSR FindLanguageAddr    ; Find start address language copied to
        LDA #$04                ; Execute code in CoPro, finished by
                                ; sending $80 to Copro in R2

;;; Start a Tube transfer with address block at $0053
;;; -------------------------------------------------

StartTransfer:
        LDY #>TubeAddr
        LDX #<TubeAddr          ; Point to Tube control block
        JMP L0406               ; Jump to do a data transfer

;;; Set Tube address to destination to copy language to
;;; ---------------------------------------------------
;;; Also sets source address at $00/$01 to $80xx

FindLanguageAddr:
        LDA #$80
        STA TubeAddr + 1        ; Set Tube address to $xxxx80xx
        LDA #>LangStart
        STA TubeCtrl + 1        ; Set source address to language
        LDA #$20
        AND LangStart + 6        ; Check relocation bit in ROM type
        TAY                     ; If no relocation address, A=0, Y=0
        STY TubeAddr            ; Set Tube address to $xxxx8000
        BEQ FindLanguageAddr2   ; Jump forward with no relocation

        LDX LangStart + 7        ; Get offset to ROM copyright
FindLanguageAddr1:
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

FindLanguageAddr2:
        STA TubeAddr + 3        ; Set Tube address high bytes
        STY TubeAddr + 2
        RTS

;;; Tube data transfer flags
;;; ------------------------

TransferFlags:
        .byte $86                ; CoPro->I/O bytes
        .byte $88                ; I/O->CoPro bytes
        .byte $96                ; CoPro->I/O words
        .byte $98                ; I/O->CoPro words
        .byte $18                ; Set Execute Address in CoPro
        .byte $18                ; Release Tube
        .byte $82                ; CoPro->I/O 256 bytes
        .byte $18                ; I/O->CoPro 256 bytes

;;; pointers to R2 commands
;;; -----------------------

R2CmdHandlers:
        .word rdch               ; A=00
        .word clii               ; A=02
        .word bytelo             ; A=04
        .word bytehi             ; A=06
        .word word               ; A=08
        .word rdline             ; A=0A
        .word args               ; A=0C
        .word bget               ; A=0E
        .word bput               ; A=10
        .word find               ; A=12
        .word file               ; A=14
        .word gbpb               ; A=16


;;; BRK handler
;;; -----------

TubeBRK:
        LDA #<TubeHostError     ; Default error messsage
        STA ERRPTR
        LDA #>TubeHostError
        STA ERRPTR+1
        LDX #$FF                ; Error number 255

TubeError:
        LDA #$FF
        JSR TubeSendR4
        LDA TubeR2              ; Get ACK byte from CoPro
.if (debug_r2 = 1)
        JSR DebugHexOut
.endif
        LDA #$00
        JSR TubeSendR2          ; Send $00 to R2 to specify ERROR
        TAY
        TXA                     ; Get the error number
        JSR TubeSendR2          ; Send via R2
TubeErrorLp:
        LDA (ERRPTR),Y
        JSR TubeSendR2          ; Send via R2
        INY
        TAX
        BNE TubeErrorLp         ; Loop until terminating $00 sent

;;; Tube Idle startup
;;; -----------------

TubeIdleStartup:
        ;; Clear stack, enable IRQs
        LDX #$FF
        TXS
        CLI
        BNE TubeIdleLoop

;;;  Tube idle loop
;;;  --------------

TubeIdle:
.if (debug_r2 = 1)
        JSR DebugNewline
.endif
TubeIdleLoop:
        JSR EscapeCheck
        BIT TubeS1
        BPL TubeIdle2           ; Nothing in VDU port, jump to check Command port
TubeWRCH:
        LDA TubeR1
        JSR AtomWRCH            ; Get character and send to OSWRCH
TubeIdle2:
        BIT TubeS2
        BPL TubeIdleLoop        ; Nothing in Command port, loop back
        BIT TubeS1
        BMI TubeWRCH            ; Check VDU port again
        LDX TubeR2              ; Get command
.if (debug_r2 = 1)
        JSR DebugNewline
        TXA
        JSR DebugHexOut
.endif
        LDA R2CmdHandlers, X    ; Read command handler
        STA R2Cmd
        LDA R2CmdHandlers + 1, X
        STA R2Cmd + 1
        JMP (R2Cmd)             ; index into jump table

TubeAddr:
        .dword $00800000



;;; *****************
;;; TUBE MOS ROUTINES
;;; *****************

;;; CHARACTER I/O CALLS
;;; ===================

;;; OSRDCH
;;; ------
rdch:
        JSR AtomRDCH            ; Wait for a character
SendCarryA:
        ROR A
        JSR TubeSendR2          ; Move Carry into b7 and send it
        ROL A
        JMP TubeSendIdle        ; Restore A and send it, return to idle loop

;;; WORD0 - Read a line
;;; -------------------
rdline:
        LDX #$05
        JSR TubeWaitBlock       ; Fetch 5-byte control block
        ;;
        ;; We have to do a RDLINE manually, as Atom doesn't provide it
        ;;
        LDY #0
RdLineLp1:
        JSR AtomRDCH
        BCS RdLineEsc           ; Escape pressed, exit
        CMP #127
        BNE RdLineChar
        CPY #0
        BEQ RdLineLp1           ; Nothing to delete
        JSR AtomWRCH
        DEY
        JMP RdLineLp1           ; Delete one character
RdLineChar:
        STA $100,Y              ; Store in string buffer
        CMP #13
        BEQ RdLineCR            ; Repeat until <cr>
        JSR AtomWRCH
        INY
        BNE RdLineLp1           ; Echo character, loop for more
RdLineCR:
        JSR OSNEWL              ; Print <newline>
        LDA #$7F
        JSR TubeSendR2          ; Send $7F via R2 to indicate no Escape
        LDY #0                  ; Point to start of string buffer
RdLineLp2:
        LDA $0100,Y
        JSR TubeSendR2          ; Send byte via R2
        INY
        CMP #13
        BNE RdLineLp2           ; Loop until <cr> sent
        JMP TubeIdle
RdLineEsc:
        LDA #$FF
        JMP TubeSendIdle        ; Return $FF for Escape, return to Tube idle loop


bytelo:
        JSR TubeWaitR2
        TAX
        JSR TubeWaitR2

        CMP #$7E
        BEQ osbyte7e

;;; Log an unsupported OSBYTE
.if (debug_unsupp = 1)
        PHA
        TXA
        PHA
        JSR STROUT
        .byte "UNSUPPORTED OSBYTE X="
        NOP
        PLA
        JSR HEXOUT
        JSR STROUT
        .byte "; A="
        NOP
        PLA
        JSR HEXOUT
        JSR OSNEWL
.endif

        LDA #0
        JMP TubeSendIdle

osbyte7e:                       ; OSBYTE 7e = Ack detection of escape condition
        JSR EscapeClear
        LDA #$ff
        JMP TubeSendIdle        ; ff = escape condition cleared

bytehi:
        JSR TubeWaitR2          ; Fetch 3-byte control block X Y A
        TAX
        JSR TubeWaitR2
        TAY
        JSR TubeWaitR2

        CMP #$98
        BEQ osbyte98
        CMP #$B1
        BEQ osbyteB1
        CMP #$D8
        BEQ osbyteD8

;;; Log an unsupported OSBYTE
.if (debug_unsupp = 1)
        PHA
        TXA
        PHA
        TYA
        PHA
        JSR STROUT
        .byte "UNSUPPORTED OSBYTE Y="
        NOP
        PLA
        JSR HEXOUT
        JSR STROUT
        .byte "; X="
        NOP
        PLA
        JSR HEXOUT
        JSR STROUT
        .byte "; A="
        NOP
        PLA
        JSR HEXOUT
        JSR OSNEWL
.endif

osbyteB1:
osbyteD8:

        LDA #0                  ; Return Cy Y X
        JSR TubeSendR2
        JSR TubeSendR2
        JMP TubeSendIdle

osbyte98:
.if (buffered_kbd = 1)
        JSR PollKeyboard
        LDA KeyBuf
        AND #$7F
        BNE osbyte98_full
osbyte98_empty:
        LDA #$ff                ; Cy=1 (Empty)
        BNE osbyte98_send
osbyte98_full:
        LDA #$00                ; Cy=0 (Not empty)
osbyte98_send:
.else
        LDA #$ff                ; Cy=1 (Empty)
.endif
        JSR TubeSendR2
        TYA                     ; Y preserved
        JSR TubeSendR2
        TXA                     ; X preserved
        JMP TubeSendIdle

word:
        JSR TubeWaitR2          ; Get A
        PHA                     ; Stack the osword number
        JSR TubeWaitR2          ; Get in-length
        TAX
        JSR TubeWaitBlock
        JSR TubeWaitR2          ; Get out-length
        TAX
        PLA                     ; Restore osword number
        CMP #$01                ; Read System Clock
        BEQ word01ReadSys
        CMP #$02                ; Write System Clock
        BEQ word02WriteSys
        CMP #$05
        BEQ word05ReadMem       ; Read host memory
        CMP #$06
        BEQ word06WriteMem      ; Write host memory
        CMP #$7F
        BEQ word7Fdisk          ; 8271 command level disk access
        CMP #$FF
        BEQ wordFFtransfer      ; host/parasite data transfer

;;; Log an unsupported OSWORD
.if (debug_unsupp = 1)
        PHA
        JSR STROUT
        .byte "UNSUPPORTED OSWORD A="
        NOP
        PLA
        JSR HEXOUT
        JSR OSNEWL
.endif

;;; Default OSWORD HANDLER
wordSendBlock:
        JSR TubeSendBlock       ; length of block in X
        JMP TubeIdle

;;; OSWORD A=1 Read System Clock
word01ReadSys:
        LDY #4                  ; Copy the 5 byte time value
word01ReadSysLoop:              ; to the Tube Control block
        LDA ViaTime, Y
        STA TubeCtrl, Y
        DEY
        BPL word01ReadSysLoop
        BMI wordSendBlock

;;; OSWORD A=2 Write System Clock
word02WriteSys:
        LDY #4                  ; Copy the 5 byte time value
word02WriteSysLoop:             ; to the Via Time
        LDA TubeCtrl, Y
        STA ViaTime, Y
        DEY
        BPL word02WriteSysLoop
        BMI wordSendBlock

;;; OSWORD A=5 Read Host Memory
word05ReadMem:
        LDY #0
        LDA (TubeCtrl), Y       ; Address in bytes 0,1 of control block
        LDY #4
        STA TubeCtrl, Y         ; Store result in byte 4 of control block
        JMP wordSendBlock

;;; OSWORD A=6 Write Host Memory
word06WriteMem:
        LDY #4
        LDA TubeCtrl, Y
        LDY #0
        STA (TubeCtrl), Y
        JMP wordSendBlock

;;; OSWORD A=7F 8271 Command based disk access
word7Fdisk:
        JSR osword7f
        LDX #$10
        BNE wordSendBlock

;;; OSWORD A=FF Host/Parasite data transfer
wordFFtransfer:
        LDX #<TubeCtrl
        LDY #>TubeCtrl
        JSR oswordff
        LDX #$01
        BNE wordSendBlock

;;; OSCLI
;;; =====
clii:

        JSR ReadString          ; Read string to $0100

        LDX #$FF
cliskip:
        INX
        LDA $100, X             ; Skip leading spaces or stars
        CMP #$20
        BEQ cliskip
        CMP #$2A
        BEQ cliskip

        CMP #$0D                ; Test for a zero-length string
        BEQ TubeSendAck         ; Skip it

        LDY #0
clicopy:
        LDA $100, X
        STA $100, Y
        INX
        INY
        CMP #$0D
        BNE clicopy

        JSR OSCLI               ; Execute it

        ;; If the command returns here, the CoPro will get $7F as an acknowledgement
        ;; The CoPro also gets sent a $7F byte if there is no language available on
        ;; Break If calling OSCLI results in code being run in the CoPro or a language
        ;; being copied over and entered, the CoPro will get an $80 acknowledgement
        ;; elsewhere

        ;; Will have to hook into Atom's OSLOAD and OSSAVE to intercept *DELETEACS and *DELETEBGET
        ;; commands to redirect to manual implementations

;;; Send $7F acknowledgement byte via R2 and return to idle loop
;;; ------------------------------------------------------------
TubeSendAck:
        LDA #$7F                ; Send $7F to CoPro

;;; Send byte in A via R2 and return to Tube idle loop
;;; --------------------------------------------------
TubeSendIdle:
        BIT TubeS2
        BVC TubeSendIdle        ; Loop until Command port free
        STA TubeR2
.if (debug_r2 = 1)
        JSR DebugHexOut
.endif
        JMP TubeIdle            ; Send byte and jump to Tube idle loop


;;; FILING LOADATNTEM CALLS
;;; ===================

;;; OSBPUT
;;; ------
bput:
        JSR TubeWaitR2
        TAY                     ; Wait for a handle
        JSR TubeWaitR2          ; Wait for a data byte
        JSR OSBPUT              ; Write to file
        JMP TubeSendAck         ; Send $7F ack, return to idle loop

;;; OSBGET
;;; ------
bget:
        JSR TubeWaitR2
        TAY                     ; Wait for a handle
        JSR OSBGET              ; Read from file
        JMP SendCarryA          ; Send Carry and A, return to idle loop

;;; OSFIND
;;; ------
find:
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
        JSR OSFIND              ; Do the OPEN
        JMP TubeSendIdle        ; Send handle back, return to idle loop

;;; CLOSE
;;; -----
close:
        JSR TubeWaitR2
        TAY                     ; Wait for a handle
        JSR OSSHUT              ; Do the CLOSE
        JMP TubeSendAck         ; Send $7F ack and jump to idle loop

;;; OSARGS
;;; ------
args:
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
;;; OSGBPB   R2 <== &16 block A
;;;          R2 ==> block Cy A

gbpb:
        LDX #$0D                ; Block length
        JSR TubeWaitBlock
        JSR TubeWaitR2          ; Get A
        PHA
        LDX #$0D                ; Block length
        JSR TubeSendBlock       ; Send block
        LDA #$00
        JSR TubeSendR2          ; Send Cy
        PLA
        JSR TubeSendR2          ; Send A
        JMP TubeIdle


;;; OSFILE
;;; ------
;;; OSFILE   R2 <== &14 block string &0D A
;;;          R2 ==> A block

;;; Block
;;;     0..3 Load Address
;;;     4..7 Exec Address
;;;     8..B Start Address
;;;     C..F End Address

file:
        LDX #$10                ; Block length
        JSR TubeWaitBlock
        LDX #$00
fileString:
        JSR TubeWaitR2          ; Get String
        STA AtomStr, X
        INX
        CMP #$0D
        BNE fileString
        JSR TubeWaitR2          ; Get A
        CMP #$00
        BEQ filesave
        CMP #$FF
        BEQ fileload

fileResponse:
        LDA #$01                ; Send object type 01 "File Found"
        JSR TubeSendR2
        LDX #$10                ; Block length
        JSR TubeSendBlock       ; Send block
        JMP TubeIdle

        ;; Map to OSLOAD
        ;;
        ;; Entry: 0,X = LSB File name string address
        ;;        1,X = MSB File name string address
        ;;        2,X = LSB Data dump start address
        ;;        3,X = MSB Data dump start address
        ;;        4,X : If bit 7 is clear, then the file's own start address is to be used

fileload:
        JSR fileinit
        LDA #$80                ; use block's load address
        LDX TubeCtrl + 4        ; if zero, use block's load address
        BEQ fileload1
        LDA #$00                ; use file's load address
fileload1:
        STA AtomCmd + 4
        LDX #AtomCmd
        JSR OSLOAD
        JMP fileResponse

        ;; Map to OSSAVE
        ;;
        ;; Entry: 0,X = LSB File name string address
        ;;        1,X = MSB File name string address
        ;;        2,X = LSB Data Reload address
        ;;        3,X = MSB Data Reload address
        ;;        4,X = LSB Data Execution address
        ;;        5,X = MSB Data Execution address
        ;;        6,X = LSB Data start address
        ;;        7,X = MSB Data start address
        ;;        8,X = LSB Data end address + 1
        ;;        9,X = MSB Data end address + 1

filesave:
        JSR fileinit
        LDX #AtomCmd
        JSR OSSAVE
        JMP fileResponse

fileinit:
        LDA #<AtomStr
        STA AtomCmd
        LDA #>AtomStr
        STA AtomCmd + 1
        LDX #$00
        LDY #$02
fileinitlp:
        LDA TubeCtrl, X         ; Copy bits 0..7 of address
        STA AtomCmd, Y
        INX
        INY
        LDA TubeCtrl, X         ; Copy bits 8..15 of address
        STA AtomCmd, Y
        INX
        INY
        INX                     ; Skip bits 16..23 of address
        INX                     ; Skip bits 24..31 of address
        CPX #$10
        BNE fileinitlp
        RTS

;;; TUBE COMMAND TRANSFERS
;;; **********************

;;; Read a string via R2 into string buffer at $0100
;;; ------------------------------------------------
ReadString:
        LDY #$00
ReadStrLp:
        JSR TubeWaitR2
        STA $0100,Y             ; Wait for byte and store in string buffer
        INY
        BEQ ReadStrFull         ; Buffer full, end loop
        CMP #$0D
        BNE ReadStrLp           ; Loop until <cr> received
ReadStrFull:
        LDX #0
        LDY #1
        RTS                     ; Return XY pointing to $0100

;;; Get control block to TubeCtrl, X
;;; --------------------------------
TubeWaitBlock:
        DEX
        BMI TubeWaitBlockExit
        JSR TubeWaitR2
        STA TubeCtrl, X
        JMP TubeWaitBlock
TubeWaitBlockExit:
        RTS

;;; Get X and A from Tube R2
;;; ------------------------
TubeWaitXA:
        JSR TubeWaitR2
        TAX

;;; Wait for data from Tube R2
;;; --------------------------
TubeWaitR2:
        BIT TubeS2
        BPL TubeWaitR2          ; Loop until data present
        LDA TubeR2
.if (debug_r2 = 1)
        JSR DebugHexOut
.endif
        RTS                     ; Get byte


;;; Send control block from TubeCtrl, X
;;; -----------------------------------
TubeSendBlock:
        DEX
        BMI TubeSendBlockExit
        LDA TubeCtrl, X
        JSR TubeSendR2
        JMP TubeSendBlock
TubeSendBlockExit:
        RTS

;;; Send byte in A via Tube R1
;;; --------------------------
TubeSendR1:
        BIT TubeS1
        BVC TubeSendR1          ; Loop until port free
        STA TubeR1
.if (debug_r1 = 1)
        JSR DebugHexOut
.endif
        RTS

;;; Send byte in A via Tube R2
;;; --------------------------
TubeSendR2:
        BIT TubeS2
        BVC TubeSendR2          ; Loop until port free
        STA TubeR2
.if (debug_r2 = 1)
        JSR DebugHexOut
.endif
        RTS


;;; Send byte in A via Tube R4
;;; --------------------------
TubeSendR4:
        BIT TubeS4
        BVC TubeSendR4          ; Loop until port free
        STA TubeR4
.if (debug_r4 = 1)
        JSR DebugHexOut
.endif
        RTS                     ; Send byte

;;; Copy Escape state across Tube
;;; -----------------------------

EscapeSet:
        LDA EscapeFlag
        ORA #$40
        BNE EscapeUpdate

EscapeClear:
        LDA EscapeFlag
        AND #$BF

EscapeUpdate:
        STA EscapeFlag

EscapeCopy:
        LDA EscapeFlag
        AND #$40                ; Bit 6 is the escape state
        ORA #$80                ; Bit 7 must be set
        JMP TubeSendR1


EscapeCheck:
        LDA $B001               ; Read keyboard hardware
        EOR EscapeFlag
        AND #$20
        BNE EscapeReturn        ; No Change

        LDA EscapeFlag          ; Update the escape key state in B5
        EOR #$20
        STA EscapeFlag

        SEC
        AND #$20                ; Is the change the key being pressed?
        BNE EscapeSet           ; If so, set the escape condition

EscapeReturn:
        CLC
        RTS


TubeHostError:
        .byte "HOST ERROR"
        BRK

;;; ***************************
;;; INTERFACE TO ATOM MOS CALLS
;;; ***************************

;;; Interface to Atom OSRDCH
;;; ------------------------
AtomRDCH:
        LDA EscapeFlag
        AND #$DF
        STA EscapeFlag
.if (buffered_kbd = 1)
AtomRDCH1:
        JSR PollKeyboard
        LDA KeyBuf              ; wait for the ISR to deposit a key press
        AND #$7F
        BEQ AtomRDCH1
        PHA
        LDA KeyBuf
        AND #$80                ; swallow the key press
        STA KeyBuf
.else
        JSR OSRDCH
        PHA
.endif
        JSR EscapeCheck
        PLA
        RTS



.if (buffered_kbd = 1)
;;; Polls the keyboard in a non-blocking fashion
;;; On return KeyBuf will return the key pressed, or zero if no key pressed
;;; If the caller consumes the key press, then they should zero KeyBuf
;;;

PollKeyboard:
        PHA
        TXA
        PHA
        TYA
        PHA
        JSR $FE71               ; scan the keyboard

        LDA KeyBuf
        BPL PollFlagClear

        BCC PollExit            ; key still pressed

        AND #$7F
        STA KeyBuf             ; update flag to indicate key released

PollFlagClear:
        BCS PollExit            ; no key pressed

        LDA KeyBuf              ; is the keyboard buffer already full
        BNE PollExit

        JSR ConvertKey          ; Convert to ASCII
        ORA #$80                ; Set the KeyFlag
        STA KeyBuf              ; store in the one character keyboard buffer

PollExit:
        PLA
        TAY
        PLA
        TAX
        PLA
        RTS

ConvertKey:
        CPY #$05                ; Lock key
        BEQ ConvertLock
        CPY #$06                ; LR Cursor
        BEQ ConvertCursor
        CPY #$07                ; UD Cursor
        BEQ ConvertCursor
        PHP                     ; To maintain a balances stack
        JMP $FEB1               ; Allow the AtomOS to handle the conversion

ConvertLock:
        LDA $E7                 ; Get the lock flag
        EOR #$60                ; ..toggle it
        STA $E7                 ; ..and restore it
        LDA #$00                ; don't return an actual keypress
        RTS

ConvertCursor:
        TYA
        AND #$05                ; LR=4 DU=5
        ROL $B001               ; C=0 if shift pressed
        ROL A                   ; L=8 R=9 D=10 U=11
        JSR AtomWRCH
        LDA #$00                ; don't return an actual keypress
        RTS

.endif



;;; Interface to Atom OSWRCH
;;; ------------------------
AtomWRCH:
        BIT GodilFlag           ; Check if running in 80x40 mode
        BMI AtomWRCH1
        CMP #$60                ; Mask lower case
        BCC AtomWRCH1
        AND #$DF
AtomWRCH1:
        JMP OSWRCH

ViaInit:
        LDA #<ViaISR            ; Setup the interrupt handler
        STA IRQ1V
        LDA #>ViaISR
        STA IRQ1V+1
        LDA #$00                ; Clear the timer
        STA ViaTime
        STA ViaTime + 1
        STA ViaTime + 2
        STA ViaTime + 3
        STA ViaTime + 4
        LDA #<9999              ; 10ms timer interrupts
        STA ViaT1CounterL
        LDA #>9999
        STA ViaT1CounterH
        LDA #$40                ; Enable T1 continuous interrupts
        STA ViaACR              ; Disable everything else
        LDA #$7F                ; Disable all interrupts
        STA ViaIER
        LDA #$C0                ; Enable T1 interrupts
        STA ViaIER
        RTS

ViaISR:
        LDA ViaT1CounterL       ; Clear the interrupts flag
        INC ViaTime
        BNE ViaExit
        INC ViaTime + 1
        BNE ViaExit
        INC ViaTime + 2
        BNE ViaExit
        INC ViaTime + 3
        BNE ViaExit
        INC ViaTime + 4

ViaExit:
        PLA                     ; the Atom stacks A for us
        RTI

ViaTime:
        .byte 0,0,0,0,0


;;; Debugging output, avoid trashing A
;;;

DebugNewline:
        PHP
        PHA
        JSR OSNEWL
        PLA
        PLP
        RTS

DebugHexOut:
        PHP
        PHA
        JSR HEXOUT
        PLA
        PLP
        RTS

.if (atommc = 1)
.include "tube.asm"
.include "file.asm"
.include "load.asm"
.include "save.asm"
.endif

.include "util.asm"
.include "osword7f.asm"
.include "oswordff.asm"

EndAddr:
