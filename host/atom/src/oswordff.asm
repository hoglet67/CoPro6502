	ctrl  = $80
        saveA = $82
	addr  = $84
	count = $86

oswordff:
        STX ctrl+0
        STY ctrl+1
        STA saveA	; Save OSWORD parameters

.if (debug_osw_ff = 1)
	LDY #$00
oswordffdebug:
        LDA (ctrl),Y
	JSR DebugHexOut
	INY
	CPY #$0D
	BNE oswordffdebug
	JSR DebugNewline
.endif
	
        LDY #$02
        LDA (ctrl),Y
        STA addr+0	; I/O address low byte
        INY
        LDA (ctrl),Y
        STA addr+1      ; I/O address high byte
        JSR L259C       ; Claim the Tube
        LDY #$0C
        LDA (ctrl),Y
        PHA		; Get read/write command
        LDA ctrl+0
        CLC
        ADC #$06
        TAX		; Point to Control+6
        LDA #$00
        ADC ctrl+1
        TAY		; XY->CoPro address in control block
        PLA
        PHA
        JSR L0406       ; Initiate specified action
        LDY #$0A
        LDA (ctrl),Y
        TAX		; Get count low byte
        INY
        LDA (ctrl),Y
        STA count       ; Get count high byte
        BNE L2544       ; Jump forward if >255 bytes to do
        TXA
        BEQ L2592       ; Jump to exit if no bytes left
L2544:
        TXA
        BEQ L2549       ; Jump forward if multiple of 256 bytes
        INC count       ; Inc high byte to balance DECs later
L2549:
        PLA
        ROR A
        BCS L2575       ; Get command back, jump if H->C

        JSR L259B
        JSR L259B
        JSR L259B	; Delay before starting
        LDY #$00        ; Zero offset for (zp),Y
L2558:
        LDA TubeR3
        STA (addr),Y    ; Transfer a byte C->H
        JSR L259B
        JSR L259B
        JSR L259B	; Delay between bytes
        INC addr+0
        BNE L256C
        INC addr+1	; Update I/O address
L256C:
        DEX
        BNE L2558       ; Loop for up to 256 bytes
        DEC count
        BNE L2558       ; Loop for each 256-byte chunk
        BEQ L2592       ; Jump to exit when finished

L2575:
        LDY #$00
L2577:
        LDA (addr),Y
        STA TubeR3       ; Transfer byte H->C
        JSR L259B
        JSR L259B
        JSR L259B       ; Delay between bytes
        INC addr+0
        BNE L258B
        INC addr+1      ; Update I/O addreL258B:
L258B:
        DEX
        BNE L2577       ; Loop for up to 256 bytes
        DEC count
        BNE L2577       ; Loop for each 256-byte chunk

L2592:
        JSR L25A4       ; Release Tube
        LDX ctrl+0
        LDY ctrl+1
        LDA saveA       ; Restore entry registers
L259B:                  ; Call here to delay 6us:
        RTS             ; And return

L259C:
        LDA #$C0+7
        JSR L0406       ; Claim with ID=7
        BCC L259C
        RTS             ; Loop until claimed

L25A4:
        LDA #$80+7
        JSR L0406       ; Release with ID=7
        RTS
