;;; 	TubeR3       = $BEE6
;;; 	TubeCtrl     = $60
;;; 	L0406        = $4006		; Tube claim/transfer/release
;;; 	L0409        = $4009		; Tube error	

;;;     TubeFlag     = $3CF		; tube enabled flag, set by atom tube host
;;;     TubeEna      = $5A		; tube enable magic value
        TubeClientId = $DD		; client ID for AtoMMC2 used in tube protocol
		
;;; Tube Handling

;;; tube_claim_wrapper
;;; 
;;; Check if tube enabled, and if so claim and setup data transfer
;;; X = where to read transfer address, in zero page
;;; Y = transfer type
;;;     00 = parasite to host (i.e. save)
;;;     01 = host to parasite (i.e. load)
;;; 
tube_claim_wrapper:

        ;; Check if the Tube has been enabled
        LDA TubeFlag
        CMP #TubeEna
        BNE tube_disabled

        ;; Claim Tube
        LDA #TubeClientId
        JSR L0406
        
        ;; Setup Data Transfer
        LDA 0, X
        STA TubeCtrl
        LDA 1, X
        STA TubeCtrl + 1
        LDA #$00
        STA TubeCtrl + 2
        STA TubeCtrl + 3
		TYA
        LDX #<TubeCtrl
        LDY #>TubeCtrl
        JMP L0406

tube_release_wrapper:
        ;; Check if the Tube has been enabled
        LDA TubeFlag
        CMP #TubeEna
        BNE tube_disabled

        ;; Release Tube
        LDA #TubeClientId - $40
        JMP L0406

tube_disabled:
        RTS

;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~
;
; Read a block of data from file to the Tube
;
; a = number of bytes to read (0 = 256)
;
tube_read_block:
        LDX TubeFlag
        CPX #TubeEna
        BEQ @tube_enabled
        JMP read_block              	; Fall back to old code if tube is disabled        

@tube_enabled:
	JSR read_block_shared
@loop:
        readportFAST AREAD_DATA_REG 	; then read it
        STA  TubeR3                 	; write to the tube data transfer register
        DEX
        BNE  @loop
        RTS

    
;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~
;
; write a block of data from the Tube to a file
;
; a = block length (0=256)
;
tube_write_block:
        LDX TubeFlag
        CPX #TubeEna
        BEQ @tube_enabled
        JMP write_block                 ; Fall back to old code if tube is disabled        

@tube_enabled:  
        TAX                     		; save away the block size
        PHA
        JSR prepare_write_data  		; take it
@loop:
        LDA TubeR3						; read data from the tube data transfer register
        writeportFAST AWRITE_DATA_REG
        DEX
        BNE @loop
	JMP write_block_shared
