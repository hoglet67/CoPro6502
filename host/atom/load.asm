;;; LODVEC entry point
;;;
;;; 0,x = file parameter block
;;;
;;; 0,x = file name string address
;;; 2,x = data dump start address
;;; 4,x  if bit 7 is clear, then the file's own start address is to be used
;;;
osloadtube:
	;; transfer control block to $c9 (LFNPTR) onward and check name
	;;
	JSR CHKNAME
        
        JSR open_file_read
        JSR read_info

	;; Claim the Tube
	LDA #$DD
	JSR L0406
	
	;; Setup Type 1 Tube Transfer
	LDA LLOAD
	STA TubeCtrl
	LDA LLOAD + 1
	STA TubeCtrl + 1
	LDA #$00
	STA TubeCtrl + 2
	STA TubeCtrl + 3
	LDA #$01
	LDX #<TubeCtrl
	LDY #>TubeCtrl
	JSR L0406
	
        BIT MONFLAG             ; 0 = mon, ff = nomon
        BMI @noprint

        JSR print_fileinfo
        JSR OSCRLF

@noprint:
	;; This writes the data to the Tube R3
	JSR read_file

	;; Release the Tube
	LDA #$9D
	JMP L0406
	
