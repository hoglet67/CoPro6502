; LODVEC entry point
;
; 0,x = file parameter block
;
; 0,x = file name string address
; 2,x = data dump start address
; 4,x  if bit 7 is clear, then the file's own start address is to be used
;
osloadcode:
    ; transfer control block to $c9 (LFNPTR) onward and check name
    ;
    jsr  CHKNAME

	jsr	open_file_read
    jsr  read_info

    bit  MONFLAG             ; 0 = mon, ff = nomon
    bmi  @noprint

    jsr  print_fileinfo
    jsr  OSCRLF

@noprint:
    jmp read_file
