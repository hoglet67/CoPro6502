Matchbox CoPro
==============

A family of designs for the Matchbox Co Pro for the BBC Micro.

For more details on the designs, see:
http://stardot.org.uk/forums/viewtopic.php?t=8852&f=44

For Jason's LX9 Hardware, see:
http://stardot.org.uk/forums/viewtopic.php?t=8932&f=8

DIP Switch Settings:
0 0 0 0 -  4MHz 65C102 (512KB external RAM using Jason's paging extensions)
0 0 0 1 -  8MHz Z80    ( 64KB external RAM)
0 0 1 0 -  4MHz 6809   ( 64KB external RAM)
0 0 1 1 - 16Mhz 80x86  (512KB external RAM)  
0 1 0 0 - BIST
0 1 0 1 - unused
0 1 1 0 - unused
0 1 1 1 - unused
1 0 0 0 - 32MHz 65C102 (64KB internal RAM, boot message shows speed)
1 0 0 1 - 16MHz 65C102 (64KB internal RAM, boot message shows speed)
1 0 1 0 -  8MHz 65C102 (64KB internal RAM, boot message shows speed)
1 0 1 1 -  4MHz 65C102 (64KB internal RAM, boot message shows speed)
1 1 0 0 - 112 MHz Z80    (64KB internal RAM, now using NextZ80 core)
1 1 0 1 -  56 MHz Z80    (64KB internal RAM, now using NextZ80 core)
1 1 1 0 -  32 MHz Z80    (64KB internal RAM, now using NextZ80 core)
1 1 1 1 -  16 MHz Z80    (64KB internal RAM, now using NextZ80 core)
