Matchbox CoPro
==============

A family of designs for the Matchbox Co Pro for the BBC Micro.

For more details on the designs, see:
http://stardot.org.uk/forums/viewtopic.php?t=8852&f=44

For Jason's LX9 Hardware, see:
http://stardot.org.uk/forums/viewtopic.php?t=8932&f=8

DIP Switch Settings:
0 0 0 0 -   4MHz 65C102 ( 64KB internal RAM,   AlanD core)
0 0 0 1 -   8MHz 65C102 ( 64KB internal RAM,   AlanD core)
0 0 1 0 -  16MHz 65C102 ( 64KB internal RAM,   AlanD core)
0 0 1 1 -  32MHz 65C102 ( 64KB internal RAM,   AlanD core)
0 1 0 0 -   8MHz Z80    ( 64KB external RAM,     T80 core)
0 1 0 1 -  32MHz Z80    ( 64KB internal RAM, NextZ80 core)
0 1 1 0 -  56MHz Z80    ( 64KB internal RAM, NextZ80 core)
0 1 1 1 - 112MHz Z80    ( 64KB internal RAM, NextZ80 core)
1 0 0 0 -  16Mhz 80286  (896KB external RAM,     Zet core)  
1 0 0 1 -   4MHz 6809   ( 64KB external RAM,   SYS09 core) 
1 0 1 0 -  16MHz 68000  (  1MB external RAM,    TG68 core)
1 0 1 1 -  32MHz PDP11  ( 64KB internal RAM, PDP2011 core)
1 1 0 0 -  32MHz ARM2   (  2MB external RAM, Amber23 core)
1 1 0 1 -  32MHz 32016  (  2MB external RAM,  m32632 core)
1 1 1 0 -   Null / SPI  (          Raspberry Pi soft core)
1 1 1 1 -   BIST        ( for manufacturing test purposes)
