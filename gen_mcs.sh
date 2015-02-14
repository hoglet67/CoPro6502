#!/bin/bash
. /opt/Xilinx/14.7/ISE_DS/settings*.sh

# The S25FL032P has space for ~12 designs if they are uncompressed
#
# multiboot loader - 0x000000 - working/ICAP_reboot.bit
# design 0000      - 0x054000 - working/LX9CoPro6502.bit
# design 0001      - 0x0a8000 - working/LX9CoProZ80.bit
# design 0010      - 0x0fc000 - working/LX9CoPro6809.bit
# design 0011      - 0x150000 - working/LX9CoPro80186.bit
# design 0100      - 0x1a4000 - working/LX9Co_BIST.bit
# design 1000      - 0x1f8000 - working/LX9CoPro6502fast.bit (32MHz)
# design 1001      - 0x24c000 - spare
# design 1010      - 0x2a0000 - spare
# design 1011      - 0x2f4000 - spare
# design 1011      - 0x348000 - spare
# design xxxx      - 0x39c000 - spare

NAME=multiboot/LX9CoProCombined_$(date +"%Y%m%d_%H%M")_$USER

mkdir -p multiboot

promgen                                 \
-u      0 working/ICAP_reboot.bit       \
-u  54000 working/LX9CoPro6502.bit      \
-u  A8000 working/LX9CoProZ80.bit       \
-u  FC000 working/LX9CoPro6809.bit      \
-u 150000 working/LX9CoPro80186.bit     \
-u 1A4000 working/LX9Co_BIST.bit        \
-u 1F8000 working/LX9CoPro6502fast.bit  \
-o $NAME.mcs  -p mcs -w -spi -s 8192

rm -f $NAME.cfi $NAME.prn

