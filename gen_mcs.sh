#!/bin/bash
. /opt/Xilinx/14.7/ISE_DS/settings64.sh

# The S25FL032P has space for ~12 designs if they are uncompressed
#
# multiboot loader - 0x000000 - working/ICAP_reboot.bit
# design 0000-0011 - 0x054000 - working/LX9CoPro6502fast.bit (4/8/16/32MHz)
# design 0100      - 0x0a8000 - working/LX9CoProZ80.bit      (8MHz)
# design 0101-0111 - 0x0fc000 - working/LX9CoProZ80fast.bit (16/56/112MHz)
# design 1000      - 0x150000 - working/LX9CoPro80186.bit
# design 1001      - 0x1a4000 - working/LX9CoPro6809.bit
# design 1010      - 0x1f8000 - working/LX9CoPro68000.bit
# design 1011      - 0x24c000 - working/LX9CoProPDP11.bit
# design 1100      - 0x2a0000 - working/LX9CoProARM2.bit
# design 1101      - 0x2f4000 - working/LX9CoPro32016.bit
# design 1110      - 0x348000 - working/LX9CoProSPI.bit
# design 1111      - 0x39c000 - working/LX9Co_BIST.bit

NAME=multiboot/LX9CoProCombined_$(date +"%Y%m%d_%H%M")_$USER

mkdir -p multiboot

promgen                                 \
-u      0 working/ICAP_reboot.bit       \
-u  54000 working/LX9CoPro6502fast.bit  \
-u  A8000 working/LX9CoProZ80.bit       \
-u  FC000 working/LX9CoProZ80fast.bit   \
-u 150000 working/LX9CoPro80186.bit     \
-u 1A4000 working/LX9CoPro6809.bit      \
-u 1F8000 working/LX9CoPro68000.bit     \
-u 24C000 working/LX9CoProPDP11.bit     \
-u 2A0000 working/LX9CoProARM2.bit      \
-u 2F4000 working/LX9CoPro32016.bit     \
-u 348000 working/LX9CoProSPI.bit       \
-u 39C000 working/LX9Co_BIST.bit        \
-o $NAME.mcs  -p mcs -w -spi -s 8192

rm -f $NAME.cfi $NAME.prm
