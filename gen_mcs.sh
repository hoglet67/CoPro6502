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
# design 10xx      - 0x1f8000 - working/LX9CoPro6502fast.bit (32/16/8/4MHz)
# design 11xx      - 0x24c000 - working/LX9CoProZ80fast.bit(36/24/12/8Mhz)
# design 0111      - 0x2a0000 - working/LX9CoProPDP11.bit
# design 0110      - 0x2f4000 - working/LX9CoProNull.bit
# design 0101      - 0x348000 - working/LX9CoPro68000.bit
# design ????      - 0x39c000 - spare

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
-u 24C000 working/LX9CoProZ80fast.bit   \
-u 2A0000 working/LX9CoProPDP11.bit     \
-u 2F4000 working/LX9CoProNull.bit      \
-u 348000 working/LX9CoPro68000.bit     \
-o $NAME.mcs  -p mcs -w -spi -s 8192

rm -f $NAME.cfi $NAME.prm

