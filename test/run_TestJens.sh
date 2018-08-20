#!/bin/bash

ghdl -a --ieee=synopsys ../src/CPU65C02/fsm_execution_unit.vhd
ghdl -a --ieee=synopsys ../src/CPU65C02/fsm_intnmi.vhd
ghdl -a --ieee=synopsys ../src/CPU65C02/regbank_axy.vhd
ghdl -a --ieee=synopsys ../src/CPU65C02/reg_pc.vhd
ghdl -a --ieee=synopsys ../src/CPU65C02/reg_sp.vhd
ghdl -a --ieee=synopsys ../src/CPU65C02/core.vhd
ghdl -a --ieee=synopsys ../src/CPU65C02/r65c02_tc.vhd
ghdl -a --ieee=synopsys TestJens.vhd

ghdl -e --ieee=synopsys TestJens

ghdl -r --ieee=synopsys TestJens --vcd=dump.vcd

gtkwave -a TestJens.gtkw dump.vcd
