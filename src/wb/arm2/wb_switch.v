/*
 *  Wishbone switch and address decoder
 *  Copyright (C) 2010  Zeus Gomez Marmolejo <zeus@aluzina.org>
 *  Copyright (C) 2008, 2009 Sebastien Bourdeauducq - http://lekernel.net
 *  Copyright (C) 2000 Johny Chi - chisuhua@yahoo.com.cn
 *
 *  This file is part of the Zet processor. This processor is free
 *  hardware; you can redistribute it and/or modify it under the terms of
 *  the GNU General Public License as published by the Free Software
 *  Foundation; either version 3, or (at your option) any later version.
 *
 *  Zet is distrubuted in the hope that it will be useful, but WITHOUT
 *  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
 *  License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with Zet; see the file COPYING. If not, see
 *  <http://www.gnu.org/licenses/>.
 */

module wb_switch #(
    parameter s0_addr_1 = 32'h00000000,
    parameter s0_mask_1 = 32'h00000000,
    parameter s0_addr_2 = 32'h00000000,
    parameter s0_mask_2 = 32'h00000000,
    parameter s1_addr_1 = 32'h00000000,
    parameter s1_mask_1 = 32'h00000000,
    parameter s2_addr_1 = 32'h00000000,
    parameter s2_mask_1 = 32'h00000000

  )(
    // Master interface
    input  [31:0] m_dat_i,
    output [31:0] m_dat_o,
    input  [31:0] m_adr_i,
    input  [ 3:0] m_sel_i,
    input         m_we_i,
    input         m_cyc_i,
    input         m_stb_i,
    output        m_ack_o,

    // Slave 0 interface
    input  [31:0] s0_dat_i,
    output [31:0] s0_dat_o,
    output [31:0] s0_adr_o,
    output [ 3:0] s0_sel_o,
    output        s0_we_o,
    output        s0_cyc_o,
    output        s0_stb_o,
    input         s0_ack_i,

    // Slave 1 interface
    input  [31:0] s1_dat_i,
    output [31:0] s1_dat_o,
    output [31:0] s1_adr_o,
    output [ 3:0] s1_sel_o,
    output        s1_we_o,
    output        s1_cyc_o,
    output        s1_stb_o,
    input         s1_ack_i,

    // Slave 2 interface
    input  [31:0] s2_dat_i,
    output [31:0] s2_dat_o,
    output [31:0] s2_adr_o,
    output [ 3:0] s2_sel_o,
    output        s2_we_o,
    output        s2_cyc_o,
    output        s2_stb_o,
    input         s2_ack_i,

    // Slave 3 interface - the default
    input  [31:0] s3_dat_i,
    output [31:0] s3_dat_o,
    output [31:0] s3_adr_o,
    output [ 3:0] s3_sel_o,
    output        s3_we_o,
    output        s3_cyc_o,
    output        s3_stb_o,
    input         s3_ack_i
    
  );

`define mbusw_ls  32 + 4 + 32 + 1 + 1 + 1  // address + byte select + data + cyc + we + stb

wire [3:0] slave_sel;
wire [31:0] i_dat_s;   // internal shared bus, slave data to master
wire        i_bus_ack; // internal shared bus, ack signal

wire [`mbusw_ls -1:0] i_bus_m;    // internal shared bus, master data and control to slave

assign m_dat_o = i_dat_s;
assign m_ack_o = i_bus_ack;

// Bus Acknowlegement
assign i_bus_ack =   s0_ack_i | s1_ack_i | s2_ack_i | s3_ack_i;

assign i_dat_s =   ({32{slave_sel[ 0]}} & s0_dat_i)
          |({32{slave_sel[ 1]}} & s1_dat_i)
          |({32{slave_sel[ 2]}} & s2_dat_i)
          |({32{slave_sel[ 3]}} & s3_dat_i)
        ;

// Bus Selection logic
assign slave_sel[0] =  ((m_adr_i & s0_mask_1) == s0_addr_1) | ((m_adr_i & s0_mask_2) == s0_addr_2);
assign slave_sel[1] = ~(|slave_sel[0]) & ((m_adr_i & s1_mask_1) == s1_addr_1);
assign slave_sel[2] = ~(|slave_sel[1:0]) & ((m_adr_i & s2_mask_1) == s2_addr_1);
assign slave_sel[3] = ~(|slave_sel[2:0]);

assign i_bus_m = {m_adr_i, m_sel_i, m_dat_i, m_we_i, m_cyc_i, m_stb_i};

assign {s0_adr_o, s0_sel_o, s0_dat_o, s0_we_o, s0_cyc_o}  = i_bus_m[`mbusw_ls -1:1];  // slave 0
assign  s0_stb_o = i_bus_m[1] & i_bus_m[0] & slave_sel[0];

assign {s1_adr_o, s1_sel_o, s1_dat_o, s1_we_o, s1_cyc_o} = i_bus_m[`mbusw_ls -1:1];    // slave 1
assign  s1_stb_o = i_bus_m[1] & i_bus_m[0] & slave_sel[1];

assign {s2_adr_o, s2_sel_o, s2_dat_o, s2_we_o, s2_cyc_o} = i_bus_m[`mbusw_ls -1:1];    // slave 2
assign  s2_stb_o = i_bus_m[1] & i_bus_m[0] & slave_sel[2];

assign {s3_adr_o, s3_sel_o, s3_dat_o, s3_we_o, s3_cyc_o} = i_bus_m[`mbusw_ls -1:1];    // slave 3
assign  s3_stb_o = i_bus_m[1] & i_bus_m[0] & slave_sel[3];

endmodule
