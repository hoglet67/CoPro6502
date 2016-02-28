// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//
// This file is part of the M32632 project
// http://opencores.org/project,m32632
//
// Filename: example.v
// Version:  1.0
// Date:     30 May 2015
//
// Copyright (C) 2015 Udo Moeller
// 
// This source file may be used and distributed without 
// restriction provided that this copyright statement is not 
// removed from the file and that any derivative work contains 
// the original copyright notice and the associated disclaimer.
// 
// This source file is free software; you can redistribute it 
// and/or modify it under the terms of the GNU Lesser General 
// Public License as published by the Free Software Foundation;
// either version 2.1 of the License, or (at your option) any 
// later version. 
// 
// This source is distributed in the hope that it will be 
// useful, but WITHOUT ANY WARRANTY; without even the implied 
// warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
// PURPOSE. See the GNU Lesser General Public License for more 
// details. 
// 
// You should have received a copy of the GNU Lesser General 
// Public License along with this source; if not, download it 
// from http://www.opencores.org/lgpl.shtml 
// 
// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//
//	Modules contained in this file:
//	example		Your first system with the M32632 CPU
//
// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

module example ( CLK, RESET_N, NMI_N, INT_N, IN_REG, OUT_REG);

input			CLK;
input			RESET_N;
input			NMI_N;
input			INT_N;

input	 [7:0]	IN_REG;

output	 [7:0]	OUT_REG;

reg				nmi_reg,int_reg;

wire			IC_MDONE;
wire			DC_MDONE;
wire			ENWR;
wire			WAMUX;
wire	[11:2]	WADDR;
wire	[31:0]	DRAM_Q;
wire	 [2:0]	IWCTRL;
wire	 [2:0]	DWCTRL;
wire			IC_ACC;
wire	[27:0]	IDRAM_ADR;
wire			DC_ACC;
wire			DC_WR;
wire	[27:0]	DRAM_ADR;
wire	[35:0]	DRAM_DI;

wire	[31:0]	IO_A;
wire	[31:0]	IO_DI;
wire	 [3:0]	IO_BE;
wire			IO_RD;
wire			IO_WR;
wire			IO_READY;
wire			ENDRAM;
wire	[31:0]	IO_Q;
wire			W_OUT_REG;
wire			RST_N;

wire	[31:0]	BOOT_DAT;
wire	[31:0]	STAT_DAT;
wire	 [7:0]	IN_DAT;

wire	 [7:0]	STATSIGS;
wire			COP_GO;
wire	[23:0]	COP_OP;
wire   [127:0]	COP_OUT;
wire			COP_DONE;
wire	[63:0]	COP_IN;

M32632	CPU(
	// ++++++++++ Basic Signals
	.BCLK(CLK),
	.MCLK(~CLK),
	.WRCFG(1'b1),
	.BRESET(RST_N),
	.NMI_N(nmi_reg),
	.INT_N(int_reg),
	.STATUS(),
	.ILO(),
	.STATSIGS(STATSIGS),
	// +++++++++ General Purpose Interface
	.IO_WR(IO_WR),
	.IO_RD(IO_RD),
	.IO_A(IO_A),
	.IO_BE(IO_BE),
	.IO_DI(IO_DI),
	.IO_Q(IO_Q),
	.IO_READY(IO_READY),
	// +++++++++ DRAM Interface In
	.ENDRAM(ENDRAM),
	.IC_MDONE(IC_MDONE),
	.DC_MDONE(DC_MDONE),
	.ENWR(ENWR),
	.WAMUX(WAMUX),
	.WADDR(WADDR),
	.DRAM_Q(DRAM_Q),
	.DWCTRL(DWCTRL),
	.IWCTRL(IWCTRL),
	// +++++++++ DRAM Interface Out
	.IC_ACC(IC_ACC),
	.IDRAM_ADR(IDRAM_ADR),
	.DC_ACC(DC_ACC),
	.DC_WR(DC_WR),
	.DRAM_ADR(DRAM_ADR),
	.DRAM_DI(DRAM_DI),
	// ++++++++++ DMA Interface
	.HOLD(1'b1),
	.HLDA(),
	.FILLRAM(1'b0),
	.DMA_AA(24'd0),
	// ++++++++++ Coprocessor Interface
	.COP_GO(COP_GO),
	.COP_OP(COP_OP),
	.COP_OUT(COP_OUT),
	.COP_DONE(COP_DONE),
	.COP_IN(COP_IN));
	
ex_io_bus_ctrl u_bus_ctrl(
	.CLK(CLK),
	.RESET_N(RESET_N),
	.RST_N(RST_N),
	.ENDRAM(ENDRAM),
	.IO_WR(IO_WR),
	.IO_RD(IO_RD),
	.IO_A(IO_A[31:28]),
	.IO_BE(IO_BE),
	.IO_Q(IO_Q),
	.IO_READY(IO_READY),
	.W_OUT_REG(W_OUT_REG),
	.IN_DAT(IN_DAT),
	.BOOT_DAT(BOOT_DAT),
	.STAT_DAT(STAT_DAT));

ex_in_reg u_in_reg(
	.CLK(CLK),
	.IN_REG(IN_REG),
	.IN_DAT(IN_DAT));
	
ex_out_reg u_out_reg(
	.CLK(CLK),
	.OUT_REG(OUT_REG),
	.W_OUT_REG(W_OUT_REG),
	.DIN(IO_DI));
	
ex_boot_rom u_boot_rom(
	.CLK(CLK),
	.ADDR(IO_A[9:2]),
	.DATA(BOOT_DAT));

ex_statcou u_statcou(
	.CLK(CLK),
	.RST_N(RST_N),
	.STATSIGS(STATSIGS),
	.ADDR(IO_A[4:2]),
	.DATA(STAT_DAT));

ex_copro u_copro(
	.CLK(CLK),
	.COP_GO(COP_GO),
	.COP_OP(COP_OP),
	.COP_INP(COP_OUT),
	.COP_DONE(COP_DONE),
	.COP_OUTP(COP_IN));
	
ex_dram_emul u_dram_emul(
	.MCLK(CLK),
	.RST_N(RST_N),
	.IC_ACC(IC_ACC),
	.IDRAM_ADR(IDRAM_ADR),
	.DC_ACC(DC_ACC),
	.DC_WR(DC_WR),
	.DRAM_ADR(DRAM_ADR),
	.DRAM_DI(DRAM_DI),
	.IC_MDONE(IC_MDONE),
	.DC_MDONE(DC_MDONE),
	.ENWR(ENWR),
	.WAMUX(WAMUX),
	.WADDR(WADDR),
	.MEM_Q(DRAM_Q),
	.DWCTRL(DWCTRL),
	.IWCTRL(IWCTRL) );

	always @(posedge CLK)	// recommended to synchronize this signals
		begin
			nmi_reg <= NMI_N;
			int_reg <= INT_N;
		end

endmodule
