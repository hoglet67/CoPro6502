//**************************************************************************
//    ph_reg3.v - 2 byte FIFO for 16b transfers in parasite to host direction
//
//    COPYRIGHT 2019 David Banks, Richard Evans, Ed Spittles
//
//    This file is part of tube - an Acorn Tube ULA compatible system.
//
//    tube is free software: you can redistribute it and/or modify
//    it under the terms of the GNU Lesser General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    tube is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU Lesser General Public License for more details.
//
//    You should have received a copy of the GNU Lesser General Public License
//    along with tube.  If not, see <http://www.gnu.org/licenses/>.
//
// ============================================================================
`timescale 1ns / 1ns

module bin_gray_counter #
  (
   parameter N = 0,     // Width of counters
   parameter INIT = 0   // Initial value of binary counter
   )
   (
    input              clk,
    input              rst,
    input              inc,
    output reg [N-1:0] binary,
    output reg [N-1:0] gray
    );

   wire [N-1:0]        next_binary = binary + 1'b1;
   wire [N-1:0]        next_gray = next_binary ^ (next_binary >> 1);

   always @(posedge clk or posedge rst) begin
      if (rst) begin
         binary <= INIT;
         gray <= INIT ^ (INIT >> 1);
      end else if (inc) begin
         binary <= next_binary;
         gray <= next_gray;
      end
   end

endmodule

module async_fifo #
  (
   parameter D_WIDTH = 0,    // FIFO data width
   parameter A_WIDTH = 0,    // Log(2) of the FIFO depth
   parameter INIT_WADDR = 0, // Inital write address
   parameter INIT_RADDR = 0  // Initial read addrtess
   )
   (
    input        rst,      // asynchrous reset
    input        wr_clk,   // write clock
    input        wr_en,    // write enable
    input [7:0]  wr_data,  // write data
    input        rd_clk,   // read clock
    input        rd_en,    // read enable
    output [7:0] rd_data,  // read data (value at the HEAD of the FIFO)
    output       rd_empty, // empty flag, synchronised to the read domain
    output       rd_full,  // full flag, synchronised to the read domain
    output       wr_empty, // empty flag, synchronised to the write domain
    output       wr_full   // full flag, synchronised to the write domain
    );

   // The read and write addresses are N+1 bits (where 2^N is the FIFO size)
   // The purpose of the additional bit is to distingish the full/empty cases

   // Write address
   wire [A_WIDTH:0] waddr;     // binary encoded
   wire [A_WIDTH:0] waddr_g;   // gray coded
   reg [A_WIDTH:0]  waddr_g1;  // gray coded, part synchronised to read domain
   reg [A_WIDTH:0]  waddr_g2;  // gray coded, fully synchronised to read domain

   // Read address
   wire [A_WIDTH:0] raddr;     // binary encoded
   wire [A_WIDTH:0] raddr_g;   // gray coded
   reg [A_WIDTH:0]  raddr_g1;  // gray coded, part synchronised to write domain
   reg [A_WIDTH:0]  raddr_g2;  // gray coded, fully synchronised to write domain

   // FIFO Data RAM
   reg [D_WIDTH-1:0] data[0:2^A_WIDTH-1];

   // Counter blocks for write address
   // - binary-coded output used for RAM write address
   // - gray-coded output used for flag logic
   bin_gray_counter #
     (
      .N(A_WIDTH+1),
      .INIT(INIT_WADDR)
      )
   waddr_counter
     (
      .clk(wr_clk),
      .rst(rst),
      .inc(wr_en && !wr_full),
      .binary(waddr),
      .gray(waddr_g)
      );

   // Counter blocks for read address
   // - binary-coded output used for RAM read address
   // - gray-coded output used for flag logic
   bin_gray_counter #
     (
      .N(A_WIDTH+1),
      .INIT(INIT_RADDR)
      )
   addr_counter
     (
      .clk(rd_clk),
      .rst(rst),
      .inc(rd_en && !rd_empty),
      .binary(raddr),
      .gray(raddr_g)
      );

   // Synchronise the gray-coded read address to the write clock domain
   always @(posedge wr_clk) begin
      raddr_g1 <= raddr_g;
      raddr_g2 <= raddr_g1;
   end

   // Synchronise the gray-coded write address to the read clock domain
   always @(posedge rd_clk) begin
      waddr_g1 <= waddr_g;
      waddr_g2 <= waddr_g1;
   end

   // Write logic
   always @(posedge wr_clk) begin
      if (wr_en && !wr_full) begin
         data[waddr[A_WIDTH-1:0]] <= wr_data;
      end
   end

   // Read logic
   assign rd_data = data[raddr[A_WIDTH-1:0]];

   // Full/Empty flags are generated from the gray coded addresses.
   //
   // The wr_ prefixed versions are valid in the write clock domain.
   // The rd_ prefixed versions are valid in the read clock domain.
   //
   // The addresses contain one extra bit to distinguish the full and empty cases.
   //
   // If the pointers match exacty, then the FIFO is empty.
   //
   // If they match, apart from the MS two bits, then the FIFO is full. I've
   // not seem this formulation used before. But by inspection it seems to be
   // correct. Caveat Emptor!
   assign wr_empty = (waddr_g ^ raddr_g2) == 0;
   assign rd_empty = (raddr_g ^ waddr_g2) == 0;
   assign wr_full  = (waddr_g ^ raddr_g2) == 3 << (A_WIDTH-1);
   assign rd_full  = (raddr_g ^ waddr_g2) == 3 << (A_WIDTH-1);

endmodule

module ph_reg3
  (
   input        h_rst_b,
   input        h_rd,
   input        h_selectData,
   input        h_phi2,

   input [7:0]  p_data,
   input        p_selectData,
   input        p_phi2,
   input        p_rdnw,
   input        one_byte_mode,
   output [7:0] h_data,
   output       h_data_available,
   output       p_empty,
   output       p_full
   );

   // Internal flags
   wire         rd_empty;    // empty flag, synchronised to the read domain
   wire         rd_full;     // full flag, synchronised to the read domain
   wire         wr_empty;    // empty flag, synchronised to the write domain
   wire         wr_full;     // full flag, synchronised to the write domain

   async_fifo #
     (
      .D_WIDTH(8),
      .A_WIDTH(1),
      .INIT_WADDR(1),
      .INIT_RADDR(0)
      )
   ph_reg3_fifo
     (
      .rst(!h_rst_b),
      .wr_clk(p_phi2),
      .wr_en(p_selectData && !p_rdnw),
      .wr_data(p_data),
      .rd_clk(!h_phi2),
      .rd_en(h_selectData && h_rd),
      .rd_data(h_data),
      .rd_empty(rd_empty),
      .rd_full(rd_full),
      .wr_empty(wr_empty),
      .wr_full(wr_full)
      );

   // Register 3 is intended to enable high speed transfers of large blocks of data across the tube.
   // It can operate in one or two byte mode, depending on the V flag. In one byte mode the status
   // bits make each FIFO appear to be a single byte latch - after one byte is written the register
   // appears to be full. In two byte mode the data available flag will only be asserted when two bytes have
   // been entered, and the not full flag will only be asserted when both bytes have been removed. Thus data
   // available going active means that two bytes are available, but it will remain active until both bytes
   // have been removed. Not full going active means that the register is empty, but it will remain active
   // until both bytes have been entered. PNMI, N and DRQ also remain active until the full two
   // byte operation is completed

   assign p_empty          = wr_empty;
   assign p_full           = one_byte_mode ? wr_full   : !wr_empty;
   assign h_data_available = one_byte_mode ? !rd_empty : rd_full;

endmodule
