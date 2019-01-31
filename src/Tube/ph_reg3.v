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

module ph_reg3 #
  (
   parameter INIT_WADDR = 1,
   parameter INIT_RADDR = 0,
   parameter INIT_DATA0 = 8'haa,
   parameter INIT_DATA1 = 8'hee
   )
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

   // Flags
   wire          rempty;    // empty flag, synchronised to the read domain
   wire          rfull;     // full flag, synchronised to the read domain
   wire          wempty;    // empty flag, synchronised to the write domain
   wire          wfull;     // full flag, synchronised to the write domain

   // The read and write addresses are N+1 bits (where 2^N is the FIFO size)
   // N=1 in this case for reg3 (i.e. two bytes)
   // The purpose of the additional bit is to distingish the full/empty cases

   // Read address
   reg [1:0]     raddr;     // binary encoded
   wire [1:0]    raddr_g;   // gray coded
   reg [1:0]     raddr_g1;  // gray coded, part synchronised to write domain
   reg [1:0]     raddr_g2;  // gray coded, fully synchronised to write domain

   // Write address
   reg [1:0]     waddr;     // binary encoded
   wire [1:0]    waddr_g;   // gray coded
   reg [1:0]     waddr_g1;  // gray coded, part synchronised to read domain
   reg [1:0]     waddr_g2;  // gray coded, fully synchronised to read domain

   // FIFO Data
   reg [7:0]     data[0:1];

   // Write logic
   always @(posedge p_phi2 or negedge h_rst_b) begin
      if (!h_rst_b) begin
         waddr <= INIT_WADDR;
         raddr_g1 <= raddr_g;
         raddr_g2 <= raddr_g;
         data[0] <= INIT_DATA0;
         data[1] <= INIT_DATA1;
      end else begin
         raddr_g1 <= raddr_g;
         raddr_g2 <= raddr_g1;
         if (p_selectData && !p_rdnw && !wfull) begin
            data[waddr[0]] <= p_data;
            waddr <= waddr + 1;
         end
      end
   end

   // Read logic
   always @(negedge h_phi2 or negedge h_rst_b) begin
      if (!h_rst_b) begin
         raddr <= INIT_RADDR;
         waddr_g1 <= waddr_g;
         waddr_g2 <= waddr_g;
      end else begin
         waddr_g1 <= waddr_g;
         waddr_g2 <= waddr_g1;
         if (h_selectData && h_rd && !rempty) begin
            raddr <= raddr + 1;
         end
      end
   end

   // Flag logic
   //
   // Full/Empty flags are generated from the gray coded addresses
   //
   // In the case of a 2 entry FIFO, this works out quite nicely.
   //
   // Note: I'm not convinced the logic for the full flag would generalize
   // to more than two bits.
   assign waddr_g = waddr ^ (waddr >> 1);
   assign wfull   = (waddr_g ^ raddr_g2) == 2'b11;
   assign wempty  = (waddr_g ^ raddr_g2) == 2'b00;
   assign raddr_g = raddr ^ (raddr >> 1);
   assign rfull  = (raddr_g ^ waddr_g2) == 2'b11;
   assign rempty = (raddr_g ^ waddr_g2) == 2'b00;

   // Output signal generation

   // Register 3 is intended to enable high speed transfers of large blocks of data across the tube.
   // It can operate in one or two byte mode, depending on the V flag. In one byte mode the status
   // bits make each FIFO appear to be a single byte latch - after one byte is written the register
   // appears to be full. In two byte mode the data available flag will only be asserted when two bytes have
   // been entered, and the not full flag will only be asserted when both bytes have been removed. Thus data
   // available going active means that two bytes are available, but it will remain active until both bytes
   // have been removed. Not full going active means that the register is empty, but it will remain active
   // until both bytes have been entered. PNMI, N and DRQ also remain active until the full two
   // byte operation is completed

   assign p_empty          = wempty;
   assign p_full           = one_byte_mode ? wfull   : !wempty;
   assign h_data_available = one_byte_mode ? !rempty : rfull;
   assign h_data           = data[raddr[0]];

endmodule
