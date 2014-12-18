//**************************************************************************
//    ph_flag_m.v - Parasite to host flag for reg 3
//   
//    COPYRIGHT 2010 Richard Evans, Ed Spittles
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
//
// The Acorn Tube uses a 6502 bus interface on the host side, but a read/write strobe 
// protocol on the parasite side. 
//
`timescale 1ns / 1ns

module ph_flag_m (
                  input rst_b, 
                  input set_b,
                  input h_rdnw,
                  input h_select,
                  input h_phi2,
                  input p_select,
                  input p_wrst_b,
                  output h_data_available,
                  output p_full
                  );
   
   
   wire    p_full_q_r;
   reg     h_data_avail_q_r;
      
   // Assign IOs
   assign h_data_available = h_data_avail_q_r;
   assign p_full = p_full_q_r | h_data_avail_q_r  ;
   
   // Combinatorial assignments
   wire set_empty_b_w = ! ( !rst_b | (h_select & h_rdnw & h_data_avail_q_r & h_phi2));   
   wire p_full_d_w = p_select | p_full_q_r;  

   // infer all state
   always @ ( posedge h_phi2 or negedge rst_b )   
     begin
        if (!rst_b)
          h_data_avail_q_r <= 1'b1;
        else
          h_data_avail_q_r <= p_full_q_r ;
     end
   
   /*
   always @ ( posedge p_wrst_b or negedge set_b or negedge set_empty_b_w )
     begin
        if (!set_b)
          p_full_q_r <= 1'b1;        
        else if (!set_empty_b_w)
          p_full_q_r <= 1'b0;
        else
          p_full_q_r <= p_full_d_w;
        
     end
   */
   dsrff ff (
    .d(p_full_d_w), 
    .clk(p_wrst_b), 
    .s(!set_b), 
    .r(!set_empty_b_w), 
    .q(p_full_q_r), 
    .qb()
    );
   
endmodule 



   