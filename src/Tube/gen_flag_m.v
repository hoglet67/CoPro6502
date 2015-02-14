//**************************************************************************
//    gen_flag_m.v - single byte buffer for transfers in host to parasite direction
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
// Generic flag crossing clock boundaries, intended for use in the Acorn Tube 
// ULA replacement system passing information between host and parasite sides.
//
//  - Port P1 (the write port) can set the flag
//  - Port P2 (the read port) can reset the flag
//
// The Acorn Tube uses a 6502 bus interface on the host side, but a read/write strobe 
// protocol on the parasite side. This logic can be configured so that both ports can
// work with either system by just tying off inputs.
//
//          P1 (Write Port)                                       P2 (Read Port)
//                  __                                    __    
//     p1_rdnw ---o|  |                                  |  |----- p2_rdnw
//     p1_phase ---|& |--|>o----*-        ---------o<|---| &|----- p2_sel
//      p1_sel ----|  |         |  `     '               |  |----- p2_phase
//              --o|__|         |   |   |                |__|--- 
//             '                |  _o___o_                      '
//             |                | | S   R |                     |
//             |                | |   Q   |                     |
//             |                | |_______|                     |
//             |     _o_    __  |     |      __      _o_        |
//    p1_full -*----|q d|--|  |-|-----*-----|& |----|d q|---*---*- p2_available
//                  |_^_|  |+ | |           |  |    |_^_|   |
//                    o    |  | `-----------|__|      |     |
//      p1_clk  ------'    |  |                       `-----|------- p2_clk 
//                         |__|-----------------------------
// 
// To use the flag on host->parasite and parasite->host interfaces, use the following hookup
// 
// gen_flag_m hp (                       gen_flag_m ph (              
//      .rst_b(h_rst_b),                    .rst_b(h_rst_b),      
//      .reset_state(1'b0),                 .reset_state(1'b0),
//      .p1_rdnw(h_rdnw),                   .p1_rdnw(p_westb_b),    
//      .p1_phase(h_phi2),                  .p1_phase(1'b1),   
//      .p1_select(h_select),               .p1_select(p_select),  
//      .p1_full(h_full),                   .p1_full(p_full),    
//      .p1_clk(h_phi2),                    .p1_clk(!(p_westb_b & p_rdstb_b)),     
//      .p2_rdnw( !p_rdstb_b),              .p2_rdnw(h_rdnw),    
//      .p2_phase(1'b1),                    .p2_phase(h_phi2),   
//      .p2_select(p_select),               .p2_select(h_select),  
//      .p2_avail(p_avail),                 .p2_avail(h_avail),        
//      .p2_clk(!(p_westb_b & p_rdstb_b))   .p2_clk(h_phi2)      
//      )                                   )              
// 
// 'phase' input is used to qualify the read signal with the clock phase                          
// - use phi2 for std 6502 bus transactions, ie rdnw valid during phi2
// - use '1' for strobe transactions (because phase and read signal are already combined in a we/rdstb signal)
// 
// Reset not shown in the diagram above but reset state selection is selected by the reset_state() input pin
// - on most flags rst_b low will cause all state to be set to '0'
// - on flags for byte 0 in register 3 rst_b low causes all state to be set to '1'
// 
// The reset logic is implemented by swapping the two edge triggered flops for set or reset types 
// and adding a small amount of gating on the S or R lines for the latch.
// 
// Cross coupled gates allow p1 and p2 operation to be at significantly different speeds
//  - cross coupled gate A ensures that p1_full flag stays high while p2 is performing a read
//  - cross coupled gate B ensures that p2_avail flag stays low while p1 is performing a write
`timescale 1ns / 1ns

module gen_flag_m (
    input rst_b,                  
    //input reset_state,
    input p1_rdnw,
    input p1_select,
    input p1_phase,
    input p1_clk,
    input p1_clk_en,
    input p2_select,
    input p2_rdnw,
    input p2_clk,
    input p2_clk_en,
    input p2_phase,                     
    output p2_data_available,
    output p1_full
                   );
   
   
   reg     p1_full_q_r,
           p2_data_avail_q_r;
		   
   wire    full_sr_q_r;
   wire 	full_sr_qbar_r;
		   
   wire    set_full_b_w,
           p1_full_d_w,   
           p2_data_avail_d_w,
           set_empty_b_w;
		   

   
   // Assign IOs
   assign p2_data_available = p2_data_avail_q_r;
   assign p1_full = p1_full_q_r   ;
   
   // Combinatorial assignments
   assign set_full_b_w = ! ( (!p1_rdnw & p1_phase & p1_select & !p1_full_q_r)) ;
   assign set_empty_b_w = ! ( (!rst_b) | ( p2_phase & p2_select & p2_rdnw & p2_data_avail_q_r));
   
   // ensure that full signal to parasite doesn't go low while (slow) host read is still in progress
   assign p1_full_d_w = full_sr_q_r |  p2_data_avail_q_r ;  
   assign p2_data_avail_d_w = full_sr_q_r & set_full_b_w ;

   // infer all state
   always @ ( posedge p2_clk or negedge rst_b )   
     begin
        if ( ! rst_b )
          p2_data_avail_q_r <= 1'b0;
        else if (p2_clk_en)
          p2_data_avail_q_r <= p2_data_avail_d_w ;
     end
   
   always @ ( negedge p1_clk or negedge rst_b )
     begin
        if ( ! rst_b)
          p1_full_q_r <= 1'b0;        
        else if (p1_clk_en)
          p1_full_q_r <= p1_full_d_w ;
     end
   
   // SR latch inputs used only on this state element
   //always @ ( set_full_b_w or set_empty_b_w )
   //  if ( set_full_b_w == 1'b0 )
   //    full_sr_q_r = 1'b1;
   //  else if ( set_empty_b_w == 1'b0) 
   //   full_sr_q_r = 1'b0;
   assign full_sr_q_r = ~( set_full_b_w & full_sr_qbar_r);
   assign full_sr_qbar_r = ~( set_empty_b_w & full_sr_q_r);
   
   
endmodule // gen_flag_m



   