module ICAP_config
  (
   input 	fastclk,

   // DIP switches in and out
   input [3:0] 	sw_in,
   output [3:0] sw_out,

   // Tube interface
   input [2:0] 	h_addr,
   input 	h_cs_b,
   inout [7:0] 	h_data,
   input 	h_phi2,
   input 	h_rdnw,
   input 	h_rst_b
  );

   reg          reconfigure_on_reset = 1'b0;
   reg          reconfigure = 1'b0;
   reg [4:0]    design_num;

   ICAP_core instance_core
     (
      .fastclk(fastclk),
      .design_num(design_num),
      .reconfigure(reconfigure),
      .powerup(1'b0),
      .sw_in(sw_in),
      .sw_out(sw_out)
      );

   always @(posedge fastclk) begin
      if (!h_rst_b) begin
         reconfigure <= reconfigure_on_reset;      
      end
   end

   // Implement a write only register at FEE6
   always @(negedge h_phi2) begin
      if (!h_cs_b && !h_rdnw && h_addr == 3'b110) begin
         design_num <= h_data[4:0];
         reconfigure_on_reset <= 1'b1;     
      end
   end
              
endmodule
