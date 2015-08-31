module ICAP_config
  (
   input 	fastclk,
   output [7:0] test,

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

   reg          reconfigure_sw_changed = 1'b0;
   reg          reconfigure_hw_changed = 1'b0;
   reg          reconfigure = 1'b0;
   reg  [4:0]   design_num;
   wire [3:0]   pwr_out;
   wire         initialized;

   ICAP_core instance_core
     (
      .fastclk(fastclk),
      .design_num(design_num),
      .reconfigure(reconfigure),
      .powerup(1'b0),
      .sw_in(sw_in),
      .sw_out(sw_out),
      .pwr_out(pwr_out),
      .initialized(initialized),
      .test(test)
      );

   always @(posedge fastclk) begin
      if (!h_rst_b) begin
         reconfigure <= reconfigure_sw_changed || reconfigure_hw_changed;      
      end
   end
   
   always @(negedge h_phi2) begin
      // Implement a write only register at FEE6 to change the current design
      if (!h_cs_b && !h_rdnw && h_addr == 3'b110) begin
         design_num <= h_data[4:0];
         reconfigure_sw_changed <= 1'b1;  
      end
      // Detect changes in the DIP Switches, and invoke the multi boot loader as for a power up
      if (initialized) begin
         if (sw_in != pwr_out) begin
            // Someone has moved the hardware DIP switches
            design_num <= 5'b10000;
            reconfigure_hw_changed <= 1'b1;
         end else begin
            // Someone has moved them back again
            reconfigure_hw_changed <= 1'b0;
         end
      end
   end

              
endmodule
