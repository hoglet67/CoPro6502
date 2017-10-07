module ICAP_config
  (
   input        fastclk,
   output [7:0] test,

   // DIP switches in and out
   input [3:0]  sw_in,
   output [3:0] sw_out,

   // Tube interface
   input [2:0]  h_addr,
   input        h_cs_b,
   inout [7:0]  h_data,
   input        h_phi2,
   input        h_rdnw,
   input        h_rst_b
  );

   reg          reconfigure_sw_changed = 1'b0;
   reg          reconfigure_hw_changed = 1'b0;
   reg          reconfigure = 1'b0;
   reg  [4:0]   design_num;
   wire [3:0]   pwr_out;
   wire         initialized;
   reg          p_rst_b = 1'b1;
   reg          h_cs_b1;
   reg [2:0]    h_addr1;
   reg          h_rdnw1;

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
      if (!p_rst_b || !h_rst_b) begin
         reconfigure <= reconfigure_sw_changed || reconfigure_hw_changed;
      end
   end

   // Latch control signals on the rising egge of Phi2
   // (avoids hold time issues, we do the same in the tube)
   always @(posedge h_phi2) begin
      h_cs_b1 <= h_cs_b;
      h_addr1 <= h_addr;
      h_rdnw1 <= h_rdnw;
   end

   always @(negedge h_phi2) begin
      // Mirror the reset bit of register FEE0, and allow this to reconfigure
      if (!h_cs_b1 && !h_rdnw1 && h_addr1 == 3'b000) begin
        if (h_data[6] && h_data[7]) begin
            // Setting the T bit (bit 6) clears all tube registers
            p_rst_b <= 1'b1;
        end else if (h_data[5]) begin
            // Setting the S bit (bit 5) asserts the parasite reset
            // Clearing the S bit (bit 5) de-asserts the parasite reset
            p_rst_b <= !h_data[7];
        end
      end
      // Implement a write only register at FEE6 to change the current design
      if (!h_cs_b1 && !h_rdnw1 && h_addr1 == 3'b110) begin
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
