module ICAP_reboot (
  input        fastclk,
  input [3:0]  sw,
  output [8:1] test
  );

   ICAP_core instance_core (
      .fastclk(fastclk),
      .design_num({1'b0, sw}),
      .reconfigure(1'b1),
      .powerup(1'b1),
      .sw_in(sw),
      .test(test)
      );
           
endmodule
