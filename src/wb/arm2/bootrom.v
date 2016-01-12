module bootrom (
    input clk,
    input rst,

    // Wishbone slave interface
    input      [31:0] wb_dat_i,
    output reg [31:0] wb_dat_o,
    input      [31:0] wb_adr_i,
    input             wb_we_i,
    input             wb_stb_i,
    input             wb_cyc_i,
    input      [ 3:0] wb_sel_i,
    output reg        wb_ack_o
  );

  // Net declarations
  reg  [31:0] rom[0:4095];  // Instantiate the ROM

  wire [ 11:0] rom_addr;
  wire        stb;
  reg        stb1;
   
  // Combinatorial logic
  assign rom_addr = wb_adr_i[13:2];
  assign stb      = wb_stb_i & wb_cyc_i;

  always @(posedge clk)
    begin
       stb1 <= stb;
       wb_ack_o <= stb & ~stb1;
       wb_dat_o <= rom[rom_addr];  
    end
  
  initial $readmemh("bootrom.dat", rom);

endmodule
