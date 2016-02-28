module tuberom_32016 (
    input             clk,
    input [12:0]      addr,
    output reg [31:0] data
);

    reg  [31:0] rom[0:8191];
   
    always @(posedge clk)
        begin
            data <= rom[addr];  
        end
  
    initial $readmemh("tuberom_32016_200.dat", rom);

endmodule
