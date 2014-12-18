`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date:    15:20:33 11/22/2014 
// Design Name: 
// Module Name:    dsr-ff 
// Project Name: 
// Target Devices: 
// Tool versions: 
// Description: 
//
// Dependencies: 
//
// Revision: 
// Revision 0.01 - File Created
// Additional Comments: 
//
//////////////////////////////////////////////////////////////////////////////////
module dsrff(
    input d,
    input clk,
    input s,
    input r,
    output q,
    output qb
    );

	wire f3;
	wire f4;
	wire f5;
	wire f6;
	
	wire sb;
	wire rb;
	
	assign sb = ~s;
	assign rb = ~r;
	
	assign q = ~(qb && f4 && sb);
	assign qb = ~(q && f5 && rb);
	
	assign f3 = ~(sb && f6 && f4);
	assign f4 = ~(f3 && clk && rb);
	assign f5 = ~(f4 && sb && clk && f6);
	assign f6 = ~(f5 && d && rb);

endmodule
