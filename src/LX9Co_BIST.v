module LX9Co_BIST (

	output [18:0] ram_addr,
	inout [31:0] ram_data,
	output reg ram_wr,
	output reg ram_oe,
	output ram_cs,
	output ram_ub_b,
	output ram_lb_b,
	
	input [2:0] h_addr,
	inout [7:0] h_data,
	input h_phi2,
	input h_cs_b,
	input h_pwr,
	input h_rdnw,
	input h_rst_b,
	
    input fastclk,
    output [8:1] test,
    input [3:0] sw,
	output h_irq_b
  );

   parameter TOP_ADDR = 19'h07ffff;		// Maximum RAM value (512k words)

  dcm_32_16 dcm (
    .CLKIN_IN(fastclk), 
    .CLK0_OUT(CLK0_OUT), 
    .CLK0_OUT1(), 
    .CLK2X_OUT()
    );
	
	
reg [18:0] addr_cnt =19'h000000;
wire [31:0] test_data;
wire [7:0] test_out;
reg cnt_rst;
reg w_rst;
reg r_rst;

assign ram_cs = 1'b0;
assign ram_ub_b = 1'b0;
assign ram_lb_b = 1'b0;
assign ram_addr = addr_cnt;
assign test_out = (state == 4) ? 8'h55 : addr_cnt[18:11];								// Display 01010101 whilst zeroing RAM out for visual confirmation
assign test_data = (state == 4) ? 32'h000000000000 : {addr_cnt[15:0],addr_cnt[15:0]}; 	// State 4 zeros memory : state 2 fills with a pattern based on the address
assign ram_data = (state == 2 || state == 4) ? test_data : 32'hzzzzzzzz;				// state 2 and state 4 we're writing to RAM, else HiZ
assign test = (sw[3]==1'b0) ? test_out: tubes[sw[2:0]] ;								// Either show RAM test progress OR show one of the Tube registers on the LEDs
  
//////
// RAM test - other than the MUX to the LEDs there is no connection to the Tube registers
//////
  
reg [4:0]     state = 0;
reg [4:0]     next_state;
always @(loop or addr_cnt or state or pass)
   begin: COMB

      case (state)
      
         1:									// Idle
            begin
               if (loop==4'b1111)
                  begin
                     next_state  = 2;
					 cnt_rst = 0;
					 w_rst = 0;				// Stop resetting the write state machine.
					 r_rst = 1;
                  end
               else
                  begin
                     next_state  = 1;
					 cnt_rst = 1;
					 w_rst = 1;
					 r_rst = 1;
                  end
            end
            
         2:									// Filling memory
            begin
               if (addr_cnt==TOP_ADDR)		// If the counter has completed, reset it and move to reading back
                  begin
                     next_state  = 3;
					 cnt_rst = 1;
					 w_rst = 1;
					 r_rst = 1;
                  end
               else
                  begin
                     next_state  = 2;
					 cnt_rst = 0;
					 w_rst = 0;				// Stop resetting the write state machine.
					 r_rst = 1;
                  end
            end

         3:									// Reading back
            begin
			if (addr_cnt==TOP_ADDR)		// If the counter has completed, reset it and proceed to Zero memory
                  begin
                     next_state  = 4;
					 cnt_rst = 1;
					 w_rst = 1;
					 r_rst = 1;
                  end
               else
			     begin
				  if (pass == 1'b1)
                  begin						// Continue to verify
                     next_state  = 3;
					 cnt_rst = 0;
					 w_rst = 1;				
					 r_rst = 0;				// Stop resetting the read state machine.
                  end else begin			// Read back borked, proceed to NOT OK
				     next_state  = 5;
					 cnt_rst = 0;
					 w_rst = 1;				
					 r_rst = 1;				
				  end
				 end
            end

		4:									// Readback was OK, now zero out
			begin
               if (addr_cnt==TOP_ADDR)		// If the counter has completed, reset it and move to refilling with data
                  begin
                     next_state  = 1;
					 cnt_rst = 1;
					 w_rst = 1;
					 r_rst = 1;
                  end
               else
                  begin
                     next_state  = 4;
					 cnt_rst = 0;
					 w_rst = 0;				// Stop resetting the write state machine.
					 r_rst = 1;
                  end
			end
		5:									// NOT OK
			begin
				next_state  = 5;
				cnt_rst = 0;
				w_rst = 1;
				r_rst = 1;
			end
          
        default:
            begin
                     next_state  = 1;
					 cnt_rst = 1;
					 w_rst = 1;
					 r_rst = 1;
            end

      endcase
   end



always@(posedge CLK0_OUT)			
begin
	if (cnt_rst)
	begin
		addr_cnt <= 19'h000000;
	end	else begin
		if (w_inc || r_inc)
		begin
			addr_cnt <= addr_cnt + 19'h000001;
		end else begin
			case (state)
			//4:
			//	addr_cnt <= 19'h555555;		// OK pattern
			5:
				addr_cnt <= 19'h333333;		// Fail pattern
			default:
				addr_cnt <= addr_cnt;
			endcase
		end
	end	
end

reg pass;
reg r_inc;
reg [1:0] r_delay;
always@(posedge CLK0_OUT)			// 4 phase read memory			
begin
	if (r_rst)
	begin
		r_inc <= 1'b0;
		r_delay <= 2'b00;
		ram_oe <= 1'b1;
		pass <= 1'b1;
	end	else begin
		r_delay <= r_delay + 2'b01;
		case (r_delay)
		2'b00:				// Allow A and D to settle
			begin
				ram_oe <= 1'b1;
				r_inc <= 1'b0;
				pass <= pass;
			end
		2'b01:				// Pulse OE low
			begin
				ram_oe <= 1'b0;
				r_inc <= 1'b0;
				pass <= pass;
			end
		2'b10:				// Bring WE high
			begin
				ram_oe <= 1'b1;
				r_inc <= 1'b0;
				if (ram_data == test_data)		// Read Data OK or fail ?
				//if (ram_data[15:0] == test_data[15:0])
					pass <= pass;
				else
					pass <= 1'b0;
			end
		2'b11:
			begin			// Advance address
				ram_oe <= 1'b1;
				r_inc <= 1'b1;
				pass <= pass;
			end
		endcase
	end	
end

reg w_inc;
reg [1:0] w_delay;
always@(posedge CLK0_OUT)			// 4 phase write memory			
begin
	if (w_rst)
	begin
		w_inc <= 1'b0;
		w_delay <= 2'b00;
		ram_wr <= 1'b1;
	end	else begin
		w_delay <= w_delay + 2'b01;
		case (w_delay)
		2'b00:				// Allow A and D to settle
			begin
				ram_wr <= 1'b1;
				w_inc <= 1'b0;
			end
		2'b01:				// Pulse WE low
			begin
				ram_wr <= 1'b0;
				w_inc <= 1'b0;
			end
		2'b10:				// Bring WE high
			begin
				ram_wr <= 1'b1;
				w_inc <= 1'b0;
			end
		2'b11:
			begin			// Advance address
				ram_wr <= 1'b1;
				w_inc <= 1'b1;
			end
		endcase
	end	
end


reg [3:0] loop = 4'b0000;
always@(posedge CLK0_OUT)			// Give a bit of delay before starting the statemachine
begin
	if (loop!= 4'b1111)
	begin
		loop <= loop + 4'b0001;
		state <= 1;
	end	else begin
		state <= next_state;
	end
	
end


//////
// Tube registers - other than the MUX to the LEDs there is no connection to the RAM test
//////

reg [7:0] tubes [0:7];		// 8 registers that just cache values for testing the host interface

// Data out for reads	
assign h_data = (h_rdnw == 1 && h_cs_b == 0 && h_phi2) ? tubes[h_addr] : 8'hzz;

// Data in for writes
always@(negedge h_phi2)
begin
	if (h_rdnw == 0 && h_cs_b == 0)
		if (h_addr == 3'b000)
		  tubes[h_addr] <= 8'hfe;		// Otherwise the Beeb won't boot
		else 
		  tubes[h_addr] <= h_data;
	else
		tubes[h_addr] <= tubes[h_addr];	
end        

assign h_irq_b = 1'bz;
        
endmodule











