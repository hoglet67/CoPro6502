module ICAP_reboot (


  input fastclk,
  input [4:1] sw,
  output [8:1] test
  );




reg   [15:0] icap_din;
reg         icap_ce;
reg         icap_wr;

reg  [15:0] ff_icap_din_reversed;
reg         ff_icap_ce;
reg         ff_icap_wr;

reg [3:0] MBT_REBOOT=4'b0000;





  ICAP_SPARTAN6 ICAP_SPARTAN6_inst (
  
    .BUSY      (),   // Busy output
    .O         (),   // 16-bit data output
    .CE        (ff_icap_ce),   // Clock enable input
    .CLK       (CLK0_OUT),         // Clock input
    .I         (ff_icap_din_reversed),  // 16-bit data input
    .WRITE     (ff_icap_wr)    // Write input
  );
  
  dcm_32_16 instance_name (
    .CLKIN_IN(fastclk), 
    .CLK0_OUT(CLK0_OUT), 
    .CLK0_OUT1(), 
    .CLK2X_OUT()
    );


assign test = {4'b0000,MBT_REBOOT};

//  -------------------------------------------------
//  --  State Machine for ICAP_SPARTAN6 MultiBoot  --
//  --   sequence.                                 --
//  -------------------------------------------------


parameter         IDLE     = 0, 
                  SYNC_H   = 1, 
                  SYNC_L   = 2, 
                  
                  CWD_H    = 3,  
                  CWD_L    = 4,  
                                 
                  GEN1_H   = 5,  
                  GEN1_L   = 6,  
                                 
                  GEN2_H   = 7,  
                  GEN2_L   = 8,  
                                 
                  GEN3_H   = 9,  
                  GEN3_L   = 10, 
                                 
                  GEN4_H   = 11, 
                  GEN4_L   = 12, 
                                 
                  GEN5_H   = 13, 
                  GEN5_L   = 14, 
                                 
                  NUL_H    = 15, 
                  NUL_L    = 16, 
                                 
                  MOD_H    = 17, 
                  MOD_L    = 18, 
                                 
                  HCO_H    = 19, 
                  HCO_L    = 20, 
                                 
                  RBT_H    = 21, 
                  RBT_L    = 22, 
                  
                  NOOP_0   = 23, 
                  NOOP_1   = 24,
                  NOOP_2   = 25,
                  NOOP_3   = 26;
                  
                   
reg [4:0]     state = IDLE;
reg [4:0]     next_state;


always @(MBT_REBOOT or state or sw)
   begin: COMB

      case (state)
      
         IDLE:
            begin
               if (MBT_REBOOT==4'b1111)
                  begin
                     next_state  = SYNC_H;
                     icap_ce     = 0;
                     icap_wr     = 0;
                     icap_din    = 16'hAA99;  // Sync word part 1 
                  end
               else
                  begin
                     next_state  = IDLE;
                     icap_ce     = 1;
                     icap_wr     = 1;
                     icap_din    = 16'hFFFF;  // Null data
                  end
            end
            
         SYNC_H:
            begin
               next_state  = SYNC_L;
               icap_ce     = 0;
               icap_wr     = 0;
               icap_din    = 16'h5566;    // Sync word part 2
            end

//--------------------

         SYNC_L:
            begin
               next_state  = GEN1_H;
               icap_ce     = 0;
               icap_wr     = 0;
               icap_din    = 16'h3261;    //  Write to GENERAL_1 Register....
            end

        GEN1_H:
            begin
               next_state  = GEN1_L;
               icap_ce     = 0;
               icap_wr     = 0;
               //icap_din    = 16'h32c1;
               
               case (sw)
                    4'b0000: icap_din    = 16'h4000;
                    4'b0001: icap_din    = 16'h8000;
                    4'b0010: icap_din    = 16'hC000;
                    4'b0011: icap_din    = 16'h0000;
                    4'b0100: icap_din    = 16'h4000;
                    4'b1000: icap_din    = 16'h8000;
                    4'b1001: icap_din    = 16'h8000;
                    4'b1010: icap_din    = 16'h8000;
                    4'b1011: icap_din    = 16'h8000;                    
                    4'b1100: icap_din    = 16'hC000;
                    4'b1101: icap_din    = 16'hC000;
                    4'b1110: icap_din    = 16'hC000;
                    4'b1111: icap_din    = 16'hC000;                    
                    default: icap_din    = 16'h32c1;
               endcase  
               
            end

         GEN1_L:
            begin
               next_state  = GEN2_H;
               icap_ce     = 0;
               icap_wr     = 0;
               icap_din    = 16'h3281;    //  Write to GENERAL_2 Register....
            end

        GEN2_H:
            begin
               next_state  = GEN2_L;
               icap_ce     = 0;
               icap_wr     = 0;
               //icap_din    = 16'h0305;
               
               case (sw)
                    4'b0000: icap_din    = 16'h0305;
                    4'b0001: icap_din    = 16'h030a;
                    4'b0010: icap_din    = 16'h030f;
                    4'b0011: icap_din    = 16'h0315;
                    4'b0100: icap_din    = 16'h031a;
                    4'b1000: icap_din    = 16'h031f;
                    4'b1001: icap_din    = 16'h031f;
                    4'b1010: icap_din    = 16'h031f;
                    4'b1011: icap_din    = 16'h031f;
                    4'b1100: icap_din    = 16'h0324;
                    4'b1101: icap_din    = 16'h0324;
                    4'b1110: icap_din    = 16'h0324;
                    4'b1111: icap_din    = 16'h0324;
                    default: icap_din    = 16'h0305;
               endcase
               
            end

//--------------------

        GEN2_L:
            begin
               next_state  = RBT_H;
               icap_ce     = 0;
               icap_wr     = 0;
               icap_din    = 16'h30A1;      //  Write to Command Register....
            end

        RBT_H:
            begin
               next_state  = RBT_L;
               icap_ce     = 0;
               icap_wr     = 0;
               icap_din    = 16'h000E;      // REBOOT Command issued....  value = 0x000E
            end

//--------------------

        RBT_L:
            begin
               next_state  = NOOP_0;
               icap_ce     = 0;
               icap_wr     = 0;
               icap_din    = 16'h2000;    //  NOOP
            end

        NOOP_0:
            begin
               next_state  = NOOP_1;
               icap_ce     = 0;
               icap_wr     = 0;
               icap_din    = 16'h2000;    // NOOP
            end

        NOOP_1:
            begin
               next_state  = NOOP_2;
               icap_ce     = 0;
               icap_wr     = 0;
               icap_din    = 16'h2000;    // NOOP
            end

        NOOP_2:
            begin
               next_state  = NOOP_3;
               icap_ce     = 0;
               icap_wr     = 0;
               icap_din    = 16'h2000;    // NOOP
            end

//--------------------

        NOOP_3:
            begin
               next_state  = IDLE;
               icap_ce     = 1;
               icap_wr     = 1;
               icap_din    = 16'h1111;    // NULL value
            end
          
        default:
            begin
               next_state  = IDLE;
               icap_ce     = 1;
               icap_wr     = 1;
               icap_din    = 16'h1111;    //  16'h1111"
            end

      endcase
   end


always@(posedge CLK0_OUT)            // Give a bit of delay before starting the statemachine
begin
    if (MBT_REBOOT != 4'b1111)
    begin
        MBT_REBOOT <= MBT_REBOOT + 4'b0001;
        state <= IDLE;
    end    else begin
        state <= next_state;
    end
    
end

always @(posedge CLK0_OUT)

   begin:   ICAP_FF
   
        ff_icap_din_reversed[0]  <= icap_din[7];   //need to reverse bits to ICAP module since D0 bit is read first
        ff_icap_din_reversed[1]  <= icap_din[6]; 
        ff_icap_din_reversed[2]  <= icap_din[5]; 
        ff_icap_din_reversed[3]  <= icap_din[4]; 
        ff_icap_din_reversed[4]  <= icap_din[3]; 
        ff_icap_din_reversed[5]  <= icap_din[2]; 
        ff_icap_din_reversed[6]  <= icap_din[1]; 
        ff_icap_din_reversed[7]  <= icap_din[0]; 
        ff_icap_din_reversed[8]  <= icap_din[15];
        ff_icap_din_reversed[9]  <= icap_din[14];
        ff_icap_din_reversed[10] <= icap_din[13];
        ff_icap_din_reversed[11] <= icap_din[12];
        ff_icap_din_reversed[12] <= icap_din[11];
        ff_icap_din_reversed[13] <= icap_din[10];
        ff_icap_din_reversed[14] <= icap_din[9]; 
        ff_icap_din_reversed[15] <= icap_din[8]; 
        
        ff_icap_ce  <= icap_ce;
        ff_icap_wr  <= icap_wr;
   end  
        
        
endmodule











