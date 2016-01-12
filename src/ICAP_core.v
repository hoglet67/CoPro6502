module ICAP_core
  (
   input        fastclk,
   input [4:0]  design_num,
   input        reconfigure,
   input        powerup,
   input [3:0]  sw_in,
   output [3:0] sw_out,
   output [3:0] pwr_out,
   output       initialized,
   output [7:0] test
   );

   reg          clk_16M00;

   reg [15:0]   icap_din;
   reg          icap_ce;
   reg          icap_wr;

   wire [15:0]  icap_dout_reversed;
   reg [15:0]   icap_dout;

   reg [15:0]   ff_icap_din_reversed;
   reg          ff_icap_ce;
   reg          ff_icap_wr;

   reg [3:0]    MBT_REBOOT = 4'b0000;

   reg [7:0]    soft_dip = 8'b00000000;

   reg test_trig;

   assign test = { clk_16M00, test_trig, busy, ff_icap_wr,
    sw_in[0] ? icap_dout[3:0] :
        (sw_in[1] ? icap_dout[7:4] :
            (sw_in[2] ? icap_dout[11:8] :
                (sw_in[3] ? icap_dout[15:12] : 4'b1010)))};

   assign sw_out = powerup ? sw_in : soft_dip[3:0];

   assign pwr_out = powerup ? sw_in : soft_dip[7:4];

   assign initialized = state == IDLE;

   wire busy;

   ICAP_SPARTAN6 ICAP_SPARTAN6_inst
     (
      .BUSY      (busy),   // Busy output
      .O         (icap_dout_reversed),   // 16-bit data output
      .CE        (ff_icap_ce),   // Clock enable input
      .CLK       (clk_16M00),         // Clock input
      .I         (ff_icap_din_reversed),  // 16-bit data input
      .WRITE     (ff_icap_wr)    // Write input
      );


   //  -------------------------------------------------
   //  --  State Machine for ICAP_SPARTAN6 MultiBoot  --
   //  --   sequence.                                 --
   //  -------------------------------------------------


   parameter

     INIT             = 0,

     RD_DUMMY         = 1,
     RD_SYNC_H        = 2,
     RD_SYNC_L        = 3,
     RD_NOOP_1        = 4,
     RD_NOOP_2        = 5,
     RD_GEN5          = 6,
     RD_NOOP_3        = 7,
     RD_NOOP_4        = 8,
     RD_NOOP_5        = 9,
     RD_NOOP_6        = 10,
     RD_AVOID_ABORT_1 = 11,
     RD_AVOID_ABORT_2 = 12,
     RD_LATCH_DATA    = 13,
     RD_AVOID_ABORT_3 = 14,
     RD_AVOID_ABORT_4 = 15,
     RD_DESYNC_H      = 16,
     RD_DESYNC_L      = 17,
     RD_NOOP_7        = 18,

     IDLE             = 19,
     DUMMY_1          = 20,
     DUMMY_2          = 21,
     SYNC_H           = 22,
     SYNC_L           = 23,

     GEN1_H           = 24,
     GEN1_L           = 25,

     GEN2_H           = 26,
     GEN2_L           = 27,

     GEN5_H           = 28,
     GEN5_L           = 29,

     RBT_H            = 30,
     RBT_L            = 31,

     RBT_NOOP_0       = 32,
     RBT_NOOP_1       = 33,
     RBT_NOOP_2       = 34,
     RBT_NOOP_3       = 35;

   reg [5:0]    state = INIT;
   reg [5:0]    next_state;

   always @(MBT_REBOOT or state or design_num or reconfigure or powerup or sw_in or busy or soft_dip)
     begin: COMB

        case (state)


          //--------------------

          INIT:
            begin
               next_state  = RD_DUMMY;
               icap_ce     = 0;
               icap_wr     = 0;
               icap_din    = 16'hFFFF;  // Null data
            end

          RD_DUMMY:
            begin
               next_state  = RD_SYNC_H;
               icap_ce     = 0;
               icap_wr     = 0;
               icap_din    = 16'hAA99;   // Sync word part 1
            end

          RD_SYNC_H:
            begin
               next_state  = RD_SYNC_L;
               icap_ce     = 0;
               icap_wr     = 0;
               icap_din    = 16'h5566;    // Sync word part 2
            end

          RD_SYNC_L:
            begin
               next_state  = RD_NOOP_1;
               icap_ce     = 0;
               icap_wr     = 0;
               icap_din    = 16'h2000;    // NOOP
            end

          RD_NOOP_1:
            begin
               next_state  = RD_NOOP_2;
               icap_ce     = 0;
               icap_wr     = 0;
               icap_din    = 16'h2000;    // NOOP
            end

          RD_NOOP_2:
            begin
               next_state  = RD_GEN5;
               icap_ce     = 0;
               icap_wr     = 0;
               icap_din    = 16'h2ae1;    //  Read General_5 register
            end

          RD_GEN5:
            begin
               next_state  = RD_NOOP_3;
               icap_ce     = 0;
               icap_wr     = 0;
               icap_din    = 16'h2000;    //  NOOP
            end

          RD_NOOP_3:
            begin
               next_state  = RD_NOOP_4;
               icap_ce     = 0;
               icap_wr     = 0;
               icap_din    = 16'h2000;    //  NOOP
            end

          RD_NOOP_4:
            begin
               next_state  = RD_NOOP_5;
               icap_ce     = 0;
               icap_wr     = 0;
               icap_din    = 16'h2000;    //  NOOP
            end

          RD_NOOP_5:
            begin
               next_state  = RD_NOOP_6;
               icap_ce     = 0;
               icap_wr     = 0;
               icap_din    = 16'h2000;    //  NOOP
            end

         RD_NOOP_6:
            begin
               next_state  = RD_AVOID_ABORT_1;
               icap_ce     = 1;
               icap_wr     = 0;
               icap_din    = 16'hffff;    // Dummy Data
            end

         RD_AVOID_ABORT_1:
            begin
               next_state  = RD_AVOID_ABORT_2;
               icap_ce     = 1;
               icap_wr     = 1;
               icap_din    = 16'hffff;    // Dummy Data
            end

         RD_AVOID_ABORT_2:
            begin
               next_state  = RD_LATCH_DATA;
               icap_ce     = 0;
               icap_wr     = 1;
               icap_din    = 16'hffff;    // Dummy Data
            end

         RD_LATCH_DATA:
            begin
               if (busy) begin
                   next_state  = RD_LATCH_DATA;
                   icap_ce     = 0;
                   icap_wr     = 1;
                   icap_din    = 16'hffff;    // Dummy Data
               end else begin
                   next_state  = RD_AVOID_ABORT_3;
                   icap_ce     = 1;
                   icap_wr     = 1;
                   icap_din    = 16'hffff;    // Dummy Data
               end
            end

         RD_AVOID_ABORT_3:
            begin
               next_state  = RD_AVOID_ABORT_4;
               icap_ce     = 1;
               icap_wr     = 0;
               icap_din    = 16'hffff;    // Dummy Data
            end

         RD_AVOID_ABORT_4:
            begin
               next_state  = RD_DESYNC_H;
               icap_ce     = 0;
               icap_wr     = 0;
               icap_din    = 16'h30a1;  // Write to CMD Register
            end

          RD_DESYNC_H:
            begin
               next_state  = RD_DESYNC_L;
               icap_ce     = 0;
               icap_wr     = 0;
               icap_din    = 16'h000d;  // DESYNC command
            end

          RD_DESYNC_L:
            begin
               next_state  = RD_NOOP_7;
               icap_ce     = 0;
               icap_wr     = 0;
               icap_din    = 16'h2000;  // NOOP
            end

          RD_NOOP_7:
            begin
               next_state  = IDLE;
               icap_ce     = 0;
               icap_wr     = 0;
               icap_din    = 16'h2000;  // NOOP
            end

          IDLE:
            begin
               if (reconfigure) begin
                  next_state  = DUMMY_1;
               end else begin
                  next_state  = IDLE;
               end
               icap_ce     = 0;
               icap_wr     = 0;
               icap_din    = 16'hFFFF;  // Null data
            end

          DUMMY_1:
            begin
               next_state  = DUMMY_2;
               icap_ce     = 0;
               icap_wr     = 0;
               icap_din    = 16'hFFFF;  // Null data
            end

          DUMMY_2:
            begin
               next_state  = SYNC_H;
               icap_ce     = 0;
               icap_wr     = 0;
               icap_din    = 16'hAA99;   // Sync word part 1
            end

          SYNC_H:
            begin
               next_state  = SYNC_L;
               icap_ce     = 0;
               icap_wr     = 0;
               icap_din    = 16'h5566;    // Sync word part 2
            end

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

               case (design_num)
                 5'b10000: icap_din    = 16'h0000;
                 5'b00000: icap_din    = 16'h4000;
                 5'b00001: icap_din    = 16'h8000;
                 5'b00010: icap_din    = 16'hC000;
                 5'b00011: icap_din    = 16'h0000;
                 5'b00100: icap_din    = 16'h4000;
                 5'b00101: icap_din    = 16'h8000;
                 5'b00111: icap_din    = 16'h0000;
                 5'b01000: icap_din    = 16'h8000;
                 5'b01001: icap_din    = 16'h8000;
                 5'b01010: icap_din    = 16'h8000;
                 5'b01011: icap_din    = 16'h8000;
                 5'b01100: icap_din    = 16'hC000;
                 5'b01101: icap_din    = 16'hC000;
                 5'b01110: icap_din    = 16'hC000;
                 5'b01111: icap_din    = 16'hC000;
                 default:  icap_din    = 16'h4000;
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

               case (design_num)
                 5'b10000: icap_din    = 16'h0300;
                 5'b00000: icap_din    = 16'h0305;
                 5'b00001: icap_din    = 16'h030a;
                 5'b00010: icap_din    = 16'h030f;
                 5'b00011: icap_din    = 16'h0315;
                 5'b00100: icap_din    = 16'h031a;
                 5'b00101: icap_din    = 16'h0334;
                 5'b00111: icap_din    = 16'h032a;
                 5'b01000: icap_din    = 16'h031f;
                 5'b01001: icap_din    = 16'h031f;
                 5'b01010: icap_din    = 16'h031f;
                 5'b01011: icap_din    = 16'h031f;
                 5'b01100: icap_din    = 16'h0324;
                 5'b01101: icap_din    = 16'h0324;
                 5'b01110: icap_din    = 16'h0324;
                 5'b01111: icap_din    = 16'h0339;
                 default:  icap_din    = 16'h032f; // The Null Co Processor
               endcase

            end

          //--------------------


          GEN2_L:
            begin
               next_state  = GEN5_H;
               icap_ce     = 0;
               icap_wr     = 0;
               icap_din    = 16'h32e1;    //  Write to GENERAL_5 Register....
            end

          GEN5_H:
            begin
               next_state  = GEN5_L;
               icap_ce     = 0;
               icap_wr     = 0;
               icap_din[15:8] = 8'b0;
               icap_din[7:4] = powerup ? sw_in : soft_dip[7:4];
               icap_din[3:0] = powerup ? sw_in : design_num[3:0];
            end


          //--------------------

          GEN5_L:
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
               next_state  = RBT_NOOP_0;
               icap_ce     = 0;
               icap_wr     = 0;
               icap_din    = 16'h2000;    //  RBT_NOOP
            end

          RBT_NOOP_0:
            begin
               next_state  = RBT_NOOP_1;
               icap_ce     = 0;
               icap_wr     = 0;
               icap_din    = 16'h2000;    // RBT_NOOP
            end

          RBT_NOOP_1:
            begin
               next_state  = RBT_NOOP_2;
               icap_ce     = 0;
               icap_wr     = 0;
               icap_din    = 16'h2000;    // RBT_NOOP
            end

          RBT_NOOP_2:
            begin
               next_state  = RBT_NOOP_3;
               icap_ce     = 0;
               icap_wr     = 0;
               icap_din    = 16'h2000;    // RBT_NOOP
            end

          //--------------------

          RBT_NOOP_3:
            begin
               next_state  = IDLE;
               icap_ce     = 0;
               icap_wr     = 0;
               icap_din    = 16'hffff;    // NULL value
            end

          default:
            begin
               next_state  = IDLE;
               icap_ce     = 0;
               icap_wr     = 0;
               icap_din    = 16'hffff;    //  16'h1111"
            end

        endcase
     end


   always@(posedge fastclk) begin
      clk_16M00 = !clk_16M00;
   end

   // Give a bit of delay before starting the state machine
   always @(posedge clk_16M00) begin
      if (MBT_REBOOT == 4'b1111) begin
         state <= next_state;
      end else begin
         MBT_REBOOT <= MBT_REBOOT + 4'b0001;
         state <= INIT;
      end
      if (state == RD_LATCH_DATA) begin
         test_trig <= 1'b1;
      end else begin
         test_trig <= 1'b0;
      end
      if (state == RD_LATCH_DATA && !busy) begin
         soft_dip <= icap_dout[7:0];
      end
   end


   always @(posedge clk_16M00) begin:   ICAP_FF
        // need to reverse bits to ICAP module since D0 bit is read first
        ff_icap_din_reversed[0]  <= icap_din[7];
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

   always @(icap_dout_reversed) begin
      // need to reverse bits to ICAP module since D0 bit is read first
      icap_dout[0]  <= icap_dout_reversed[7];
      icap_dout[1]  <= icap_dout_reversed[6];
      icap_dout[2]  <= icap_dout_reversed[5];
      icap_dout[3]  <= icap_dout_reversed[4];
      icap_dout[4]  <= icap_dout_reversed[3];
      icap_dout[5]  <= icap_dout_reversed[2];
      icap_dout[6]  <= icap_dout_reversed[1];
      icap_dout[7]  <= icap_dout_reversed[0];
      icap_dout[8]  <= icap_dout_reversed[15];
      icap_dout[9]  <= icap_dout_reversed[14];
      icap_dout[10] <= icap_dout_reversed[13];
      icap_dout[11] <= icap_dout_reversed[12];
      icap_dout[12] <= icap_dout_reversed[11];
      icap_dout[13] <= icap_dout_reversed[10];
      icap_dout[14] <= icap_dout_reversed[9];
      icap_dout[15] <= icap_dout_reversed[8];
   end

endmodule
