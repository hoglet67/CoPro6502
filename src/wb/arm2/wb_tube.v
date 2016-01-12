//----------------------------------------------------------------------------
// Wishbone Tube controller
//----------------------------------------------------------------------------
module wb_tube #(
        parameter                  latency   = 0    // 0 .. 7
) (
        input                      clk,
        input                      reset,
        // Wishbone interface
        input                      wb_stb_i,
        input                      wb_cyc_i,
        output reg                 wb_ack_o,
        input                      wb_we_i,
        input                [2:0] wb_adr_i,
        input                [3:0] wb_sel_i,
        input               [31:0] wb_dat_i,
        output reg          [31:0] wb_dat_o,

        // TUBE connection
        output reg           [2:0] tube_adr,
        inout                [7:0] tube_dat,
        output reg                 tube_cs_n,    // Chip Select
        output reg                 tube_rd_n,    // Read
        output reg                 tube_wr_n     // Write
);

//----------------------------------------------------------------------------
//
//----------------------------------------------------------------------------

// Wishbone handling
wire wb_rd = wb_stb_i & wb_cyc_i & ~wb_we_i & ~wb_ack_o;
wire wb_wr = wb_stb_i & wb_cyc_i &  wb_we_i & ~wb_ack_o;

// Tri-State-Driver
reg [7:0] wdat;
reg       wdat_oe;

assign tube_dat = wdat_oe ? wdat : 8'bz;


// Latency countdown
reg  [2:0] lcount;

//----------------------------------------------------------------------------
// State Machine
//----------------------------------------------------------------------------
parameter s_idle   = 0;
parameter s_read   = 1;
parameter s_write  = 2;

reg [2:0] state;

always @(posedge clk)
begin
        if (reset) begin
                state    <= s_idle;
                lcount   <= 0;
                wb_ack_o <= 0;
        end else begin
                case (state)
                s_idle: begin
                        wb_ack_o <= 0;

                        if (wb_rd) begin
                                tube_cs_n  <=  0;
                                tube_rd_n  <=  0;
                                tube_wr_n  <=  1;
                                tube_adr   <=  wb_adr_i;
                                wdat_oe    <=  0;
                                lcount     <=  latency;
                                state      <=  s_read;
                        end else if (wb_wr) begin
                                tube_cs_n  <=  0;
                                tube_rd_n  <=  1;
                                tube_wr_n  <=  0;
                                tube_adr   <=  wb_adr_i;
                                wdat       <=  wb_dat_i[7:0];
                                wdat_oe    <=  1;
                                lcount     <=  latency;
                                state      <=  s_write;
                        end else begin
                                tube_cs_n  <=  1;
                                tube_rd_n  <=  1;
                                tube_wr_n  <=  1;
                                wdat_oe    <=  0;
                        end
                end
                s_read: begin
                        if (lcount != 0) begin
                                lcount     <= lcount - 1;
                        end else begin
                                tube_cs_n  <=  1;
                                tube_rd_n  <=  1;
                                tube_wr_n  <=  1;
                                wb_dat_o   <=  tube_dat;
                                wb_ack_o   <=  1;
                                state      <=  s_idle;
                        end
                end
                s_write: begin
                        if (lcount != 0) begin
                                lcount     <= lcount - 1;
                        end else begin
                                tube_cs_n  <=  1;
                                tube_rd_n  <=  1;
                                tube_wr_n  <=  1;
                                wb_ack_o   <=  1;       // XXX   We could acknoledge write  XXX
                                state      <=  s_idle;  // XXX   requests 1 cycle ahead     XXX
                        end
                end
                endcase
        end
end

endmodule
