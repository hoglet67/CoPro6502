module LX9CoPro32016 (
    input fastclk,

    // GOP Signals
    output[8:1] test,
    input [3:0] sw,

    // Tube signals
    input h_phi2,
    input [2:0] h_addr,
    inout [7:0] h_data,
    input h_rdnw,
    input h_cs_b,
    input h_rst_b,
    output h_irq_b,

    // Ram Signals
    output reg ram_ub_b,
    output reg ram_lb_b,
    output reg ram_cs,
    output reg ram_oe,
    output reg ram_wr,
    output reg [18:0] ram_addr,
    inout  [31:0] ram_data
);

    wire        clk;
    reg         rst_reg;
    reg         nmi_reg;
    reg         irq_reg;

    wire [2:0]  p_addr;
    wire        p_cs_b;
    wire [7:0]  p_data_in;
    wire [7:0]  p_data_out;
    wire        p_rd_b;
    wire        p_wr_b;
    wire        p_rst_b;
    wire        p_nmi_b;
    wire        p_irq_b;

    wire        IO_WR;
    wire        IO_RD;
    wire [31:0] IO_A;
    wire  [3:0] IO_BE;
    wire [31:0] IO_DI;
    wire [31:0] IO_Q;
    wire        IO_READY;

    wire        ram_enable;
    wire        rom_enable;
    wire        tube_enable;
    //wire        config_enable;

    reg  [31:0] ram_dout;
    wire [31:0] rom_dout;

    reg         bootmode;
    reg         rd_rdy;

    wire [3:0]  status;
    wire [7:0]  statsigs;
    wire fetchc;
    wire fetchd;

//    dcm_32_16 inst_dcm (
//        .CLKIN_IN(fastclk),
//        .CLK0_OUT(clk),
//        .CLK0_OUT1(),
//        .CLK2X_OUT()
//    );

    assign clk = fastclk;
    

//    reg         gsr0;
//    reg         gsr1;
//    reg         gsr2;
//    always @(posedge clk)
//        begin
//           gsr0 <= !p_rst_b;
//           gsr1 <= gsr0;
//           gsr2 <= gsr1 && !gsr0;
//        end
//
//    STARTUP_SPARTAN6 startup_inst (
//        .CFGCLK(),
//        .CFGMCLK(),
//        .EOS(),
//        .CLK(),
//        .GSR(gsr2),
//        .GTS(),
//        .KEYCLEARB()
//      );

    M32632 cpu (

        // ++++++++++ Basic Signals
        .BCLK(clk),                 // input
        .MCLK(~clk),                // input
        .WRCFG(1'b1),               // input
        .BRESET(rst_reg),           // input
        .NMI_N(nmi_reg),            // input
        .INT_N(irq_reg),            // input
        .STATUS(status),            // output
        .ILO(),                     // output
        .STATSIGS(statsigs),        // output

        // +++++++++ General Purpose Interface
        .IO_WR(IO_WR),              // output
        .IO_RD(IO_RD),              // output
        .IO_A(IO_A),                // output
        .IO_BE(IO_BE),              // output
        .IO_DI(IO_DI),              // output
        .IO_Q(IO_Q),                // input
        .IO_READY(IO_READY),        // input

        // +++++++++ DRAM Interface In
        .ENDRAM(1'b0),              // input
        .IC_MDONE(1'b0),            // input
        .DC_MDONE(1'b0),            // input
        .ENWR(1'b0),                // input
        .WAMUX(1'b0),               // input
        .WADDR(10'b0),              // input
        .DRAM_Q(32'b0),             // input
        .DWCTRL(3'b0),              // input
        .IWCTRL(3'b0),              // input

        // +++++++++ DRAM Interface Out
        .IC_ACC(),
        .IDRAM_ADR(),               // output
        .DC_ACC(),                  // output
        .DC_WR(),                   // output
        .DRAM_ADR(),                // output
        .DRAM_DI(),                 // output

        // +++++++++ DMA Interface
        .HOLD(1'b1),                // input
        .HLDA(),                    // output
        .FILLRAM(1'b0),             // input
        .DMA_AA(24'b0),             // input

        // ++++++++++ Coprocessor Interface
        .COP_GO(),                  // output
        .COP_OP(),                  // output
        .COP_OUT(),                 // output
        .COP_DONE(1'b0),            // input
        .COP_IN(64'b0)              // input
    );

    assign IO_Q = ram_enable    ? ram_dout :
                  rom_enable    ? rom_dout :
                  tube_enable   ? {p_data_out, p_data_out, p_data_out, p_data_out} :
                  32'b0;

   //               config_enable ? 32'b0 :
   //               32'hAAAAAAAA;

    // Memory Map during booting
    // 000000-FFFFFF ROM (32KB, repeating)

    // Memory Map after booting
    // 000000-1FFFFF RAM (2MB)
    // F00000-F3FFFF ROM (32KB, repeating)
    // F90000        Config Switches (A is bit 7, H is bit 0, 1=present)
    //               A - RSVD;  B - RSVD;  C - RSVD;  D - RSVD;
    //               E - RSVD;  F - RSVD;  G - MMU;   H - FPU;
    // FFFFF0-FFFFFE Tube (even bytes)

    assign rom_enable    = (IO_RD)         & ( bootmode | (IO_A[23:18] == 6'b111100));
    assign ram_enable    = (IO_RD | IO_WR) & (!bootmode & (IO_A[23:21] == 3'b000));
    assign tube_enable   = (IO_RD | IO_WR) & (!bootmode & (IO_A[23: 4] == 20'hFFFFF));
    assign config_enable = (IO_RD)         & (!bootmode & (IO_A[23: 4] == 20'hF9000));

    // Internal ROM 8Kx32 bits

    tuberom_32016 rom(
        .clk(clk),
        .addr(IO_A[14:2]),
        .data(rom_dout)
    );

    // External RAM
    // in spite of the naming, these are all active low

    //assign ram_ub_b = 1'b0;
    //assign ram_lb_b = 1'b0;
    //assign ram_cs   = !(ram_enable);
    //assign ram_oe   = !(ram_enable & IO_RD);
    //assign ram_wr   = !(ram_enable & IO_WR);
    //assign ram_addr = IO_A[20:2];
    //assign ram_dout = ram_data;
    //assign ram_data = (ram_enable & IO_WR) ? IO_DI : 32'bz;

    // Tube
    assign p_data_in = IO_A[1] ? IO_DI[23:16] : IO_DI[7:0];
    assign p_cs_b = !tube_enable;
    assign p_addr = IO_A[3:1];
    assign p_wr_b = !IO_WR;

    tube tube_inst(
        .h_addr(h_addr),
        .h_cs_b(h_cs_b),
        .h_data(h_data),
        .h_phi2(h_phi2),
        .h_rdnw(h_rdnw),
        .h_rst_b(h_rst_b),
        .h_irq_b(),
        .p_addr(p_addr),
        .p_cs_b(p_cs_b),
        .p_data_in(p_data_in),
        .p_data_out(p_data_out),
        .p_rdnw(p_wr_b),
        .p_phi2(clk),
        .p_rst_b(p_rst_b),
        .p_nmi_b(p_nmi_b),
        .p_irq_b(p_irq_b)
    );

    always @(posedge clk)
        begin
            rst_reg  <= p_rst_b;
            nmi_reg  <= p_nmi_b;
            irq_reg  <= p_irq_b;
            rd_rdy   <= IO_RD & ~rd_rdy;
            if (!rst_reg)
                bootmode <= 1'b1;
            else if (IO_RD & (IO_A[23:18] == 6'b111100))
                bootmode <= 1'b0;
        end

    assign IO_READY = ram_enable ? ram_rdy :
                      (rom_enable | (tube_enable & (IO_A[3:1] != 3'b101)))  ? (IO_WR | rd_rdy) :
                      (IO_WR | IO_RD);

//  LOAD doesn't work in this version, because reads take two cycles 
//  assign IO_READY = ram_enable ? ram_rdy : (IO_WR | rd_rdy);

    assign h_irq_b  = 1;


    //----------------------------------------------------------------------------
    // State Machine machine performing read-modify-write cycles
    //----------------------------------------------------------------------------

    parameter rd_latency          = 1;    // 0 .. 7, must be odd or m32632 messes up
    parameter wr_latency          = 1;    // 0 .. 7
    parameter rmw_rd_latency      = 1;    // 0 .. 7
    parameter rmw_wr_latency      = 1;    // 0 .. 7

    parameter s_idle              = 0;
    parameter s_read              = 1;
    parameter s_read_modify_write = 2;
    parameter s_write             = 3;
    parameter s_done              = 4;

    reg [2:0]  state;
    reg        ram_rdy;

    // Latency countdown
    reg [2:0]  lcount;

    // Tri-State-Driver
    reg [31:0] wdat;
    reg        wdat_oe;  
    assign ram_data = wdat_oe ? wdat : 32'bz;

    // Merged data for byte enables writes
    wire [31:0] merged_dat = {(IO_BE[3] ? IO_DI[31:24] : ram_data[31:24]),
                              (IO_BE[2] ? IO_DI[23:16] : ram_data[23:16]),
                              (IO_BE[1] ? IO_DI[15: 8] : ram_data[15: 8]),
                              (IO_BE[0] ? IO_DI[ 7: 0] : ram_data[ 7: 0])};
   
    always @(posedge clk) begin
        if (~rst_reg) begin
            state   <= s_idle;
            lcount  <= 0;
            ram_rdy <= 0;
        end else begin
            case (state)
            s_idle: begin
                ram_rdy <= 0;
                if (ram_enable & IO_RD) begin
                    ram_cs     <= 0;
                    ram_oe     <= 0;
                    ram_wr     <= 1;
                    ram_addr   <= IO_A[20:2];
                    ram_ub_b   <= 0;
                    ram_lb_b   <= 0;
                    wdat_oe    <= 0;
                    lcount     <= rd_latency;
                    state      <= s_read;
                end else if (ram_enable & IO_WR & (IO_BE == 4'b1111)) begin
                    ram_cs     <= 0;
                    ram_oe     <= 1;
                    ram_wr     <= 0;
                    ram_addr   <= IO_A[20:2];
                    ram_ub_b   <= 0;
                    ram_lb_b   <= 0;
                    wdat       <= IO_DI;
                    wdat_oe    <= 1;
                    lcount     <= wr_latency;
                    state      <= s_write;
                end else if (ram_enable & IO_WR) begin
                    ram_cs     <= 0;
                    ram_oe     <= 0;
                    ram_wr     <= 1;
                    ram_addr   <= IO_A[20:2];
                    ram_ub_b   <= 0;
                    ram_lb_b   <= 0;
                    wdat_oe    <= 0;
                    lcount     <= rmw_rd_latency;
                    state      <= s_read_modify_write;
                end else begin
                    ram_cs     <= 1;
                    ram_oe     <= 1;
                    ram_wr     <= 1;
                    wdat_oe    <= 0;
                end
            end
            s_read: begin
                if (lcount != 0) begin
                    lcount     <= lcount - 1;
                end else begin
                    ram_cs     <= 1;
                    ram_oe     <= 1;
                    ram_wr     <= 1;
                    ram_dout   <= ram_data;
                    ram_rdy    <= 1;
                    state      <= s_done;
                end
            end
            s_read_modify_write: begin
                if (lcount != 0) begin
                    lcount     <= lcount - 1;
                end else begin
                    ram_cs     <= 0;
                    ram_oe     <= 1;
                    ram_wr     <= 0;
                    ram_addr   <= IO_A[20:2];
                    ram_ub_b   <= 0;
                    ram_lb_b   <= 0;
                    wdat       <= merged_dat;
                    wdat_oe    <= 1;
                    lcount     <= rmw_wr_latency;
                    state      <= s_write;
                end
            end
            s_write: begin
                if (lcount != 0) begin
                    lcount     <= lcount - 1;
                end else begin
                    ram_cs     <= 1;
                    ram_oe     <= 1;
                    ram_wr     <= 1;
                    ram_rdy    <= 1;       // XXX   We could acknoledge write  XXX
                    state      <= s_done;  // XXX   requests 1 cycle ahead     XXX
                end
            end
            s_done: begin
                ram_rdy    <= 0;
                state      <= s_idle;
            end            
            endcase
        end
    end

    //----------------------------------------------------------------------------
    // Test outputs
    //----------------------------------------------------------------------------

    // default to hi-impedence, to avoid conflicts with
    // a Raspberry Pi connected to the test connector

    assign fetchc = IO_RD & (status == 4'b1000);
    assign fetchd = IO_RD & (status == 4'b1010);

    assign test = sw[3] ? {rst_reg, fetchc, fetchd, bootmode,  status} :
                  sw[2] ? {rst_reg, fetchc, fetchd, IO_A[14:10]} :
                  sw[1] ? {rst_reg, ram_enable, IO_RD, IO_WR, ram_cs, ram_oe, ram_wr, ram_rdy} :
                  sw[0] ? {rst_reg, tube_enable, p_cs_b, p_wr_b, p_data_in[3:0]} :
                          {p_irq_b, p_nmi_b, bootmode, IO_RD, IO_WR, ram_enable, rom_enable, tube_enable};

endmodule
