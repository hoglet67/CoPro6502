module LX9CoProARM2 (

        input fastclk,
        
        // GOP Signals
        inout [8:1] test,
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
        output ram_ub_b,
        output ram_lb_b,
        output ram_cs,
        output ram_oe,
        output ram_wr,
        output [18:0] ram_addr,
        inout  [31:0] ram_data
);

  // Registers and nets
  wire        clk;
  wire [31:0] dat_o;
  wire [31:0] dat_i;
  wire [31:0] adr;
  wire        we;
  wire [ 3:0] sel;
  wire        stb;
  wire        cyc;
  wire        ack;

  // wires to BIOS ROM
  wire [31:0] rom_dat_o;
  wire [31:0] rom_dat_i;
  wire [31:0] rom_adr_i;
  wire [ 3:0] rom_sel_i;
  wire        rom_we_i;
  wire        rom_cyc_i;
  wire        rom_stb_i;
  wire        rom_ack_o;
  
  // wires to RAM
  wire [31:0] ram_dat_o;
  wire [31:0] ram_dat_i;
  wire [31:0] ram_adr_i;
  wire [ 3:0] ram_sel_i;
  wire        ram_we_i;
  wire        ram_cyc_i;
  wire        ram_stb_i;
  wire        ram_ack_o;

  // wires to Tube
  wire [31:0] tube_dat_o;
  wire [31:0] tube_dat_i;
  wire [31:0] tube_adr_i;
  wire [ 3:0] tube_sel_i;
  wire        tube_we_i;
  wire        tube_cyc_i;
  wire        tube_stb_i;
  wire        tube_ack_o;

  // wires to default slave
  wire        def_cyc_i;
  wire        def_stb_i;
  
  wire [2:0]  p_addr;
  wire        p_cs_b; 
  wire [7:0]  p_data; 
  wire        p_rd_b; 
  wire        p_wr_b;
  wire        p_rst_b;
  wire        p_nmi_b;
  wire        p_irq_b;

  wire        irq;
  wire        nmi;
  
  reg         gsr0;
  reg         gsr1;
  reg         gsr2;
  reg         tubeint0;
  reg         tubeint1;
  reg         tubeint2;
  reg         tubenmi0;
  reg         tubenmi1;
  reg         tubenmi2;
  
  reg [8:0]   reset_counter;

  wire [3:0]  sw_out;
   
//  ICAP_config inst_ICAP_config (
//    .fastclk(fastclk),
//    .sw_in  (sw),
//    .sw_out (sw_out),
//    .h_addr (h_addr),
//    .h_cs_b (h_cs_b),
//    .h_data (h_data),
//    .h_phi2 (h_phi2),
//    .h_rdnw (h_rdnw),
//    .h_rst_b(h_rst_b)
//  );
   
  
  dcm_32_16 inst_dcm (
    .CLKIN_IN(fastclk), 
    .CLK0_OUT(clk), 
    .CLK0_OUT1(), 
    .CLK2X_OUT()
  );

  // Ensure reset is held active for 256 clock cycles on power up
  // Needed as Beeb's reset is missed when using multiboot loader as initialization takes too long
  always @(posedge clk)
    begin
      gsr0 <= !p_rst_b;
      gsr1 <= gsr0;
      gsr2 <= gsr1 && !gsr0;
      if (reset_counter[8] == 0)
          reset_counter <= reset_counter + 1;
  end
    
  wire rst;
  assign rst = !p_rst_b | !reset_counter[8];

  STARTUP_SPARTAN6 startup_inst (
    .CFGCLK(),
    .CFGMCLK(),                                 
    .EOS(),
    .CLK(),
    .GSR(gsr2),
    .GTS(),
    .KEYCLEARB()
  );

  bootrom bootrom (
    .clk (clk),            // Wishbone slave interface
    .rst (rst),
    .wb_dat_i (rom_dat_i),
    .wb_dat_o (rom_dat_o),
    .wb_adr_i (rom_adr_i),
    .wb_we_i  (rom_we_i ),
    .wb_stb_i (rom_stb_i),
    .wb_cyc_i (rom_cyc_i),
    .wb_sel_i (rom_sel_i),
    .wb_ack_o (rom_ack_o)
  );
  
wb_sram32 wb_sram32 (
    .clk(clk), 
    .reset(rst), 
    .wb_dat_i(ram_dat_i), 
    .wb_dat_o(ram_dat_o), 
    .wb_adr_i(ram_adr_i), 
    .wb_we_i(ram_we_i),
    .wb_stb_i(ram_stb_i), 
    .wb_cyc_i(ram_cyc_i), 
    .wb_sel_i(ram_sel_i), 
    .wb_ack_o(ram_ack_o),
    
    .sram_adr(ram_addr), 
    .sram_dat(ram_data), 
    .sram_be_n({ram_ub_b, ram_lb_b}), 
    .sram_ce_n(ram_cs), 
    .sram_oe_n(ram_oe), 
    .sram_we_n(ram_wr)
    );

wb_tube wb_tube_inst(
    .clk(clk), 
    .reset(rst), 
    .wb_stb_i(tube_stb_i), 
    .wb_cyc_i(tube_cyc_i), 
    .wb_ack_o(tube_ack_o), 
    .wb_we_i(tube_we_i),
    .wb_adr_i(tube_adr_i[4:2]), 
    .wb_sel_i(tube_sel_i), 
    .wb_dat_i(tube_dat_i), 
    .wb_dat_o(tube_dat_o), 
    .tube_adr(p_addr), 
    .tube_dat(p_data), 
    .tube_cs_n(p_cs_b), 
    .tube_rd_n(p_rd_b), 
    .tube_wr_n(p_wr_b)
    );
    
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
    .p_data(p_data),
    .p_rdnw(p_wr_b), 
    .p_phi2(clk), 
    .p_rst_b(p_rst_b), 
    .p_nmi_b(p_nmi_b), 
    .p_irq_b(p_irq_b)
    );

// Instantiate the module
a23_core instance_name (
    .i_clk(clk), 
    .i_irq(irq), 
    .i_firq(nmi), 
    .i_system_rdy(1'b1), 
    .o_wb_adr(adr), 
    .o_wb_sel(sel), 
    .o_wb_we(we), 
    .i_wb_dat(dat_i), 
    .o_wb_dat(dat_o), 
    .o_wb_cyc(cyc), 
    .o_wb_stb(stb), 
    .i_wb_ack(ack), 
    .i_wb_err(1'b0) 
    );
    
  wb_switch #(
    .s0_addr_1 (32'b0000_0000_0000_0000_0000_0000_0000_0000), // ROM  at 0x00000000-0x00000003
    .s0_mask_1 (32'b0000_0011_1111_1111_1111_1111_1111_1100),

    .s0_addr_2 (32'b0000_0011_0000_0000_0000_0000_0000_0000), // ROM  at 0x03000000-0x03ffffff
    .s0_mask_2 (32'b0000_0011_0000_0000_0000_0000_0000_0000),

    .s1_addr_1 (32'b0000_0001_0000_0000_0000_0000_0000_0000), // Tube at 0x01000000-0x01ffffff
    .s1_mask_1 (32'b0000_0011_0000_0000_0000_0000_0000_0000),

    .s2_addr_1 (32'b0000_0000_0000_0000_0000_0000_0000_0000), // RAM  at 0x00000000-0x001fffff
    .s2_mask_1 (32'b0000_0011_1110_0000_0000_0000_0000_0000)

    ) wbs (

    // Master interface
    .m_dat_i (dat_o),
    .m_dat_o (dat_i),
    .m_adr_i (adr),
    .m_sel_i (sel),
    .m_we_i  (we),
    .m_cyc_i (cyc),
    .m_stb_i (stb),
    .m_ack_o (ack),

    // Slave 0 interface - ROM
    .s0_dat_i (rom_dat_o),
    .s0_dat_o (rom_dat_i),
    .s0_adr_o (rom_adr_i),
    .s0_sel_o (rom_sel_i),
    .s0_we_o  (rom_we_i),
    .s0_cyc_o (rom_cyc_i),
    .s0_stb_o (rom_stb_i),
    .s0_ack_i (rom_ack_o),

    // Slave 1 interface - Tube
    .s1_dat_i (tube_dat_o),
    .s1_dat_o (tube_dat_i),
    .s1_adr_o (tube_adr_i),
    .s1_sel_o (tube_sel_i),
    .s1_we_o  (tube_we_i),
    .s1_cyc_o (tube_cyc_i),
    .s1_stb_o (tube_stb_i),
    .s1_ack_i (tube_ack_o),

     // Slave 2 interface - RAM
    .s2_dat_i (ram_dat_o),
    .s2_dat_o (ram_dat_i),
    .s2_adr_o (ram_adr_i),
    .s2_sel_o (ram_sel_i),
    .s2_we_o  (ram_we_i),
    .s2_cyc_o (ram_cyc_i),
    .s2_stb_o (ram_stb_i),
    .s2_ack_i (ram_ack_o),

    .s3_dat_i (32'haaaaaaaa),
    .s3_dat_o (),
    .s3_adr_o (),
    .s3_sel_o (),
    .s3_we_o  (),
    .s3_cyc_o (def_cyc_i),
    .s3_stb_o (def_stb_i),
    .s3_ack_i (def_cyc_i & def_stb_i)
  );

  always @(posedge clk) begin
     if (rst) begin
         tubeint0 <= 0;
         tubeint1 <= 0;
         tubeint2 <= 0;
         tubenmi0 <= 0;
         tubenmi1 <= 0;
         tubenmi2 <= 0;
     end else begin
         tubeint0 <= ~p_irq_b;
         tubeint1 <= tubeint0;
         tubeint2 <= tubeint1;
         tubenmi0 <= ~p_nmi_b;
         tubenmi1 <= tubenmi0;
         tubenmi2 <= tubenmi1;
     end     
  end

  assign nmi      = tubenmi2;
  assign irq      = tubeint2;
  assign h_irq_b  = 1;

  assign test[8] = rst;
  assign test[7] = irq;
  assign test[6] = nmi;
  assign test[5] = stb;
  assign test[4] = we;
  assign test[3] = (adr[25:24] == 2'b11) ? 1 : 0;
  assign test[2] = (adr[25:24] == 2'b01) ? 1 : 0;
  assign test[1] = (adr[25:24] == 2'b00) ? 1 : 0;
  //assign test[3] = ({6'b0, adr[25:2], 2'b0} == 32'h03001858) ? 1 : 0; 
  //assign test[2] = ({6'b0, adr[25:2], 2'b0} == 32'h03001848) ? 1 : 0; 
  //assign test[1] = ({6'b0, adr[25:2], 2'b0} == 32'h03001824) ? 1 : 0; 
  
  // default to hi-impedence, to avoid conflicts with
  // a Raspberry Pi connected to the test connector
  // assign test = 8'bZ;

endmodule
