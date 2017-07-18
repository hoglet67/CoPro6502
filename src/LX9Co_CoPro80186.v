/*
 *  Zet SoC top level file for Altera DE1 board
 *  Copyright (C) 2009, 2010  Zeus Gomez Marmolejo <zeus@aluzina.org>
 *
 *  This file is part of the Zet processor. This processor is free
 *  hardware; you can redistribute it and/or modify it under the terms of
 *  the GNU General Public License as published by the Free Software
 *  Foundation; either version 3, or (at your option) any later version.
 *
 *  Zet is distrubuted in the hope that it will be useful, but WITHOUT
 *  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
 *  License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with Zet; see the file COPYING. If not, see
 *  <http://www.gnu.org/licenses/>.
 */

module LX9CoPro80186 (

        input fastclk,
        
        // GOP Signals
        inout [8:1] test,
        input [3:0] sw,
        
        // Tube signals (use 16 out of 22 DIL pins)
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
        inout  [15:0] ram_data
);

  // Registers and nets
  wire        clk;
  wire        rst_lck;
  wire [15:0] dat_o;
  wire [15:0] dat_i;
  wire [19:1] adr;
  wire        we;
  wire        tga;
  wire [ 1:0] sel;
  wire        stb;
  wire        cyc;
  wire        ack;
  wire        lock;

  // wires to BIOS ROM
  wire [15:0] rom_dat_o;
  wire [15:0] rom_dat_i;
  wire        rom_tga_i;
  wire [19:1] rom_adr_i;
  wire [ 1:0] rom_sel_i;
  wire        rom_we_i;
  wire        rom_cyc_i;
  wire        rom_stb_i;
  wire        rom_ack_o;
  
    // wires to RAM
  wire [15:0] ram_dat_o;
  wire [15:0] ram_dat_i;
  wire        ram_tga_i;
  wire [19:1] ram_adr_i;
  wire [ 1:0] ram_sel_i;
  wire        ram_we_i;
  wire        ram_cyc_i;
  wire        ram_stb_i;
  wire        ram_ack_o;

  wire [18:0] ram_addr_int;

  // wires to Tube
  wire [15:0] tube_dat_o;
  wire [15:0] tube_dat_i;
  wire        tube_tga_i;
  wire [19:1] tube_adr_i;
  wire [ 1:0] tube_sel_i;
  wire        tube_we_i;
  wire        tube_cyc_i;
  wire        tube_stb_i;
  wire        tube_ack_o;


  wire        drq;
  wire        dack_b;
  
  wire [2:0]  p_addr;
  wire        p_cs_b; 
  wire [7:0]  p_data; 
  wire        p_rd_b; 
  wire        p_wr_b;
  wire        p_rst_b;
  wire        p_nmi_b;
  wire        p_irq_b;


  // unused slaves
  wire        s3_cyc_i;
  wire        s3_stb_i;
  wire        s4_cyc_i;
  wire        s4_stb_i;
  wire        s5_cyc_i;
  wire        s5_stb_i;
  wire        s6_cyc_i;
  wire        s6_stb_i;
  wire        s7_cyc_i;
  wire        s7_stb_i;
  wire        s9_cyc_i;
  wire        s9_stb_i;
  wire        sa_cyc_i;
  wire        sa_stb_i;
  wire        def_cyc_i;
  wire        def_stb_i;


  // wires to default stb/ack
  wire [15:0] sw_dat_o;

  wire [ 7:0] intv;
  wire [ 2:0] iid;
  wire        intr;
  wire        inta;

  wire        nmi;
  wire        nmia;
  
  reg tubeint0;
  reg tubeint1;
  reg tubeint2;
  reg tubeint3;
  reg tubenmi0;
  reg tubenmi1;
  reg tubenmi2;
  reg tubenmi3;
  
  wire [19:0] pc;
  wire trigger;
  reg [8:0] reset_counter;

  wire [3:0] sw_out;
   
  ICAP_config inst_ICAP_config (
    .fastclk(fastclk),
    .sw_in  (sw),
    .sw_out (sw_out),
    .h_addr (h_addr),
    .h_cs_b (h_cs_b),
    .h_data (h_data),
    .h_phi2 (h_phi2),
    .h_rdnw (h_rdnw),
    .h_rst_b(h_rst_b)
  );
   
  
  dcm_32_12 inst_dcm (
    .CLKIN_IN(fastclk), 
    .CLK0_OUT(clk), 
    .CLK0_OUT1(), 
    .CLK2X_OUT()
  );

  // Ensure reset is held active for 256 clock cycles on power up
  // Needed as Beeb's reset is missed when using multiboot loader as initialization takes too long
  always @(posedge clk)
  begin
      if (reset_counter[8] == 0)
          reset_counter <= reset_counter + 1;
  end
    
  wire rst;
  assign rst = !p_rst_b | !reset_counter[8];


  bootrom bootrom (
    .clk (clk),            // Wishbone slave interface
    .rst (rst),
    .wb_dat_i (rom_dat_i),
    .wb_dat_o (rom_dat_o),
    .wb_adr_i (rom_adr_i),
    .wb_we_i  (rom_we_i ),
    .wb_tga_i (rom_tga_i),
    .wb_stb_i (rom_stb_i),
    .wb_cyc_i (rom_cyc_i),
    .wb_sel_i (rom_sel_i),
    .wb_ack_o (rom_ack_o)
  );
  
wb_sram16 wb_sram16 (
    .clk(clk), 
    .reset(rst), 
    .wb_dat_i(ram_dat_i), 
    .wb_dat_o(ram_dat_o), 
    .wb_adr_i(ram_adr_i), 
    .wb_we_i(ram_we_i),
    .wb_tga_i(ram_tga_i),
    .wb_stb_i(ram_stb_i), 
    .wb_cyc_i(ram_cyc_i), 
    .wb_sel_i(ram_sel_i), 
    .wb_ack_o(ram_ack_o),
    
    .sram_adr(ram_addr_int), 
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
    .wb_tga_i(tube_tga_i),
    .wb_adr_i(tube_adr_i[3:1]), 
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

//  simple_pic pic0 (
//    .clk  (clk),
//    .rst  (rst),
//    .intv (intv),
//    .inta (inta),
//    .intr (intr),
//    .iid  (iid)
//  );


  zet zet (
    .pc (pc),

    // Wishbone master interface
    .wb_clk_i (clk),
    .wb_rst_i (rst),
    .wb_dat_i (dat_i),
    .wb_dat_o (dat_o),
    .wb_adr_o (adr),
    .wb_we_o  (we),
    .wb_tga_o (tga),
    .wb_sel_o (sel),
    .wb_stb_o (stb),
    .wb_cyc_o (cyc),
    .wb_ack_i (ack),
    .wb_tgc_i (intr),
    .wb_tgc_o (inta),
    .nmi      (nmi),
    .nmia     (nmia)
  );


  // Interrupt Control Registers (0x20-0x3E)
  // &0FF22 - EOI Register
  // &0FF38 - INT0 Control Register                 

  // Timer Control Registers (0x50-0x66)
  // &0FF52                  
  // &0FF56                  
  // &0FF60                  
  // &0FF62                  
  // &0FF66

  // Chip Select Control Registers (0xA0-0xA8)   
  // &0FFA0                  
  // &0FFA2                  
  // &0FFA4                  
  // &0FFA6                  
  // &0FFA8                  

  // DMA Channel 0 (0xC0 - 0xCA)
  // &0FFC0                  
  // &0FFC2                  
  // &0FFC4                  
  // &0FFC6                  
  // &0FFCA
  
  wb_switch #(
    .s0_addr_1 (20'b0_1111_0000_0000_0000_000), // bios boot mem 0xf0000 - 0xfffff
    .s0_mask_1 (20'b1_1111_0000_0000_0000_000), // bios boot ROM Memory

    .s1_addr_1 (20'b0_0000_0000_0000_0000_000), // mem 0x00000-0x7ffff
    .s1_mask_1 (20'b1_1000_0000_0000_0000_000), // main memory

    .s1_addr_2 (20'b0_1000_0000_0000_0000_000), // mem 0x80000-0xbffff
    .s1_mask_2 (20'b1_1100_0000_0000_0000_000), // main memory

    .s1_addr_3 (20'b0_1100_0000_0000_0000_000), // mem 0xC0000-0xDffff
    .s1_mask_3 (20'b1_1110_0000_0000_0000_000), // main memory

    .s1_addr_4 (20'b0_1110_0000_0000_0000_000), // mem 0xE0000-0xEffff
    .s1_mask_4 (20'b1_1111_0000_0000_0000_000), // main memory

    .s2_addr_1 (20'b1_0000_1111_1111_0010_000), // io 0xFF20 - 0xFF3E
    .s2_mask_1 (20'b1_0000_1111_1111_1110_000), // Interrupt Control Registers

    .s3_addr_1 (20'b1_0000_1111_1111_0101_000), // io 0xFF50 - 0xFF56
    .s3_mask_1 (20'b1_0000_1111_1111_1111_100), // Timer Control 0 Registers

    .s4_addr_1 (20'b1_0000_1111_1111_0101_100), // io 0xFF58 - 0xFF5E
    .s4_mask_1 (20'b1_0000_1111_1111_1111_100), // Timer Control 1 Registers

    .s5_addr_1 (20'b1_0000_1111_1111_0110_000), // io 0xFF60 - 0xFF66
    .s5_mask_1 (20'b1_0000_1111_1111_1111_100), // Timer Control 2 Registers
	      
    .s6_addr_1 (20'b1_0000_1111_1111_1010_000), // io 0xFFA0 - 0xFFAF
    .s6_mask_1 (20'b1_0000_1111_1111_1111_000), // Chip Select Control Registers

    .s7_addr_1 (20'b1_0000_1111_1111_1100_000), // io 0xFFC0 - 0xFFCf
    .s7_mask_1 (20'b1_0000_1111_1111_1111_000), // DMA Channel 0

    .s8_addr_1 (20'b1_0000_0000_0000_1000_000), // io 0x0080 - 0x008E
    .s8_mask_1 (20'b1_0000_1111_1111_1111_000), // Tube ULA

    .s9_addr_1 (20'b1_0000_0000_0000_0000_000), // Unused
    .s9_mask_1 (20'b1_0000_1111_1111_1111_000), //

    .sA_addr_1 (20'b1_0000_0000_0000_0000_000), // Unused
    .sA_mask_1 (20'b1_0000_1111_1111_1111_000), //

    .sA_addr_2 (20'b1_0000_0000_0000_0000_000), // Unused
    .sA_mask_2 (20'b1_0000_1111_1111_1111_000)  //

    ) wbs (

    // Master interface
    .m_dat_i (dat_o),
    .m_dat_o (sw_dat_o),
    .m_adr_i ({tga,adr}),
    .m_sel_i (sel),
    .m_we_i  (we),
    .m_cyc_i (cyc),
    .m_stb_i (stb),
    .m_ack_o (ack),

    // Slave 0 interface - bios rom
    .s0_dat_i (rom_dat_o),
    .s0_dat_o (rom_dat_i),
    .s0_adr_o ({rom_tga_i,rom_adr_i}),
    .s0_sel_o (rom_sel_i),
    .s0_we_o  (rom_we_i),
    .s0_cyc_o (rom_cyc_i),
    .s0_stb_o (rom_stb_i),
    .s0_ack_i (rom_ack_o),

     // Slave 1 interface - main memory
    .s1_dat_i (ram_dat_o),
    .s1_dat_o (ram_dat_i),
    .s1_adr_o ({ram_tga_i,ram_adr_i}),
    .s1_sel_o (ram_sel_i),
    .s1_we_o  (ram_we_i),
    .s1_cyc_o (ram_cyc_i),
    .s1_stb_o (ram_stb_i),
    .s1_ack_i (ram_ack_o),

    // Slave 2 interface - Interrupt Control
    .s2_dat_i (16'h0000),
    .s2_dat_o (),
    .s2_adr_o (),
    .s2_sel_o (),
    .s2_we_o  (),
    .s2_cyc_o (s2_cyc_i),
    .s2_stb_o (s2_stb_i),
    .s2_ack_i (s2_cyc_i && s2_stb_i),

    // Slave 3 interface - Timer Control 0
    .s3_dat_i (16'h0000),
    .s3_dat_o (),
    .s3_adr_o (),
    .s3_sel_o (),
    .s3_we_o  (),
    .s3_cyc_o (s3_cyc_i),
    .s3_stb_o (s3_stb_i),
    .s3_ack_i (s3_cyc_i && s3_stb_i),

    // Slave 4 interface - Timer Control 1
    .s4_dat_i (16'h0000),
    .s4_dat_o (),
    .s4_adr_o (),
    .s4_sel_o (),
    .s4_we_o  (),
    .s4_cyc_o (s4_cyc_i),
    .s4_stb_o (s4_stb_i),
    .s4_ack_i (s4_cyc_i && s4_stb_i),

    // Slave 5 interface - Timer Control 2
    .s5_dat_i (16'h0000),
    .s5_dat_o (),
    .s5_adr_o (),
    .s5_sel_o (),
    .s5_we_o  (),
    .s5_cyc_o (s5_cyc_i),
    .s5_stb_o (s5_stb_i),
    .s5_ack_i (s5_cyc_i && s5_stb_i),

    // Slave 6 interface - Chip Select Registers
    .s6_dat_i (16'h0000),
    .s6_dat_o (),
    .s6_adr_o (),
    .s6_sel_o (),
    .s6_we_o  (),
    .s6_cyc_o (s6_cyc_i),
    .s6_stb_o (s6_stb_i),
    .s6_ack_i (s6_cyc_i && s6_stb_i),

    // Slave 7 interface - DMA Channel 0
    .s7_dat_i (16'h0000),
    .s7_dat_o (),
    .s7_adr_o (),
    .s7_sel_o (),
    .s7_we_o  (),
    .s7_cyc_o (s7_cyc_i),
    .s7_stb_o (s7_stb_i),
    .s7_ack_i (s7_cyc_i && s7_stb_i),

    // Slave 8 interface - Tube
    .s8_dat_i (tube_dat_o),
    .s8_dat_o (tube_dat_i),
    .s8_adr_o ({tube_tga_i,tube_adr_i}),
    .s8_sel_o (tube_sel_i),
    .s8_we_o  (tube_we_i),
    .s8_cyc_o (tube_cyc_i),
    .s8_stb_o (tube_stb_i),
    .s8_ack_i (tube_ack_o),

    // Slave 9 interface - not connected
    .s9_dat_i (16'h0000),
    .s9_dat_o (),
    .s9_adr_o (),
    .s9_sel_o (),
    .s9_we_o  (),
    .s9_cyc_o (s9_cyc_i),
    .s9_stb_o (s9_stb_i),
    .s9_ack_i (s9_cyc_i && s9_stb_i),

    // Slave A interface - not connected
    .sA_dat_i (16'h0000),
    .sA_dat_o (),
    .sA_adr_o (),
    .sA_sel_o (),
    .sA_we_o  (),
    .sA_cyc_o (sa_cyc_i),
    .sA_stb_o (sa_stb_i),
    .sA_ack_i (sa_cyc_i && sa_stb_i),

    // Slave B interface - default
    .sB_dat_i (16'h0000),
    .sB_dat_o (),
    .sB_adr_o (),
    .sB_sel_o (),
    .sB_we_o  (),
    .sB_cyc_o (def_cyc_i),
    .sB_stb_o (def_stb_i),
    .sB_ack_i (def_cyc_i & def_stb_i)
  );

  always @(posedge clk) begin
     if (rst) begin
         tubeint0 <= 0;
         tubeint1 <= 0;
         tubeint2 <= 0;
         tubeint3 <= 0;
         tubenmi0 <= 0;
         tubenmi1 <= 0;
         tubenmi2 <= 0;
         tubenmi3 <= 0;
     end else begin
         tubeint0 <= ~p_irq_b;
         tubeint1 <= tubeint0;
         tubeint2 <= tubeint1;
         if (inta) begin
             tubeint3 <= 0;
         end else if (tubeint1 & ~tubeint2) begin
             tubeint3 <= 1;
         end
         tubenmi0 <= ~p_nmi_b;
         tubenmi1 <= tubenmi0;
         tubenmi2 <= tubenmi1;
         if (nmia) begin
             tubenmi3 <= 0;
         end else if (tubenmi1 & ~tubenmi2) begin
             tubenmi3 <= 1;
         end
     end     
  end



  assign nmi = tubenmi3;
  assign intr = tubeint3;
  
//  assign intv[0] = tubeint3;
//  assign intv[7:1] = 0;
  
  assign dat_i = nmia ? 16'h0002 :
                (inta ? 16'h000c :
                        sw_dat_o);
                        
  // 06bb:0a55                      
  assign trigger = (pc[15:0] == 16'h7605) ? 1 : 0; 

  // default to hi-impedence, to avoid conflicts with
  // a Raspberry Pi connected to the test connector
  assign test = 8'bZ;
  
  assign dack_b = 1;
  
  //assign ram_addr = {1'b0, ram_addr_int[18] ^ ram_addr_int[17], ram_addr_int[16:0]};
  assign ram_addr = ram_addr_int;
  
  assign h_irq_b = 1;
  



endmodule
