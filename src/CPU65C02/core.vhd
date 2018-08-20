-- VHDL Entity r65c02_tc.core.symbol
--
-- Created:
--          by - jens
--          at - 11:36:32 07/26/13
--
-- Generated by Mentor Graphics' HDL Designer(TM) 2012.2a (Build 3)
--
LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.std_logic_arith.all;

ENTITY core IS
   PORT( 
      clk_clk_i   : IN     std_logic;
      d_i         : IN     std_logic_vector (7 DOWNTO 0);
      irq_n_i     : IN     std_logic;
      nmi_n_i     : IN     std_logic;
      rdy_i       : IN     std_logic;
      rst_rst_n_i : IN     std_logic;
      so_n_i      : IN     std_logic;
      a_o         : OUT    std_logic_vector (15 DOWNTO 0);
      d_o         : OUT    std_logic_vector (7 DOWNTO 0);
      rd_o        : OUT    std_logic;
      sync_o      : OUT    std_logic;
      wr_n_o      : OUT    std_logic;
      wr_o        : OUT    std_logic
   );

-- Declarations

END core ;

-- (C) 2008 - 2013 Jens Gutschmidt
-- (email: scantara2003@yahoo.de)
-- 
-- Versions:
-- Revision 1.8  2013/07/26 11:34:00  jens
-- - Bug Fix RMB, SMB Bug - Bit position decoded wrong. Adding a priority encoder.
-- 
-- Revision 1.7  2013/07/21 11:11:00  jens
-- - Changing the title block and internal revision history
-- 
-- Revision 1.6  2009/01/04 10:20:47  eda
-- Changes for cosmetic issues only
-- 
-- Revision 1.5  2009/01/04 09:23:10  eda
-- - Delete unused nets and blocks (same as R6502_TC)
-- - Rename blocks
-- 
-- Revision 1.4  2009/01/03 16:53:02  eda
-- - Unused nets and blocks deleted
-- - Renamed blocks
-- 
-- Revision 1.3  2009/01/03 16:42:02  eda
-- - Unused nets and blocks deleted
-- - Renamed blocks
-- 
-- Revision 1.2  2008/12/31 19:31:24  eda
-- Production Release
--  
-- 
--
-- VHDL Architecture r65c02_tc.core.struct
--
-- Created:
--          by - jens
--          at - 12:18:23 07/26/13
--
-- Generated by Mentor Graphics' HDL Designer(TM) 2012.2a (Build 3)
--
-- COPYRIGHT (C) 2008 - 2013 by Jens Gutschmidt and OPENCORES.ORG
-- 
-- This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or any later version.
-- 
-- This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License along with this program.  If not, see <http://www.gnu.org/licenses/>.
-- 
-- 
LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.std_logic_arith.all;

--LIBRARY r65c02_tc;

ARCHITECTURE struct OF core IS

   -- Architecture declarations

   -- Internal signal declarations
   SIGNAL adr_nxt_pc_o_i     : std_logic_vector(15 DOWNTO 0);
   SIGNAL adr_o_i            : std_logic_vector(15 DOWNTO 0);
   SIGNAL adr_pc_o_i         : std_logic_vector(15 DOWNTO 0);
   SIGNAL adr_sp_o_i         : std_logic_vector(15 DOWNTO 0);
   SIGNAL ch_a_o_i           : std_logic_vector(7 DOWNTO 0);
   SIGNAL ch_b_o_i           : std_logic_vector(7 DOWNTO 0);
   SIGNAL d_alu_n_o_i        : std_logic;
   SIGNAL d_alu_o_i          : std_logic_vector(7 DOWNTO 0);
   SIGNAL d_alu_or_o_i       : std_logic;
   SIGNAL d_alu_prio_o_i     : std_logic_vector(7 DOWNTO 0);
   SIGNAL d_regs_in_o_i      : std_logic_vector(7 DOWNTO 0);
   SIGNAL d_regs_out_o_i     : std_logic_vector(7 DOWNTO 0);
   SIGNAL ld_o_i             : std_logic_vector(1 DOWNTO 0);
   SIGNAL ld_pc_o_i          : std_logic;
   SIGNAL ld_sp_o_i          : std_logic;
   SIGNAL load_regs_o_i      : std_logic;
   SIGNAL nmi_o_i            : std_logic;
   SIGNAL offset_o_i         : std_logic_vector(15 DOWNTO 0);
   SIGNAL q_a_o_i            : std_logic_vector(7 DOWNTO 0);
   SIGNAL q_x_o_i            : std_logic_vector(7 DOWNTO 0);
   SIGNAL q_y_o_i            : std_logic_vector(7 DOWNTO 0);
   SIGNAL reg_0flag_o_i      : std_logic;
   SIGNAL reg_1flag_o_i      : std_logic;
   SIGNAL reg_7flag_o_i      : std_logic;
   SIGNAL rst_nmi_o_i        : std_logic;
   SIGNAL sel_pc_in_o_i      : std_logic;
   SIGNAL sel_pc_val_o_i     : std_logic_vector(1 DOWNTO 0);
   SIGNAL sel_rb_in_o_i      : std_logic_vector(1 DOWNTO 0);
   SIGNAL sel_rb_out_o_i     : std_logic_vector(1 DOWNTO 0);
   SIGNAL sel_reg_o_i        : std_logic_vector(1 DOWNTO 0);
   SIGNAL sel_sp_as_o_i      : std_logic;
   SIGNAL sel_sp_in_o_i      : std_logic;
   SIGNAL var_shift_data_o_i : std_logic_vector(7 DOWNTO 0);


   -- Component Declarations
   COMPONENT fsm_execution_unit
   PORT (
      adr_nxt_pc_i : IN     std_logic_vector (15 DOWNTO 0);
      adr_pc_i     : IN     std_logic_vector (15 DOWNTO 0);
      adr_sp_i     : IN     std_logic_vector (15 DOWNTO 0);
      clk_clk_i    : IN     std_logic ;
      d_alu_i      : IN     std_logic_vector ( 7 DOWNTO 0 );
      d_alu_prio_i : IN     std_logic_vector (7 DOWNTO 0);
      d_i          : IN     std_logic_vector ( 7 DOWNTO 0 );
      d_regs_out_i : IN     std_logic_vector ( 7 DOWNTO 0 );
      irq_n_i      : IN     std_logic ;
      nmi_i        : IN     std_logic ;
      q_a_i        : IN     std_logic_vector ( 7 DOWNTO 0 );
      q_x_i        : IN     std_logic_vector ( 7 DOWNTO 0 );
      q_y_i        : IN     std_logic_vector ( 7 DOWNTO 0 );
      rdy_i        : IN     std_logic ;
      reg_0flag_i  : IN     std_logic ;
      reg_1flag_i  : IN     std_logic ;
      reg_7flag_i  : IN     std_logic ;
      rst_rst_n_i  : IN     std_logic ;
      so_n_i       : IN     std_logic ;
      a_o          : OUT    std_logic_vector (15 DOWNTO 0);
      adr_o        : OUT    std_logic_vector (15 DOWNTO 0);
      ch_a_o       : OUT    std_logic_vector ( 7 DOWNTO 0 );
      ch_b_o       : OUT    std_logic_vector ( 7 DOWNTO 0 );
      d_o          : OUT    std_logic_vector ( 7 DOWNTO 0 );
      d_regs_in_o  : OUT    std_logic_vector ( 7 DOWNTO 0 );
      ld_o         : OUT    std_logic_vector ( 1 DOWNTO 0 );
      ld_pc_o      : OUT    std_logic ;
      ld_sp_o      : OUT    std_logic ;
      load_regs_o  : OUT    std_logic ;
      offset_o     : OUT    std_logic_vector ( 15 DOWNTO 0 );
      rd_o         : OUT    std_logic ;
      rst_nmi_o    : OUT    std_logic ;
      sel_pc_in_o  : OUT    std_logic ;
      sel_pc_val_o : OUT    std_logic_vector ( 1 DOWNTO 0 );
      sel_rb_in_o  : OUT    std_logic_vector ( 1 DOWNTO 0 );
      sel_rb_out_o : OUT    std_logic_vector ( 1 DOWNTO 0 );
      sel_reg_o    : OUT    std_logic_vector ( 1 DOWNTO 0 );
      sel_sp_as_o  : OUT    std_logic ;
      sel_sp_in_o  : OUT    std_logic ;
      sync_o       : OUT    std_logic ;
      wr_n_o       : OUT    std_logic ;
      wr_o         : OUT    std_logic 
   );
   END COMPONENT;
   COMPONENT fsm_intnmi
   PORT (
      clk_clk_i   : IN     std_logic ;
      nmi_n_i     : IN     std_logic ;
      rst_nmi_i   : IN     std_logic ;
      rst_rst_n_i : IN     std_logic ;
      nmi_o       : OUT    std_logic 
   );
   END COMPONENT;
   COMPONENT reg_pc
   PORT (
      adr_i        : IN     std_logic_vector (15 DOWNTO 0);
      clk_clk_i    : IN     std_logic ;
      ld_i         : IN     std_logic_vector (1 DOWNTO 0);
      ld_pc_i      : IN     std_logic ;
      offset_i     : IN     std_logic_vector (15 DOWNTO 0);
      rst_rst_n_i  : IN     std_logic ;
      sel_pc_in_i  : IN     std_logic ;
      sel_pc_val_i : IN     std_logic_vector (1 DOWNTO 0);
      adr_nxt_pc_o : OUT    std_logic_vector (15 DOWNTO 0);
      adr_pc_o     : OUT    std_logic_vector (15 DOWNTO 0)
   );
   END COMPONENT;
   COMPONENT reg_sp
   PORT (
      adr_low_i   : IN     std_logic_vector (7 DOWNTO 0);
      clk_clk_i   : IN     std_logic ;
      ld_low_i    : IN     std_logic ;
      ld_sp_i     : IN     std_logic ;
      rst_rst_n_i : IN     std_logic ;
      sel_sp_as_i : IN     std_logic ;
      sel_sp_in_i : IN     std_logic ;
      adr_sp_o    : OUT    std_logic_vector (15 DOWNTO 0)
   );
   END COMPONENT;
   COMPONENT regbank_axy
   PORT (
      clk_clk_i    : IN     std_logic ;
      d_regs_in_i  : IN     std_logic_vector (7 DOWNTO 0);
      load_regs_i  : IN     std_logic ;
      rst_rst_n_i  : IN     std_logic ;
      sel_rb_in_i  : IN     std_logic_vector (1 DOWNTO 0);
      sel_rb_out_i : IN     std_logic_vector (1 DOWNTO 0);
      sel_reg_i    : IN     std_logic_vector (1 DOWNTO 0);
      d_regs_out_o : OUT    std_logic_vector (7 DOWNTO 0);
      q_a_o        : OUT    std_logic_vector (7 DOWNTO 0);
      q_x_o        : OUT    std_logic_vector (7 DOWNTO 0);
      q_y_o        : OUT    std_logic_vector (7 DOWNTO 0)
   );
   END COMPONENT;

   -- Optional embedded configurations
   -- pragma synthesis_off
   --FOR ALL : fsm_execution_unit USE ENTITY r65c02_tc.fsm_execution_unit;
   --FOR ALL : fsm_intnmi USE ENTITY r65c02_tc.fsm_intnmi;
   --FOR ALL : reg_pc USE ENTITY r65c02_tc.reg_pc;
   --FOR ALL : reg_sp USE ENTITY r65c02_tc.reg_sp;
   --FOR ALL : regbank_axy USE ENTITY r65c02_tc.regbank_axy;
   -- pragma synthesis_on


BEGIN
   -- Architecture concurrent statements
   -- HDL Embedded Text Block 1 eb1
   -- eb1 1
   var_shift_data_o_i <= x"01";


   -- ModuleWare code(v1.12) for instance 'U_11' of 'add'
   u_11combo_proc: PROCESS (ch_a_o_i, ch_b_o_i)
   VARIABLE temp_din0 : std_logic_vector(8 DOWNTO 0);
   VARIABLE temp_din1 : std_logic_vector(8 DOWNTO 0);
   VARIABLE temp_sum : unsigned(8 DOWNTO 0);
   VARIABLE temp_carry : std_logic;
   BEGIN
      temp_din0 := '0' & ch_a_o_i;
      temp_din1 := '0' & ch_b_o_i;
      temp_carry := '0';
      temp_sum := unsigned(temp_din0) + unsigned(temp_din1) + temp_carry;
      d_alu_o_i <= conv_std_logic_vector(temp_sum(7 DOWNTO 0),8);
      reg_0flag_o_i <= temp_sum(8) ;
   END PROCESS u_11combo_proc;

   -- ModuleWare code(v1.12) for instance 'U_8' of 'inv'
   reg_1flag_o_i <= NOT(d_alu_or_o_i);

   -- ModuleWare code(v1.12) for instance 'U_9' of 'inv'
   reg_7flag_o_i <= NOT(d_alu_n_o_i);

   -- ModuleWare code(v1.12) for instance 'U_10' of 'inv'
   d_alu_n_o_i <= NOT(d_alu_o_i(7));

   -- ModuleWare code(v1.12) for instance 'U_5' of 'lshift'
   u_5combo_proc : PROCESS (var_shift_data_o_i, ch_a_o_i)
   VARIABLE temp_shift : std_logic_vector (3 DOWNTO 0);
   VARIABLE temp_dout : std_logic_vector (7 DOWNTO 0);
   VARIABLE temp_din : std_logic_vector (7 DOWNTO 0);
   BEGIN
      temp_din := (OTHERS=> 'X');
      temp_shift := ch_a_o_i(3 downto 0);
      temp_din := var_shift_data_o_i;
      FOR i IN 0 TO 3 LOOP
         IF (i < 3) THEN
            IF (temp_shift(i) = '1') THEN
               temp_dout := (OTHERS => '0');
               temp_dout(7 DOWNTO 2**i) := temp_din(7 - 2**i DOWNTO 0);
            ELSIF (temp_shift(i) = '0') THEN
               temp_dout := temp_din;
            ELSE
               temp_dout := (OTHERS => 'X');
            END IF;
         ELSE
            IF (temp_shift(i) = '1') THEN
               temp_dout := (OTHERS => '0');
            ELSIF (temp_shift(i) = '0') THEN
               temp_dout := temp_din;
            ELSE
               temp_dout := (OTHERS => 'X');
            END IF;
         END IF;
         temp_din := temp_dout;
      END LOOP;
      d_alu_prio_o_i <= temp_dout;
   END PROCESS u_5combo_proc;

   -- ModuleWare code(v1.12) for instance 'U_7' of 'por'
   d_alu_or_o_i <= d_alu_o_i(0) OR  d_alu_o_i(1) OR  d_alu_o_i(2) OR  d_alu_o_i(3) OR  d_alu_o_i(4) OR  d_alu_o_i(5) OR  d_alu_o_i(6) OR  d_alu_o_i(7);

   -- Instance port mappings.
   U_4 : fsm_execution_unit
      PORT MAP (
         adr_nxt_pc_i => adr_nxt_pc_o_i,
         adr_pc_i     => adr_pc_o_i,
         adr_sp_i     => adr_sp_o_i,
         clk_clk_i    => clk_clk_i,
         d_alu_i      => d_alu_o_i,
         d_alu_prio_i => d_alu_prio_o_i,
         d_i          => d_i,
         d_regs_out_i => d_regs_out_o_i,
         irq_n_i      => irq_n_i,
         nmi_i        => nmi_o_i,
         q_a_i        => q_a_o_i,
         q_x_i        => q_x_o_i,
         q_y_i        => q_y_o_i,
         rdy_i        => rdy_i,
         reg_0flag_i  => reg_0flag_o_i,
         reg_1flag_i  => reg_1flag_o_i,
         reg_7flag_i  => reg_7flag_o_i,
         rst_rst_n_i  => rst_rst_n_i,
         so_n_i       => so_n_i,
         a_o          => a_o,
         adr_o        => adr_o_i,
         ch_a_o       => ch_a_o_i,
         ch_b_o       => ch_b_o_i,
         d_o          => d_o,
         d_regs_in_o  => d_regs_in_o_i,
         ld_o         => ld_o_i,
         ld_pc_o      => ld_pc_o_i,
         ld_sp_o      => ld_sp_o_i,
         load_regs_o  => load_regs_o_i,
         offset_o     => offset_o_i,
         rd_o         => rd_o,
         rst_nmi_o    => rst_nmi_o_i,
         sel_pc_in_o  => sel_pc_in_o_i,
         sel_pc_val_o => sel_pc_val_o_i,
         sel_rb_in_o  => sel_rb_in_o_i,
         sel_rb_out_o => sel_rb_out_o_i,
         sel_reg_o    => sel_reg_o_i,
         sel_sp_as_o  => sel_sp_as_o_i,
         sel_sp_in_o  => sel_sp_in_o_i,
         sync_o       => sync_o,
         wr_n_o       => wr_n_o,
         wr_o         => wr_o
      );
   U_3 : fsm_intnmi
      PORT MAP (
         clk_clk_i   => clk_clk_i,
         nmi_n_i     => nmi_n_i,
         rst_nmi_i   => rst_nmi_o_i,
         rst_rst_n_i => rst_rst_n_i,
         nmi_o       => nmi_o_i
      );
   U_0 : reg_pc
      PORT MAP (
         adr_i        => adr_o_i,
         clk_clk_i    => clk_clk_i,
         ld_i         => ld_o_i,
         ld_pc_i      => ld_pc_o_i,
         offset_i     => offset_o_i,
         rst_rst_n_i  => rst_rst_n_i,
         sel_pc_in_i  => sel_pc_in_o_i,
         sel_pc_val_i => sel_pc_val_o_i,
         adr_nxt_pc_o => adr_nxt_pc_o_i,
         adr_pc_o     => adr_pc_o_i
      );
   U_1 : reg_sp
      PORT MAP (
         adr_low_i   => adr_o_i(7 DOWNTO 0),
         clk_clk_i   => clk_clk_i,
         ld_low_i    => ld_o_i(0),
         ld_sp_i     => ld_sp_o_i,
         rst_rst_n_i => rst_rst_n_i,
         sel_sp_as_i => sel_sp_as_o_i,
         sel_sp_in_i => sel_sp_in_o_i,
         adr_sp_o    => adr_sp_o_i
      );
   U_2 : regbank_axy
      PORT MAP (
         clk_clk_i    => clk_clk_i,
         d_regs_in_i  => d_regs_in_o_i,
         load_regs_i  => load_regs_o_i,
         rst_rst_n_i  => rst_rst_n_i,
         sel_rb_in_i  => sel_rb_in_o_i,
         sel_rb_out_i => sel_rb_out_o_i,
         sel_reg_i    => sel_reg_o_i,
         d_regs_out_o => d_regs_out_o_i,
         q_a_o        => q_a_o_i,
         q_x_o        => q_x_o_i,
         q_y_o        => q_y_o_i
      );

END struct;
