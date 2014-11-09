library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

entity CoPro6502 is
    generic (
       UseT65Core    : boolean := true;
       UseJensCore   : boolean := false;
       UseAlanDCore  : boolean := false
    );
    port (
        -- GOP Signals
        fastclk   : in    std_logic;
        tp        : out   std_logic_vector(8 downto 2);
        test      : out   std_logic_vector(6 downto 1);
        sw        : in    std_logic_vector(2 downto 1);
        fcs       : out   std_logic;
        
        -- Tube signals (use 16 out of 22 DIL pins)
        h_phi2    : in    std_logic;  -- 1,2,12,21,23 are global clocks
        h_addr    : in    std_logic_vector(2 downto 0);
        h_data    : inout std_logic_vector(7 downto 0);
        h_rdnw    : in    std_logic;
        h_cs_b    : in    std_logic;
        h_rst_b   : in    std_logic;
        h_irq_b   : inout std_logic;


        -- Ram Signals
        ram_cs       : out   std_logic;
        ram_oe       : out   std_logic;
        ram_wr       : out   std_logic;
        ram_addr     : out   std_logic_vector (18 downto 0);
        ram_data     : inout std_logic_vector (7 downto 0)
    );
end CoPro6502;

architecture BEHAVIORAL of CoPro6502 is
 
    component dcm1
        port (
            CLKIN_IN  : in  std_logic;
            CLK0_OUT  : out std_logic;
            CLK0_OUT1 : out std_logic;
            CLK2X_OUT : out std_logic
        ); 
    end component;

    component tuberom_65c102
        port (
            CLK  : in  std_logic;
            ADDR : in  std_logic_vector(10 downto 0);
            DATA : out std_logic_vector(7 downto 0));
    end component;

    component T65
        port(
            Mode    : in  std_logic_vector(1 downto 0);
            Res_n   : in  std_logic;
            Enable  : in  std_logic;
            Clk     : in  std_logic;
            Rdy     : in  std_logic;
            Abort_n : in  std_logic;
            IRQ_n   : in  std_logic;
            NMI_n   : in  std_logic;
            SO_n    : in  std_logic;
            DI      : in  std_logic_vector(7 downto 0);          
            R_W_n   : out std_logic;
            Sync    : out std_logic;
            EF      : out std_logic;
            MF      : out std_logic;
            XF      : out std_logic;
            ML_n    : out std_logic;
            VP_n    : out std_logic;
            VDA     : out std_logic;
            VPA     : out std_logic;
            A       : out std_logic_vector(23 downto 0);
            DO      : out std_logic_vector(7 downto 0)
        );
    end component;

    component r65c02_tc
    port(
        clk_clk_i   : in std_logic;
        d_i         : in std_logic_vector(7 downto 0);
        irq_n_i     : in std_logic;
        nmi_n_i     : in std_logic;
        rdy_i       : in std_logic;
        rst_rst_n_i : in std_logic;
        so_n_i      : in std_logic;          
        a_o         : out std_logic_vector(15 downto 0);
        d_o         : out std_logic_vector(7 downto 0);
        rd_o        : out std_logic;
        sync_o      : out std_logic;
        wr_n_o      : out std_logic;
        wr_o        : out std_logic
        );
    end component;
    
    component r65c02
    port(
        reset       : in std_logic;
        clk         : in std_logic;
        enable      : in std_logic;
        nmi_n       : in std_logic;
        irq_n       : in std_logic;
        di          : in std_logic_vector(7 downto 0);          
        do          : out std_logic_vector(7 downto 0);
        addr        : out std_logic_vector(15 downto 0);
        nwe         : out std_logic;
        sync        : out std_logic;
        sync_irq    : out std_logic
        );
    end component;

    component tube
        port(
            h_addr     : in    std_logic_vector(2 downto 0);
            h_cs_b     : in    std_logic;
            h_data     : inout std_logic_vector(7 downto 0);
            h_phi2     : in    std_logic;
            h_rdnw     : in    std_logic;
            h_rst_b    : in    std_logic;
            h_irq_b    : inout std_logic;
         -- drq        : out   std_logic;
         -- dackb      : in    std_logic;
            p_addr     : in    std_logic_vector(2 downto 0);
            p_cs_b     : in    std_logic;
            p_data_in  : in    std_logic_vector(7 downto 0);
            p_data_out : out   std_logic_vector(7 downto 0);
            p_rdnw     : in    std_logic;
            p_phi2     : in    std_logic;
            p_rst_b    : out   std_logic;
            p_nmi_b    : inout std_logic;
            p_irq_b    : inout std_logic
          );
    end component;

-------------------------------------------------
-- clock and reset signals
-------------------------------------------------

    signal clk_16M00     : std_logic;
    signal phi           : std_logic;
    signal phi2          : std_logic;
    signal cpu_clken     : std_logic;
    signal clken_counter : std_logic_vector (3 downto 0);
    signal bootmode      : std_logic;
    signal RSTn          : std_logic;

-------------------------------------------------
-- parasite signals
-------------------------------------------------
    
    signal p_cs_b        : std_logic;
    signal p_data_out    : std_logic_vector (7 downto 0);

-------------------------------------------------
-- ram/rom signals
-------------------------------------------------

    signal ram_cs_b        : std_logic;
    signal ram_oe_int      : std_logic;
    signal ram_wr_int      : std_logic;
    signal rom_cs_b        : std_logic;
    signal rom_data_out    : std_logic_vector (7 downto 0);
-------------------------------------------------
-- cpu signals
-------------------------------------------------

    signal debug_clk  : std_logic;
    signal cpu_R_W_n  : std_logic;
    signal cpu_addr   : std_logic_vector (23 downto 0);
    signal cpu_din    : std_logic_vector (7 downto 0);
    signal cpu_dout   : std_logic_vector (7 downto 0);
    signal cpu_IRQ_n  : std_logic;
    signal cpu_NMI_n  : std_logic;
    signal sync       : std_logic;
begin

---------------------------------------------------------------------
-- instantiated components
---------------------------------------------------------------------

    inst_dcm1 : dcm1 port map (
        CLKIN_IN  => fastclk,
        CLK0_OUT  => clk_16M00,
        CLK0_OUT1 => open,
        CLK2X_OUT => open
    );

    inst_tuberom : tuberom_65c102 port map (
        CLK             => clk_16M00,
        ADDR            => cpu_addr(10 downto 0),
        DATA            => rom_data_out
    );

    GenT65Core: if UseT65Core generate
        inst_T65 : T65 port map (
            Mode            => "01",
            Abort_n         => '1',
            SO_n            => '1',
            Res_n           => RSTn,
            Enable          => cpu_clken,
            Clk             => clk_16M00,
            Rdy             => '1',
            IRQ_n           => cpu_IRQ_n,
            NMI_n           => cpu_NMI_n,
            R_W_n           => cpu_R_W_n,
            Sync            => sync,
            A(23 downto 0)  => cpu_addr,
            DI(7 downto 0)  => cpu_din,
            DO(7 downto 0)  => cpu_dout
        );
        -- For debugging only
        debug_clk <= cpu_clken;        
    end generate;
    
    GenJensCore: if UseJensCore generate
        Inst_r65c02_tc: r65c02_tc PORT MAP(
            clk_clk_i   => phi2,
            d_i         => cpu_din,
            irq_n_i     => cpu_IRQ_n,
            nmi_n_i     => cpu_NMI_n,
            rdy_i       => '1',
            rst_rst_n_i => RSTn,
            so_n_i      => '1',
            a_o         => cpu_addr(15 downto 0),
            d_o         => cpu_dout,
            rd_o        => open,
            sync_o      => sync,
            wr_n_o      => cpu_R_W_n,
            wr_o        => open
        );
        -- For debugging only
        debug_clk <= phi2;    
    end generate;

    GenAlanDCore: if UseAlanDCore generate
        inst_r65c02: r65c02 port map(
            reset    => RSTn,
            clk      => clk_16M00,
            enable   => cpu_clken,
            nmi_n    => cpu_NMI_n,
            irq_n    => cpu_IRQ_n,
            di       => cpu_din,
            do       => cpu_dout,
            addr     => cpu_addr(15 downto 0),
            nwe      => cpu_R_W_n,
            sync     => sync,
            sync_irq => open
        );    
        -- For debugging only
        debug_clk <= cpu_clken;        
    end generate;

    inst_tube: tube port map (
        h_addr          => h_addr,
        h_cs_b          => h_cs_b,
        h_data          => h_data,
        h_phi2          => h_phi2,
        h_rdnw          => h_rdnw,
        h_rst_b         => h_rst_b,
        h_irq_b         => h_irq_b,
        p_addr          => cpu_addr(2 downto 0),
        p_cs_b          => p_cs_b,
        p_data_in       => cpu_dout,
        p_data_out      => p_data_out,
        p_rdnw          => cpu_R_W_n,
        p_phi2          => phi2,
        p_rst_b         => RSTn,
        p_nmi_b         => cpu_NMI_n,
        p_irq_b         => cpu_IRQ_n
    );


    p_cs_b <= '0' when cpu_addr(15 downto 3) = "1111111011111" else '1';

    rom_cs_b <= '0' when cpu_addr(15 downto 11) = "11111" and cpu_R_W_n = '1' and bootmode = '1' else '1';

    ram_cs_b <= '0' when p_cs_b = '1' and rom_cs_b = '1' else '1';

    cpu_din <=
        p_data_out   when p_cs_b = '0' else
        rom_data_out when rom_cs_b = '0' else
        ram_data     when ram_cs_b = '0' else
        x"f1";

    
    ram_cs <= ram_cs_b;
    ram_oe_int <= not ((not ram_cs_b) and cpu_R_W_n);
    ram_oe <= ram_oe_int;
    ram_wr_int <= not ((not ram_cs_b) and (not cpu_R_W_n) and Phi2);
    ram_wr <= ram_wr_int;
    ram_addr <= "000" & cpu_addr(15 downto 0);
    ram_data <= cpu_dout when cpu_R_W_n = '0' else "ZZZZZZZZ";

    fcs <= '1';
    
    testpr : process(sw, debug_clk, sync, cpu_addr, h_addr, h_cs_b, cpu_dout, p_data_out, p_cs_b, cpu_NMI_n, cpu_IRQ_n)
    begin
        if (sw(1) = '1' and sw(2) = '1') then
        
            test(6) <= debug_clk;
            test(5) <= RSTn;
            test(4) <= sync;
            test(3) <= cpu_addr(9);
            test(2) <= cpu_addr(8);
            test(1) <= cpu_addr(7);

            tp(8) <= cpu_addr(6);
            tp(7) <= cpu_addr(5);
            tp(6) <= cpu_addr(4);
            tp(5) <= cpu_addr(3);
            tp(4) <= cpu_addr(2);
            tp(3) <= cpu_addr(1);
            tp(2) <= cpu_addr(0);
        else
    
            test(6) <= CPU_NMI_n;
            test(5) <= '0';
            if h_addr(2 downto 0) = "101" and h_cs_b = '0' then
                test(4) <= '1';
            else
                test(4) <= '0';
            end if;
            if cpu_addr(2 downto 0) = "101" and p_cs_b = '0' then
                test(3) <= '1';
            else
                test(3) <= '0';
            end if;
            test(2) <= debug_clk;
            test(1) <= cpu_dout(7);
            tp(8) <= cpu_dout(6);
            tp(7) <= cpu_dout(5);
            tp(6) <= cpu_dout(4);
            tp(5) <= cpu_dout(3);
            tp(4) <= cpu_dout(2);
            tp(3) <= cpu_dout(1);
            tp(2) <= cpu_dout(0);
        end if;
    end process;

    
--------------------------------------------------------
-- boot mode generator
--------------------------------------------------------
    boot_gen : process(clk_16M00, RSTn)
    begin
        if RSTn = '0' then
            bootmode <= '1';
        elsif rising_edge(clk_16M00) then
            if p_cs_b = '0' then
                bootmode <= '0';
            end if;
        end if;
    end process;

--------------------------------------------------------
-- clock enable generator

-- 4MHz
-- cpu_clken active on cycle 0, 4, 8, 12
-- address/data changes on cycle 1, 5, 9, 13
-- phi2 active on cycle 2..3, 6..7 10..11 14..15
--------------------------------------------------------
    clk_gen : process(clk_16M00, RSTn)
    begin
--        if RSTn = '0' then
--            clken_counter <= (others => '0');
--            cpu_clken <= '0';
--            phi       <= '0';
--            phi2      <= '0';
--        els
        if rising_edge(clk_16M00) then
            clken_counter <= clken_counter + 1;
            cpu_clken     <= clken_counter(0) and clken_counter(1);
            phi           <= not clken_counter(1);
             -- delay by 1 cycle so address and data will be stable for 62.5ns before phi2
            phi2          <= phi;
        end if;
    end process;
    
end BEHAVIORAL;


