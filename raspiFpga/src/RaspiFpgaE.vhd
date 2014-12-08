library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

library machxo2;
  use machxo2.components.all;



entity RaspiFpgaE is
  port (
    --+ SPI slave if
    SpiSclk_i    : inout std_logic;
    SpiSte_i     : in    std_logic;
    SpiMosi_i    : inout std_logic;
    SpiMiso_o    : inout std_logic;
    --* interrupt line to raspi
    RaspiIrq_o   : out   std_logic
  );
end entity RaspiFpgaE;



architecture rtl of RaspiFpgaE is


  --+ Wishbone master component
  component WishBoneMasterE is
    generic (
      G_ADR_WIDTH  : positive := 8;  --* address bus width
      G_DATA_WIDTH : positive := 8   --* data bus width
    );
    port (
      --+ wishbone system if
      WbRst_i       : in  std_logic;
      WbClk_i       : in  std_logic;
      --+ wishbone outputs
      WbCyc_o       : out std_logic;
      WbStb_o       : out std_logic;
      WbWe_o        : out std_logic;
      WbAdr_o       : out std_logic_vector(G_ADR_WIDTH-1 downto 0);
      WbDat_o       : out std_logic_vector(G_DATA_WIDTH-1 downto 0);
      --+ wishbone inputs
      WbDat_i       : in  std_logic_vector(G_DATA_WIDTH-1 downto 0);
      WbAck_i       : in  std_logic;
      WbErr_i       : in  std_logic;
      --+ local register if
      LocalWen_i    : in  std_logic;
      LocalRen_i    : in  std_logic;
      LocalAdress_i : in  std_logic_vector(G_ADR_WIDTH-1 downto 0);
      LocalData_i   : in  std_logic_vector(G_DATA_WIDTH-1 downto 0);
      LocalData_o   : out std_logic_vector(G_DATA_WIDTH-1 downto 0);
      LocalAck_o    : out std_logic;
      LocalError_o  : out std_logic
    );
  end component WishBoneMasterE;


  component RaspiFpgaCtrlE is
    port (
      --+ System if
      Rst_n_i       : in  std_logic;
      Clk_i         : in  std_logic;
      --+ local register if
      LocalWen_o    : out std_logic;
      LocalRen_o    : out std_logic;
      LocalAdress_o : out std_logic_vector(7 downto 0);
      LocalData_i   : in  std_logic_vector(7 downto 0);
      LocalData_o   : out std_logic_vector(7 downto 0);
      LocalAck_i    : in  std_logic;
      LocalError_i  : in  std_logic;
      --+ EFB if
      EfbSpiIrq_i   : in  std_logic;
      --+ RNG if
      RngStart_o     : out std_logic;
      RngWait_o      : out std_logic_vector(7 downto 0);
      RngRun_o       : out std_logic_vector(7 downto 0);
      RngDataValid_i : in  std_logic;
      RngData_i      : in  std_logic_vector(7 downto 0)
    );
  end component RaspiFpgaCtrlE;


  component FiRoCtrlE is
    generic (
      EXTRACT : boolean := true
    );
    port (
      --+ system if
      Clk_i       : in  std_logic;
      Reset_i     : in  std_logic;
      --+ ctrl/status
      Start_i     : in  std_logic;
      Wait_i      : in  std_logic_vector(7 downto 0);
      Run_i       : in  std_logic_vector(7 downto 0);
      --+ rnd data
      DataValid_o : out std_logic;
      Data_o      : out std_logic_vector(7 downto 0);
      -- firo
      Run_o       : out std_logic;
      Data_i      : in  std_logic
    );
  end component FiRoCtrlE;


  component FiRoE is
    generic (
      TOGGLE   : boolean := true
    );
    port (
      FiRo_o : out std_logic;
      Run_i  : in  std_logic
    );
  end component FiRoE;


  --+ EFB SPI slave component
  component EfbSpiSlave is
    port (
      wb_clk_i : in    std_logic;
      wb_rst_i : in    std_logic;
      wb_cyc_i : in    std_logic;
      wb_stb_i : in    std_logic;
      wb_we_i  : in    std_logic;
      wb_adr_i : in    std_logic_vector(7 downto 0);
      wb_dat_i : in    std_logic_vector(7 downto 0);
      wb_dat_o : out   std_logic_vector(7 downto 0);
      wb_ack_o : out   std_logic;
      spi_clk  : inout std_logic;
      spi_miso : inout std_logic;
      spi_mosi : inout std_logic;
      spi_scsn : in    std_logic;
      spi_irq  : out   std_logic
    );
  end component EfbSpiSlave;


  --+ oscillator component
  component OSCH is
    generic (
      NOM_FREQ : string := "26.60"
    );
    port (
      STDBY    : in  std_logic;
      OSC      : out std_logic;
      SEDSTDBY : out std_logic
    );
   end component OSCH;


  attribute NOM_FREQ : string;
  attribute NOM_FREQ of i_OSC : label is "26.60";


  --+ system signals
  signal s_sys_clk : std_logic;
  signal s_sys_rst : std_logic := '1';

  signal s_spi_sclk : std_logic;
  signal s_spi_miso : std_logic;
  signal s_spi_mosi : std_logic;

  --+ Wishbone bus signals
  signal s_wb_clk        : std_logic;
  signal s_wb_rst        : std_logic;
  signal s_wb_cyc        : std_logic;
  signal s_wb_stb        : std_logic;
  signal s_wb_we         : std_logic;
  signal s_wb_adr        : std_logic_vector(7 downto 0);
  signal s_wb_master_dat : std_logic_vector(7 downto 0);
  signal s_wb_slave_dat  : std_logic_vector(7 downto 0);
  signal s_wb_ack        : std_logic;

  --+ EFB signals
  signal s_efb_irq       : std_logic;

  --+ Wishbone master signals
  signal s_local_wen        : std_logic;
  signal s_local_ren        : std_logic;
  signal s_local_adr        : std_logic_vector(7 downto 0);
  signal s_local_read_data  : std_logic_vector(7 downto 0);
  signal s_local_write_data : std_logic_vector(7 downto 0);
  signal s_local_ack        : std_logic;

  --+ RNG signals
  signal s_rng_start      : std_logic;
  signal s_rng_wait       : std_logic_vector(7 downto 0);
  signal s_rng_run        : std_logic_vector(7 downto 0);
  signal s_rng_data_valid : std_logic;
  signal s_rng_data       : std_logic_vector(7 downto 0);
  signal s_firo_run       : std_logic;
  signal s_firo_data      : std_logic;


begin


  --+ Oscillator instance
  --+ It's generating our 26.6 MHz system lock
  i_OSC : OSCH
    generic map (
      NOM_FREQ => "26.60"
    )
    port map (
      STDBY    => '0',
      OSC      => s_sys_clk,
      SEDSTDBY => open
    );


  s_wb_clk <= s_sys_clk;
  s_wb_rst <= not(s_sys_rst);


  ResetP : process (s_sys_clk) is
    variable v_clk_count : natural range 0 to 15 := 15;
  begin
    if(rising_edge(s_sys_clk)) then
      if(v_clk_count = 0) then
        s_sys_rst <= '1';
      else
        s_sys_rst   <= '0';
        v_clk_count := v_clk_count - 1;
      end if;
    end if;
  end process ResetP;


  --+ EFB SPI slave instance
  i_EfbSpiSlave : EfbSpiSlave
    port map (
      wb_clk_i => s_wb_clk,
      wb_rst_i => s_wb_rst,
      wb_cyc_i => s_wb_cyc,
      wb_stb_i => s_wb_stb,
      wb_we_i  => s_wb_we,
      wb_adr_i => s_wb_adr,
      wb_dat_i => s_wb_master_dat,
      wb_dat_o => s_wb_slave_dat,
      wb_ack_o => s_wb_ack,
      spi_clk  => SpiSclk_i,
      spi_miso => SpiMiso_o,
      spi_mosi => SpiMosi_i,
      spi_scsn => SpiSte_i,
      spi_irq  => s_efb_irq
    );


  i_WishBoneMasterE : WishBoneMasterE
    generic map (
      G_ADR_WIDTH  => 8,
      G_DATA_WIDTH => 8
    )
    port map (
      --+ wishbone system if
      WbRst_i       => s_wb_rst,
      WbClk_i       => s_wb_clk,
      --+ wishbone outputs
      WbCyc_o       => s_wb_cyc,
      WbStb_o       => s_wb_stb,
      WbWe_o        => s_wb_we,
      WbAdr_o       => s_wb_adr,
      WbDat_o       => s_wb_master_dat,
      --+ wishbone inputs
      WbDat_i       => s_wb_slave_dat,
      WbAck_i       => s_wb_ack,
      WbErr_i       => '0',
      --+ local register if
      LocalWen_i    => s_local_wen,
      LocalRen_i    => s_local_ren,
      LocalAdress_i => s_local_adr,
      LocalData_i   => s_local_write_data,
      LocalData_o   => s_local_read_data,
      LocalAck_o    => s_local_ack,
      LocalError_o  => open
    );


  i_RaspiFpgaCtrlE : RaspiFpgaCtrlE
    port map (
      --+ System if
      Rst_n_i       => s_sys_rst,
      Clk_i         => s_sys_clk,
      --+ local register if
      LocalWen_o    => s_local_wen,
      LocalRen_o    => s_local_ren,
      LocalAdress_o => s_local_adr,
      LocalData_i   => s_local_read_data,
      LocalData_o   => s_local_write_data,
      LocalAck_i    => s_local_ack,
      LocalError_i  => '0',
      --+ EFB if
      EfbSpiIrq_i   => s_efb_irq,
      --+ RNG if
      RngStart_o     => s_rng_start,
      RngWait_o      => s_rng_wait,
      RngRun_o       => s_rng_run,
      RngDataValid_i => s_rng_data_valid,
      RngData_i      => s_rng_data
    );


  i_FiRoCtrlE : FiRoCtrlE
    generic map (
      EXTRACT => true
    )
    port map (
      --+ system if
      Clk_i       => s_sys_clk,
      Reset_i     => s_sys_rst,
      --+ ctrl/status
      Start_i     => s_rng_start,
      Wait_i      => s_rng_wait,
      Run_i       => s_rng_run,
      --+ rnd data
      DataValid_o => s_rng_data_valid,
      Data_o      => s_rng_data,
      -- firo
      Run_o       => s_firo_run,
      Data_i      => s_firo_data
    );


  i_FiRoE : FiRoE
    generic map (
      TOGGLE => true
    )
    port map (
      FiRo_o => s_firo_data,
      Run_i  => s_firo_run
    );


  RaspiIrq_o <= '0';


end architecture rtl;
