library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;



entity RaspiFpgaCtrlE is
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
    RngDataValid_i : in  std_logic;
    RngData_i      : in  std_logic_vector(7 downto 0)
  );
end entity RaspiFpgaCtrlE;



architecture rtl of RaspiFpgaCtrlE is


  --+ EFB SPI slave register addresses
  constant C_SPICR0   : std_logic_vector(7 downto 0) := x"54";  --* ctrl reg 0
  constant C_SPICR1   : std_logic_vector(7 downto 0) := x"55";  --* ctrl reg 1
  constant C_SPICR2   : std_logic_vector(7 downto 0) := x"56";  --* ctrl reg 2
  constant C_SPIBR    : std_logic_vector(7 downto 0) := x"57";  --* clk pre-scale
  constant C_SPICSR   : std_logic_vector(7 downto 0) := x"58";  --* master chip select
  constant C_SPITXDR  : std_logic_vector(7 downto 0) := x"59";  --* transmit data
  constant C_SPIISR   : std_logic_vector(7 downto 0) := x"5A";  --* status
  constant C_SPIRXDR  : std_logic_vector(7 downto 0) := x"5B";  --* receive data
  constant C_SPIIRQ   : std_logic_vector(7 downto 0) := x"5C";  --* interrupt request
  constant C_SPIIRQEN : std_logic_vector(7 downto 0) := x"5D";  --* interrupt request enable


  type t_cmdctrl_fsm is (IDLE, INIT_SET, INIT_ACK, TXDR_SET, TXDR_ACK, INT_WAIT,
                         RXDR_SET, RXDR_ACK, INT_CLEAR_SET, INT_CLEAR_ACK);

  signal s_cmdctrl_fsm : t_cmdctrl_fsm;

  type t_wb_master is record
    adr  : std_logic_vector(7 downto 0);
    data : std_logic_vector(7 downto 0);
  end record t_wb_master;

  type t_wb_master_array is array (natural range <>) of t_wb_master;

  constant C_INIT : t_wb_master_array := ((C_SPICR1,   x"80"),
                                          (C_SPICR2,   x"00"),
                                          (C_SPIIRQEN, x"08"));

  signal s_init_cnt : natural range 0 to C_INIT'length;

  type t_byte_array is array (natural range <>) of std_logic_vector(7 downto 0);
  signal s_register : t_byte_array(0 to 127);

  signal s_register_we      : std_logic;
  signal s_register_address : natural range s_register'range;

  type t_spi_frame is (NOP, HEADER, WRITE_DATA, READ_DATA);
  signal s_spi_frame : t_spi_frame;


begin


  --+ FSM to write/request data from the wishbone master
  --+ Combinatoral outputs
  LocalWen_o    <= '1' when s_cmdctrl_fsm = INIT_SET or s_cmdctrl_fsm = TXDR_SET or s_cmdctrl_fsm = INT_CLEAR_SET else '0';
  LocalRen_o    <= '1' when s_cmdctrl_fsm = RXDR_SET else '0';
  LocalAdress_o <= C_INIT(s_init_cnt).adr when s_cmdctrl_fsm = INIT_SET      else
                   C_SPITXDR              when s_cmdctrl_fsm = TXDR_SET      else
                   C_SPIRXDR              when s_cmdctrl_fsm = RXDR_SET      else
                   C_SPIIRQ               when s_cmdctrl_fsm = INT_CLEAR_SET else
                   (others => '0');
  LocalData_o   <= C_INIT(s_init_cnt).data        when s_cmdctrl_fsm = INIT_SET                             else
                   s_register(s_register_address) when s_cmdctrl_fsm = TXDR_SET and s_spi_frame = READ_DATA else
                   x"FF";


  CmdCtrlP : process (Clk_i) is
  begin
    if (rising_edge(Clk_i)) then
      if (Rst_n_i = '0') then
        s_cmdctrl_fsm <= IDLE;
      else
        FsmC : case s_cmdctrl_fsm is

          when IDLE =>
            s_cmdctrl_fsm <= INIT_SET;

          when INIT_SET =>
            s_cmdctrl_fsm <= INIT_ACK;

          when INIT_ACK =>
            if (LocalAck_i = '1') then
              if (s_init_cnt = C_INIT'length) then
                s_cmdctrl_fsm <= TXDR_SET;
              else
                s_cmdctrl_fsm <= INIT_SET;
              end if;
            end if;

          when TXDR_SET =>
            s_cmdctrl_fsm <= TXDR_ACK;

          when TXDR_ACK =>
            if (LocalAck_i = '1') then
              s_cmdctrl_fsm <= INT_WAIT;
            end if;

          when INT_WAIT =>
            if (EfbSpiIrq_i = '1') then
              s_cmdctrl_fsm <= RXDR_SET;
            end if;

          when RXDR_SET =>
            s_cmdctrl_fsm <= RXDR_ACK;

          when RXDR_ACK =>
            if (LocalAck_i = '1') then
              s_cmdctrl_fsm <= INT_CLEAR_SET;
            end if;

          when INT_CLEAR_SET =>
            s_cmdctrl_fsm <= INT_CLEAR_ACK;

          when INT_CLEAR_ACK =>
            if (LocalAck_i = '1') then
              s_cmdctrl_fsm <= TXDR_SET;
            end if;

          when others =>
            null;

        end case FsmC;
      end if;
    end if;
  end process CmdCtrlP;


  CmdRegisterP : process (Clk_i) is
  begin
    if (rising_edge(Clk_i)) then
      if (Rst_n_i = '0') then
        s_init_cnt         <= 0;
        s_spi_frame        <= NOP;
        s_register_address <= 0;
      else

        case s_cmdctrl_fsm is
          when IDLE =>
            s_init_cnt <= 0;
            s_spi_frame        <= NOP;
            s_register_address <= 0;

          when INIT_SET =>
            s_init_cnt <= s_init_cnt + 1;

          when RXDR_ACK =>
            if (LocalAck_i = '1') then
              if (s_spi_frame = HEADER) then
                s_register_address <= to_integer(unsigned(LocalData_i(6 downto 0)));
                if (LocalData_i(7) = '0') then
                  s_spi_frame <= READ_DATA;
                else
                  s_spi_frame <= WRITE_DATA;
                end if;
              else
                if (LocalData_i = x"00") then
                  s_spi_frame <= HEADER;
                else
                  s_spi_frame <= NOP;
                end if;
              end if;
            end if;

          when others =>
            null;

        end case;

      end if;
    end if;
  end process CmdRegisterP;


  s_register_we <= LocalAck_i when s_cmdctrl_fsm = RXDR_ACK and s_spi_frame = WRITE_DATA else '0';


  RegisterFileP : process (Clk_i) is
  begin
    if (rising_edge(Clk_i)) then
      if (Rst_n_i = '0') then
        s_register <= (others => (others => '0'));
      else
        s_register(0)(0) <= '0';  --* reset RNG start after each clock cycle
        if (s_register_we = '1') then
          s_register(s_register_address) <= LocalData_i;
        end if;
        --+ register RNG data
        if (RngDataValid_i = '1') then
          s_register(0)(1) <= '1';
          s_register(1)    <= RngData_i;
        end if;
      end if;
    end if;
  end process RegisterFileP;


end architecture rtl;
