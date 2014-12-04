library ieee;
  use ieee.std_logic_1164.all;



entity RaspiFpgaCtrlE is
  generic (
    G_ADR_WIDTH  : positive := 8;  --* address bus width
    G_DATA_WIDTH : positive := 8   --* data bus width
  );
  port (
    --+ System if
    Rst_n_i       : in  std_logic;
    Clk_i         : in  std_logic;
    --+ local register if
    LocalWen_o    : out std_logic;
    LocalRen_o    : out std_logic;
    LocalAdress_o : out std_logic_vector(G_ADR_WIDTH-1 downto 0);
    LocalData_i   : in  std_logic_vector(G_DATA_WIDTH-1 downto 0);
    LocalData_o   : out std_logic_vector(G_DATA_WIDTH-1 downto 0);
    LocalAck_i    : in  std_logic;
    LocalError_i  : in  std_logic;
    --+ EFB if
    EfbSpiIrq_i   : in  std_logic
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

  --+ init fsm types and signals
  type t_init_fsm is (IDLE, SET, ACK, INIT_DONE);
  signal s_init_fsm : t_init_fsm;

  signal s_init_done   : boolean;
  signal s_init_wen    : std_logic;
  signal s_init_adress : std_logic_vector(G_ADR_WIDTH-1 downto 0);
  signal s_init_data   : std_logic_vector(G_DATA_WIDTH-1 downto 0);

  signal s_ctl_wen    : std_logic;
  signal s_ctl_adress : std_logic_vector(G_ADR_WIDTH-1 downto 0);
  signal s_ctl_data   : std_logic_vector(G_DATA_WIDTH-1 downto 0);

  type t_adress_array is array (natural range <>) of std_logic_vector(G_ADR_WIDTH-1 downto 0);
  constant C_INIT_ADR  : t_adress_array := (C_SPICR1, C_SPICR2, C_SPIIRQEN);

  type t_data_array is array (natural range <>) of std_logic_vector(G_DATA_WIDTH-1 downto 0);
  constant C_INIT_DATA : t_data_array := (x"80", x"00", x"18");

  signal s_init_cnt : natural range 0 to C_INIT_ADR'length-1;


begin


  LocalWen_o    <= s_init_wen    when not(s_init_done) else s_ctl_wen;
  LocalAdress_o <= s_init_adress when not(s_init_done) else s_ctl_adress;
  LocalData_o   <= s_init_data   when not(s_init_done) else s_ctl_data;


  InitP : process (Clk_i) is
  begin
    if (rising_edge(Clk_i)) then
      if (Rst_n_i = '0') then
        s_init_cnt <= 0;
        s_init_fsm <= IDLE;
      else
        FsmC : case s_init_fsm is

          when IDLE =>
            s_init_cnt <= 0;
            s_init_fsm <= SET;

          when SET =>
            s_init_fsm <= ACK;

          when ACK =>
            if (LocalAck_i = '1') then
              if (s_init_cnt = C_INIT_ADR'length-1) then
                s_init_fsm <= INIT_DONE;
              else
                s_init_cnt <= s_init_cnt + 1;
                s_init_fsm <= SET;
              end if;
            end if;

          when INIT_DONE =>
            null;

          when others =>
            null;

        end case FsmC;
      end if;
    end if;
  end process InitP;


  s_init_wen    <= '1' when s_init_fsm = SET else '0';
  s_init_adress <= C_INIT_ADR(s_init_cnt);
  s_init_data   <= C_INIT_DATA(s_init_cnt);

  s_init_done <= true when s_init_fsm = INIT_DONE else false;



end architecture rtl;
