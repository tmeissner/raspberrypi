library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;



entity FiRoCtrlE is
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
end entity FiRoCtrlE;



architecture rtl of FiRoCtrlE is


  signal s_firo_run   : std_logic;
  signal s_firo_valid : std_logic;

  type t_neumann_state is (BIT1, BIT2, BIT3, BIT4);
  signal s_neumann_state : t_neumann_state;
  signal s_neumann_buffer : std_logic_vector(2 downto 0);

  type t_register_state is (SLEEP, COLLECT);
  signal s_register_state   : t_register_state;
  signal s_register_enable  : std_logic;
  signal s_register_din     : std_logic_vector(1 downto 0);
  signal s_register_data    : std_logic_vector(8 downto 0);
  signal s_register_counter : unsigned(2 downto 0);
  signal s_register_length  : natural range 1 to 2;

  signal s_data : std_logic_vector(3 downto 0);


begin


  Run_o  <= s_run when s_register_state = COLLECT else '0';
  s_data <= s_neumann_buffer & Data_i;


  ControllerP : process (Clk_i) is
    variable v_wait_cnt : unsigned(7 downto 0);
    variable v_run_cnt  : unsigned(7 downto 0);
  begin
    if (rising_edge(Clk_i)) then
      if (s_register_state = SLEEP) then
        v_wait_cnt    := unsigned(Wait_i);
        v_run_cnt     := unsigned(Run_i);
        s_firo_run    <= '0';
        s_firo_valid  <= '0';
      else
        s_firo_valid  <= '0';
        if (v_wait_cnt = 0) then
          s_firo_run <= '1';
        else
          v_wait_cnt := v_wait_cnt - 1;
        end if;
        if (v_run_cnt = 0) then
          s_firo_run   <= '0';
        elsif (v_run_cnt = 1) then
          s_firo_valid <= '1';
        else
          if (v_wait_cnt = 0) then
            v_run_cnt := v_run_cnt - 1;
          end if;
        end if;
      end if;
    end if;
  end process ControllerP;


  extractor : if EXTRACT generate
    VonNeumannP : process (Clk_i) is
    begin
      if (rising_edge(Clk_i)) then
        if (Reset_i = '0') then
          s_neumann_state   <= BIT1;
        else
          case s_neumann_state is

            when BIT1 =>
              s_register_enable <= '0';
              if (s_firo_valid = '1') then
                s_neumann_buffer(2) <= Data_i;
                s_neumann_state     <= BIT2;
              end if;

            when BIT2 =>
              if (s_firo_valid = '1') then
                s_neumann_buffer(1) <= Data_i;
                s_neumann_state     <= BIT3;
              end if;

            when BIT3 =>
              if (s_firo_valid = '1') then
                s_neumann_buffer(0) <= Data_i;
                s_neumann_state     <= BIT4;
              end if;

            when BIT4 =>
              if (s_firo_valid = '1') then
                s_register_enable <= '1';
                s_register_length <= 1;
                s_register_din    <= "00";
                s_neumann_state   <= BIT1;
                case (s_data) is
                  when x"5" =>
                    s_register_din <= "01";
                  when x"1" | x"6" | x"7" =>
                    s_register_length <= 2;
                  when x"2" | x"9" | x"b" =>
                    s_register_din    <= "01";
                    s_register_length <= 2;
                  when x"4" | x"a" | x"d" =>
                    s_register_din    <= "10";
                    s_register_length <= 2;
                  when x"8" | x"c" | x"e" =>
                    s_register_din    <= "11";
                    s_register_length <= 2;
                  when x"0" | x"f" =>
                    s_register_enable <= '0';
                  when others =>  -- incl. x"3"
                    null;
                end case;
              end if;

            when others =>
              null;

          end case;
        end if;
      end if;
    end process VonNeumannP;
  end generate;


  no_extractor : if not(EXTRACT) generate
    s_register_enable <= s_firo_valid;
    s_register_din(0) <= Data_i;
    s_register_length <= 1;
  end generate;


  Data_o <= s_register_data(7 downto 0);


  ShiftRegisterP : process (Clk_i) is
  begin
    if (rising_edge(Clk_i)) then
      if (Reset_i = '0') then
        s_register_counter <= (others => '1');
        s_register_state   <= SLEEP;
        DataValid_o              <= '0';
      else
        case s_register_state is

          when SLEEP =>
             DataValid_o        <= '0';
            if (Start_i = '1' and Run_i /= x"00") then
              s_register_state   <= COLLECT;
              s_register_data(0) <= s_register_data(8);
            end if;

          when COLLECT =>
            if (s_register_enable = '1') then
              if (s_register_counter = 0) then
                s_register_data  <= s_register_din(1) & s_register_data(6 downto 0) & s_register_din(0);
                DataValid_o      <= '1';
                s_register_state <= SLEEP;
              elsif (s_register_counter = 1) then
                if (s_register_length = 1) then
                  s_register_data(7 downto 0) <= s_register_data(6 downto 0) & s_register_din(0);
                end if;
                if (s_register_length = 2) then
                  s_register_data(7 downto 0) <= s_register_data(5 downto 0) & s_register_din;
                  DataValid_o                 <= '1';
                  s_register_state            <= SLEEP;
                end if;
              else
                if (s_register_length = 1) then
                  s_register_data(7 downto 0) <= s_register_data(6 downto 0) & s_register_din(0);
                else
                  s_register_data(7 downto 0) <= s_register_data(5 downto 0) & s_register_din;
                end if;
              end if;
              s_register_counter <= s_register_counter - s_register_length;
            end if;

          when others =>
            null;

        end case;
      end if;
    end if;
  end process ShiftRegisterP;


end architecture rtl;
