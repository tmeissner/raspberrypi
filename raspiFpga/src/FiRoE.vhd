library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

library machxo2;
  use machxo2.components.all;



entity FiRoE is
  generic (
    IMP    : string  := "HDL",
    TOGGLE : boolean := true
  );
  port (
    FiRo_o : out std_logic;
    Run_i  : in  std_logic
  );
end entity FiRoE;



architecture rtl of FiRoE is


  --+ signal for inverter loop
  signal s_ring : std_logic_vector(15 downto 0);
  signal s_tff  : std_logic;

  --+ attributes for synthesis tool to preserve inverter loop
  attribute syn_keep : boolean;
  attribute syn_hier : string;
  attribute syn_hier of rtl    : architecture is "hard";
  attribute syn_keep of s_ring : signal is true;
  attribute syn_keep of s_tff  : signal is true;

  --+ Attributes for lattice map tool to not merging inverter loop
  attribute nomerge : boolean;
  attribute nomerge of s_ring : signal is true;


begin


  FiroRingG : for index in 0 to 30 generate

    HdlG : if IMP = "HDL" generate

      s_ring(index) <= not(s_ring(index - 1));

    end generate HdlG;

    LutG : if IMP = "LUT" generate

      lut : LUT4
        generic map (
          init => x"FFFF"
        )
        port map (
          Z => s_ring(i-1),
          A => s_ring(i),
          B => '0',
          C => '0',
          D => '0'
        );

      end generate LutG;

  end generate FiroRingG;


  s_ring(0) <= (s_ring(15) xor s_ring(14) xor s_ring(7) xor s_ring(6) xor s_ring(5) xor s_ring(4) xor s_ring(2)) and Run_i;


  WithToggleG : if TOGGLE generate

    tffP : process(Run_i, s_ring(15)) is
    begin
      if(Run_i = '0') then
        s_tff <= '0';
      elsif(rising_edge(s_ring(15))) then
        s_tff <= not s_tff;
      end if;
    end process tffP;

    FiRo_o <= s_ring(15) xor s_tff;

  end generate WithToggleG;


  WithoutToggleG : if not(TOGGLE) generate

    FiRo_o <= s_ring(15);

  end generate WithoutToggleG;


end architecture rtl;
