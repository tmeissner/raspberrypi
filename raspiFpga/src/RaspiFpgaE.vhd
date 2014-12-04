library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

library machxo2;
  use machxo2.components.all;



entity RaspiFpgaE is
  port (
    --+ SPI slave if
    SpiSclk_i    : in  std_logic;
    SpiSte_i     : in  std_logic;
    SpiMosi_i    : in  std_logic;
    SpiMiso_o    : out std_logic
  );
end entity RaspiFpgaE;



architecture rtl of RaspiFpgaE is


begin


end architecture rtl;