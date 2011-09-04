library ieee;
use ieee.std_logic_1164.all;
use work.all;
use modv.all;

entity HexDisplay is
port(Q: in logv(3 downto 0);
en: in boolean;
HEX: out logv(6 downto 0));

end HexDisplay;

architecture default of HexDisplay is
signal tmp:logv(6 downto 0);
begin
HEX<= tmp when en else "1111111";
with Q select tmp <=
"1000000" when X"0",
"1111001" when X"1",
"0100100" when X"2",
"0110000" when X"3",
"0011001" when X"4",
"0010010" when X"5",
"0000010" when X"6",
"1111000" when X"7",
"0000000" when X"8",
"0011000" when X"9",
"0001000" when X"a",
"0000011" when X"b",
"1000110" when X"c",
"0100001" when X"d",
"0000110" when X"e",
"0001110" when X"f";

end architecture;
