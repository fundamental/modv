library ieee;
use ieee.std_logic_1164.all;
use work.all;
use modv.all;

entity donothing is
port(a: in logv(4 downto 0);
h: in logv(3 downto 0);
clk: in log;
b: out logv(4 downto 0);
c: out logv(7 downto 0);
d: out logv(7 downto 0));

end donothing;

architecture default of donothing is
signal temp:log;
begin
process(all)
begin
if(rising_edge(clk)) then
temp<=a;
end if;
end process;
b<=temp;
c<=X"fe";
with h select d <=
X"45" when X"1",
X"73" when X"3",
X"af" when X"5",
X"ff" when X"8",
X"fc" when X"9",
X"ba" when X"a",
X"01" when X"c",
X"00" when X"d",
X"33" when X"f",
X"9f" when others;

end architecture;
