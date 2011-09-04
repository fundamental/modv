library ieee;
use ieee.std_logic_1164.all;
use work.all;
use modv.all;

entity vutest is
port(clock_50: in log;
sw: in logv(3 downto 0);
key: in logv(3 downto 0);
ledr: out logv(7 downto 0);
ledg: out logv(7 downto 0));

end vutest;

architecture default of vutest is
signal input:logv(3 downto 0);
signal output:logv(15 downto 0);
signal sysTick:log;
begin
ledg(6 downto 0)<=output(6 downto 0);
ledr(6 downto 0)<=output(14 downto 8);
with input select output <=
"000000000000000" when X"0",
"000000000000001" when X"1",
"000000000000011" when X"2",
"000000000000111" when X"3",
"000000000001111" when X"4",
"000000000011111" when X"5",
"000000000111111" when X"6",
"000000001111111" when X"7",
"000000011111111" when X"8",
"000000111111111" when X"9",
"000001111111111" when X"a",
"000011111111111" when X"b",
"000111111111111" when X"c",
"001111111111111" when X"d",
"011111111111111" when X"e",
"111111111111111" when X"f";
process(all)
begin
if(rising_edge(clk)) then
case (foobar) is 
when area =>
statement<=X"beef";

another<=statement;

when beyond =>
argv<="123123";

to<=long;

when default =>
the<=cases;
end case;
end if;
end process;
S0:Counter generic map(4) port map('0','1','1','1',sysTick,clock_50,X"f",input);
S1:ClockEnabler generic map(50000000) port map(clock_50,sysTick);

end architecture;
