library ieee;
use ieee.std_logic_1164.all;
use work.all;
use modv.all;

entity leds is
port(cpu: inout sys_bus;
leds: buffer byte);

end leds;

architecture default of leds is
begin
process(all)
begin
if(rising_edge(cpu.clk)and cpu.addr = X"01" and not cpu.rw) then
leds<=cpu.data;
end if;
end process;

end architecture;
