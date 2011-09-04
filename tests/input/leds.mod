module leds

head:
    cpu  inout sys_bus
    leds buf   byte
end

arch:
    sync(cpu.clk, cpu.addr==0x01 and not cpu.rw)
        leds   = cpu.data
end
