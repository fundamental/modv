module leds

head:
    cpu  inout  sys_bus
    leds buffer byte
end

arch:
    sync(cpu.clk, cpu.addr=0x01 and !cpu.rw)
        leds   = cpu.data
end
