module donothing

head:
    a   in  logv[5]
    h   in  logv[4]
    clk in  log
    b   out logv[5]
    c   out logv[8]
    d   out logv[8]
end

arch:
    signal temp log

    sync(clk) temp = a

    b = temp

    c = 0xfe

    d = lookup(h):
        0x1: 0x45
        0x3: 0x73
        0x5: 0xaf
        0x8: 0xff
        0x9: 0xfc
        0xa: 0xba
        0xc: 0x01
        0xd: 0x00
        0xf: 0x33
        others: 0x9f
    end
end
