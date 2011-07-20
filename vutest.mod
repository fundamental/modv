module vutest

head:
    clock_50    in  log
    sw          in  logv[4]
    key         in  logv[4]
    ledr        out logv[8]
    ledg        out logv[8]
end

arch:
    signal input   logv[4]
    signal output  logv[16]
    signal sysTick log

    
    ledg[7:0] = output[7:0]
    ledr[7:0] = output[15:8]

    output = lookup(input):
        0x0: "000000000000000"
        0x1: "000000000000001"
        0x2: "000000000000011"
        0x3: "000000000000111"
        0x4: "000000000001111"
        0x5: "000000000011111"
        0x6: "000000000111111"
        0x7: "000000001111111"
        0x8: "000000011111111"
        0x9: "000000111111111"
        0xa: "000001111111111"
        0xb: "000011111111111"
        0xc: "000111111111111"
        0xd: "001111111111111"
        0xe: "011111111111111"
        0xf: "111111111111111"
    end

    sync(clk):
        case(foobar):
            area:
                statement = 0xbeef
                another   = statement
            beyond:
                argv = "123123"
                to   = long
            default:
                the = cases
        end
    end
        


    module(Counter):
        generic(4)
        port('0','1','1','1',sysTick,clock_50,0xf,input)
    end

    module(ClockEnabler):
        generic(50000000)
        port(clock_50, sysTick)
    end
end
