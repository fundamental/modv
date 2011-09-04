module HexDisplay

head:
    Q   in  logv[4]
    en  in  boolean
    HEX out logv[7]
end

arch:
    signal tmp logv[7]
    
    HEX = en ? tmp : "1111111"
    tmp = lookup(Q):
        0x0:"1000000"
        0x1:"1111001"
        0x2:"0100100"
        0x3:"0110000"
        0x4:"0011001"
        0x5:"0010010"
        0x6:"0000010"
        0x7:"1111000"
        0x8:"0000000"
        0x9:"0011000"
        0xa:"0001000"
        0xb:"0000011"
        0xc:"1000110"
        0xd:"0100001"
        0xe:"0000110"
        0xf:"0001110"
    end
end
