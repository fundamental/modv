module donothing

head:
    a   in  logv[5]
    clk in  log
    b   out logv[5]
    c   out logv[8]
end

arch:
    signal temp log

    sync(clk) temp = a

    b = temp

    c = 0xfe

end
