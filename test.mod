module donothing

head:
    a   in  log
    clk in  log
    b   out log
end

arch:
    signal temp log

    sync(clk) temp = a

    b    = temp
end
