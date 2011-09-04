macro(check_output fname)
    add_test(${fname}
        "${SRC_DIR}/tests/run_test.sh"
        "${BIN_DIR}/modv-comp"
        "${SRC_DIR}/tests/input/${fname}.mod"
        "${BIN_DIR}/${fname}.vhd"
        "${SRC_DIR}/tests/output/${fname}.vhd")
endmacro(check_output)
check_output(HexDisplay)
check_output(leds)
check_output(test)
check_output(vutest)
