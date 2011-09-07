cmake_minimum_required(VERSION 2.8)
project(MODV)

find_package(FLEX REQUIRED)
find_package(BISON REQUIRED)
find_package(Boost REQUIRED)

set(BIN_DIR ${CMAKE_CURRENT_BINARY_DIR})
set(SRC_DIR ${CMAKE_CURRENT_SOURCE_DIR})
set(CMAKE_CXX_FLAGS "--std=gnu++0x -Wall -Wextra")

BISON_TARGET(Parser modv.y ${BIN_DIR}/parse.cpp COMPILE_FLAGS "-v -t")
FLEX_TARGET(Scanner modv.lex ${BIN_DIR}/lexer.cpp)
ADD_FlEX_BISON_DEPENDENCY(Scanner Parser)

include_directories(${BIN_DIR} ${SRC_DIR})
add_executable(modv-comp
    ${BISON_Parser_OUTPUTS}
    ${FLEX_Scanner_OUTPUTS}
)

enable_testing()
add_subdirectory(tests)
