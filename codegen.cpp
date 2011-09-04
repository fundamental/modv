#include <stdio.h>
#include <string>

extern FILE *ofile;
extern std::string mod_name;
namespace codeGen {
    const char *fmt = 
        "library ieee;\n"
        "use ieee.std_logic_1164.all;\n"
        "use work.all;\n"
        "use modv.all;\n"
        "%s"
        "\n"
        "entity %s is\n"
        "%s\n"
        "end %s;\n"
        "\n"
        "architecture default of %s is\n"
        "%s\n"
        "end architecture;\n";

    void module(const char *use, const char *head,const char *arch)
    {
        const char *n = mod_name.c_str();
        fprintf(ofile, fmt, use, n, head, n, n, arch);
    }
}
