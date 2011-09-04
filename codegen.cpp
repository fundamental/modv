#include <stdio.h>
#include <string>
#include <iostream>
#include <string>
#include <list>
#include <typeinfo>
#include <boost/format.hpp>


extern FILE *ofile;
namespace CodeGen {
    using std::cout;
    using std::endl;
    using std::ostream;
    using std::string;
    using std::type_info;
    using boost::format;

    bool ast = false;

#define ast_format(fmt, args) string ast_string(void) const \
    {ast=true;return str(format(fmt) % args);}
#define code_format(fmt, args) string code_string(void) const \
    {ast=false;return str(format(fmt) % args);}
#define ast_format0(fmt) string ast_string(void) const \
    {return fmt;}
#define code_format0(fmt) string code_string(void) const \
    {return fmt;}

    struct Value
    {
        virtual string ast_string(void) const=0;
        virtual string code_string(void) const=0;
    };

    ostream &operator<<(ostream &o, const Value &v)
    {
        if(ast)
            return o << v.ast_string();
        else
            return o << v.code_string();
    }

    ostream &operator<<(ostream &o, const Value *v)
    { return o << (*v); }

    template<class T>
    ostream &operator<<(ostream &o, const std::list<T> &p)
    {
        for(auto itr = p.begin(); itr != p.end(); ++itr)
            o << *itr << " ";
        return o;
    }

    struct Expr : public Value {};

    struct Symbol : public Expr
    {
        Symbol(string _name) :name(_name){}
        string name;
        ast_format("(symbol %1%)", name);
        code_format("%1%", name);
    };

    struct Context
    {
        Symbol *modname;
    } context;

    struct Port : public Value
    {
        Port(const char *_dir, Symbol *_name, Symbol *_type) :dir(_dir), name(_name), type(_type) {}
        const char *dir;
        Symbol *name;
        Symbol *type;

        ast_format("(port %1% %2% %3%)", dir % name % type);
        code_format("%1% : %2% %3%", name % dir % type);
    };

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
        const char *n = context.modname->code_string().c_str();
        fprintf(ofile, fmt, use, n, head, n, n, arch);
    }
}
