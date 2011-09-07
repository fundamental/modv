#include <string>
#include <sstream>
#include <boost/format.hpp>
#include <list>

namespace CodeGen {
    using std::string;
    using std::stringstream;
    using boost::format;
    extern bool ast;

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

    struct Expr : public Value {};
    struct Lvalue : public Expr {};

    struct Symbol : public Lvalue
    {
        Symbol(string _name) :name(_name){}
        Symbol(char _name) :name(&_name, 1) {}
        Symbol(int _name)
        {
            stringstream ss;
            ss << _name;
            name = ss.str();
        }
        string name;
        ast_format("(symbol %1%)", name);
        code_format("%1%", name);
    };

    typedef std::list<Value*> Vlist;
}
