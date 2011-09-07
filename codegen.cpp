#include <codegen.h>

#include <stdio.h>
#include <string>
#include <iostream>
#include <list>
#include <typeinfo>

namespace CodeGen {
    using std::cout;
    using std::endl;
    using std::ostream;
    using std::string;
    using std::type_info;

    bool ast = false;

    ostream &operator<<(ostream &o, const Value &v)
    {
        if(ast)
            return o << v.ast_string();
        else
            return o << v.code_string();
    }

    ostream &operator<<(ostream &o, const Value *v)
    {
        if(v)
            return o << (*v);
        else
            return o << "(null)";
    }

    template<class T>
    ostream &operator<<(ostream &o, const std::list<T> &p)
    {
        for(auto itr = p.begin(); itr != p.end(); ++itr)
            o << *itr << " ";
        return o;
    }

    template<class T>
    ostream &operator<<(ostream &o, const std::list<T> *p)
    {
        return o << *p;
    }

    struct PortPrinter : public Value
    {
        PortPrinter(const std::list<class Port*> &_v) :v(_v) {}
        const std::list<class Port*> &v;
        ast_format0("(PortPrinter)");
        string code_string(void) const
        {
            stringstream st;
            for(auto itr = v.begin(); itr != v.end();) {
                st <<  (Value*)*itr;
                ++itr;
                if(itr != v.end())
                    st << ";\n";
            }
            return st.str();
        }
    };

    template<class T>
    struct ListPrinter : public Value
    {
        ListPrinter(const std::list<T> &_v) :v(_v) {}
        const std::list<T> &v;
        ast_format0("(List)");
        string code_string(void) const
        {
            stringstream st;
            for(auto itr = v.begin(); itr != v.end();++itr)
                st <<  *itr << ";\n";
            return st.str();
        }
    };

    template<class T>
    struct ArgsPrinter : public Value
    {
        ArgsPrinter(const std::list<T> *_v) :v(_v) {}
        const std::list<T> *v;
        ast_format0("(ArgsPrinter)");
        string code_string(void) const
        {
            stringstream st;
            for(auto itr = v->begin(); itr != v->end();) {
                st <<  *itr;
                ++itr;
                st << (itr == v->end() ? "" : ",");
            }
            return st.str();
        }
    };

    Symbol genSym(void)
    {
        static int idx = 0;
        stringstream st;
        st << "S" << idx++;
        return Symbol(st.str());
    }

    struct Context
    {
        Symbol *modname;
        //FIXME
        void add_use(void *) const {};
    } context;

    struct SimpleLvalue : public Lvalue
    {
        SimpleLvalue(Symbol *_sym) :sym(_sym){}
        Symbol *sym;
        ast_format("(SimpleLvalue %1%)", sym);
        code_format("%1%", sym);
    };

    struct Int : public Value
    {
        Int(int _num) :num(_num) {}
        int num;
        ast_format("(Int %1%)", num);
        code_format("%1%", num);
    };

    struct RangedLvalue : public Lvalue
    {
        RangedLvalue(Symbol *_sym, Value *_idx) :sym(_sym), idx(_idx){}
        Symbol *sym;
        Value  *idx;
        ast_format("(RangedLvalue %1% %2%)", sym % idx);
        code_format("%1%(%2%)", sym % idx);
    };

    struct Range : public Value
    {
        Range(int _high, int _low) :high(_high), low(_low) {}
        Range(int size) :high(size-1), low(0) {}
        int high, low;
        ast_format("(Range %1% %2%)", high % low);
        code_format("%1% downto %2%", high % low);
    };

    struct SyncRelation : virtual Value {};
    struct ConcRelation : virtual Value {};

    struct Ternary : public ConcRelation
    {
        Ternary(Lvalue *_l, Expr *_test, Expr *_tcase, Expr *_fcase)
        :l(_l), test(_test), tcase(_tcase), fcase(_fcase) {}
        Lvalue *l;
        Expr *test, *tcase, *fcase;
        ast_format("(Ternary %1% %2% %3% %4%)", l % test % tcase % fcase);
        code_format("%1% <= %3% when %2% else %4%", l % test % tcase % fcase);
    };

    struct Function : public Expr
    {
        Function(Symbol *_fname, Expr *_exp) :fname(_fname), exp(_exp) {}
        Symbol *fname;
        Expr   *exp;
        ast_format("(Function %1% %2%)", fname % exp);
        code_format("%1%(%2%)", fname % exp);
    };

    struct Others : public Expr
    {
        Others(Value *_v) :v(_v) {}
        Value *v;
        ast_format0("(Others)");
        code_format0("NO_CODEGEN [(others =>  + $2 + )");
    };

    struct SimpleRelation : virtual SyncRelation, virtual ConcRelation
    {
        SimpleRelation(Lvalue *_lval, Expr *_rval) :lval(_lval), rval(_rval) {}
        Lvalue *lval;
        Expr   *rval;

        ast_format("(%1% %2% %3%)", "SimpleRelation" % lval % rval);
        code_format("%1% <= %2%", lval % rval);
    };

    struct Op2 : public Expr
    {
        Op2(const char *_op, Value *_lhs, Value *_rhs) :op(_op), lhs(_lhs), rhs(_rhs) {}
        const char *op;
        const Value *lhs, *rhs;
        ast_format("(op2 %1% %2% %3%)", op % lhs % rhs);
        code_format("%1% %2% %3%", lhs % op % rhs);
    };

    struct Op1 : public Expr
    {
        Op1(const char *_op, Value *_oper) :op(_op), oper(_oper){}
        const char *op;
        const Value *oper;
        ast_format("(op1 %1% %2%)", op % oper);
        code_format("%1% %2%", op % oper);
    };

    struct Type : public Value
    {
    };

    struct VectorType : public Type
    {
        VectorType(Symbol *_sym, Range *_r) :sym(_sym),r(_r) {}
        Symbol *sym;
        Range *r;
        ast_format("(VectorType %1% %2%)", sym % r);
        code_format("%1%(%2%)", sym % r);
    };

    struct ScalarType : public Type
    {
        ScalarType(Symbol *_sym) :sym(_sym) {}
        Symbol *sym;
        ast_format("(ScalarType %1%)", sym)
        code_format("%1%", sym);
    };

    struct Port : public Value
    {
        Port(Symbol *_dir, Symbol *_name, Type *_type) :dir(_dir), name(_name), type(_type) {}
        Symbol *dir;
        Symbol *name;
        Type   *type;

        ast_format("(port %1% %2% %3%)", dir % name % type);
        code_format("%1% : %2% %3%", name % dir % type);
    };

    struct Signal : public Value
    {
        Signal(Symbol *_name, Type *_type) :name(_name), type(_type) {}
        Symbol *name;
        Type   *type;

        ast_format("(Signal %1% %2%)", name % type);
        code_format("signal %1% : %2%", name % type);
    };

    struct ModDef : public Value
    {
        std::list<Port*> in, out;
        ast_format("(moddef %1% %2%)", in % out);
        code_format("entity %1% is\nport(%2%%3%);\nend %1%;",
                context.modname % PortPrinter(in) % PortPrinter(out));
    };

    struct ElsePrinter : public Value
    {
        ElsePrinter(const std::list<SyncRelation*> &_l) :l(_l) {}
        const std::list<SyncRelation*> &l;
        ast_format0("(else)");
        string code_string(void) const
        {
            stringstream st;
            if(l.empty())
                return "";
            st << "else\n";
            for(auto itr = l.begin(); itr != l.end(); ++itr)
                st <<  *itr << ";\n";
            return st.str();
        }
    };

    struct Cond : public SyncRelation
    {
        Cond(Expr *_cond) :cond(_cond) {}
        Expr *cond;
        std::list<SyncRelation*> when;
        std::list<SyncRelation*> unless;
        ast_format("(If %1% (when %2%) (unless %3%))", cond % when % unless);
        code_format("if(%1%) then\n%2%%3%end if", cond % ListPrinter<SyncRelation*>(when) % ElsePrinter(unless));
    };

#if 0
    struct OptPrinter : public Value
    {
        OptPrinter(const char *_fmt, Value *_v)
            :fmt(_fmt), v(_v) {}
        const char *fmt;
        Value *v;
        ast_format0("OptPrinter");
        string code_string(void) const
        {return v ? str(format(fmt % v)) : "";}
    };
#endif

    struct Sync : public ConcRelation
    {
        Sync(Symbol *sym) :cond(new Cond(new Function(new Symbol("rising_edge"), sym))) {}
        Sync(Symbol *sym, Expr *expr) :cond(new Cond(new Op2("and", new Function(new Symbol("rising_edge"), sym), expr))) {}
        Cond *cond;
        std::list<SyncRelation*> async;
        ast_format("(Sync %1% %2%)", cond % async);
        code_format("process(all)\nbegin\n%1%;\n%2%\nend process", cond % async);
    };

    struct LookupPair : public Value
    {
        LookupPair(Expr *b) :src(new Symbol("others")), dst(b){};
        LookupPair(Value *a, Expr *b) :src(a), dst(b){};
        Value *src;
        Expr   *dst;
        ast_format("(LookupPair %1% %2%)", src % dst);
        code_format("%1% when %2%", dst % src);
    };

    struct LookupPrinter : public Value
    {
        LookupPrinter(const std::list<LookupPair*> &_l) :l(_l) {}
        const std::list<LookupPair*> &l;
        ast_format0("(LookupPrinter)");
        string code_string(void) const
        {
            stringstream st;
            for(auto itr = l.begin(); itr != l.end();) {
                st <<  *itr;
                ++itr;
                st << (itr == l.end() ? "" : ",\n");
            }
            return st.str();
        }
    };

    struct Lookup : public ConcRelation
    {
        //TODO make dest into Lvalue
        Lookup(Value *dest, Expr *src) :selected(dest), selector(src) {}
        Value *selected;
        Expr *selector;
        std::list<LookupPair*> entries;
        ast_format("(Lookup %1% %2% %3%)", selector % selected % entries);
        code_format("with %1% select %2% <=\n%3%", selector % selected % LookupPrinter(entries));
    };

    struct CaseLabel : public Value
    {
        CaseLabel(Symbol *_sym) :sym(_sym) {}
        Symbol *sym;
        ast_format("(CaseLabel %1%)", sym);
        code_format("%1%", sym);
    };

    struct CaseBlock : public Value
    {
        CaseBlock(CaseLabel *_sel) :sel(_sel) {}
        CaseLabel *sel;
        std::list<SyncRelation*> code;
        ast_format("(case-block %1% %2%)", sel % code);
        code_format("when %1% =>\n%2%", sel % ListPrinter<SyncRelation*>(code));
    };

    struct Case : public SyncRelation
    {
        Case(Expr *_sel) :sel(_sel) {}
        Expr *sel;
        std::list<CaseBlock*> cases;
        ast_format("(case %1% %2%)", sel % cases);
        code_format("case(%1%) is\n%2%\nend case", sel % cases);

        void buildCases(Vlist *list)
        {
            //get first entry
            assert(!list->empty());
            auto itr=list->begin();
            CaseLabel *label = dynamic_cast<CaseLabel*>(*itr);
            ++itr;
            std::cerr << list << endl;
            std::cerr << typeid(**itr).name() << endl;;
            assert(label);

            CaseBlock *current = new CaseBlock(label);

            for(; itr != list->end(); ++itr) {
                SyncRelation *s = dynamic_cast<SyncRelation*>(*itr);
                CaseLabel    *l = dynamic_cast<CaseLabel*>(*itr);
                assert(s||l);
                if(s)
                    current->code.push_back(s);
                if(l) {
                    cases.push_back(current);
                    current = new CaseBlock(l);
                }
            }
            cases.push_back(current);
        }
    };

    struct Arch : public Value
    {
        std::list<Signal*> signals;
        std::list<ConcRelation*> code;
        void add(const Vlist *v)
        {
            for(auto itr = v->begin(); itr != v->end(); ++itr) {
                Signal *sig = dynamic_cast<Signal*>(*itr);
                ConcRelation *rel = dynamic_cast<ConcRelation*>(*itr);
                if(sig)
                    signals.push_back(sig);
                else if(rel)
                    code.push_back(rel);
                else {
                    warnx("bad type of %p", *itr);
                    warnx("type is %s", typeid(**itr).name());
                    errx(1, "Impossible condition at %d of %s", __LINE__, __FILE__);
                }
            }
        }
        ast_format("(Arch %1% %2%)", signals % code);
        code_format("architecture default of %1% is\n%2%\nbegin\n%3%\nend architecture;\n",
                context.modname % ListPrinter<Signal*>(signals) % ListPrinter<ConcRelation*>(code));
    };

    struct GenericBinding : public Value
    {
        GenericBinding(Vlist *_v) :v(_v){}
        Vlist *v;
        ast_format("(GenericBinding %1%)", v);
        code_format("generic map(%1%)", ArgsPrinter<Value*>(v));
    };

    struct PortBinding : public Value
    {
        PortBinding(Vlist *_v) :v(_v){}
        Vlist *v;
        ast_format("(PortBinding %1%)", v);
        code_format("port map(%1%)", ArgsPrinter<Value*>(v));
    };

    struct SubModule : public ConcRelation
    {
        SubModule(Symbol *_name) : name(_name), sym(genSym()) {}
        Symbol *name, sym;
        Vlist *connections;
        ast_format("(SubModule %1% %2% :ports %3%)", name % sym % connections);
        code_format("%2%: %1% %3%", name % sym % connections);
    };

    struct Include : public Value {};

    Symbol *toSym(Vlist &vlst)
    {
        Symbol *result = dynamic_cast<Symbol*>(*vlst.begin());
        if(result)
            return result;
        return new Symbol("invalid vlist");
    }

    template<class T>
    void operator<<(std::list<T*> &t, Vlist *_v)
    {
        Vlist &v = *_v;
        for(auto itr = v.begin(); itr != v.end(); ++itr) {
            T *_t = dynamic_cast<T*>(*itr);
            assert(_t);
            t.push_back(_t);
        }
    }

    template<class T>
    void operator<<(std::list<T*> &t, Value *v)
    {
        T *_t = dynamic_cast<T*>(v);
        assert(_t);
        t.push_back(_t);
    }

    void append(Vlist *a, Vlist *b)
    {
        for(auto itr = b->begin(); itr != b->end(); ++itr)
            a->push_back(*itr);
    }

    static const char *header =
        "library ieee;\n"
        "use ieee.std_logic_1164.all;\n"
        "use work.all;\n"
        "use modv.all;\n\n";

    void module(const Context &ctx, const ModDef &_head, const Arch &_arch)
    {
        ast = false;
        ofile << header << _head << endl << endl << _arch << endl;
        cout << ofile.rdstate() << endl;
        if(!ofile.good())
            errx(1, "output file is not good!");
    }
}
