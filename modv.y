%{
#include <fstream>
#include <string>
#include <sstream>
#include <cstdio>
#include <cstdlib>
#include <error.h>
#include <err.h>
#include <errno.h>
extern std::ofstream ofile;
#include "codegen.cpp"
using std::string;
using std::stringstream;
using namespace CodeGen;

extern int yylineno;

bool insync = false;

void yyerror(const char *str)
{
    error(1,0,"near line %d: %s\n", yylineno, str);
}

int yywrap(void)
{
    return 1;
}

int yyparse();
int yylex();

string gen_sym(void)
{
    static int idx = 0;
    stringstream st;
    st << "S" << idx++ << ":";
    return st.str();
}

string dec(string str)
{
    stringstream st;
    st << atoi(str.c_str())-1;
    return st.str();
}

Symbol _IN("in"),
       _OUT("out"),
       _INOUT("inout"),
       _BUF("buffer"),
       _GENERIC("generic");


std::ofstream ofile;
int main(int argc, char **argv)
{
    if(argc!=3)
        error(1,0,"usage:: %s in-file out-file", *argv);

    extern FILE *yyin;
    if(!(yyin=fopen(argv[1], "r")))
        error(1, errno, "Failed to open input");

    ofile.open(argv[2]);
    if(!ofile.good())
        error(1, errno, "Failed to open output");

    //extern int yydebug;
    //yydebug=1;
    yyparse();

    fclose(yyin);
    ofile.close();
    return 0;
}

string compound(string cur, string next)
{
    if(next[0]=='^')
        return cur.insert(cur.find("begin"),next.substr(1));
    return cur + next;
}

Symbol *make_use(Symbol *sym)
{
    if(sym->name == "unsigned")
        sym->name = "ieee.std_logic_unsigned";
    sym->name = "use " + sym->name + ".all;\n";
    return sym;
}

string get_port(string &plist)
{
    size_t pos = plist.find("@");
    if(pos == string::npos)
        fprintf(stderr,"error: string logic error in get_port()");
    string ret(plist.substr(0,pos));
    plist = plist.substr(pos+1);
    return ret;
}
string format_ports(string plist)
{
    string ports, generics;
    while(!plist.empty())
    {
        string port = get_port(plist);
        char type = port[0];
        port = port.substr(1);
        if(type=='g')
            generics += port + ";\n";
        else
            ports += port + ";\n";
    }
    if(!generics.empty())
        generics.erase(generics.length()-2);
    if(!ports.empty())
        ports.erase(ports.length()-2);
    string result = "port(" + ports + ");\n";
    if(!generics.empty())
        result = "generic(" + generics + ");\n" + result;
    return result;
}


string make_sync(string clk, string extra="")
{
    string ret("if(rising_edge("+clk+")");

    if(extra.empty())
        return ret + ") then\n";
    return ret + "and " + extra + ") then\n";
}
string make_syncfull(string init, string stmts)
{
    string ret("process(all)\nbegin\n");
    int low;
    if((low = stmts.find('@'))!=string::npos) { //handle async statements
        int high = stmts.find('@',low+1);
        ret += stmts.substr(low+1,high-1);
        stmts.erase(low,high-low+2);
    }

    ret += init + stmts + "end if;\nend process;\n";
    return ret;
}
string make_ternary(string var, string cond, string t_case, string f_case)
{
    if(insync)
        return "if(" + cond + ") then\n " + var + "<= " + t_case + ";\nelse\n" +
               var + "<= " + f_case + ";\nend if;\n";
    else
        return var + "<= " + t_case + " when " + cond + " else " + f_case + ";\n";
}

//TODO FIXME
Vlist *make_async(Vlist *vstmts)
{
    return vstmts;
}

Vlist *make_signals(Vlist *names, Type *t)
{
    Vlist *list = new Vlist();
    for(auto itr = names->begin(); itr != names->end(); ++itr) {
        Symbol *sym = dynamic_cast<Symbol*>(*itr);
        assert(sym);
        list->push_back(new Signal(sym, t));
    }
    return list;
}

Vlist *make_ports(Vlist *names, Symbol *dir, Type *t)
{
    Vlist *list = new Vlist();
    for(auto itr = names->begin(); itr != names->end(); ++itr) {
        Symbol *name = dynamic_cast<Symbol*>(*itr);
        assert(name);
        list->push_back(new Port(dir, name, t));
    }
    return list;
}
%}

%union
{
    int     num;
    char    chr;
    char   *string;
    struct Expr   *expr;
    struct Port   *port;
    struct Arch   *arch;
    struct Sync   *sync;
    struct Cond   *cond;
    struct Case   *cse;
    struct Type   *type;
    struct Range  *range;
           Vlist  *vlst;
    struct Value  *val;
    struct ModDef *header;
    struct Symbol *sym;
    struct Signal *sig;
    struct Lvalue *lval;

    struct Lookup  *lup;

    struct Ternary *ternary;

    struct SubModule *mref;
    struct CaseBlock *cblock;


    struct SimpleRelation *srelation;
}

%token MODULE ARCH SIGNAL HEAD END OUT IN SYNC LOOKUP DEFAULT SYMBOL STRING INT
%token INOUT HEX BUF ENUM PORT GENERIC CHAR CASE IF ELSE USE ASYNC

%type<num>          INT
%type<chr>          CHAR
%type<sym>          direction SYMBOL DEFAULT
%type<cse>          case
%type<lup>          lookup_block
%type<sym>          use
%type<type>         type
%type<val>          literal sync_statement type_declare mod_conn
%type<val>          lookup_entry module_block simple_relation case_stmt
%type<vlst>         slist ports statements sync_statements sdeclares cases
%type<vlst>         lookup_entries explist mod_conns signal_block
%type<vlst>         async_block sdeclare 
%type<vlst>         port statement
%type<lval>         lvalue
%type<mref>         mod_ref
%type<expr>         expr others_expr
%type<arch>         arch_desc
%type<sync>         sync_block sync_srt
%type<cond>         if_block
%type<range>        range
%type<string>       enum_declare
%type<header>       head_declare
%type<ternary>      ternary

%left AND
%left OR
%left EQEQ
%left NEQ
%left XOR
%left '&' '<' '>' '+' '-'
%left ELSE
%nonassoc NOT

%%
module: mod_declare uses head_declare arch_desc
      {module(context, *$3, *$4);}
      ;
uses:
    | uses use {context.add_use($2);}
    ;
use: USE SYMBOL {$$=make_use($2);}
   ;

mod_declare: MODULE SYMBOL {context.modname = $2;}
           ;
head_declare: HEAD ':' ports END {$$ = new ModDef(); $$->in << $3;}
            ;
port: slist direction type {$$ = make_ports($1, $2, $3)}
    | slist direction type '(' literal ')'{$$ = NULL;/*new Port($2, toSym(*$1), $3);neglects init*/}
    | slist error type {yyerror("bad direction\n");}
    ;
slist: SYMBOL {$$=new Vlist; $$->push_back($1)}
     | slist ',' SYMBOL {$$ = $1; $1->push_back($3);}
     ;
ports: port {$$=$1;}
     | ports port {append(($$=$1), $2);}
     ;
type: SYMBOL {$$=new ScalarType($1)}
    | SYMBOL '[' INT ']' {$$=new VectorType($1, new Range($3));}
    | SYMBOL '[' range ']' {$$=new VectorType($1, $3);}
    ;
range: INT ':' INT {$$ = new Range($1, $3);}
     ;
arch_desc: ARCH ':' statements END {$$=new Arch(); $$->add($3);}
         ;
statement: signal_block
         | sync_block      {$$=new Vlist(); $$->push_back($1);}
         | lookup_block    {$$=new Vlist(); $$->push_back($1);}
         | simple_relation {$$=new Vlist(); $$->push_back($1);}
         | ternary         {$$=new Vlist(); $$->push_back($1);}
         | type_declare    {$$=new Vlist(); $$->push_back($1);}
         | module_block    {$$=new Vlist(); $$->push_back($1);}
         | error {$$=NULL;errx(1,"bad statement on %d\n",yylineno);}
         ;
statements: {$$=new Vlist();}
          | statements statement {append($$=$1, $2);}
          ;

sync_statement: simple_relation
              | if_block {$$=static_cast<Value*>($1);}
              | case
              | async_block
              ;
sync_statements: sync_statement {($$=new Vlist())->push_back($1)}
               | sync_statements sync_statement {$$=$1; $1->push_back($2);}
               ;
if_block: IF '(' expr ')' sync_statement ELSE sync_statement
        {$$ = new Cond($3);
         $$->when << $5; $$->unless << $7;}
        | IF '(' expr ')' ':' sync_statements ELSE ':' sync_statements END
        {$$ = new Cond($3);
         $$->when << $6; $$->unless << $9;}
        | IF '(' expr ')' sync_statement
        {$$ = new Cond($3); $$->when << $5;}
        | IF '(' expr ')' ':' sync_statements END
        {$$ = new Cond($3); $$->when << $6;}
        ;
case: CASE '(' expr ')' ':' cases END {($$=new Case($3))->buildCases($6);}
          ;
cases: case_stmt {($$=new Vlist())->push_back($1);}
     | cases case_stmt {($$=$1)->push_back($2);}
     ;
case_stmt: SYMBOL  ':'    {$$=new CaseLabel($1);}
         | DEFAULT ':'    {$$=new CaseLabel($1);}
         | sync_statement
         ;

sync_srt: SYNC '(' SYMBOL ')' {$$ = new Sync($3);}
        | SYNC '(' SYMBOL ',' expr ')' {$$ = new Sync($3, $5);}
        ;
sync_block: sync_srt ':' sync_statements END {($$=$1)->cond->when << $3;}
          | sync_srt sync_statement          {($$=$1)->cond->when << $2;}
          ;
async_block: ASYNC ':' sync_statements END {$$ = make_async($3);}
           | ASYNC sync_statement {Vlist v; v << $2;$$ = make_async(&v);}
lookup_block: lvalue '=' LOOKUP '(' expr ')' ':' lookup_entries END
            {$$ = new Lookup($1, $5); $$->entries << $8;/*ignores entries*/;}
            ;
lookup_entries: lookup_entry {($$=new Vlist())->push_back($1)}
              | lookup_entries lookup_entry {($$=$1)->push_back($2);}
              ;
lookup_entry: literal ':' expr {$$ = new LookupPair($1,$3);}
            | DEFAULT ':' expr {$$ = new LookupPair($3);}
            ;
literal: INT {$$=new Int($1);}
       | STRING
       | HEX
       | CHAR
       | SYMBOL
       | SYMBOL '[' expr ']'  {$$ = new RangedLvalue($1, $3);}
       | SYMBOL '[' range ']' {$$ = new RangedLvalue($1, $3);}
       ;

signal_block: SIGNAL sdeclare {$$ = $2;}
            | SIGNAL ':' sdeclares END {$$ = $3;}
            ;

sdeclare: slist type {$$ = make_signals($1, $2);}
        | slist type '(' literal ')' {$$ = make_signals($1, $2)/*ignores init*/;}
        ;
sdeclares: sdeclare
         | sdeclares sdeclare
         ;

direction: IN       {$$ = &_IN;}
         | OUT      {$$ = &_OUT;}
         | INOUT    {$$ = &_INOUT;}
         | BUF      {$$ = &_BUF;}
         | GENERIC  {$$ = &_GENERIC;}
         ;
simple_relation: lvalue '=' expr  {$$ = new SimpleRelation($1, $3);}
               ;
ternary: lvalue '=' expr '?' expr ':' expr {$$ = new Ternary($1,$3,$5,$7);}
       ;
expr: literal
    | others_expr
    | SYMBOL '(' explist ')' {$$ = new Function($1, NULL)/*$1 + "(" + $3 + ")"*/;}
    | '(' expr ')' {$$ = NULL;/*"(" + $2 + ")"*/;}
    | expr AND expr  {$$ = new Op2("and", $1, $3);}
    | expr OR expr   {$$ = new Op2("or",  $1, $3);}
    | expr XOR expr  {$$ = new Op2("xor", $1, $3);}
    | expr EQEQ expr {$$ = new Op2("=" ,  $1, $3);}
    | expr NEQ expr  {$$ = new Op2("/=",  $1, $3);}
    | expr '&' expr  {$$ = new Op2("&" ,  $1, $3);}
    | expr '<' expr  {$$ = new Op2("<" ,  $1, $3);}
    | expr '>' expr  {$$ = new Op2(">" ,  $1, $3);}
    | expr '+' expr  {$$ = new Op2("+" ,  $1, $3);}
    | expr '-' expr  {$$ = new Op2("-" ,  $1, $3);}
    | NOT expr {$$ = new Op1("not", $2);}
    | error {yyerror("Unhandled expression");}
    ;

lvalue: SYMBOL '[' expr ']'  {$$ = new RangedLvalue($1, $3);}
      | SYMBOL '[' range ']' {$$ = new RangedLvalue($1, $3);}
      | SYMBOL {$$=new SimpleLvalue($1);}
      ;

others_expr: '*' CHAR {$$ = new Others(new Symbol($2));}
           ;

type_declare: enum_declare
            ;
enum_declare: ENUM SYMBOL '(' slist ')' {$$=NULL; "BAD_CODEGEN";//"^type " + $2 + " is (" + $4 + ");\n";
            }
            ;

module_block: mod_ref mod_conn {($1->connections = new Vlist())->push_back($2); $$=$1;}
            | mod_ref ':' mod_conns END {$1->connections = $3; $$=$1;}
            ;
mod_ref: MODULE '(' SYMBOL ')' {$$ = new SubModule($3);}
       ;
mod_conn: GENERIC '(' explist ')' {$$ = new GenericBinding($3);}
        | PORT    '(' explist ')' {$$ = new PortBinding($3);}
        ;
mod_conns: mod_conn {($$=new Vlist())->push_back($1);}
         | mod_conns mod_conn {($$=$1)->push_back($2);}
explist: expr {$$ = new Vlist(); $$->push_back($1);}
       | explist ',' expr {$$ = $1;$1->push_back($3);}
       ;

