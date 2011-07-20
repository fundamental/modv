%{

#define YYSTYPE string
#include <string>
#include <sstream>
#include <cstdio>
#include <cstdlib>
#include <error.h>
#include <errno.h>
using namespace std;

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


FILE *ofile = NULL;

int main(int argc, char **argv)
{
    if(argc!=3)
        error(1,0,"usage:: %s in-file out-file", *argv);

    extern FILE *yyin;
    if(!(yyin=fopen(argv[1], "r")))
        error(1, errno, "Failed to open input");

    if(!(ofile=fopen(argv[2], "w+")))
        error(1, errno, "Failed to open output");

    //extern int yydebug;
    //yydebug=1;
    yyparse();

    fclose(yyin);
    fclose(ofile);
    return 0;
}

string mod_name;
const char *fmt = 
"library ieee;\n"\
"use ieee.std_logic_1164.all;\n"\
"use work.all;\n"\
"use modv.all;\n"\
"%s"\
"\n"\
"entity %s is\n"\
"%s\n"\
"end %s;\n"\
"\n"\
"architecture default of %s is\n"\
"%s\n"\
"end architecture;\n";

void print_module(const char *use, const char *head,const char *arch)
{
    const char *n = mod_name.c_str();
    fprintf(ofile, fmt, use, n, head, n, n, arch);
}

string compound(string cur, string next)
{
    if(next[0]=='^')
        return cur.insert(cur.find("begin"),next.substr(1));
    return cur + next;
}

string make_use(string use_name)
{
    if(use_name == "unsigned")
        use_name = "ieee.std_logic_unsigned";
    return "use " + use_name + ".all;\n";
}

string make_port(string slist, string dir, string type)
{
    string direction;
    if(dir=="i")
        direction = " in ";
    else if(dir=="o")
        direction = " out ";
    else if(dir=="x")
        direction = " inout ";
    else
        direction = " ";
    return dir + slist + ':' + direction + type + "@";
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

string make_async(string stmts)
{
    return "@" + stmts + "@";
}

%}

%token MODULE ARCH SIGNAL HEAD END OUT IN SYNC LOOKUP DEFAULT SYMBOL STRING INT
%token INOUT HEX BUF ENUM PORT GENERIC CHAR CASE IF ELSE USE ASYNC

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
      {print_module($2.c_str(),$3.c_str(),$4.c_str());}
      ;
uses: {$$=""}
    | uses use {$$=$1+$2;}
    ;
use: USE SYMBOL {$$=make_use($2);}
   ;

mod_declare: MODULE SYMBOL {mod_name=$2;}
           ;
head_declare: HEAD ':' ports END {$$ = format_ports($3);}
            ;
port: slist direction type {$$ = make_port($1,$2,$3);}
    | slist direction type init {$$ = make_port($1,$2,$3+$4);}
    | slist error type {$$="";yyerror("bad direction\n");}
    ;
init: '(' literal ')' {$$ = " := " + $2;}
    ;
slist: SYMBOL
     | slist ',' SYMBOL {$$ = $1 + ", " + $3;}
     ;
ports: port
     | ports port {$$=$1+$2;}
     ;
type: SYMBOL
    | SYMBOL '[' literal ']' {$$=$1 + "(" + dec($3) + " downto 0)"}
    | SYMBOL '[' range ']' {$$=$1 + "(" + $3 + ")"}
    ;
range: INT '.' '.' INT {$$ = dec($1) + " downto " + $4;}
     ;
arch_desc: ARCH ':' statements END {$$=$3;}
         ;
statement: signal_block
         | sync_block
         | lookup_block
         | relation
         | type_declare
         | module_block
         | error {printf("bad statement[%s](%d)\n",$1.c_str(),yylineno);}
         ;
statements: {$$ = "begin\n";}
          | statements statement {$$=compound($1,$2);}
          ;

sync_statement: relation
              | if_block
              | case_block
              | async_block
              ;
sync_statements: sync_statement
               | sync_statements sync_statement {$$ = $1 + '\n' + $2;}
               ;
if_block: IF '(' expr ')' sync_statement opt_else{$$ = "if (" + $3 + ") then\n"
        + $5 + $6 + "end if;\n";}
        | IF '(' expr ')' ':' sync_statements opt_else END{$$ = "if (" + $3 + ") then\n"
        + $6 + $7 + "end if;\n";}
        ;
opt_else: {$$ = "";}
        | ELSE sync_statement {$$ = "else\n" + $2 + "\n";}
        | ELSE ':' sync_statements {$$ = "else\n" + $3 + "\n";}
        ;
case_srt: CASE '(' expr ')' {$$ = "case (" + $3 + ") is \n";}
        ;
case_block: case_srt ':' cases END {$$ = $1 + $3 + "end case;\n";}
          ;
case: SYMBOL ':' {$$ = "when " + $1 + " =>";}
    | DEFAULT ':' {$$ = "when others =>";}
    | sync_statement
    ;
cases: case
     | cases case {$$ = $1 + '\n' + $2;}
     ;
sync_srt: SYNC '(' SYMBOL ')' {insync = true; $$ = make_sync($3);}
        | SYNC '(' SYMBOL ',' expr ')' {insync=true;$$ = make_sync($3, $5);}
        ;
sync_block: sync_srt ':' sync_statements END {insync=0;$$ = make_syncfull($1, $3);}
          | sync_srt sync_statement {insync=0;$$ = make_syncfull($1,$2);}
          ;
async_block: ASYNC ':' sync_statements END {$$ = make_async($3);}
           | ASYNC sync_statement {$$ = make_async($2);}
lookup_block: lvalue '=' LOOKUP '(' expr ')' ':' lookup_entries END
            {$$ = "with " + $5 + " select " + $1 + " <=\n" + $8 + ";\n";}
            ;
lookup_entries: lookup_entry
              | lookup_entries lookup_entry {$$ = $1+",\n"+$2;}
              ;
lookup_entry: literal ':' expr {$$ = $3 + " when " + $1;}
            | DEFAULT ':' expr {$$ = $3 + " when others";}
            ;
literal: INT 
       | STRING
       | HEX
       | CHAR
       | SYMBOL
       | SYMBOL '[' expr ']' {$$ = $1 + "(" + $3 + ")";}
       | SYMBOL '[' range ']' {$$ = $1 + "(" + $3 + ")";} 
       ;

signal_block: SIGNAL sdeclare {$$ = "^" + $2;}
            | SIGNAL ':' sdeclares END {$$ = "^" + $3;}
            ;

sdeclare: slist type {$$ = "signal " + $1 + ":" + $2 + ";\n";}
        | slist type init {$$ = "signal " + $1 + ":" + $2 + $3 + ";\n";}
        ;
sdeclares: sdeclare
         | sdeclares sdeclare
         ;

direction: IN       {$$ = "i";}
         | OUT      {$$ = "o";}
         | INOUT    {$$ = "x";}
         | BUF      {$$ = "b";}
         | GENERIC  {$$ = "g";}
         ;
relation: lvalue '=' expr  {$$ = $1 + "<=" + $3 + ";\n";}
        | lvalue '=' expr '?' expr ':' expr {$$ = make_ternary($1,$3,$5,$7);}
        ;
expr: literal
    | others_exp
    | SYMBOL '(' explist ')' {$$ = $1 + "(" + $3 + ")";}
    | '(' expr ')' {$$ = "(" + $2 + ")";}
    | expr AND expr  {$$ = $1 + " and " + $3;}
    | expr OR expr   {$$ = $1 + " or "  + $3;}
    | expr XOR expr  {$$ = $1 + " xor " + $3;}
    | expr EQEQ expr {$$ = $1 + " = "   + $3;}
    | expr NEQ expr  {$$ = $1 + " /= "  + $3;}
    | expr '&' expr  {$$ = $1 + " & "   + $3;}
    | expr '<' expr  {$$ = $1 + " < "   + $3;}
    | expr '>' expr  {$$ = $1 + " > "   + $3;}
    | expr '+' expr  {$$ = $1 + " + "   + $3;}
    | expr '-' expr  {$$ = $1 + " - "   + $3;}
    | NOT expr {$$ = $1 + " " + $2;}
    | error {yyerror("Unhandled expression");}
    ;

lvalue: SYMBOL '[' expr ']' {$$ = $1 + "(" + $3 + ")";}
      | SYMBOL '[' range ']' {$$ = $1 + "(" + $3 + ")";} 
      | SYMBOL
      ;

others_exp: '*' CHAR {$$ = "(others => " + $2 + ")"}
          ;

type_declare: enum_declare
            ;
enum_declare: ENUM SYMBOL '(' slist ')' {$$= "^type " + $2 + " is (" + $4 + ");\n";}
            ;

module_block: mod_ref mod_conn {$$ = $1 + " " + $2 + ";\n"}
            | mod_ref ':' mod_conns END {$$ = $1 + " " + $3 + ";\n"}
            ;
mod_ref: MODULE '(' SYMBOL ')' {$$ = gen_sym() + $3;}
       ;
mod_conn: GENERIC '(' explist ')' {$$ = "generic map(" + $3 + ")";}
        | PORT    '(' explist ')' {$$ = "port map(" + $3 + ")";}
        ;
mod_conns: mod_conn
         | mod_conns mod_conn {$$ = $1 + " " + $2;}
explist: expr
       | explist ',' expr {$$ = $1 + ',' + $3;}
       ;

