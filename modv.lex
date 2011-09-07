%{
#include <string>
#include <err.h>
#include <codegen.h>
using CodeGen::Vlist;
using CodeGen::Symbol;
#include "parse.hpp"

using namespace std;

%}

%option noyywrap yylineno

%%
use         return USE;      //Keywords
enum        return ENUM;
async       return ASYNC;
module      return MODULE;
arch        return ARCH;
signal      return SIGNAL;
head        return HEAD;
end         return END;
sync        return SYNC;
lookup      return LOOKUP;
others      return DEFAULT;
port        return PORT;
generic     return GENERIC;
case        return CASE;
if          return IF;
else        return ELSE;

out         return OUT;     //Directions
in          return IN;
inout       return INOUT;
buf         return BUF;


and         return AND;     //Operators
or          return OR;
not         return NOT;
xor         return XOR;
==          return EQEQ;
!=          return NEQ;
\+          return '+';
\-          return '-';
\<          return '<';
\>          return '>';
,           return ',';
=           return '=';
\*          return '*';

:           return ':';
\&          return '&';
\[          return '[';
\]          return ']';
\?          return '?';
\(          return '(';
\)          return ')';

0x[[:xdigit:]]+  {yylval.sym = new Symbol(std::string("X\"") + (yytext + 2) + "\"");
                  return HEX;} //Values
[[:digit:]]+      yylval.num = atoi(yytext); return INT;
\"[[:alnum:]]*\"  yylval.sym = new Symbol(yytext); return STRING;
\'[[:alnum:]]\'   yylval.sym = new Symbol(yytext); return CHAR;
[[:alpha:]][[:alnum:]_\.]* yylval.sym = new Symbol(yytext); return SYMBOL;


--[^\n]*       //ignored
[[:blank:]\n]+ //ignored

. errx(1, "Unparsable character encountered - '%c'", *yytext);
%%
