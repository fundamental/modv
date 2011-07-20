%{
#define YYSTYPE std::string
#include <string>
#include <err.h>
#include "modv.tab.h"

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

0x[[:xdigit:]]+  {yylval = std::string("X\"") + (yytext + 2) + "\"";
                  return HEX;} //Values
[[:digit:]]+      yylval = yytext; return INT;
\"[[:alnum:]]*\"  yylval = yytext; return STRING;
\'[[:alnum:]]\'   yylval = yytext; return CHAR;
[[:alpha:]][[:alnum:]_\.]* yylval = yytext; return SYMBOL;


--[^\n]*       //ignored
[[:blank:]\n]+ //ignored

. errx(1, "Unparsable character encountered - '%c'", *yytext);
%%
