%{
#define YYSTYPE string
#include <string>
using namespace std;
#include "modv.tab.h"

%}

%option noyywrap yylineno

%%
use return USE;
enum return ENUM;
module return MODULE;
arch return ARCH;
signal yylval=yytext;return SIGNAL;
head return HEAD;
end return END;
out yylval=yytext;return OUT;
in yylval=yytext;return IN;
inout yylval=yytext;return INOUT;
buf   yylval="buffer";return BUF;
sync return SYNC;
lookup return LOOKUP;
others return DEFAULT;
port   return PORT;
generic return GENERIC;
case return CASE;
if return IF;
else return ELSE;
and yylval=yytext;return AND;
or yylval=yytext;return OR;
not yylval=yytext;return NOT;    
xor yylval=yytext;return XOR;
== return EQEQ;
!= return NEQ;
\+ return '+';
\- return '-';
\< return '<';
\> return '>';
, return ',';
= return '=';
\* return '*';
: return ':';
\& yylval=yytext;return '&';
\[ return '[';
\] return ']';
\? return '?';
0x[[:xdigit:]]+ yylval=string()+"X\"" + (yytext+2)+"\""; return HEX;
[[:digit:]]+ yylval = yytext; return INT;
\(   return '(';
\)   return ')';
\"[[:alnum:]]*\" yylval=yytext; return STRING;
\'[[:alnum:]]\' yylval=yytext; return CHAR;
[[:alpha:]][[:alnum:]_\.]* yylval = yytext; return SYMBOL;
\. return '.';
--[^\n]* //ignored
[[:blank:]\n]+ //ignored 
. printf("ERROR: cannot parse '%c'", *yytext);
%%
