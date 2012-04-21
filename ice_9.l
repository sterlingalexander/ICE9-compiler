/* 
    ICE-9 scanner 
*/

%{
//    #include <stdio.h>
//    #include <unistd.h>
//    #include "y.tab.h"
    #include <iostream>
    using namespace std;
    #define YY_DECL extern "C" int yylex()
    #include "ice_9.tab.h"
    int line_num = 1;
%}

%%
[ \t\r]*                ;
#[^\n]*\n               { ++line_num; }
true                    { yylval.sval = strdup(yytext);  return TOK_TRUE; }
false                   { yylval.sval = strdup(yytext);  return TOK_FALSE; }
read                    { yylval.sval = strdup(yytext);  return TOK_READ; }
break                   { yylval.sval = strdup(yytext);  return TOK_BREAK; }
exit                    { yylval.sval = strdup(yytext);  return TOK_EXIT; }
writes                  { yylval.sval = strdup(yytext);  return TOK_WRITES; }
write                   { yylval.sval = strdup(yytext);  return TOK_WRITE; }
type                    { yylval.sval = strdup(yytext);  return TOK_TYPE; }
var                     { yylval.sval = strdup(yytext);  return TOK_VAR; }
return                  { yylval.sval = strdup(yytext);  return TOK_RETURN; }
if                      { yylval.sval = strdup(yytext);  return TOK_IF; }
else                    { yylval.sval = strdup(yytext);  return TOK_ELSE; }
fi                      { yylval.sval = strdup(yytext);  return TOK_FI; }
do                      { yylval.sval = strdup(yytext);  return TOK_DO; }
od                      { yylval.sval = strdup(yytext);  return TOK_OD; }
fa                      { yylval.sval = strdup(yytext);  return TOK_FA; }
to                      { yylval.sval = strdup(yytext);  return TOK_TO; }
af                      { yylval.sval = strdup(yytext);  return TOK_AF; }
proc                    { yylval.sval = strdup(yytext);  return TOK_PROC; }
end                     { yylval.sval = strdup(yytext);  return TOK_END; }
forward                 { yylval.sval = strdup(yytext);  return TOK_FORWARD; }
:=                      { yylval.sval = strdup(yytext);  return TOK_ASSIGN; }
->                      { yylval.sval = strdup(yytext);  return TOK_ARROW; }
:                       { yylval.sval = strdup(yytext);  return TOK_COLON; }
"[]"                    { yylval.sval = strdup(yytext);  return TOK_SQUARE; }
=                       { yylval.sval = strdup(yytext);  return TOK_EQUALS; }
"?"                     { return yytext[0]; }
";"                     { return yytext[0]; }
","                     { return yytext[0]; }
"["                     { return yytext[0]; }
"]"                     { return yytext[0]; }
"("                     { return yytext[0]; }
")"                     { return yytext[0]; }
"<="                    { yylval.sval = strdup(yytext);  return TOK_LESS_EQUAL; }
">="                    { yylval.sval = strdup(yytext);  return TOK_GREATER_EQUAL; }
"!="                    { yylval.sval = strdup(yytext);  return TOK_NOT_EQUAL; }
"<"                     { return yytext[0]; }
">"                     { return yytext[0]; }
"+"                     { return yytext[0]; }
"-"                     { return yytext[0]; }
"/"                     { return yytext[0]; }
"*"                     { return yytext[0]; }
"%"                     { return yytext[0]; }
[a-zA-Z][a-zA-Z0-9_]*   { yylval.idval = strdup(yytext);  return ID;}  
[0-9]+                  { yylval.intval = atoi(yytext);  return INT; }
\"(\\.|[^\\"])*\"       { yylval.sval = strdup(yytext);  return STRING; }
\'(\\.|[^\\'])*\'       { yylval.sval = strdup(yytext);  return STRING; }
\n                      { ++line_num; }
.                       { printf( "Line %d: Lexical error near: %s\n", line_num, yytext ); exit(1); }
%%

void yyerror(const char *s) {
	printf("Line %d: %s near %s\n", line_num, s, yytext);
}

/*

.                       { printf( "Unrecognized character: %s\n", yytext ); }

.	                    { yyerror("Illegal character"; }

\n                      { ++line_num; }  //return ENDL;
*/