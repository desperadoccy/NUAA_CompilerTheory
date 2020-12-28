%{
  #include <stdlib.h>
  #include <stdio.h>
  #include <string.h>
  #include "node.h"
  #include "test.tab.h"

  void yyerror(char *);

%}

%%

/* 关键字 */
auto                        { return AUTO; }
short                       { return SHORT; }
int                         { return INT; }
long                        { return LONG; }
float                       { return FLOAT; }
double                      { return DOUBLE; }
char                        { return CHAR; }
struct                      { return STRUCT; }
union                       { return UNION; }
enum                        { return ENUM; }
typedef                     { return TYPEDEF; }
const                       { return CONST; }
unsigned                    { return UNSIGNED; }
extern                      { return EXTERN; }
register                    { return REGISTER; }
static                      { return STATIC; }
volatile                    { return VOLATILE; }
void                        { return VOID; }
if                          { return IF; }
else                        { return ELSE; }
switch                      { return SWITCH; }
case                        { return CASE; }
for                         { return FOR; }
do                          { return DO; }
while                       { return WHILE; }
goto                        { return GOTO; }
continue                    { return CONTINUE; }
break                       { return BREAK; }
default                     { return DEFAULT; }
sizeof                      { return SIZEOF; }
return                      { return RETURN; }

/* 变量和数据 */
[a-zA-Z][a-zA-Z0-9]*        { return IDENTITY; }
[0-9]+                      { return INT_NUMBER; }
[0-9]+\.[0-9]+              { return FLOAT_NUMBER; }
\"(\\.|[^"])*\"             { return STRING; }

/* 操作符 */
[-+&~|^/%*(),.;!]           { return yytext[0]; }
"&&"                        { return AND; }
"||"                        { return OR; }
"="                         { return ASSIGN; }
"=="					    { return CEQ; }
"!="                    puts("TCNE"); return TOKEN(TCNE);
"<"                     puts("TCLT"); return TOKEN(TCLT);
"<="                    puts("TCLE"); return TOKEN(TCLE);
">"                     puts("TCGT"); return TOKEN(TCGT);
">="                    puts("TCGE"); return TOKEN(TCGE);
"("                     puts("TLPAREN"); return TOKEN(TLPAREN);
")"                     puts("TRPAREN"); return TOKEN(TRPAREN);
"{"                     puts("TLBRACE"); return TOKEN(TLBRACE);
"}"                     puts("TRBRACE"); return TOKEN(TRBRACE);
"["                     puts("TLBRACKET"); return TOKEN(TLBRACKET);
"]"                     puts("TRBRACKET"); return TOKEN(TRBRACKET);
"."                     puts("TDOT"); return TOKEN(TDOT);
","                     puts("TCOMMA"); return TOKEN(TCOMMA);
"+"                     puts("TPLUS"); return TOKEN(TPLUS);
"-"                     puts("TMINUS"); return TOKEN(TMINUS);
"*"                     puts("TMUL"); return TOKEN(TMUL);
"/"                     puts("TDIV"); return TOKEN(TDIV);
"&"                     puts("TAND"); return TOKEN(TAND);
"|"                     puts("TOR"); return TOKEN(TOR);
"^"                     puts("TXOR"); return TOKEN(TXOR);
"%"                     puts("TMOD"); return TOKEN(TMOD);
">>"                    puts("TSHIFTR"); return TOKEN(TSHIFTR);
"<<"                    puts("TSHIFTL"); return TOKEN(TSHIFTL);
";"                     puts("TSEMICOLON"); return TOKEN(TSEMICOLON);

/* 注释 */
/\*(.|[\r\n])*?\*/          { return COMMENT; }
//[^\n]*                    { return COMMENT; }

/* 空格和换行*/
[ \t\n]

/* 其他 */
.                           { yyerror("odd characters"); }
%%

int yywrap(void) {
    return 1;
}

