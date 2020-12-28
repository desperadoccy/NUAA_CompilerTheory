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
auto        { return AUTO; }
short       { return SHORT; }
int         { return INT; }
long        { return LONG; }
float       { return FLOAT; }
double      { return DOUBLE; }
char        { return CHAR; }
struct      { return STRUCT; }
union       { return UNION; }
enum        { return ENUM; }
typedef     { return TYPEDEF; }
const       { return CONST; }
unsigned    { return UNSIGNED; }
extern      { return EXTERN; }
register    { return REGISTER; }
static      { return STATIC; }
volatile    { return VOLATILE; }
void        { return VOID; }
if          { return IF; }
else        { return ELSE; }
switch      { return SWITCH; }
case        { return CASE; }
for         { return FOR; }
do          { return DO; }
while       { return WHILE; }
goto        { return GOTO; }
continue    { return CONTINUE; }
break       { return BREAK; }
default     { return DEFAULT; }
sizeof      { return SIZEOF; }
return      { return RETURN; }

/* 变量和数据 */
[a-zA-Z][a-zA-Z0-9]*        { return IDENTITY; }
[0-9]+                      { return INT_NUMBER; }
[0-9]+\.[0-9]+              { return FLOAT_NUMBER; }
\"(\\.|\"\"|[^"\n"])*\"     { return STRING; }

/* 操作符 */
/* 注释 */

/* 空格和换行*/
"//".*                      { return COMMENT; }
"/*"                        { return COMMENT; }

/* 其他 */
.           { yyerror("odd characters"); }
%%

void add_buff(char *buff) {
    strcat(sBuff[iBuffX],buff);
    iBuffY+=strlen(buff);
}

void add_var(char *mark) {
    if(iMaxIndex==0){
        strcpy(strMem[0].sMark,mark);
        iMaxIndex++;
        iCurIndex=0;
        return;
    }

    int i;
    for(i=0;i<=iMaxIndex-1;i++) {
        if(strcmp(strMem[i].sMark,mark)==0) {
            iCurIndex=i;
            return;
        }
    }

    strcpy(strMem[iMaxIndex].sMark,mark);
    iCurIndex=iMaxIndex;
    iMaxIndex++;
}

int yywrap(void) {
    return 1;
}

