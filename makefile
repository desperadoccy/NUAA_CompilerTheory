bison -d test.y
lex test.l
gcc -g -o parser lex.yy.cc test.tab.c
