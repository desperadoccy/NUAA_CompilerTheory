all: parser

test.tab.c test.tab.h: test.y
		bison -t -v -d test.y

lex.yy.c: test.l test.tab.h
		flex test.l

parser: lex.yy.c test.tab.c test.tab.h
		gcc -o parser test.tab.c lex.yy.c

clean:
		rm parser test.tab.c lex.yy.c test.tab.h
