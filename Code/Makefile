all: parser

OBJS = parser.o  \
	   main.o    \
       codegen.o \
       tokens.o  \
       corefn.o  \
	   util.o  \

LLVMCONFIG = llvm-config-6.0
CPPFLAGS = `$(LLVMCONFIG) --cppflags` -std=c++14
LDFLAGS = `$(LLVMCONFIG) --ldflags` -lpthread -ldl -lz -lncurses -rdynamic
LIBS = `$(LLVMCONFIG) --libs`

clean:
	$(RM) -rf parser.cpp parser.hpp parser tokens.cpp $(OBJS)

parser.cpp: cm.y
	bison -d -o $@ $^
	
parser.hpp: parser.cpp

tokens.cpp: cm.l parser.hpp
	flex -o $@ $^

%.o: %.cpp
	g++ -c $(CPPFLAGS) -o $@ $<


parser: $(OBJS)
	g++ -o $@ $(OBJS) $(LIBS) $(LDFLAGS)

test: ./parser
