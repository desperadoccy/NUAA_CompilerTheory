#include <iostream>
#include "codegen.h"
#include "util.h"
#include "scan.h"
#include "parse.h"

int lineno = 0;
FILE * source;
FILE * listing;

using namespace std;

//extern int yyparse();
TreeNode* syntaxTree;

void createCoreFunctions(CodeGenContext& context);

int main(int argc, char **argv)
{
	listing = stdout; 	  //输出到屏幕，可改成文件 
	char pgm[120];
	string testNum;
	string filename = "../Test/test";
	cout << "Please input file name\n";
//	cin >> pgm;
	cin >> testNum;
	filename = filename.append(testNum).append("/test").append(testNum).append(".txt");
	source = fopen(filename.c_str(), "r");
	if (source == NULL)
	{
		fprintf(listing, "File %s not found\n", pgm);
		exit(0);
	}

	syntaxTree = parse();   //调用写在cm.y里的parse()函数进行构造语法分析树 
	fprintf(listing, "**********\nSyntax tree:\n**********\n");
	printTree(syntaxTree, 0, 1);
	fclose(source);

	if (!syntaxTree)
	{
		fprintf(listing, "Syntax error!\n");
		exit(0);
	}
	InitializeNativeTarget();
	InitializeNativeTargetAsmPrinter();
	InitializeNativeTargetAsmParser();
	CodeGenContext context;
	createCoreFunctions(context);
	context.generateCode(syntaxTree);
	context.runCode();

	return 0;
}

