#ifndef GLOBALS_H
#define GLOBALS_H
#include <iostream>
using namespace std;

#ifndef YYPARSER
#include "parser.hpp"
#endif

#ifndef FALSE
#define FALSE 0
#endif

#ifndef TRUE
#define TRUE 1
#endif

#define MAXRESERVED 8

typedef int TokenType;


extern FILE* source; 
extern FILE* listing; 


extern int lineno; 

/**************************************************/
/***********   Syntax tree for parsing ************/
/**************************************************/

typedef enum {
    ErrorK,
	
	ProgramK, 
    VariableDeclarationK,
    ArrayDeclarationK,
    FunctionDeclarationK,

    VariableParameterK,
    ArrayParameterK,

    CompoundStatementK,
    ExpressionStatementK,
    SelectionStatementK,
    IterationStatementK,
    ReturnStatementK,

    AssignExpressionK,
    ComparisonExpressionK,
    AdditiveExpressionK,
    MultiplicativeExpressionK,

    VariableK,
    ArrayK,
    CallK,

    ConstantK,
    TokenTypeK,
} NodeKind;

//llvm::Values * codegen(treeNode *node)
//{
//	switch(node->nodeKind)
//	{
//		case VariableDeclarationK : 
//		
//	}
//}

typedef struct treeNode {
  struct treeNode *sibling;
  int lineno;
  NodeKind nodeKind;

  union {
  	//ProgramK
  	struct
  	{
  		struct treeNode *cur;	
	} Pro;
  	
  	
      // VariableDeclarationK
      struct {
          struct treeNode *type_spec;
          struct treeNode *_var;
      } varDecl;

      // ArrayDeclarationK
      struct {
          struct treeNode *type_spec;
          struct treeNode *_var;
          struct treeNode *_num;
      } arrDecl;

      // FunctionDeclarationK
      struct {
          struct treeNode *type_spec;
          struct treeNode *_var;
          struct treeNode *params;
          struct treeNode *cmpd_stmt;
      } funcDecl;

      // VariableParameterK
      struct {
          struct treeNode *type_spec;
          struct treeNode *_var;
      } varParam;

      // ArrayParameterK
      struct {
          struct treeNode *type_spec;
          struct treeNode *_var;
      } arrParam;
      
      // CompoundStatementK
      struct {
          struct treeNode *local_decl;
          struct treeNode *stmt_list;
          //ExpType retType;
      } cmpdStmt;

      // ExpressionStatementK
      struct {
          struct treeNode *expr;
      } exprStmt;

      // SelectionStatementK
      struct {
          struct treeNode *expr;
          struct treeNode *if_stmt;
          struct treeNode *else_stmt;
      } selectStmt;

      // IterationStatementK
      struct {
          struct treeNode *expr;
          struct treeNode *loop_stmt;
      } iterStmt;

      // ReturnStatementK
      struct {
          struct treeNode *expr;
          //ExpType retType;
      } retStmt;

      // AssignExpressionK
      struct {
          struct treeNode *expr;
          struct treeNode *_var;
      } assignStmt;

      // ComparisonExpressionK
      struct {
          struct treeNode *lexpr;
          struct treeNode *op;
          struct treeNode *rexpr;
      } cmpExpr;

      // AdditiveExpressionK
      struct {
          struct treeNode *lexpr;
          struct treeNode *op;
          struct treeNode *rexpr;
      } addExpr;

      // MultiplicativeExpressionK
      struct {
          struct treeNode *lexpr;
          struct treeNode *op;
          struct treeNode *rexpr;
      } multExpr;

      // ArrayK
      struct {
          struct treeNode *_var;
          struct treeNode *arr_expr;
      } arr;

      // CallK
      struct {
          struct treeNode *_var;
          struct treeNode *expr_list;
      } call;

      // VariableK
      struct {
          char *ID;
      };

      // ConstantK
      struct {
          int NUM;
      };

      // TokenTypeK
      struct {
          TokenType TOK;
      };
  } attr;
} TreeNode;

#endif
