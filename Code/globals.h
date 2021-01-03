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
    TError,
	
	TProgram, 
    TVariableDeclaration,
    TArrayDeclaration,
    TFunctionDeclaration,

    TVariableParameter,
    TArrayParameter,

    TCompoundStatement,
    TExpressionStatement,
    TSelectionStatement,
    TIterationStatement,
    TReturnStatement,

    TAssignExpression,
    TComparisonExpression,
    TAdditiveExpression,
    TMultiplicativeExpression,

    TVariable,
    TArray,
    TCall,

    TConstant,
    TTokenType,
} NodeKind;

//llvm::Values * codegen(treeNode *node)
//{
//	switch(node->nodeKind)
//	{
//		case TVariableDeclaration : 
//		
//	}
//}

typedef struct treeNode {
  struct treeNode *sibling;
  int lineno;
  NodeKind nodeKind;

  union {
  	//TProgram
  	struct
  	{
  		struct treeNode *cur;	
	} Pro;
  	
  	
      // TVariableDeclaration
      struct {
          struct treeNode *type_spec;
          struct treeNode *_var;
      } varDecl;

      // TArrayDeclaration
      struct {
          struct treeNode *type_spec;
          struct treeNode *_var;
          struct treeNode *_num;
      } arrDecl;

      // TFunctionDeclaration
      struct {
          struct treeNode *type_spec;
          struct treeNode *_var;
          struct treeNode *params;
          struct treeNode *cmpd_stmt;
      } funcDecl;

      // TVariableParameter
      struct {
          struct treeNode *type_spec;
          struct treeNode *_var;
      } varParam;

      // TArrayParameter
      struct {
          struct treeNode *type_spec;
          struct treeNode *_var;
      } arrParam;
      
      // TCompoundStatement
      struct {
          struct treeNode *local_decl;
          struct treeNode *stmt_list;
          //ExpType retType;
      } cmpdStmt;

      // TExpressionStatement
      struct {
          struct treeNode *expr;
      } exprStmt;

      // TSelectionStatement
      struct {
          struct treeNode *expr;
          struct treeNode *if_stmt;
          struct treeNode *else_stmt;
      } selectStmt;

      // TIterationStatement
      struct {
          struct treeNode *expr;
          struct treeNode *loop_stmt;
      } iterStmt;

      // TReturnStatement
      struct {
          struct treeNode *expr;
          //ExpType retType;
      } retStmt;

      // TAssignExpression
      struct {
          struct treeNode *expr;
          struct treeNode *_var;
      } assignStmt;

      // TComparisonExpression
      struct {
          struct treeNode *lexpr;
          struct treeNode *op;
          struct treeNode *rexpr;
      } cmpExpr;

      // TAdditiveExpression
      struct {
          struct treeNode *lexpr;
          struct treeNode *op;
          struct treeNode *rexpr;
      } addExpr;

      // TMultiplicativeExpression
      struct {
          struct treeNode *lexpr;
          struct treeNode *op;
          struct treeNode *rexpr;
      } multExpr;

      // TArray
      struct {
          struct treeNode *_var;
          struct treeNode *arr_expr;
      } arr;

      // TCall
      struct {
          struct treeNode *_var;
          struct treeNode *expr_list;
      } call;

      // TVariable
      struct {
          char *ID;
      };

      // TConstant
      struct {
          int NUM;
      };

      // TTokenType
      struct {
          TokenType TOK;
      };
  } attr;
} TreeNode;

#endif
