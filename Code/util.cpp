/****************************************************/
/* File: util.c                                     */
/* Utility function implementation                  */
/* for the C- compiler                              */
/* Compiler Construction: Principles and Practice   */
/* Kenneth C. Louden                                */
/* Modified by Kwanghee Choi                        */
/****************************************************/

#include "globals.h"
#include "util.h"

static const char * const nodeName[] = {
    "ErrorK",
    "VariableDeclarationK",
    "ArrayDeclarationK",
    "FunctionDeclarationK",
    "VariableParameterK",
    "ArrayParameterK",

    "CompoundStatementK",

    "ExpressionStatementK",
    "SelectionStatementK",
    "IterationStatementK",
    "ReturnStatementK",
    "AssignExpressionK",

    "ComparisonExpressionK",
    "AdditiveExpressionK",
    "MultiplicativeExpressionK",

    "VariableK",
    "ArrayK",
    "CallK",

    "ConstantK",
    "TokenTypeK"
};

/* Procedure printToken prints a token 
 * and its lexeme to the listing file
 */
void printToken(TokenType token, const char* tokenString)
{
  switch (token)
    {
    case ENDFILE: fprintf(listing,"EOF\n"); break;
    case ERROR:   fprintf(listing,
                          "ERROR\t\t\t%s\n",
                          tokenString); break;

    case ID:      fprintf(listing,
                          "ID\t\t\t%s\n",
                          tokenString); break;
    case NUM:     fprintf(listing,
                          "NUM\t\t\t%s\n",
                          tokenString); break;

    case ELSE:    fprintf(listing,
                          "ELSE\t\t\t%s\n",
                          tokenString); break;
    case IF:      fprintf(listing,
                          "IF\t\t\t%s\n",
                          tokenString); break;
    case INT:     fprintf(listing,
                          "INT\t\t\t%s\n",
                          tokenString); break;
    case RETURN:  fprintf(listing,
                          "RETURN\t\t\t%s\n",
                          tokenString); break;
    case VOID:    fprintf(listing,
                          "VOID\t\t\t%s\n",
                          tokenString); break;
    case WHILE:   fprintf(listing,
                          "WHILE\t\t\t%s\n",
                          tokenString); break;

    case PLUS:    fprintf(listing,
                          "PLUS\t\t\t%s\n",
                          tokenString); break;
    case MINUS:   fprintf(listing,
                          "MINUS\t\t\t%s\n",
                          tokenString); break;
    case TIMES:   fprintf(listing,
                          "TIMES\t\t\t%s\n",
                          tokenString); break;
    case OVER:    fprintf(listing,
                          "OVER\t\t\t%s\n",
                          tokenString); break;

    case LT:      fprintf(listing,
                          "<\t\t\t%s\n",
                          tokenString); break;
    case LE:      fprintf(listing,
                          "<=\t\t\t%s\n",
                          tokenString); break;
    case GT:      fprintf(listing,
                          ">\t\t\t%s\n",
                          tokenString); break;
    case GE:      fprintf(listing,
                          ">=\t\t\t%s\n",
                          tokenString); break;
    case EQ:      fprintf(listing,
                          "==\t\t\t%s\n",
                          tokenString); break;
    case NE:      fprintf(listing,
                          "!=\t\t\t%s\n",
                          tokenString); break;

    case ASSIGN:  fprintf(listing,
                          "=\t\t\t%s\n",
                          tokenString); break;
    case SEMI:    fprintf(listing,
                          ";\t\t\t%s\n",
                          tokenString); break;
    case COMMA:   fprintf(listing,
                          ",\t\t\t%s\n",
                          tokenString); break;

    case LPAREN:   fprintf(listing,
                          "(\t\t\t%s\n",
                          tokenString); break;
    case RPAREN:   fprintf(listing,
                          ")\t\t\t%s\n",
                          tokenString); break;
    case LBRACK:   fprintf(listing,
                          "[\t\t\t%s\n",
                          tokenString); break;
    case RBRACK:   fprintf(listing,
                          "]\t\t\t%s\n",
                          tokenString); break;
    case LBRACE:   fprintf(listing,
                          "{\t\t\t%s\n",
                          tokenString); break;
    case RBRACE:   fprintf(listing,
                          "}\t\t\t%s\n",
                          tokenString); break;

    default:       /* should never happen */
           fprintf(listing,"error!!!!"); 
  }
}

TreeNode* makehead(TreeNode *cur)
{
	TreeNode *t = new TreeNode;
	if (t == NULL)
    {
      fprintf(listing,
              "Out of memory error at line %d\n",
              lineno);
    }
  	else
    {
      t->nodeKind=TProgram;
      t->attr.Pro.cur=cur;
    }
  return t;
}

TreeNode* addSibling(TreeNode *origin, TreeNode *follow)
{
  if (origin != NULL) {
    TreeNode *t = origin;
    while (t->sibling != NULL) t = t->sibling;
    t->sibling = follow;
  }
  else {
    origin = follow;
  }
  return origin;
}

TreeNode* allocateTreeNode(void)
{
  TreeNode *t = new TreeNode;
  if (t == NULL)
    {
      fprintf(listing,
              "Out of memory error at line %d\n",
              lineno);
    }
  else
    {
      t->sibling = NULL;
      t->lineno = lineno;
    }

  return t;
}

TreeNode* newVariableDeclarationNode(TreeNode *type_specifier,TreeNode *_var)
{
  TreeNode * t = allocateTreeNode();
  if (t != NULL)
    {
      t->nodeKind = TVariableDeclaration;
      t->attr.varDecl.type_spec = type_specifier;
      t->attr.varDecl._var = _var;
    }

  return t;
}

TreeNode*
newArrayDeclarationNode(TreeNode *type_specifier,
                        TreeNode *_var,
                        TreeNode *_num)
{
  TreeNode * t = allocateTreeNode();
  if (t != NULL)
    {
      t->nodeKind = TArrayDeclaration;
      t->attr.arrDecl.type_spec = type_specifier;
      t->attr.arrDecl._var = _var;
      t->attr.arrDecl._num = _num;

    }

  return t;
}

TreeNode*
newFunctionDeclarationNode(TreeNode *type_specifier,
                           TreeNode *_var,
                           TreeNode *params,
                           TreeNode *compound_stmt)
{
  TreeNode * t = allocateTreeNode();
  if (t != NULL)
    {
      t->nodeKind = TFunctionDeclaration;
      t->attr.funcDecl.type_spec = type_specifier;
      t->attr.funcDecl._var = _var;
      t->attr.funcDecl.params = params;
      t->attr.funcDecl.cmpd_stmt = compound_stmt;
    }

  return t;
}

TreeNode*
newVariableParameterNode(TreeNode *type_specifier,
                         TreeNode *_var)
{
  TreeNode * t = allocateTreeNode();
  if (t != NULL)
    {
      t->nodeKind = TVariableParameter;
      t->attr.varParam.type_spec = type_specifier;
      t->attr.varParam._var = _var;
    }

  return t;
}

TreeNode*
newArrayParameterNode(TreeNode *type_specifier,
                      TreeNode *_var)
{
  TreeNode * t = allocateTreeNode();
  if (t != NULL)
    {
      t->nodeKind = TArrayParameter;
      t->attr.arrParam.type_spec = type_specifier;
      t->attr.arrParam._var = _var;
    }

  return t;
}

TreeNode*
newCompoundStatementNode(TreeNode *local_declarations, // nullable
                         TreeNode *statement_list) // nullable
{
  TreeNode * t = allocateTreeNode();
  if (t != NULL)
    {
      t->nodeKind = TCompoundStatement;
      t->attr.cmpdStmt.local_decl = local_declarations;
      t->attr.cmpdStmt.stmt_list = statement_list;
    }

  return t;
}

TreeNode*
newExpressionStatementNode(TreeNode *expression) // nullable
{
  TreeNode * t = allocateTreeNode();
  if (t != NULL)
    {
      t->nodeKind = TExpressionStatement;
      t->attr.exprStmt.expr = expression;
    }

  return t;
}

TreeNode*
newSelectionStatementNode(TreeNode *expression, // nullable
                          TreeNode *if_statement,
                          TreeNode *else_statement) // nullable
{
  TreeNode * t = allocateTreeNode();
  if (t != NULL)
    {
      t->nodeKind = TSelectionStatement;
      t->attr.selectStmt.expr = expression;
      t->attr.selectStmt.if_stmt = if_statement;
      t->attr.selectStmt.else_stmt = else_statement;
    }

  return t;
}

TreeNode*
newIterationStatementNode(TreeNode *expression,
                          TreeNode *statement) // nullable
{
  TreeNode * t = allocateTreeNode();
  if (t != NULL)
    {
      t->nodeKind = TIterationStatement;
      t->attr.iterStmt.expr = expression;
      t->attr.iterStmt.loop_stmt = statement;
    }

  return t;
}

TreeNode*
newReturnStatementNode(TreeNode *expression) // nullable
{
  TreeNode * t = allocateTreeNode();
  if (t != NULL)
    {
      t->nodeKind = TReturnStatement;
      t->attr.retStmt.expr = expression;
    }

  return t;
}

TreeNode*
newAssignExpressionNode(TreeNode *var,
                        TreeNode *expression)
{
  TreeNode * t = allocateTreeNode();
  if (t != NULL)
    {
      t->nodeKind = TAssignExpression;
      t->attr.assignStmt._var = var;
      t->attr.assignStmt.expr = expression;
    }

  return t;
}

TreeNode*
newComparisonExpressionNode(TreeNode *left_expression,
                            TreeNode *relop,
                            TreeNode *right_expression)
{
  TreeNode * t = allocateTreeNode();
  if (t != NULL)
    {
      t->nodeKind = TComparisonExpression;
      t->attr.cmpExpr.lexpr = left_expression;
      t->attr.cmpExpr.op = relop;
      t->attr.cmpExpr.rexpr = right_expression;
    }

  return t;
}

TreeNode*
newAdditiveExpressionNode(TreeNode *left_expression,
                          TreeNode *addop,
                          TreeNode *right_expression)
{
  TreeNode * t = allocateTreeNode();
  if (t != NULL)
    {
      t->nodeKind = TAdditiveExpression;
      t->attr.addExpr.lexpr = left_expression;
      t->attr.addExpr.op = addop;
      t->attr.addExpr.rexpr = right_expression;
    }

  return t;
}

TreeNode*
newMultiplicativeExpressionNode(TreeNode *left_expression,
                                TreeNode *mulop,
                                TreeNode *right_expression)
{
  TreeNode * t = allocateTreeNode();
  if (t != NULL)
    {
      t->nodeKind = TMultiplicativeExpression;
      t->attr.multExpr.lexpr = left_expression;
      t->attr.multExpr.op = mulop;
      t->attr.multExpr.rexpr = right_expression;
    }

  return t;
}

TreeNode *
newArrayNode(TreeNode *_var,
             TreeNode *expression)
{
  TreeNode * t = allocateTreeNode();
  if (t != NULL)
    {
      t->nodeKind = TArray;
      t->attr.arr._var = _var;
      t->attr.arr.arr_expr = expression;
    }

  return t;
}

TreeNode*
newCallNode(TreeNode *_var,
            TreeNode *args) // nullable
{
  TreeNode * t = allocateTreeNode();
  if (t != NULL)
    {
      t->nodeKind = TCall;
      t->attr.call._var = _var;
      t->attr.call.expr_list = args;
    }

  return t;
}

TreeNode *
newVariableNode(char *_ID)
{
  TreeNode * t = allocateTreeNode();
  if (t != NULL)
    {
      	t->nodeKind = TVariable;
      	t->attr.ID = copyString(_ID);
    }
  return t;
}

TreeNode *
newConstantNode(char *_NUM)
{
  TreeNode * t = allocateTreeNode();
  if (t != NULL)
    {
      t->nodeKind = TConstant;
      t->attr.NUM = atoi(_NUM);
    }

  return t;
}

TreeNode *
newTokenTypeNode(TokenType token)
{
  TreeNode * t = allocateTreeNode();
  if (t != NULL)
    {
      t->nodeKind = TTokenType;
      t->attr.TOK = token;
    }

  return t;

}

/* Function copyString allocates and makes a new
 * copy of an existing string
 */
char*
copyString(char * s)
{
  size_t n;
  char * t;
  if (s==NULL) return NULL;
  n = strlen(s)+1;
  t = new char[n];
  strcpy(t,s);
  return t;
}

/* Variable indentno is used by printTree to
 * store current number of spaces to indent
 */

/* macros to increase/decrease indentation */


/* printSpaces indents by printing spaces */
static void printSpaces(int cnt)
{
  int i;
  for (i=0; i<cnt; i++) fprintf(listing," ");
}

/* operatorString returns string of operator */
static const char * operatorString(TokenType op)
{
  if(op == INT) return "int";
  if(op == VOID) return "void";

  if(op == PLUS) return "+";
  if(op == MINUS) return "-";
  if(op == TIMES) return "*";
  if(op == OVER) return "/";

  if(op == LT) return "<";
  if(op == LE) return "<=";
  if(op == GT) return ">";
  if(op == GE) return ">=";
  if(op == EQ) return "==";
  if(op == NE) return "!=";

  return "";
}


/* procedure printTree prints a syntax tree to the 
 * listing file using indentation to indicate subtrees
 */
void printTree(TreeNode* tree,int cnt,int cl)
{
  if (tree == NULL) 
  	return;
  for (; tree != NULL; tree = tree->sibling)
    {
      switch (tree->nodeKind)
        {
        case TError:
        	printSpaces(cnt);
          	fprintf(listing,"[DEBUG] TError at printTree\n");
          	break;
		
		case TProgram:
			fprintf(listing,"starting\n");
			printTree(tree->attr.Pro.cur,cnt,1);
			fprintf(listing,"ended\n");
			break;
		
        case TVariableDeclaration:
        	printTree(tree->attr.varDecl.type_spec,cnt,0);
          	printTree(tree->attr.varDecl._var,2,1);
          	break;

        case TArrayDeclaration:
          	printTree(tree->attr.arrDecl.type_spec,cnt,0);
          	printTree(tree->attr.arrDecl._var,2,0);
          	fprintf(listing,"[");
          	printTree(tree->attr.arrDecl._num,0,0);
          	fprintf(listing,"]\n");
          	break;

        case TFunctionDeclaration:
          	printTree(tree->attr.funcDecl.type_spec,cnt,0);
          	printTree(tree->attr.funcDecl._var,2,1);
          	printTree(tree->attr.funcDecl.params,cnt+2,1);
          	printTree(tree->attr.funcDecl.cmpd_stmt,cnt+2,1);
          	break;

        case TVariableParameter:
          	printTree(tree->attr.varParam.type_spec,cnt,0);
          	fprintf(listing,"  (");
          	printTree(tree->attr.varParam._var,0,0);
          	fprintf(listing,")\n");
          break;

        case TArrayParameter:
          printTree(tree->attr.arrParam.type_spec,cnt,0);
          fprintf(listing,"  (");
          printTree(tree->attr.arrParam._var,0,0);
          fprintf(listing,"[])\n");
          break;

        case TCompoundStatement:
          	printTree(tree->attr.cmpdStmt.local_decl,cnt,1);
          	printTree(tree->attr.cmpdStmt.stmt_list,cnt,1);
          	break;

        case TExpressionStatement:
          	printTree(tree->attr.exprStmt.expr,cnt,1);
          	break;

        case TSelectionStatement:
        	printSpaces(cnt);
			fprintf(listing,"Selection\n");
          	printTree(tree->attr.selectStmt.expr,cnt+2,1);
          	printTree(tree->attr.selectStmt.if_stmt,cnt+2,1);
          	printTree(tree->attr.selectStmt.else_stmt,cnt+2,1);
          	break;

        case TIterationStatement:
          	printSpaces(cnt);
			fprintf(listing,"While\n");
          	printTree(tree->attr.iterStmt.expr,cnt+2,1);
          	printTree(tree->attr.iterStmt.loop_stmt,cnt+2,1);
          break;

        case TReturnStatement:
          	printSpaces(cnt);
			fprintf(listing,"Return\n");
         	printTree(tree->attr.retStmt.expr,cnt+2,1);
         	break;

        case TAssignExpression:
          	printSpaces(cnt);
          	fprintf(listing,"=\n");
          	printTree(tree->attr.assignStmt._var,cnt+2,1);
          	printTree(tree->attr.assignStmt.expr,cnt+2,1);
          	break;

        case TComparisonExpression:
          	printTree(tree->attr.cmpExpr.op,cnt,1);
          	printTree(tree->attr.cmpExpr.lexpr,cnt+2,1);
          	printTree(tree->attr.cmpExpr.rexpr,cnt+2,1);
          	break;

        case TAdditiveExpression:
          	printTree(tree->attr.addExpr.op,cnt,1);
         	printTree(tree->attr.addExpr.lexpr,cnt+2,1);
          	printTree(tree->attr.addExpr.rexpr,cnt+2,1);
          	break;

        case TMultiplicativeExpression:
          	printTree(tree->attr.multExpr.op,cnt,1);
          	printTree(tree->attr.multExpr.lexpr,cnt+2,1);
          	printTree(tree->attr.multExpr.rexpr,cnt+2,1);
          	break;

        case TVariable:
        	printSpaces(cnt);
          	fprintf(listing,"%s", tree->attr.ID);
          	if(cl)
          		fprintf(listing,"\n");
          	break;

        case TArray:
        	printSpaces(cnt);
        	fprintf(listing,"Array_ele\n");
          	printTree(tree->attr.arr._var,cnt+2,1);
          	printTree(tree->attr.arr.arr_expr,cnt+2,1);
          	break;

        case TCall:
        	printSpaces(cnt);
          	fprintf(listing,"Call\n");
          	printTree(tree->attr.call._var,cnt+2,1);
          	printTree(tree->attr.call.expr_list,cnt+4,1);
          	break;

        case TConstant:
        	printSpaces(cnt);
          	fprintf(listing,"%d", tree->attr.NUM);
          	if(cl)
          		fprintf(listing,"\n");
          	break;

        case TTokenType:
        	printSpaces(cnt);
        	fprintf(listing,"%s",operatorString(tree->attr.TOK));
        	if(cl)
          		fprintf(listing,"\n");
          	break;

        default:
        	printSpaces(cnt);
          	fprintf(listing,"[DEBUG] No such nodeKind\n");
        }
    }
}

