#include <iostream>
#include <vector>
#include <llvm/IR/Value.h>
#include <llvm/IR/IRBuilder.h>
#include "codegen.h"
#include "parser.hpp"

using namespace std;

class CodeGenContext;
Function *realmain;		//��¼������main����
Function *curfc;		//��¼��ǰ���ĸ�����������
int manum = 0;			//Դ���붨��main�����ĸ���
IRBuilder<> builder(MyContext);
BasicBlock *curret;		//��¼��ǰ������exit��

Value* codegen(TreeNode *cur, CodeGenContext& context);


/* Compile the AST into a module */
void CodeGenContext::generateCode(TreeNode* root)
{
	std::cout << "Generating code...\n";
	
	/* Create the top level interpreter function to call as entry */
	vector<Type*> argTypes;
	FunctionType *ftype = FunctionType::get(Type::getVoidTy(MyContext), makeArrayRef(argTypes), false);
	mainFunction = Function::Create(ftype, GlobalValue::InternalLinkage, "clo", module);
	BasicBlock *bblock = BasicBlock::Create(MyContext, "entry", mainFunction);
	
	builder.SetInsertPoint(bblock);
	curfc = mainFunction;
	/* Push a new variable/block context */
	pushBlock(bblock);
	codegen(root,*this); /* emit bytecode for the toplevel block */
	ReturnInst::Create(MyContext, bblock);
	popBlock();
	
	/* Print the bytecode in a human-readable format 
	   to see if our program compiled properly
	 */
	std::cout << "Code is generated.\n";
	// module->dump();

	legacy::PassManager pm;
	pm.add(createPrintModulePass(outs()));
	pm.run(*module);
}

/* Executes the AST by running the main function */
GenericValue CodeGenContext::runCode() {
	if (manum != 1)			//���Դ����main������û�ж�������ض���
	{
		std::cout << "Main function ilegal!\n";
		exit(0);
	}
	std::cout << "Running code...\n";
	ExecutionEngine *ee = EngineBuilder( unique_ptr<Module>(module) ).create();
	ee->finalizeObject();
	vector<GenericValue> noargs;
	GenericValue v = ee->runFunction(realmain, noargs);		//����ǵ�Ҫ��������main������ʼִ�д���
	std::cout << "Code was run.\n";
	return v;
}

//���ص�ǰ�ڵ����������
static Type *typeOf(TreeNode* type_spec) 
{
	if(type_spec->attr.TOK==INT)
		return Type::getInt64Ty(MyContext);
	else
		return Type::getVoidTy(MyContext);
}


/* -- Code Generation -- */

Value* codegen(TreeNode *cur, CodeGenContext& context)
{
	if (cur == NULL)
	{
		return NULL;
	}
	switch (cur->nodeKind)		//�Բ�ͬ�Ľڵ�Ҫ�ò�ͬ�Ĺ��췽��
	{
	case TProgram:
	{
		//֮���Զඨ��һ��TProgram���ͣ�������Ϊ����ͬ�ڸ�����䣬������ȫ�ֱ����ͺ����Ľ��涨��
		TreeNode *head = cur->attr.Pro.cur;
		Value *last = NULL;
		while (head != NULL)
		{
			codegen(head, context);
			head = head->sibling;
		}
		return last;
	}
	case TVariableDeclaration:
	{
		//��������Ĺ��̣��ȼ����û���ض��壬Ȼ�����API����һ���ռ伴�ɣ��ǵ÷�����ű�
		std::cout<<"Creating variable declaration ";
		string tmp;
		if (cur->attr.varDecl.type_spec->attr.TOK == INT)
		{
			std::cout << "int";
			tmp = "int";
		}
		else
		{
			std::cout << "void";
			tmp = "void";
		}
		std::cout<<" "<< cur->attr.varDecl._var->attr.ID<<'\n';
		string curname = cur->attr.varDecl._var->attr.ID;
		if (context.locals().find(curname) != context.locals().end())	//�����û�б�����
		{
			std::cout << "Variable has existed!\n";
			exit(0);
		}
		AllocaInst *alloc = new AllocaInst(typeOf(cur->attr.varDecl.type_spec), 8, tmp, context.currentBlock());
		context.locals()[curname] = alloc;		//��¼��������ĵ�ַ��������ű�
		return alloc;
	}
	case TArrayDeclaration:
	{
		//����Ķ���߶������ڱ������壬����Ҫע��API����ʱ�����캯��Ҫ�����������͵ģ�
		//��Ҫ�ഫ�ݱ���һ�������С�ı���
		std::cout << "Creating array declaration ";
		string tmp;
		
		if (cur->attr.arrDecl.type_spec->attr.TOK == INT)
		{
			std::cout << "int";
			tmp = "int";
		}
		else
		{
			std::cout << "void";
			tmp = "void";
		}
		std::cout << " " <<  cur->attr.arrDecl._var->attr.ID << '\n';
		string curname = cur->attr.arrDecl._var->attr.ID;
		if (context.locals().find(curname) != context.locals().end())
		{
			std::cout << "array has existed!\n";
			exit(0);
		}
		//����һ����ʱ����tt�����ڱ�ʾҪ����������Ĵ�С
		Value * tt = ConstantInt::get(Type::getInt64Ty(MyContext), cur->attr.arrDecl._num->attr.NUM, true);
		AllocaInst *alloc = new AllocaInst(typeOf(cur->attr.varDecl.type_spec), 8, tt, tmp, context.currentBlock());
		context.locals()[curname] = alloc;
		return alloc;
	}
	case TFunctionDeclaration:
	{
		//��������Ĳ��ֱȽϸ��ӣ���Ҫ����API���Ҵ��ݺܶ����ݡ�
		vector<Type*> argTypes;
		TreeNode *head;
		TreeNode *tmp;
		vector<TreeNode *> mknew;
		mknew.clear();
		head = cur->attr.funcDecl.params;
		//����Ҫ�����������ͼ�¼������װ��һ��vector���Ϊһ������
		while (head != NULL)
		{
			std::cout << "Creating function parm type\n";
			argTypes.push_back(typeOf(head->attr.varParam.type_spec));
			//����Ϊÿһ�������½���һ�����������ҽ���ڵ����͸�Ϊ�˱�������
			//������ΪҪ�ڵ�ǰ�������ڣ�Ϊ��Щ��������ռ䣬������Щ�����ں������ڲ��ǿ��õ�
			tmp = new TreeNode;
			tmp->nodeKind = TVariableDeclaration;
			tmp->attr.varDecl.type_spec = head->attr.varParam.type_spec;
			tmp->attr.varDecl._var = head->attr.varParam._var;
			mknew.push_back(tmp);
			head = head->sibling;
		}
		string fname = cur->attr.funcDecl._var->attr.ID;
		string paname;
		//���ﰴ�ղ������API���Ҵ�����Ӧ��������
		FunctionType *ftype = FunctionType::get(typeOf(cur->attr.funcDecl.type_spec), makeArrayRef(argTypes), false);
		Function *function = Function::Create(ftype, GlobalValue::InternalLinkage, fname.c_str(), context.module);
		BasicBlock *bblock = BasicBlock::Create(MyContext, "entry", function, 0);
		//���ﶨ����һ��exit�飬��Ϊ�˽��returnָ������õ�bug
		//�����е�return��䶼�������������ת��ȫ����ת��exit�飬���ɽ������
		BasicBlock *eblock = BasicBlock::Create(MyContext, "exit", function, 0);
		curfc = function;
		curret = eblock;

		if (fname == "main")
		{
			realmain = function;
			manum++;
		}
		builder.SetInsertPoint(bblock);
		context.pushBlock(bblock);

		//������ʱ��ź�������ֵ�ı������������ݳ���ִ��ʱ�ľ������������������ֵ
		AllocaInst *alloc;
		string rret = "this_will_never_been_used";
		if (typeOf(cur->attr.funcDecl.type_spec) == Type::getInt64Ty(MyContext))
		{
			alloc = new AllocaInst(Type::getInt64Ty(MyContext), 8, "int", context.currentBlock());
			context.locals()[rret] = alloc;
		}

		Function::arg_iterator argsValues = function->arg_begin();
		Value* argumentValue;
		//Ϊ��������ռ䣬��Ҫ�ǵ��ͷŵ���ʱ����
		vector<TreeNode *>::iterator it;
		for (it = mknew.begin(); it != mknew.end(); it++) 
		{
			codegen(*it, context);
			argumentValue = &*argsValues++;
			paname = (*it)->attr.varDecl._var->attr.ID;
			std::cout << "Function parm value" << paname << '\n';
			argumentValue->setName(paname);
			StoreInst *inst = new StoreInst(argumentValue, context.locals()[paname], false, bblock);
			delete(*it);
			*it = NULL;
		}
		mknew.clear();
		codegen(cur->attr.funcDecl.cmpd_stmt,context);

		//����return��䣬�Ӷ����һ�������ĵ��ù���
		builder.CreateBr(eblock);
		if (typeOf(cur->attr.funcDecl.type_spec) == Type::getInt64Ty(MyContext))
		{
			Value *retval = new LoadInst(alloc, "", false, eblock);
			context.setCurrentReturnValue(retval);
		}
		ReturnInst::Create(MyContext, context.getCurrentReturnValue(), eblock);

		context.popBlock();
		builder.SetInsertPoint(context.currentBlock());
		std::cout << "Created function: " << cur->attr.funcDecl._var->attr.ID << '\n';
		return function;
	}
	case TCompoundStatement:
	{
		//�������ķ��룬����ڵ�ֻ�ǵݹ�Ľ��ڲ��ڵ�codegen����
		TreeNode* head = cur->attr.cmpdStmt.local_decl;
		Value *last = NULL;
		while (head != NULL)
		{
			last = codegen(head, context);
			head = head->sibling;
		}

		head = cur->attr.cmpdStmt.stmt_list;
		while (head != NULL)
		{
			last = codegen(head, context);
			head = head->sibling;
		}
		return last;
	}
	case TExpressionStatement:
	{
		std::cout << "Creating code for expression\n";
		return codegen(cur->attr.exprStmt.expr,context);
	}
	case TSelectionStatement:
	{
		//ѡ��ṹ��ѭ���ṹ��ת�����̵���Ҫ˼���ĵط���Ҳ���ѵ�
		//��Ҫ����LLVM IR�Ľṹ���������������ĸ��
		//��������˼·�Ͳ���ע����׸��������Ϊ����������ת����ע��
		std::cout << "Creating code for IF ELSE\n";
		Value *cond = codegen(cur->attr.selectStmt.expr, context);
		Type * condty = cond->getType();
		if (condty->isIntegerTy())
		{
			//����ж������������� a = b+1���֣�����ֵ��Int��Ҫ��ӳ�䵽Bool
			cond = builder.CreateIntCast(cond, Type::getInt64Ty(MyContext), true);
			cond = builder.CreateICmpNE(cond, ConstantInt::get(Type::getInt64Ty(MyContext), 0, true));
		}

		//�������������飬Ϊʵ��IF ELSE�ṹ��׼�����京��������һ��
		BasicBlock * ifthen = BasicBlock::Create(MyContext, "ifthen", curfc);
		BasicBlock * elsethen = BasicBlock::Create(MyContext, "elsethen", curfc);
		BasicBlock * merge = BasicBlock::Create(MyContext, "merge", curfc);
		
		//IF���� ���ж���������ִ���ĸ�������
		builder.CreateCondBr(cond, ifthen, elsethen);

		//IF then������
		builder.SetInsertPoint(ifthen);
		context.pushBlock(ifthen);
		codegen(cur->attr.selectStmt.if_stmt, context);
		//���ǵ�һ��Ҫֱ������merge�飬�����ִ��else then������������
		builder.CreateBr(merge);
		context.popBlock();

		//else then ������
		builder.SetInsertPoint(elsethen);
		context.pushBlock(elsethen);
		codegen(cur->attr.selectStmt.else_stmt, context);
		builder.CreateBr(merge);
		context.popBlock();

		//������Ϊmerge��̳�ִ��IF���ǰ�ķ��ű���ʹ��ִ��IF���ǰ��Ŀ��ܹ�����
		//ͬһ�ݷ��ű��������ŷ�������
		std::map<std::string, Value*> nowloc = context.locals();
		Value* nowret = context.getCurrentReturnValue();
		context.popBlock();
		context.pushBlock(merge, nowloc, nowret);
		builder.SetInsertPoint(merge);
		return nowret;
	}
	case TIterationStatement:
	{
		//WHILE���ķ��룬��DXͬѧ�����ѣ���ʵWHILE�����ʵ��continue��break�쳣�ļ�
		//ֻ��Ҫ��������ֱ����������������ת���ɣ�һ��ָ��wl�飬һ��ָ��merge�鼴��
		//��ɹ���
		std::cout << "Creating code for WHILE\n";
		//wl����while��������жϵĵ���һ�������飬��Ϊ��Ҫ�������ת����
		BasicBlock * wl = BasicBlock::Create(MyContext, "wl", curfc);
		//while������
		BasicBlock * then = BasicBlock::Create(MyContext, "then", curfc);
		//while��������
		BasicBlock * merge = BasicBlock::Create(MyContext, "merge", curfc);
		
		builder.CreateBr(wl);
		builder.SetInsertPoint(wl);

		context.pushBlock(wl);
		Value *cond = codegen(cur->attr.iterStmt.expr, context);
		Type * condty = cond->getType();
		if (condty->isIntegerTy())
		{
			cond = builder.CreateIntCast(cond, Type::getInt64Ty(MyContext), true);
			cond = builder.CreateICmpNE(cond, ConstantInt::get(Type::getInt64Ty(MyContext), 0, true));
		}
		builder.CreateCondBr(cond, then, merge);
		context.popBlock();

		builder.SetInsertPoint(then);
		context.pushBlock(then);
		codegen(cur->attr.iterStmt.loop_stmt, context);
		builder.CreateBr(wl);
		context.popBlock();

		//������IF�����merge��Ҳ��Ҫ����֮ǰ�ķ��ű�������while֮���������
		//��֤���߶����õ���ͬ�ķ��ţ���ͬ��ֵ
		std::map<std::string, Value*> nowloc = context.locals();
		Value* nowret = context.getCurrentReturnValue();
		context.popBlock();
		context.pushBlock(merge, nowloc, nowret);
		builder.SetInsertPoint(merge);
		return nowret;
	}
	case TReturnStatement:
	{
		//return���Ӧ����API���õ�ԭ�򣬲�������������return������
		//���Խ�Ϻ��������е���ʱ��ŷ���ֵ�ı���������ֻ��Ҫ��һ��return���
		//�����һ����������ת����������ĩβ��exit�飩�����Ұѷ���ֵ������ʱ��������
		std::cout << "Creating return code\n";
		Value *returnValue = codegen(cur->attr.retStmt.expr,context);
		string name = "this_will_never_been_used";
		Value *addr = context.isexist(name);
		StoreInst* st = new StoreInst(returnValue, addr, false, context.currentBlock());
		builder.CreateBr(curret);
		return returnValue;
	}
	case TAssignExpression:
	{
		//��ֵ��䣬��ʵ���Ƕ��ұߵ�ʽ�ӽ���codegen�������õ���ֵ�浽��ߵı�����
		//������Ҫ��������ǵ����ı�������������Ԫ�أ����������Ԫ�أ�����Ҫ������Ԫ��
		//���±������codegen,��arr[x*y]����ôҪ��ȷ�����±�ſ���
		std::cout << "Creating assignment for ";
		//�˴�ע�⣡Ϊ����������ʽ���Լ������жϣ�����ֵӦ�����ұ߱���ʽ��ֵ��Ҫ�ȼ�¼����
		//����ֵ��Ӧ����storeָ��ķ���ֵ�����Ǹ���ַ....
		Value* tmp = codegen(cur->attr.assignStmt.expr, context);
		if (cur->attr.assignStmt._var->nodeKind == TVariable)
		{
			std::cout << "Variable " << cur->attr.assignStmt._var->attr.ID << '\n';
			string leftname = cur->attr.assignStmt._var->attr.ID;
			//������ߵı��������Ҳ鿴�˱����Ƿ񱻶���
			Value *lef = context.isexist(leftname);
			if (lef == NULL)
			{
				std::cout << "Undeclared variable " << leftname << '\n';
				return NULL;
			}
			StoreInst* st = new StoreInst(tmp, lef, false, context.currentBlock());
		}
		else
		{
			std::cout << "array ele " << cur->attr.assignStmt._var->attr.arr._var->attr.ID << '\n';
			string name = cur->attr.assignStmt._var->attr.arr._var->attr.ID;
			Value *arr = context.isexist(name);
			if (arr == NULL)
			{
				std::cout << "Undeclared array: " << name << '\n';
				return NULL;
			}
			//���������±꣬���ҵ�����Ӧ��get element ptr��API���Ӷ�����Ԫ�ص�ַ
			Value *pos = codegen(cur->attr.assignStmt._var->attr.arr.arr_expr, context);
			Value *ele = builder.CreateGEP(arr, pos);
			StoreInst* st = new StoreInst(tmp, ele, false, context.currentBlock());
		}
		return tmp;
	}
	case TComparisonExpression:
	{
		//��ϵ������䣬������־��ǵݹ�ĵ�����������ʽ�ӵ�codegen������
		//������ӦAPI���ɣ�����һ��Ҫע��һ�㣬API������������������һ��
		//�����ڱ���ʱ�ͻᱨ���������޷�ִ��
		std::cout << "Creating CMP operation\n";
		Value *left = codegen(cur->attr.cmpExpr.lexpr, context);
		Value *right = codegen(cur->attr.cmpExpr.rexpr, context);
		switch (cur->attr.cmpExpr.op->attr.TOK)
		{
		case LT:
			return builder.CreateICmpSLT(left, right);
		case LE:
			return builder.CreateICmpSLE(left, right);
		case GT:
			return builder.CreateICmpSGT(left, right);
		case GE:
			return builder.CreateICmpSGE(left, right);
		case EQ:
			return builder.CreateICmpEQ(left, right);
		case NE:
			return builder.CreateICmpNE(left, right);
		default:
			std::cout << "CMP error!\n";
			exit(0);
		}
	}
	//���ƹ�ϵ�������ֻ�ǻ��������������
	case TMultiplicativeExpression:
	case TAdditiveExpression:
	{
		fprintf(listing, "Creating MATH operation \n");
		Instruction::BinaryOps instr;
		switch (cur->attr.addExpr.op->attr.TOK) 
		{
		case PLUS: 	instr = Instruction::Add; goto math;
		case MINUS: 	instr = Instruction::Sub; goto math;
		case TIMES: 		instr = Instruction::Mul; goto math;
		case OVER: 		instr = Instruction::SDiv; goto math;

			/* TODO comparison */
		}

		return NULL;
	math:
		return BinaryOperator::Create(instr, codegen(cur->attr.addExpr.lexpr,context),
			codegen(cur->attr.addExpr.rexpr,context), "", context.currentBlock());
	}
	case TArray:
	{
		//�����������������򵥱�������һ�������±��codegen
		std::cout << "Creating array identifier reference: " << cur->attr.arr._var->attr.ID << '\n';
		string name = cur->attr.arr._var->attr.ID;
		Value *arr = context.isexist(name);
		if (arr == NULL)
		{
			std::cout << "Undeclared array: " << name << '\n';
			return NULL;
		}
		Value *pos = codegen(cur->attr.arr.arr_expr, context);
		Value *ele = builder.CreateGEP(arr, pos);
		return new LoadInst(ele, "", false, context.currentBlock());
	}
	case TCall:
	{
		//������������codegen����Ҫ��Ҫ����һ�������б�����İ���API���ù���������
		string fname = cur->attr.call._var->attr.ID;
		Function *function = context.module->getFunction(fname);
		if (function == NULL) 
		{
			std::cout << "No such function " << cur->attr.call._var->attr.ID << '\n';
		}
		std::vector<Value*> args;
		TreeNode *head = cur->attr.call.expr_list;
		while (head != NULL)
		{
			args.push_back(codegen(head,context));
			head = head->sibling;
		}
		
		CallInst *call = CallInst::Create(function, makeArrayRef(args), "", context.currentBlock());
		std::cout << "Created method call: " << cur->attr.call._var->attr.ID << '\n';
		return call;
	}
	case TVariable:
	{
		//���ر���
		std::cout << "Creating identifier reference: " << cur->attr.ID <<'\n';
		string name = cur->attr.ID;
		Value *arg = context.isexist(name);
		if (arg == NULL)
		{
			std::cout << "Undeclared variable: " << cur->attr.ID <<'\n';
			return NULL;
		}
		return new LoadInst(arg, "", false, context.currentBlock());
	}
	case TConstant:
	{
		//����ֵ������C�����е�һ��int����ֵ���õ�һ��llvm ir���õ�Value����ֵ
		std::cout << "Creating integer: " << cur->attr.NUM <<'\n';
		return ConstantInt::get(Type::getInt64Ty(MyContext), cur->attr.NUM, true);
	}
	default:
		return NULL;
	}
}

