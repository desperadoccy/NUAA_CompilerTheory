#include <iostream>
#include <vector>
#include <llvm/IR/Value.h>
#include <llvm/IR/IRBuilder.h>
#include "codegen.h"
#include "parser.hpp"

using namespace std;

class CodeGenContext;
Function *realmain;		//记录真正的main函数
Function *curfc;		//记录当前在哪个函数定义内
int manum = 0;			//源代码定义main函数的个数
IRBuilder<> builder(MyContext);
BasicBlock *curret;		//记录当前函数的exit块

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
	if (manum != 1)			//检测源代码main函数有没有定义或者重定义
	{
		std::cout << "Main function ilegal!\n";
		exit(0);
	}
	std::cout << "Running code...\n";
	ExecutionEngine *ee = EngineBuilder( unique_ptr<Module>(module) ).create();
	ee->finalizeObject();
	vector<GenericValue> noargs;
	GenericValue v = ee->runFunction(realmain, noargs);		//这里记得要从真正的main函数开始执行代码
	std::cout << "Code was run.\n";
	return v;
}

//返回当前节点的数据类型
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
	switch (cur->nodeKind)		//对不同的节点要用不同的构造方法
	{
	case ProgramK:
	{
		//之所以多定义一个ProgramK类型，就是因为他不同于复合语句，它允许全局变量和函数的交替定义
		TreeNode *head = cur->attr.Pro.cur;
		Value *last = NULL;
		while (head != NULL)
		{
			codegen(head, context);
			head = head->sibling;
		}
		return last;
	}
	case VariableDeclarationK:
	{
		//定义变量的过程：先检测有没有重定义，然后调用API申请一个空间即可，记得放入符号表
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
		if (context.locals().find(curname) != context.locals().end())	//检测有没有被定义
		{
			std::cout << "Variable has existed!\n";
			exit(0);
		}
		AllocaInst *alloc = new AllocaInst(typeOf(cur->attr.varDecl.type_spec), 8, tmp, context.currentBlock());
		context.locals()[curname] = alloc;		//记录下来申请的地址，放入符号表
		return alloc;
	}
	case ArrayDeclarationK:
	{
		//数组的定义高度类似于变量定义，但是要注意API调用时，构造函数要调用数组类型的，
		//需要多传递表明一个数组大小的变量
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
		//创建一个临时标量tt，用于表示要创建的数组的大小
		Value * tt = ConstantInt::get(Type::getInt64Ty(MyContext), cur->attr.arrDecl._num->attr.NUM, true);
		AllocaInst *alloc = new AllocaInst(typeOf(cur->attr.varDecl.type_spec), 8, tt, tmp, context.currentBlock());
		context.locals()[curname] = alloc;
		return alloc;
	}
	case FunctionDeclarationK:
	{
		//函数定义的部分比较复杂，需要调用API并且传递很多数据。
		vector<Type*> argTypes;
		TreeNode *head;
		TreeNode *tmp;
		vector<TreeNode *> mknew;
		mknew.clear();
		head = cur->attr.funcDecl.params;
		//首先要将参数的类型记录下来，装在一个vector里，作为一个参数
		while (head != NULL)
		{
			std::cout << "Creating function parm type\n";
			argTypes.push_back(typeOf(head->attr.varParam.type_spec));
			//这里为每一个参数新建了一个副本，并且将其节点类型改为了变量声明
			//这是因为要在当前函数体内，为这些参数申请空间，这样这些参数在函数体内才是可用的
			tmp = new TreeNode;
			tmp->nodeKind = VariableDeclarationK;
			tmp->attr.varDecl.type_spec = head->attr.varParam.type_spec;
			tmp->attr.varDecl._var = head->attr.varParam._var;
			mknew.push_back(tmp);
			head = head->sibling;
		}
		string fname = cur->attr.funcDecl._var->attr.ID;
		string paname;
		//这里按照步骤调用API并且传递相应参数即可
		FunctionType *ftype = FunctionType::get(typeOf(cur->attr.funcDecl.type_spec), makeArrayRef(argTypes), false);
		Function *function = Function::Create(ftype, GlobalValue::InternalLinkage, fname.c_str(), context.module);
		BasicBlock *bblock = BasicBlock::Create(MyContext, "entry", function, 0);
		//这里定义了一个exit块，是为了解决return指令不起作用的bug
		//将所有的return语句都翻译成无条件跳转，全部跳转到exit块，即可解决问题
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

		//定义临时存放函数返回值的变量，用来根据程序执行时的具体情况，来决定返回值
		AllocaInst *alloc;
		string rret = "this_will_never_been_used";
		if (typeOf(cur->attr.funcDecl.type_spec) == Type::getInt64Ty(MyContext))
		{
			alloc = new AllocaInst(Type::getInt64Ty(MyContext), 8, "int", context.currentBlock());
			context.locals()[rret] = alloc;
		}

		Function::arg_iterator argsValues = function->arg_begin();
		Value* argumentValue;
		//为参数申请空间，并要记得释放掉临时变量
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

		//构造return语句，从而完成一个函数的调用功能
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
	case CompoundStatementK:
	{
		//复合语句的翻译，这个节点只是递归的将内部节点codegen即可
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
	case ExpressionStatementK:
	{
		std::cout << "Creating code for expression\n";
		return codegen(cur->attr.exprStmt.expr,context);
	}
	case SelectionStatementK:
	{
		//选择结构与循环结构是转换过程的需要思考的地方，也是难点
		//需要理解LLVM IR的结构，并且理解基本块的概念。
		//具体的设计思路就不在注释中赘述，这里为几个块间的跳转加上注释
		std::cout << "Creating code for IF ELSE\n";
		Value *cond = codegen(cur->attr.selectStmt.expr, context);
		Type * condty = cond->getType();
		if (condty->isIntegerTy())
		{
			//如果判断条件是类似于 a = b+1这种，返回值是Int，要先映射到Bool
			cond = builder.CreateIntCast(cond, Type::getInt64Ty(MyContext), true);
			cond = builder.CreateICmpNE(cond, ConstantInt::get(Type::getInt64Ty(MyContext), 0, true));
		}

		//定义三个基本块，为实现IF ELSE结构做准备，其含义与命名一致
		BasicBlock * ifthen = BasicBlock::Create(MyContext, "ifthen", curfc);
		BasicBlock * elsethen = BasicBlock::Create(MyContext, "elsethen", curfc);
		BasicBlock * merge = BasicBlock::Create(MyContext, "merge", curfc);
		
		//IF语句后 由判断条件决定执行哪个基本块
		builder.CreateCondBr(cond, ifthen, elsethen);

		//IF then基本块
		builder.SetInsertPoint(ifthen);
		context.pushBlock(ifthen);
		codegen(cur->attr.selectStmt.if_stmt, context);
		//最后记得一定要直接跳到merge块，否则会执行else then，造成语义错误
		builder.CreateBr(merge);
		context.popBlock();

		//else then 基本块
		builder.SetInsertPoint(elsethen);
		context.pushBlock(elsethen);
		codegen(cur->attr.selectStmt.else_stmt, context);
		builder.CreateBr(merge);
		context.popBlock();

		//这里是为merge块继承执行IF语句前的符号表，使得执行IF语句前后的块能够共用
		//同一份符号表，这样才符合语义
		std::map<std::string, Value*> nowloc = context.locals();
		Value* nowret = context.getCurrentReturnValue();
		context.popBlock();
		context.pushBlock(merge, nowloc, nowret);
		builder.SetInsertPoint(merge);
		return nowret;
	}
	case IterationStatementK:
	{
		//WHILE语句的翻译，受DX同学的提醒，其实WHILE语句里实现continue和break异常的简单
		//只需要将这两句分别翻译成两个无条件跳转即可，一个指向wl块，一个指向merge块即可
		//完成功能
		std::cout << "Creating code for WHILE\n";
		//wl块是while语句条件判断的单独一个基本块，因为需要被多次跳转到此
		BasicBlock * wl = BasicBlock::Create(MyContext, "wl", curfc);
		//while函数体
		BasicBlock * then = BasicBlock::Create(MyContext, "then", curfc);
		//while语句后的语句
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

		//类似于IF语句后的merge，也需要将其之前的符号表拷贝到while之后的语句块中
		//保证二者都能用到相同的符号，相同的值
		std::map<std::string, Value*> nowloc = context.locals();
		Value* nowret = context.getCurrentReturnValue();
		context.popBlock();
		context.pushBlock(merge, nowloc, nowret);
		builder.SetInsertPoint(merge);
		return nowret;
	}
	case ReturnStatementK:
	{
		//return语句应该是API调用的原因，并不能起到真正的return的作用
		//所以结合函数定义中的临时存放返回值的变量，我们只需要将一个return语句
		//翻译成一个无条件跳转（跳到函数末尾的exit块），并且把返回值赋给临时变量即可
		std::cout << "Creating return code\n";
		Value *returnValue = codegen(cur->attr.retStmt.expr,context);
		string name = "this_will_never_been_used";
		Value *addr = context.isexist(name);
		StoreInst* st = new StoreInst(returnValue, addr, false, context.currentBlock());
		builder.CreateBr(curret);
		return returnValue;
	}
	case AssignExpressionK:
	{
		//赋值语句，其实就是对右边的式子进行codegen，并将得到的值存到左边的变量中
		//但是需要区分左边是单纯的变量，还是数组元素，如果是数组元素，还需要将数组元素
		//的下标进行先codegen,如arr[x*y]，那么要先确定其下标才可以
		std::cout << "Creating assignment for ";
		//此处注意！为了能用连等式，以及条件判断，返回值应该是右边表达式的值，要先记录下来
		//返回值不应该是store指令的返回值，那是个地址....
		Value* tmp = codegen(cur->attr.assignStmt.expr, context);
		if (cur->attr.assignStmt._var->nodeKind == VariableK)
		{
			std::cout << "Variable " << cur->attr.assignStmt._var->attr.ID << '\n';
			string leftname = cur->attr.assignStmt._var->attr.ID;
			//加载左边的变量，并且查看此变量是否被定义
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
			//加载数组下标，并且调用相应的get element ptr的API，从而加载元素地址
			Value *pos = codegen(cur->attr.assignStmt._var->attr.arr.arr_expr, context);
			Value *ele = builder.CreateGEP(arr, pos);
			StoreInst* st = new StoreInst(tmp, ele, false, context.currentBlock());
		}
		return tmp;
	}
	case ComparisonExpressionK:
	{
		//关系运算语句，这个部分就是递归的调用左右两边式子的codegen，并且
		//调用相应API即可，但是一定要注意一点，API的两个参数必须类型一致
		//否则在编译时就会报错，导致无法执行
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
	//类似关系运算符，只是换成了算数运算符
	case MultiplicativeExpressionK:
	case AdditiveExpressionK:
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
	case ArrayK:
	{
		//加载数组变量，比起简单变量，多一个数组下标的codegen
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
	case CallK:
	{
		//函数调用语句的codegen，主要是要构造一个参数列表，别的按照API调用规则来即可
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
	case VariableK:
	{
		//加载变量
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
	case ConstantK:
	{
		//常量值，即从C语言中的一个int类型值，得到一个llvm ir中用的Value类型值
		std::cout << "Creating integer: " << cur->attr.NUM <<'\n';
		return ConstantInt::get(Type::getInt64Ty(MyContext), cur->attr.NUM, true);
	}
	default:
		return NULL;
	}
}

