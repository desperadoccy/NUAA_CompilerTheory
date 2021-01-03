#include <stack>
#include <typeinfo>
#include "globals.h"
#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/CallingConv.h>
#include <llvm/IR/IRPrintingPasses.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/Bitcode/BitstreamReader.h>
#include <llvm/Bitcode/BitstreamWriter.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/MCJIT.h>
#include <llvm/ExecutionEngine/GenericValue.h>
#include <llvm/Support/raw_ostream.h>

using namespace llvm;

static LLVMContext MyContext;

class CodeGenBlock {
public:
    BasicBlock *block;
    Value *returnValue;
    std::map<std::string, Value*> locals;
};

class CodeGenContext 
{
    std::stack<CodeGenBlock *> blocks;
	Function *mainFunction;

public:

    Module *module;
	CodeGenContext() { module = new Module("main", MyContext); }
	Value* isexist(string name)
	{
		CodeGenBlock* tmp;
		std::stack<CodeGenBlock *> help;
		Value *ret = NULL;
		while (!blocks.empty())
		{
			tmp = blocks.top();
			blocks.pop();
			help.push(tmp);
			if (tmp->locals.find(name) != tmp->locals.end())
			{
				ret = tmp->locals[name];
				break;
			}
		}
		while (!help.empty())
		{
			tmp = help.top();
			help.pop();
			blocks.push(tmp);
		}
		return ret;
	}
    void generateCode(TreeNode* root);
    GenericValue runCode();
    std::map<std::string, Value*>& locals() { return blocks.top()->locals; }
    BasicBlock *currentBlock() { return blocks.top()->block; }
    void pushBlock(BasicBlock *block) { blocks.push(new CodeGenBlock()); blocks.top()->returnValue = NULL; blocks.top()->block = block; }
	void pushBlock(BasicBlock *block, std::map<std::string, Value*>& locals, Value* returnValue) 
	{
		blocks.push(new CodeGenBlock());
		blocks.top()->returnValue = returnValue;
		blocks.top()->block = block;
		blocks.top()->locals = locals;
	}
    void popBlock() { CodeGenBlock *top = blocks.top(); blocks.pop(); delete top; }
    void setCurrentReturnValue(Value *value) 
	{
		CodeGenBlock* tmp;
		std::stack<CodeGenBlock *> help;
		while (!blocks.empty())
		{
			tmp = blocks.top();
			blocks.pop();
			tmp->returnValue = value;
			help.push(tmp);
		}
		while (!help.empty())
		{
			tmp = help.top();
			help.pop();
			blocks.push(tmp);
		}
	}
    Value* getCurrentReturnValue() { return blocks.top()->returnValue; }
	void setmain(Function* fc) { mainFunction = fc; }
};
