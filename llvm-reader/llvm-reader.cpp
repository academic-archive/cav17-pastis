/* Quentin Carbonneaux - 2016 */

#include <iostream>
#include <fstream>
#include <vector>

#include <llvm/IR/LLVMContext.h>
#include <llvm/Bitcode/ReaderWriter.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/ModuleSlotTracker.h>
#include <llvm/IR/ValueMap.h>
#include <llvm/Support/DataStream.h>

using namespace llvm;

bool Debug = true;

struct Expr {

	/* Expression class to build and output
	 * the expressions used by the compexity
	 * analysis.
	 */

	enum Type {
		Add,
		Sub,
		Mul,
		Num,
		Var,
		Rnd
	};

	Type type;
	int64_t value;
	std::string var;
	std::unique_ptr<Expr> left, right;

	bool isRandom()
	{
		return type == Rnd;
	}

	friend std::ostream &operator<<(std::ostream &os, const Expr &e)
	{
		switch (e.type) {
		case Rnd:
			return os << "random";
		case Var:
			return os << e.var;
		case Num:
			return os << e.value;
		case Add:
			return os << "(" << *e.left << " + " << *e.right << ")";
		case Sub:
			return os << "(" << *e.left << " - " << *e.right << ")";
		case Mul:
			return os << "(" << *e.left << " * " << *e.right << ")";
		}
		return os;
	}

	Expr(Type t, std::unique_ptr<Expr> l, std::unique_ptr<Expr> r)
	: type(t), left(std::move(l)), right(std::move(r))
	{
		if (left->isRandom() || right->isRandom()) {
			type = Rnd;
			left = nullptr;
			right = nullptr;
		}
	}

	Expr(int64_t con)
	: type(Num), value(con) {}

	Expr(std::string v)
	: type(Var), var(v) {}

	Expr()
	: type(Rnd) {}
};

struct Cond {

	/* Conditions matched by the translator are
	 * very simple because C's && and || are
	 * translated to branches.
	 */

	enum Type {
		True,
		False,
		LE,
		GE,
		LT,
		GT,
		EQ,
		NE,
		Random
	};

	Type type;
	std::shared_ptr<Expr> left, right;

	bool isRandomOrTrue()
	{
		return type == Random || type == True;
	}

	Cond negate()
	{
		Type t;
		switch (type) {
		case True:
			t = False;
			break;
		case False:
			t = True;
			break;
		case LE:
			t = GT;
			break;
		case GE:
			t = LT;
			break;
		case LT:
			t = GE;
			break;
		case GT:
			t = LE;
			break;
		case EQ:
			t = NE;
			break;
		case NE:
			t = EQ;
			break;
		case Random:
			t = Random;
			break;
		}
		return Cond(t, left, right);
	}

	friend std::ostream &operator<<(std::ostream &os, const Cond &c)
	{
		switch(c.type) {
		case True:
			return os << "true";
		case False:
			return os << "false";
		case Random:
			return os << "random";
		case LE:
			return os << *c.left << " <= " << *c.right;
		case GE:
			return os << *c.left << " >= " << *c.right;
		case LT:
			return os << *c.left << " < " << *c.right;
		case GT:
			return os << *c.left << " > " << *c.right;
		case EQ:
			return os << *c.left << " = " << *c.right;
		case NE:
			return os << *c.left << " != " << *c.right;
		}
		return os;
	}

	Cond(Type t, std::shared_ptr<Expr> l, std::shared_ptr<Expr> r)
	: type(t), left(std::move(l)), right(std::move(r))
	{
		if ((left && left->isRandom())
		||  (right && right->isRandom())) {
			type = Random;
			left = nullptr;
			right = nullptr;
		}
	}

	Cond(Type t)
	: type(t) {}

	Cond()
	: type(Random) {}
};

struct Edge {

	/* An edge of the graph we will pass to
	 * the complexity analysis.  Edges bear
	 * program actions on them.
	 */

	enum Type {
		Guard,
		Assign,
		None,
		Invalid,
	};

	Type type;
	unsigned dest;
	Cond cond;
	std::string var;
	std::unique_ptr<Expr> expr;

	Edge(unsigned d, const Cond &c)
	: type(Guard), dest(d), cond(c)
	{
		if (cond.isRandomOrTrue()) {
			type = None;
			cond = Cond();
		}
	}

	Edge(unsigned d, const std::string &v, std::unique_ptr<Expr> e)
	: type(Assign), dest(d), var(v), expr(std::move(e)) {}

	Edge(unsigned d)
	: type(None), dest(d) {}
};

struct Func {

	std::string name;
	std::vector<std::string> arguments;
	std::vector<std::string> locals;
	std::vector<std::vector<Edge>> body;
	unsigned start, ret;
	unsigned nextNode;

	unsigned newNode()
	{
		return nextNode++;
	}

	void addEdge(unsigned s, Edge &&e)
	{
		assert(s < nextNode && e.dest < nextNode);
		body.resize(nextNode);
		body.at(s).push_back(std::move(e));
	}

	Func(const std::string &n, const std::vector<std::string> &args)
	: name(n), arguments(args), ret(-1u), nextNode(0) {}
};

std::unique_ptr<ModuleSlotTracker> Machine;

std::string valueName(Value *v)
{
	if (v->hasName())
		return v->getName();
	else {
		int SlotNum = Machine->getLocalSlot(v);
		return "_" + std::to_string(SlotNum);
	}
}

bool isTracked(Value *v)
{
	AllocaInst *AI = dyn_cast<AllocaInst>(v);
	return AI && AI->isStaticAlloca() && AI->getAllocatedType()->isIntegerTy();
}

std::unique_ptr<Expr> valueExpr(Value *v)
{
	if (isa<Instruction>(*v)) {

		if (LoadInst *LI = dyn_cast<LoadInst>(v)) {
			Value *vptr = LI->getPointerOperand();
			if (isTracked(vptr))
				return make_unique<Expr>(valueName(vptr));
		}

		if (BinaryOperator *BO = dyn_cast<BinaryOperator>(v)) {
			Expr::Type ety;

			switch (BO->getOpcode()) {
			case Instruction::Add:
				ety = Expr::Add;
				break;
			case Instruction::Sub:
				ety = Expr::Sub;
				break;
			case Instruction::Mul:
				ety = Expr::Mul;
				break;
			default:
				return make_unique<Expr>();
			}

			return make_unique<Expr>(
				ety,
				valueExpr(BO->getOperand(0)),
				valueExpr(BO->getOperand(1))
			);
		}
	}

	if (isa<Argument>(*v))
		return make_unique<Expr>(v->getName());

	if (ConstantInt *CI = dyn_cast<ConstantInt>(v)) {
		int64_t truncVal = CI->getValue().getLimitedValue();
		if (CI->getType()->isIntegerTy(32))
			return make_unique<Expr>((int32_t)truncVal);
		else
			return make_unique<Expr>(truncVal);
	}

	return make_unique<Expr>();
}

Cond valueCond(Value *v)
{
	if (ICmpInst *CI = dyn_cast<ICmpInst>(v)) {
		Cond::Type cty;

		switch(CI->getSignedPredicate()) {
		case CmpInst::ICMP_EQ:
			cty = Cond::EQ;
			break;
		case CmpInst::ICMP_NE:
			cty = Cond::NE;
			break;
		case CmpInst::ICMP_SLE:
			cty = Cond::LE;
			break;
		case CmpInst::ICMP_SLT:
			cty = Cond::LT;
			break;
		case CmpInst::ICMP_SGE:
			cty = Cond::GE;
			break;
		case CmpInst::ICMP_SGT:
			cty = Cond::GT;
			break;
		default:
			return Cond();
		}

		return Cond(
			cty,
			valueExpr(CI->getOperand(0)),
			valueExpr(CI->getOperand(1))
		);
	}

	if (ConstantInt *CI = dyn_cast<ConstantInt>(v)) {
		if (CI->getValue() == 0)
			return Cond(Cond::False);
		else
			return Cond(Cond::True);
	}

	return Cond();
}

unsigned processBlock(BasicBlock *BB, Func &f, ValueMap<BasicBlock *, unsigned> &bbMap)
{
	unsigned node = -1u;

	{
		auto it = bbMap.find(BB);
		if (it != bbMap.end()) {
			node = it->second;
			if (node == -1u)
				return it->second = f.newNode();
			else
				return node;
		}
	}

	bbMap[BB] = -1u;

	// errs() << "In block " << Machine->getLocalSlot(BB) << '\n';

	for (auto BI = BB->rbegin(), BE = BB->rend(); BI != BE; ++BI) {
		Instruction *CurI = &*BI;

		if (StoreInst *SI = dyn_cast<StoreInst>(CurI)) {

			/* The only kind of regular instructions
			 * that triggers inspection is stores to
			 * local variables.  When we spot one, we
			 * try to fetch the full expression that
			 * is assigned and export it.
			 */

			Value *vptr = SI->getPointerOperand();
			if (isTracked(vptr)) {
				unsigned newNode = f.newNode();
				auto e = valueExpr(SI->getValueOperand());
				f.addEdge(newNode, Edge(node, valueName(vptr), std::move(e)));
				node = newNode;
			}
		}

		else if (BranchInst *BI = dyn_cast<BranchInst>(CurI)) {
			if (BI->isConditional()) {
				Cond C = valueCond(BI->getCondition());
				Cond CNeg = C.negate();

				BasicBlock *BBTrue = BI->getSuccessor(0);
				BasicBlock *BBFalse = BI->getSuccessor(1);

				node = f.newNode();

				f.addEdge(node, Edge(processBlock(BBTrue, f, bbMap), C));
				f.addEdge(node, Edge(processBlock(BBFalse, f, bbMap), CNeg));
			}
			else {
				BasicBlock *Succ = BI->getSuccessor(0);
				node = processBlock(Succ, f, bbMap);
			}

		}

		else if (isa<ReturnInst>(CurI)) {
			node = f.newNode();
			if (f.ret != -1u)
				f.addEdge(node, Edge(f.ret));
			else
				f.ret = node;
		}

		else if (TerminatorInst *TI = dyn_cast<TerminatorInst>(CurI)) {

			/* Conservative case:
			 * we do not know what the condition for the
			 * jump is, but we know all the different
			 * possible targets.
			 */

			unsigned nsucc = TI->getNumSuccessors();
			assert(nsucc > 0);

			if (nsucc == 1)
				node = processBlock(TI->getSuccessor(0), f, bbMap);
			else {
				node = f.newNode();
				for (unsigned s = 0; s < nsucc; ++s) {
					BasicBlock *succ = TI->getSuccessor(s);
					unsigned nodeSucc = processBlock(succ, f, bbMap);
					f.addEdge(node, Edge(nodeSucc));
				}
			}
		}
	}

	assert(node != -1u);
	unsigned newNode = bbMap[BB];
	if (newNode == -1u)
		bbMap[BB] = node;
	else
		f.addEdge(newNode, Edge(node));
	return node;
}

Func extractFunc(Function &F)
{
	std::vector<std::string> args;
	for (Argument &A: F.getArgumentList())
		args.push_back(valueName(&A));
	Func f(valueName(&F), args);
	ValueMap<BasicBlock *, unsigned> bbMap;
	f.start = processBlock(&F.getEntryBlock(), f, bbMap);
	return f;
}

int main(int argc, char *argv[])
{
	LLVMContext Context;
	std::string Filename = "test.o";

	std::string ErrorMessage;
	std::unique_ptr<DataStreamer> Streamer =
	  getDataFileStreamer(Filename, &ErrorMessage);

	if (!Streamer) {
		errs() << argv[0] << ": " << ErrorMessage << '\n';
		return 1;
	}

	ErrorOr<std::unique_ptr<Module>> ErrOrM =
		getStreamedBitcodeModule(Filename, std::move(Streamer), Context);
	std::unique_ptr<Module> M = std::move(*ErrOrM);
	M->materializeAll();


	Module::iterator MI = M->begin();
	if (MI == M->end()) {
		errs() << argv[0] << ": Empty module\n";
		return 1;
	}

	Machine = make_unique<ModuleSlotTracker>(M.get());
	Machine->incorporateFunction(*MI);
	Func f = extractFunc(*MI);

	if (Debug) {
		std::ofstream ofs;
		ofs.open(Filename + ".dot");

		ofs << "digraph " << f.name << "{\n";
		unsigned n = 0, e = 0;;
		for (auto &vec: f.body) {
			ofs << "n_" << n << " [shape=circle,fontsize=8,label=\"";
			if (n == f.start)
				ofs << "start";
			if (n == f.ret)
				ofs << "end";
			ofs << "\"];\n";
			for (auto &edge: vec) {
				if (edge.type == Edge::None) {
					ofs << "n_" << n << " -> " << "n_" << edge.dest << ";\n";
					continue;
				}
				ofs << "e_" << e << " [shape=box,fontsize=12,label=\"";
				switch (edge.type) {
				case Edge::Guard:
					ofs << "guard " << edge.cond;
					break;
				case Edge::Assign:
					ofs << "assign " << edge.var << " = " << *edge.expr;
					break;
				default:
					break;
				}
				ofs << "\"];\n";
				ofs << "n_" << n << " -> " << "e_" << e << ";\n";
				ofs << "e_" << e << " -> " << "n_" << edge.dest << ";\n";
				e++;
			}
			n++;
		}
		ofs << "}\n";
	}

	return 0;
}
