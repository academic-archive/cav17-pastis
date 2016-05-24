/* Quentin Carbonneaux - 2016 */

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
#include <llvm/Support/DataStream.h>
#include <llvm/ADT/SmallSet.h>
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

	void dump()
	{
		/* For debugging output. */

		const char *op;
		switch (type) {
		case Rnd:
			errs() << "random";
			return;
		case Var:
			errs() << var;
			return;
		case Num:
			errs() << value;
			return;
		case Add:
			op = " + ";
			break;
		case Sub:
			op = " - ";
			break;
		case Mul:
			op = " * ";
			break;
		}
		errs() << "(";
		left->dump();
		errs() << op;
		right->dump();
		errs() << ")";
	}

	Expr(Type t, std::unique_ptr<Expr> l, std::unique_ptr<Expr> r)
	: type(t)
	{
		left = std::move(l);
		right = std::move(r);

		if (left->isRandom() || right->isRandom()) {
			type = Rnd;
			left.reset();
			right.reset();
		}
	}

	Expr(int64_t con)
	: type(Num), value(con)
	{ }

	Expr(std::string v)
	: type(Var), var(v)
	{ }

	Expr()
	: type(Rnd)
	{ }
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

void emitFunction(Function &F)
{
	for (Function::iterator BI = F.begin(), BE = F.end(); BI != BE; ++BI) {

		if (Debug)
			errs() << "> Block " << Machine->getLocalSlot(&*BI) << '\n';

		for (BasicBlock::iterator II = BI->begin(), IE = BI->end(); II != IE; ++II) {

			if (StoreInst *SI = dyn_cast<StoreInst>(II)) {

				/* The only kind of regular instructions
				 * that triggers inspection is stores to
				 * local variables.  When we spot one, we
				 * try to fetch the full expression that
				 * is assigned and export it.
				 */

				Value *vptr = SI->getPointerOperand();
				if (isTracked(vptr)) {
					auto e = valueExpr(SI->getValueOperand());
					if (Debug) {
						errs() << "  " << valueName(vptr) << " = ";
						e->dump();
						errs() << '\n';
					}
				}
			}

			/* Handle conditional branch. */
			/* Handle return. */
			if (TerminatorInst *TI = dyn_cast<TerminatorInst>(II)) {

				/* Conservative case:
				 * we do not know what the condition for the
				 * jump is, but we know all the different
				 * possible targets.
				 */

				if (Debug) {
					errs() << "  jump to ";
					for (unsigned s = 0, nsucc = TI->getNumSuccessors(); s < nsucc; ++s) {
						errs() << Machine->getLocalSlot(TI->getSuccessor(s)) << " ";
					}
					errs() << "\n";
				}
			}
		}
	}
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
	emitFunction(*MI);

	return 0;
}
