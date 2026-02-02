//
//  ContantBranch.cpp
//  uscc
//
//  Implements Constant Branch Folding opt pass.
//  This converts conditional branches on constants to
//  unconditional branches.
//
//---------------------------------------------------------
//  Copyright (c) 2014, Sanjay Madhav
//  All rights reserved.
//
//  This file is distributed under the BSD license.
//  See LICENSE.TXT for details.
//---------------------------------------------------------
#include "Passes.h"
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wconversion"
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Constants.h>
#pragma clang diagnostic pop
#include <set>

using namespace llvm;

namespace uscc
{
namespace opt
{
	
bool ConstantBranch::runOnFunction(Function& F)
{
	bool changed = false;
	
	// PA5: Implement

	// Make a set that contains the instructions we'll remove
	std::set<BranchInst*> removeSet;

	// Loop through each block
	Function::iterator blockIter = F.begin();

	while (blockIter != F.end())
	{
		// Loop through instructions in block
		BasicBlock::iterator instrIter = blockIter->begin();
		while (instrIter != blockIter->end())
		{
			// Is this a branch instruction?
			if (BranchInst* branchInst = dyn_cast<BranchInst>(instrIter))
			{
				// Is it conditional?
				if (branchInst->isConditional())
				{
					// Is the condition a constant int?
					if (ConstantInt* cond = dyn_cast<ConstantInt>(branchInst->getCondition()))
					{
						// Replace the instruction
						removeSet.insert(branchInst);
					}
				}
			}
			++instrIter;
		}
		
		++blockIter;
	}

	// Now remove any instructions we flagged
	if (removeSet.size() > 0)
	{
		changed = true;
		for (std::set<BranchInst*>::iterator i = removeSet.begin();
			 i != removeSet.end();
			 ++i)
		{
			BranchInst* branchInst = *i;
			ConstantInt* cond = cast<ConstantInt>(branchInst->getCondition());
			
			BasicBlock* targetBlock = nullptr;
			BasicBlock* removedBlock = nullptr;
			
			if (cond->isZero())
			{
				// False branch - take successor 1
				targetBlock = branchInst->getSuccessor(1);
				removedBlock = branchInst->getSuccessor(0);
			}
			else
			{
				// True branch - take successor 0
				targetBlock = branchInst->getSuccessor(0);
				removedBlock = branchInst->getSuccessor(1);
			}
			
			// Create a new unconditional branch to the target block
			BranchInst::Create(targetBlock, branchInst);
			
			// Notify the removed successor
			removedBlock->removePredecessor(branchInst->getParent());
			
			// Erase the old branch instruction
			branchInst->eraseFromParent();
		}
	}
	
	return changed;
}

void ConstantBranch::getAnalysisUsage(AnalysisUsage& Info) const
{
	// PA5: Implement

	// This pass only executes once the ConstantOps pass has run on the function.
	// Helps ensure all constants are propagated before we do branch folding.
	Info.addRequired<ConstantOps>();


}
	
} // opt
} // uscc

char uscc::opt::ConstantBranch::ID = 0;
