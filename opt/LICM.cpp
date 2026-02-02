//
//  LICM.cpp
//  uscc
//
//  Implements basic loop invariant code motion
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
#include <llvm/IR/Dominators.h>
#include <llvm/Analysis/ValueTracking.h>
#pragma clang diagnostic pop

using namespace llvm;

namespace uscc
{
namespace opt
{
	
bool LICM::runOnLoop(llvm::Loop *L, llvm::LPPassManager &LPM)
{
	mChanged = false;
	
	// PA5: Implement
	
	// Save the current loop
	mCurrLoop = L;
	// Grab the loop info
	mLoopInfo = &getAnalysis<LoopInfo>();
	// Grab the dominator tree
	mDomTree = &getAnalysis<DominatorTreeWrapperPass>().getDomTree();

	hoistPreOrder(mDomTree->getNode(mCurrLoop->getHeader()));
	
	return mChanged;
}

bool LICM::isSafeToHoistInstr(Instruction* instr)
{
	// Only hoist certain instruction types from LLVM implementation & other checks
	return (isa<BinaryOperator>(instr) || isa<CmpInst>(instr) || isa<CastInst>(instr) ||
	        isa<SelectInst>(instr) || isa<GetElementPtrInst>(instr)) &&
	       isSafeToSpeculativelyExecute(instr) &&
	       mCurrLoop->hasLoopInvariantOperands(instr);
}

void LICM::hoistInstr(Instruction* instr)
{
	// Move instruction to preheader
	instr->moveBefore(mCurrLoop->getLoopPreheader()->getTerminator());
	// Changed instructions in the loop, so we have to denote it
	mChanged = true;
}

void LICM::hoistPreOrder(DomTreeNode* node)
{
	// Hoist instructions in this block
	BasicBlock* block = node->getBlock();
	if (mCurrLoop->contains(block))
	{
		for (BasicBlock::iterator instrIter = block->begin();
			 instrIter != block->end();)
		{
			Instruction* currInstr = &*instrIter;
			++instrIter; // Increment now since we may move the instruction, otherwise invalidated iterator
			if (isSafeToHoistInstr(currInstr))
			{
				hoistInstr(currInstr);
			}
		}
	}

	// Recurse for children
	for (DomTreeNode* child : node->getChildren())
	{
		hoistPreOrder(child);
	}
}

void LICM::getAnalysisUsage(AnalysisUsage &Info) const
{
	// PA5: Implement

	// Does not modify the CFG
	Info.setPreservesCFG();
	// Execute after dead blocks are removed
	Info.addRequired<DeadBlocks>();
	// Use the built-in Dominator tree and loop info passes
	Info.addRequired<DominatorTreeWrapperPass>();
	Info.addRequired<LoopInfo>();
}
	
} // opt
} // uscc

char uscc::opt::LICM::ID = 0;
