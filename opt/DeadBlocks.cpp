//
//  DeadBlocks.cpp
//  uscc
//
//  Implements Dead Block Removal optimization pass.
//  This removes blocks from the CFG which are unreachable.
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
#include <llvm/IR/CFG.h>
#include <llvm/ADT/DepthFirstIterator.h>
#pragma clang diagnostic pop
#include <set>

using namespace llvm;

namespace uscc
{
namespace opt
{
	
bool DeadBlocks::runOnFunction(Function& F)
{
	bool changed = false;
	
	// PA5: Implement

	// Track visited blocks
	std::set<BasicBlock*> visitedSet;

	for (df_ext_iterator<BasicBlock*, std::set<BasicBlock*>> 
			DI = df_ext_begin(&F.getEntryBlock(), visitedSet),
			DE = df_ext_end(&F.getEntryBlock(), visitedSet);
			DI != DE; ++DI);
	
	// Now loop through all blocks and remove any not in visitedSet
	Function::iterator blockIter = F.begin();
	std::set<BasicBlock*> unreachableSet;
	while (blockIter != F.end())
	{
		BasicBlock* currentBlock = &*blockIter;
		if (visitedSet.find(currentBlock) == visitedSet.end())
		{
			// Not found, so schedule to remove it but not here to avoid iterator invalidation
			unreachableSet.insert(currentBlock);	
		}
		++blockIter; // Increment now since we may delete the block	
	}
	
	if (unreachableSet.size() > 0)
	{
		changed = true;
		for (std::set<BasicBlock*>::iterator i = unreachableSet.begin();
			 i != unreachableSet.end();
			 ++i)
		{
			for (succ_iterator SI = succ_begin(*i), SE = succ_end(*i); SI != SE; ++SI)
			{
				BasicBlock* succBlock = *SI;
				succBlock->removePredecessor(*i);
			}
			(*i)->eraseFromParent();
		}
	}
	return changed;
}
	
void DeadBlocks::getAnalysisUsage(AnalysisUsage& Info) const
{
	// PA5: Implement
	Info.addRequired<ConstantBranch>();
}

} // opt
} // uscc

char uscc::opt::DeadBlocks::ID = 0;
