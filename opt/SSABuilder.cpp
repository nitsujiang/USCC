//
//  SSABuilder.cpp
//  uscc
//
//  Implements SSABuilder class
//  
//---------------------------------------------------------
//  Copyright (c) 2014, Sanjay Madhav
//  All rights reserved.
//
//  This file is distributed under the BSD license.
//  See LICENSE.TXT for details.
//---------------------------------------------------------

#include "SSABuilder.h"
#include "../parse/Symbols.h"

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wconversion"
#include <llvm/IR/Value.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/Constants.h>
#pragma clang diagnostic pop

#include <list>

using namespace uscc::opt;
using namespace uscc::parse;
using namespace llvm;

// Called when a new function is started to clear out all the data
void SSABuilder::reset()
{
	// PA4: Implement
	for (auto& pair : mVarDefs)
	{
		delete pair.second;
	}
	mVarDefs.clear();
	for (auto& pair : mIncompletePhis)
	{
		delete pair.second;
	}
	mIncompletePhis.clear();
	mSealedBlocks.clear();
}

// For a specific variable in a specific basic block, write its value
void SSABuilder::writeVariable(Identifier* var, BasicBlock* block, Value* value)
{
	// PA4: Implement
	mVarDefs[block]->operator[](var) = value;
}

// Read the value assigned to the variable in the requested basic block
// Will recursively search predecessor blocks if it was not written in this block
Value* SSABuilder::readVariable(Identifier* var, BasicBlock* block)
{
	// PA4: Implement
	if (mVarDefs[block]->find(var) != mVarDefs[block]->end())
	{
		return mVarDefs[block]->operator[](var);
	}
	return readVariableRecursive(var, block);
}

// This is called to add a new block to the maps
void SSABuilder::addBlock(BasicBlock* block, bool isSealed /* = false */)
{
	// PA4: Implement
	mVarDefs[block] = new SubMap();
	mIncompletePhis[block] = new SubPHI();

	if (isSealed)
	{
		sealBlock(block);
	}
}

// This is called when a block is "sealed" which means it will not have any
// further predecessors added. It will complete any PHI nodes (if necessary)
void SSABuilder::sealBlock(llvm::BasicBlock* block)
{
	// PA4: Implement
	// Add all incomplete PHI operands for this block
	for (auto& pair : *mIncompletePhis[block])
	{
		addPhiOperands(pair.first, pair.second);
	}
	
	// Mark this block as sealed
	mSealedBlocks.insert(block);
}

// Recursively search predecessor blocks for a variable
Value* SSABuilder::readVariableRecursive(Identifier* var, BasicBlock* block)
{
	Value* retVal = nullptr;
	
	// PA4: Implement
	// Check if block is NOT sealed (incomplete CFG)
	if (mSealedBlocks.find(block) == mSealedBlocks.end())
	{
		// Insert PHI at the beginning of the block
		if (!block->empty()) {
			retVal = PHINode::Create(var->llvmType(), 0, "", &block->front());
		} else {
			retVal = PHINode::Create(var->llvmType(), 0, "", block);
		}
		mIncompletePhis[block]->operator[](var) = static_cast<PHINode*>(retVal);
	}
	else if (block->getSinglePredecessor())
	{
		retVal = readVariable(var, block->getSinglePredecessor());
	}
	else
	{
		if (!block->empty()) {
			retVal = PHINode::Create(var->llvmType(), 0, "", &block->front());
		} else {
			retVal = PHINode::Create(var->llvmType(), 0, "", block);
		}
		writeVariable(var, block, retVal);
		retVal = addPhiOperands(var, static_cast<PHINode*>(retVal));
	}
	writeVariable(var, block, retVal);
	return retVal;
}

// Adds phi operands based on predecessors of the containing block
Value* SSABuilder::addPhiOperands(Identifier* var, PHINode* phi)
{
	// PA4: Implement
	for (auto it = pred_begin(phi->getParent()); it != pred_end(phi->getParent()); ++it)
	{
		BasicBlock* pred = *it;
		Value* value = readVariable(var, pred);
		phi->addIncoming(value, pred);
	}
	return tryRemoveTrivialPhi(phi);
}

// Removes trivial phi nodes
Value* SSABuilder::tryRemoveTrivialPhi(llvm::PHINode* phi)
{
	Value* same = nullptr;
	
	// PA4: Implement
	// Check if phi is trivial (all operands are same or self-reference)
	for (unsigned i = 0; i < phi->getNumIncomingValues(); ++i)
	{
		Value* op = phi->getIncomingValue(i);
		// Skip if operand is same as the value we already found, or self-reference
		if (op == same || op == phi)
		{
			continue;
		}
		// If we already found a different value, phi is not trivial
		if (same != nullptr)
		{
			return phi;
		}
		same = op;
	}
	
	// If same is still nullptr, phi is unreachable or in start block
	if (same == nullptr)
	{
		same = UndefValue::get(phi->getType());
	}
	
	// Remember all users except the phi itself
	std::list<User*> users;
	for (auto it = phi->user_begin(); it != phi->user_end(); ++it)
	{
		if (*it != phi)
		{
			users.push_back(*it);
		}
	}
	
	// Replace all uses of phi with same and remove phi
	phi->replaceAllUsesWith(same);
	
	// Update mVarDefs to point to 'same' instead of the deleted phi
	BasicBlock* block = phi->getParent();
	for (auto& pair : *mVarDefs[block])
	{
		if (pair.second == phi)
		{
			pair.second = same;
		}
	}
	
	phi->eraseFromParent();
	
	// Try to recursively remove all phi users, which might have become trivial
	for (auto use : users)
	{
		// Could be any type of instruction
		if (PHINode* userPhi = llvm::dyn_cast<PHINode>(use))
		{
			tryRemoveTrivialPhi(userPhi);
		}
	}
	
	return same;
}
