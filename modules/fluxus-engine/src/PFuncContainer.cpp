// Copyright (C) 2005 Dave Griffiths
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

#include "PFuncContainer.h"
#include "ArithmeticPrimFunc.h"
#include "GenSkinWeightsPrimFunc.h"
#include "SkinWeightsToVertColsPrimFunc.h"
#include "SkinningPrimFunc.h"

using namespace Fluxus;

PFuncContainer::PFuncContainer()
{
}

PFuncContainer::~PFuncContainer()
{
}

unsigned int PFuncContainer::Make(const string &name)
{
	if (name=="arithmetic")
	{
		m_PFuncVec.push_back(new ArithmeticPrimFunc);
		return m_PFuncVec.size()-1;
	}
	else if (name=="genskinweights")
	{
		m_PFuncVec.push_back(new GenSkinWeightsPrimFunc);
		return m_PFuncVec.size()-1;
	}
	else if (name=="skinweights->vertcols")
	{
		m_PFuncVec.push_back(new SkinWeightsToVertColsPrimFunc);
		return m_PFuncVec.size()-1;
	}
	else if (name=="skinning")
	{
		m_PFuncVec.push_back(new SkinningPrimFunc);
		return m_PFuncVec.size()-1;
	}
	return 0;
}

void PFuncContainer::Run(unsigned int id, Primitive *p, const SceneGraph *sg)
{
	if (id<m_PFuncVec.size())
	{
		m_PFuncVec[id]->Run(*p,*sg);
	}	
}

void PFuncContainer::Clear()
{
	for (vector<PrimitiveFunction*>::iterator i=m_PFuncVec.begin();
		i!=m_PFuncVec.end(); i++)
	{
		delete *i;
	}
	m_PFuncVec.clear();
}

