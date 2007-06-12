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

#ifndef PFUNC_CONTAINER
#define PFUNC_CONTAINER

#include <string>
#include <map>
#include "Renderer.h"
#include "PrimitiveFunction.h"
#include "ArithmeticPrimFunc.h"

using namespace std;

namespace Fluxus
{

class PFuncContainer
{
public:
	PFuncContainer();
	~PFuncContainer();
	
	unsigned int Make(const string &name);
	template <class T>
	void SetArg(unsigned int id, const string &name, const T &arg);
	void Run(unsigned int id, Primitive *p, const SceneGraph *sg);
	void Clear();

private:

	vector<PrimitiveFunction*> m_PFuncVec;

};

template <class T>
void PFuncContainer::SetArg(unsigned int id, const string &name, const T &arg)
{
	if (id<m_PFuncVec.size())
	{
		m_PFuncVec[id]->SetArg<T>(name,arg);
	}
}


}

#endif
