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

#include "ArithmeticPrimFunc.h"
#include "Primitive.h"
#include "SceneGraph.h"

using namespace Fluxus;

ArithmeticPrimFunc::ArithmeticPrimFunc()
{
}

ArithmeticPrimFunc::~ArithmeticPrimFunc()
{
}

void ArithmeticPrimFunc::Run(Primitive &prim, const SceneGraph &world)
{
	string op = GetArg<string>("operator",string("add"));
	PData *src = prim.GetDataRaw(GetArg<string>("src",string("p")));
	PData *other = NULL;
	if (ArgExists<string>("other"))
	{
		other = prim.GetDataRaw(GetArg<string>("other",string("p")));
	}

	// need at least a source
	if (src!=NULL)
	{
		if (other!=NULL) // pdata array as second argument
		{
			if (src->Size()==other->Size()) 
			{
				prim.SetDataRaw(GetArg<string>("dst",string("p")),
					OperatorFirst(op,src,other));
			}
		}
		else if (ArgExists<float>("constant")) 	// float as the second argument
		{
			prim.SetDataRaw(GetArg<string>("dst",string("p")),
				OperatorFloatFirst(op,src,GetArg<float>("constant",1)));
		}
	}
}

PData *ArithmeticPrimFunc::OperatorFirst(const string &op, PData* first, PData *second)
{
	TypedPData<dVector> *data = dynamic_cast<TypedPData<dVector>*>(first);	
	if (data) return OperatorSecond<dVector>(op,data,second);
	else
	{
		TypedPData<dColour> *data = dynamic_cast<TypedPData<dColour>*>(first);
		if (data) return OperatorSecond<dColour>(op,data,second);
		else 
		{
			TypedPData<float> *data = dynamic_cast<TypedPData<float>*>(first);
			if (data) return OperatorSecond<float>(op,data,second);
			else 
			{
				TypedPData<dMatrix> *data = dynamic_cast<TypedPData<dMatrix>*>(first);
				if (data) return OperatorSecond<dMatrix>(op,data,second);
			}
		}
	}
	return NULL;
}

PData *ArithmeticPrimFunc::OperatorFloatFirst(const string &op, PData* first, float second)
{
	TypedPData<dVector> *data = dynamic_cast<TypedPData<dVector>*>(first);	
	if (data) return OperatorFloatSecond<dVector>(op,data,second);
	else
	{
		TypedPData<dColour> *data = dynamic_cast<TypedPData<dColour>*>(first);
		if (data) return OperatorFloatSecond<dColour>(op,data,second);
		else 
		{
			TypedPData<float> *data = dynamic_cast<TypedPData<float>*>(first);
			if (data) return OperatorFloatSecond<float>(op,data,second);
			else 
			{
				TypedPData<dMatrix> *data = dynamic_cast<TypedPData<dMatrix>*>(first);
				if (data) return OperatorFloatSecond<dMatrix>(op,data,second);
			}
		}
	}
	return NULL;
}

