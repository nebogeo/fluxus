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


#include "PDataOperator.h"

#ifndef N_PDATA_ARITH_OPERATOR
#define N_PDATA_ARITH_OPERATOR

using namespace std;

// todo:
// sin/cos nearest divide minus 

namespace fluxus
{

class AddOperator : public PDataOperator
{
public:
	AddOperator() {}
	
	template <class S, class T>
	static PData *Operate(TypedPData<S> *a, T b)
	{
		cerr<<"AddOperator has no operator for types: "<<typeid(a).name()<<" and "	
			<<typeid(b).name()<<endl;
		return NULL;
	}
	
};

template<>
PData *AddOperator::Operate(TypedPData<float> *a, float b);
template<>
PData *AddOperator::Operate(TypedPData<dVector> *a, float b);
template<>
PData *AddOperator::Operate(TypedPData<dVector> *a, dVector b);
template<>
PData *AddOperator::Operate(TypedPData<float> *a, TypedPData<float> *b);
template<>
PData *AddOperator::Operate(TypedPData<dVector> *a, TypedPData<float> *b);
template<>
PData *AddOperator::Operate(TypedPData<dVector> *a, TypedPData<dVector> *b);

class MultOperator : public PDataOperator
{
public:
	MultOperator() {}
	
	template <class S, class T>
	static PData *Operate(TypedPData<S> *a, T b)
	{
		cerr<<"MultOperator has no operator for types: "<<typeid(a).name()<<" and "	
			<<typeid(b).name()<<endl;
		return NULL;
	}
	
};

template<>
PData *MultOperator::Operate(TypedPData<float> *a, float b);
template<>
PData *MultOperator::Operate(TypedPData<dVector> *a, float b);
template<>
PData *MultOperator::Operate(TypedPData<dVector> *a, dVector b);
template<>
PData *MultOperator::Operate(TypedPData<float> *a, TypedPData<float> *b);
template<>
PData *MultOperator::Operate(TypedPData<dVector> *a, TypedPData<float> *b);
template<>
PData *MultOperator::Operate(TypedPData<dVector> *a, TypedPData<dVector> *b);

}

#endif
