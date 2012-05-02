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

#ifndef N_PDATA_ARITH_OPERATOR
#define N_PDATA_ARITH_OPERATOR

#include <typeinfo>
#include "PDataOperator.h"

using namespace std;

namespace Fluxus
{

///////////////////////////////////////////////////////////////
/// A collection of common arithmetic which may be useful for 
/// operations on pdata arrays. The way this works is a little
/// bit strange at present, and will be changed in a future 
/// release...

/// All this template code makes the pdata operators easier to write, as it means the compiler
/// generates a lot of the code for type/function mapping and also all the combinations that
/// an operator doesn't support don't have to be written by hand (the default one returns 
/// a warning message). This is admittedly a bit odd and may all be replaced at some point. 
class AddOperator : public PDataOperator
{
public:
	AddOperator() {}
	
	template <class S, class T>
	static PData *Operate(TypedPData<S> *a, T b)
	{
		Trace::Stream<<"AddOperator has no operator for types: "<<typeid(a).name()<<" and "	
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
template<>
PData *AddOperator::Operate(TypedPData<dColour> *a, float b);
template<>
PData *AddOperator::Operate(TypedPData<dColour> *c, TypedPData<float> *d);
template<>
PData *AddOperator::Operate(TypedPData<dColour> *c, dColour d);
template<>
PData *AddOperator::Operate(TypedPData<dColour> *c, TypedPData<dColour> *d);

class MultOperator : public PDataOperator
{
public:
	MultOperator() {}
	
	template <class S, class T>
	static PData *Operate(TypedPData<S> *a, T b)
	{
		Trace::Stream<<"MultOperator has no operator for types: "<<typeid(a).name()<<" and "	
			<<typeid(b).name()<<endl;
		return NULL;
	}
	
};

template<>
PData *MultOperator::Operate(TypedPData<float> *a, float b);
template<>
PData *MultOperator::Operate(TypedPData<dVector> *a, float b);
template<>
PData *MultOperator::Operate(TypedPData<dColour> *a, float b);
template<>
PData *MultOperator::Operate(TypedPData<dVector> *a, dVector b);
template<>
PData *MultOperator::Operate(TypedPData<float> *a, TypedPData<float> *b);
template<>
PData *MultOperator::Operate(TypedPData<dVector> *a, TypedPData<float> *b);
template<>
PData *MultOperator::Operate(TypedPData<dVector> *a, TypedPData<dVector> *b);

class SineOperator : public PDataOperator
{
public:
	SineOperator() {}
	
	template <class S, class T>
	static PData *Operate(TypedPData<S> *a, T b)
	{
		Trace::Stream<<"SineOperator has no operator for types: "<<typeid(a).name()<<" and "	
			<<typeid(b).name()<<endl;
		return NULL;
	}
	
};

template<>
PData *SineOperator::Operate(TypedPData<float> *a, TypedPData<float> *b);

class CosineOperator : public PDataOperator
{
public:
	CosineOperator() {}
	
	template <class S, class T>
	static PData *Operate(TypedPData<S> *a, T b)
	{
		Trace::Stream<<"CosineOperator has no operator for types: "<<typeid(a).name()<<" and "	
			<<typeid(b).name()<<endl;
		return NULL;
	}
	
};

template<>
PData *CosineOperator::Operate(TypedPData<float> *a, TypedPData<float> *b);

class ClosestOperator : public PDataOperator
{
public:
	ClosestOperator() {}
	
	template <class S, class T>
	static PData *Operate(TypedPData<S> *a, T b)
	{
		Trace::Stream<<"ClosestOperator has no operator for types: "<<typeid(a).name()<<" and "	
			<<typeid(b).name()<<endl;
		return NULL;
	}
	
};

template<>
PData *ClosestOperator::Operate(TypedPData<dVector> *a, dVector b);
template<>
PData *ClosestOperator::Operate(TypedPData<dVector> *a, float b);

}

#endif
