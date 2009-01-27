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

#ifndef N_ARITHMETIC_PRIMITIVE_FUNCTION
#define N_ARITHMETIC_PRIMITIVE_FUNCTION

#include "PrimitiveFunction.h"
#include "Primitive.h"
#include "SceneGraph.h"

using namespace std;

namespace Fluxus
{

//////////////////////////////////////////////////
/// A general arithmetic primitive function for
/// doing maths on pdata arrays (for particle effects,
/// etc etc)
class ArithmeticPrimFunc : public PrimitiveFunction
{
public:
	ArithmeticPrimFunc();
	~ArithmeticPrimFunc();

	virtual void Run(Primitive &prim, const SceneGraph &world);

private:

	///////////////////////////////
	///@name Template layers
	///@{
	/// Deduce operator types as layers of template functions
	PData *OperatorFirst(const string &op, PData* first, PData *second);
	template<class T>
	PData *OperatorSecond(const string &op, TypedPData<T>* first, PData *second);
	template<class T, class S>
	PData *OperatorThird(const string &op, TypedPData<T>* first, TypedPData<S>* second);

	PData *OperatorFloatFirst(const string &op, PData* first, float second);
	template<class T>
	PData *OperatorFloatSecond(const string &op, TypedPData<T>* first, float second);
	///@}
};

template<class T>
PData *ArithmeticPrimFunc::OperatorSecond(const string &op, TypedPData<T>* first, PData *second)
{
	// the second parameter can either be a float or T (the same type as the first parameter)
	TypedPData<T> *data = dynamic_cast<TypedPData<T>*>(second);	
	if (data) return OperatorThird<T,T>(op,first,data);
	else
	{
		TypedPData<float> *data = dynamic_cast<TypedPData<float>*>(second);
		if (data) return OperatorThird<T,float>(op,first,data);
	}
	return NULL;
}

template<class T, class S>
PData *ArithmeticPrimFunc::OperatorThird(const string &op, TypedPData<T>* first, TypedPData<S>* second)
{
	if (op=="add") 
	{	
		TypedPData<T> *tmp=new TypedPData<T>(first->Size());
		for (unsigned int n=0; n<first->Size(); n++)
		{
			tmp->m_Data[n]=first->m_Data[n]+second->m_Data[n];
		}
		return tmp;
	}
	else if (op=="sub") 
	{	
		TypedPData<T> *tmp=new TypedPData<T>(first->Size());
		for (unsigned int n=0; n<first->Size(); n++)
		{
			tmp->m_Data[n]=first->m_Data[n]-second->m_Data[n];
		}
		return tmp;
	}
	else if (op=="mul") 
	{	
		TypedPData<T> *tmp=new TypedPData<T>(first->Size());
		for (unsigned int n=0; n<first->Size(); n++)
		{
			tmp->m_Data[n]=first->m_Data[n]*second->m_Data[n];
		}
		return tmp;
	}
	else if (op=="div") 
	{	
		TypedPData<T> *tmp=new TypedPData<T>(first->Size());
		for (unsigned int n=0; n<first->Size(); n++)
		{
			tmp->m_Data[n]=first->m_Data[n]/second->m_Data[n];
		}
		return tmp;
	}
	
	return NULL;
}

template<class T>
PData *ArithmeticPrimFunc::OperatorFloatSecond(const string &op, TypedPData<T>* first, float second)
{
	if (op=="add") 
	{	
		TypedPData<T> *tmp=new TypedPData<T>(first->Size());
		for (unsigned int n=0; n<first->Size(); n++)
		{
			tmp->m_Data[n]=first->m_Data[n]+second;
		}
		return tmp;
	}
	else if (op=="sub") 
	{	
		TypedPData<T> *tmp=new TypedPData<T>(first->Size());
		for (unsigned int n=0; n<first->Size(); n++)
		{
			tmp->m_Data[n]=first->m_Data[n]-second;
		}
		return tmp;
	}
	else if (op=="mul") 
	{	
		TypedPData<T> *tmp=new TypedPData<T>(first->Size());
		for (unsigned int n=0; n<first->Size(); n++)
		{
			tmp->m_Data[n]=first->m_Data[n]*second;
		}
		return tmp;
	}
	else if (op=="div") 
	{	
		TypedPData<T> *tmp=new TypedPData<T>(first->Size());
		for (unsigned int n=0; n<first->Size(); n++)
		{
			tmp->m_Data[n]=first->m_Data[n]/second;
		}
		return tmp;
	}
	
	return NULL;
}


}

#endif
