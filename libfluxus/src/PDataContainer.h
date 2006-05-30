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

#include <map>
#include "PData.h"
#include "PDataOperator.h"
#include "PDataArithmetic.h"

#ifndef PDATA_CONTAINER
#define PDATA_CONTAINER

using namespace std;

namespace fluxus
{

// All this template code makes the pdata operators easier to write, as it means the compiler
// generates a lot of the code for type/function mapping and also all the combinations that
// an operator doesn't support don't have to be written by hand (the default one returns 
// an warning message). I'm not too fond of templates personally, but here they have taken
// over, and it seems to work well at this point.

class PDataContainer
{
public:
	PDataContainer();
	virtual ~PDataContainer(); 
	
	void AddData(const string &name, PData* pd);
	void CopyData(const string &name, string newname);
	template<class T> vector<T>* GetDataVec(const string &name);
	void RemoveDataVec(const string &name);
	// returns existance
	bool GetDataInfo(const string &name, char &type, unsigned int &size);
	// unchecked for speed - use GetDataInfo()
	template<class T> void SetData(const string &name, unsigned int index, T s);	
	template<class T> T    GetData(const string &name, unsigned int index);
	template<class T> PData *DataOp(const string &op, const string &name, T operand);
	PData* GetDataRaw(const string &name);
	void SetDataRaw(const string &name, PData* pd);
	template <class S, class T> PData *FindOperate(const string &name, TypedPData<S> *a, T b);
	// erases all current data
	void Resize(unsigned int size);
	unsigned int Size();

protected:

	// called when a named pdata mapping changes 
	virtual void PDataDirty() = 0;
	
	// todo: replace with a hashmap
	map<string,PData*> m_PData;

};

template<class T> 
void PDataContainer::SetData(const string &name, unsigned int index, T s)	
{
	dynamic_cast<TypedPData<T>*>(m_PData[name])->m_Data[index]=s;
}

template<class T> 
T PDataContainer::GetData(const string &name, unsigned int index)
{
	return dynamic_cast<TypedPData<T>*>(m_PData[name])->m_Data[index];
}

template<class T>
vector<T>* PDataContainer::GetDataVec(const string &name)
{
	map<string,PData*>::iterator i=m_PData.find(name);
	if (i==m_PData.end())
	{
		cerr<<"Primitive::GetPDataVec: pdata: "<<name<<" doesn't exists"<<endl;
		return NULL;
	}
	
	TypedPData<T> *ptr=dynamic_cast<TypedPData<T> *>(i->second);
	if (!ptr) 
	{
		cerr<<"Primitive::GetPDataVec: pdata: "<<name<<" is not of type: "<<typeid(TypedPData<T>).name()<<endl;
		return NULL;
	}
	
	return &ptr->m_Data;
}

template<class T>
PData *PDataContainer::DataOp(const string &op, const string &name, T operand)
{
	map<string,PData*>::iterator i=m_PData.find(name);
	if (i==m_PData.end())
	{
		cerr<<"Primitive::DataOp: pdata: "<<name<<" doesn't exists"<<endl;
		return NULL;
	}
	
	TypedPData<dVector> *data = dynamic_cast<TypedPData<dVector>*>(i->second);	
	if (data) return FindOperate<dVector,T>(op, data, operand);
	else
	{
		TypedPData<dColour> *data = dynamic_cast<TypedPData<dColour>*>(i->second);
		if (data) return FindOperate<dColour, T>(op, data, operand);
		else 
		{
			TypedPData<float> *data = dynamic_cast<TypedPData<float>*>(i->second);
			if (data) return FindOperate<float, T>(op, data, operand);
			else 
			{
				TypedPData<dMatrix> *data = dynamic_cast<TypedPData<dMatrix>*>(i->second);
				if (data) return FindOperate<dMatrix, T>(op, data, operand);
			}
		}
	}
	
	return NULL;
}

template <class S, class T>
PData *PDataContainer::FindOperate(const string &name, TypedPData<S> *a, T b)
{
	if (name=="+") return AddOperator::Operate<S,T>(a,b);
	else if (name=="*") return MultOperator::Operate<S,T>(a,b);
	else if (name=="closest") return ClosestOperator::Operate<S,T>(a,b);
	else if (name=="sin") return SineOperator::Operate<S,T>(a,b);
	else if (name=="cos") return CosineOperator::Operate<S,T>(a,b);
	
	cerr<<"operator "<<name<<" not found"<<endl;
	return NULL;
}

};

#endif
