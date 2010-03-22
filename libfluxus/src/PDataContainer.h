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

#ifndef PDATA_CONTAINER
#define PDATA_CONTAINER

#include <map>
#include "PData.h"
#include "PDataOperator.h"
#include "PDataArithmetic.h"

using namespace std;

namespace Fluxus
{

///////////////////////////////////////////////////
/// The base pdata container class. Primitive data (pdata)
/// means vertex data, colours, positions, etc etc which are
/// not generated internally and need to be accessed by 
/// users. All primitive data should be stored using pdata 
/// arrays, with the following caviats: 
/// - The number of elements is global to all PData 
/// arrays in a primitive.
/// - Only supported types are used.
/// All primitive data stored in this way can be accessed 
/// by this interface, the primitive need not expose it 
/// itself at all - and we can use one common interface
/// for all access.
class PDataContainer
{
public:
	PDataContainer();
	PDataContainer(const PDataContainer &other);
	virtual ~PDataContainer();
	virtual PDataContainer *Clone() const=0;

	/// Clear all pdata, leaves the container empty
	void Clear();
	
	/// Make a new pdata array, fails if one already exists 
	/// with this name
	void AddData(const string &name, PData* pd);
	
	/// Copy data from one array to another - deletes the 
	/// old one if it exists
	void CopyData(const string &name, string newname);
	
	/// Retrieves a pointer to the internal vector by name
	/// Returns NULL if it doesn't exist, or is not the 
	/// type given in the template call.
	template<class T> vector<T>* GetDataVec(const string &name);      
	
	/// Destroys a pdata array
	void RemoveDataVec(const string &name);
	
	/// From the supplied name, fills in information about this pdata,
	/// returns false if it doesn't actually exist
	bool GetDataInfo(const string &name, char &type, unsigned int &size) const;
	
	/// Sets an element of the array. Not checked, for 
	/// speed - use GetDataInfo() to check
	template<class T> void SetData(const string &name, unsigned int index, T s);
	
	/// Gets an element of the array. Not checked, for 
	/// speed - use GetDataInfo() to check
	template<class T> T GetData(const string &name, unsigned int index) const;
		
	/// Runs a pdata operation on the given pdata array
	template<class T> PData *DataOp(const string &op, const string &name, T operand);
	
	/// Gets the whole pdata array, returns NULL if it doesn't exist
	PData* GetDataRaw(const string &name);

	/// Gets the whole const pdata array, returns NULL if it doesn't exist
	const PData* GetDataRawConst(const string &name) const;
	
	/// Sets the whole pdata array
	void SetDataRaw(const string &name, PData* pd);
	
	/// Maps the name of a pdata operator to the actual object, all pdata ops
	/// need to be registered inside this function (see below)
	template <class S, class T> PData *FindOperate(const string &name, TypedPData<S> *a, T b);
	
	/// Erases all current data!
	void Resize(unsigned int size);
	
	/// Returns the size of pdata for this object
	unsigned int Size() const;

	/// Returns a vector of names of PData that this container contains
	void GetDataNames(vector<string> &names) const;

protected:

	/// Called when a named pdata mapping changes 
	virtual void PDataDirty()=0;
	
	///Todo: no const [] for m_PData[name] so m_PData has to be mutable??? (see below)
	///\todo replace with a hashmap?
	mutable map<string,PData*> m_PData;

};

template<class T> 
void PDataContainer::SetData(const string &name, unsigned int index, T s)	
{
	static_cast<TypedPData<T>*>(m_PData[name])->m_Data[index]=s;
}

///Todo: no const [] for m_PData[name] so m_PData has to be mutable???
template<class T> 
T PDataContainer::GetData(const string &name, unsigned int index) const
{
	return static_cast<TypedPData<T>*>(m_PData[name])->m_Data[index];
}

template<class T>
vector<T>* PDataContainer::GetDataVec(const string &name)
{
	map<string,PData*>::iterator i=m_PData.find(name);
	if (i==m_PData.end())
	{
		Trace::Stream<<"Primitive::GetPDataVec: pdata: "<<name<<" doesn't exists"<<endl;
		return NULL;
	}
	
	TypedPData<T> *ptr=dynamic_cast<TypedPData<T> *>(i->second);
	if (!ptr) 
	{
		Trace::Stream<<"Primitive::GetPDataVec: pdata: "<<name<<" is not of type: "<<typeid(TypedPData<T>).name()<<endl;
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
		Trace::Stream<<"Primitive::DataOp: pdata: "<<name<<" doesn't exists"<<endl;
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
	
	Trace::Stream<<"operator "<<name<<" not found"<<endl;
	return NULL;
}

};

#endif
