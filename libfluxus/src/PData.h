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

#include <vector>
#include <string>
#include "dada.h"

#ifndef N_PDATA
#define N_PDATA

using namespace std;

namespace Fluxus
{

/////////////////////////////////////////////////
/// The base pdata array class
class PData
{
public:
	PData() {}
	virtual ~PData() {}
	virtual PData *Copy() const=0;
	virtual unsigned int Size() const=0;
	virtual void Resize(unsigned int size)=0;
	
	char GetType() const { return m_Type; }
	
protected:
	void SetType(const char s) { m_Type=s; }
	
private:
	char m_Type;
};

/////////////////////////////////////////////////
/// The templated pdata array class
template<class T>
class TypedPData : public PData
{
public:
	TypedPData() {}	
	TypedPData(T first) { m_Data.push_back(first); }	
	TypedPData(unsigned int size) { Resize(size); }	
	TypedPData(vector<T> s) : m_Data(s) {}
	virtual ~TypedPData() {}
	
	virtual PData *Copy() const
	{
		TypedPData<T> *newdata = new TypedPData<T>;
		newdata->m_Data=m_Data;
		return newdata;
	}
	
	virtual unsigned int Size() const
	{
		return m_Data.size();
	}

	virtual void Resize(unsigned int size)
	{
		m_Data.resize(size);
	}
	
	///\todo add operator[] and make m_Data private
	vector<T> m_Data;
};

}

#endif

