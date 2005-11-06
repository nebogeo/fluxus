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

namespace fluxus
{

class PData
{
public:
	PData() {}
	virtual ~PData() {}
	virtual PData *Copy()=0;
	virtual unsigned int Size()=0;
};

template<class T>
class TypedPData : public PData
{
public:
	TypedPData() {}
	virtual ~TypedPData() {}
	virtual PData *Copy()
	{
		TypedPData<T> *newdata = new TypedPData<T>;
		newdata->m_Data=m_Data;
		return newdata;
	}
	
	virtual unsigned int Size()
	{
		return m_Data.size();
	}

	vector<T> m_Data;
};


}

#endif

