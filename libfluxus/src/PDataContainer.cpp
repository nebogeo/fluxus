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

#include "PDataContainer.h"

using namespace fluxus;

bool PDataContainer::GetDataInfo(const string &name, char &type, unsigned int &size)
{
	map<string,PData*>::iterator i=m_PData.find(name);
	if (i==m_PData.end())
	{
		return false;
	}
	
	size=i->second->Size();
	
	TypedPData<dVector> *data = dynamic_cast<TypedPData<dVector>*>(i->second);	
	if (data) type='v';
	else
	{
		TypedPData<dColour> *data = dynamic_cast<TypedPData<dColour>*>(i->second);
		if (data) type='c';
		else 
		{
			TypedPData<float> *data = dynamic_cast<TypedPData<float>*>(i->second);
			if (data) type='f';
			else 
			{
				TypedPData<dMatrix> *data = dynamic_cast<TypedPData<dMatrix>*>(i->second);
				if (data) type='m';
			}
		}
	}
		
	return true;
}
	
void PDataContainer::AddData(const string &name, PData* pd)
{
	map<string,PData*>::iterator i=m_PData.find(name);
	if (i!=m_PData.end())
	{
		cerr<<"Primitive::AddData: pdata: "<<name<<" already exists"<<endl;
		return;
	}
	
	m_PData[name]=pd;
}

void PDataContainer::CopyData(const string &name, string newname)
{
	//map<string,PData*>::iterator i=m_PData.find(newname);
	//if (i!=m_PData.end())
	//{
		
	//}
	
	map<string,PData*>::iterator i=m_PData.find(name);
	if (i==m_PData.end())
	{
		cerr<<"Primitive::CopyData: pdata: "<<name<<" doesn't exists"<<endl;
		return;
	}
	
	m_PData[newname]=i->second->Copy();
}

void PDataContainer::RemoveDataVec(const string &name)
{
	map<string,PData*>::iterator i=m_PData.find(name);
	if (i==m_PData.end())
	{
		cerr<<"Primitive::RemovePDataVec: pdata: "<<name<<" doesn't exist"<<endl;
		return;
	}
	
	delete i->second;
	m_PData.erase(i);
}

PData* PDataContainer::GetDataRaw(const string &name)
{
	map<string,PData*>::iterator i=m_PData.find(name);
	if (i==m_PData.end())
	{
		cerr<<"Primitive::GetDataRaw: pdata: "<<name<<" doesn't exist"<<endl;
		return NULL;
	}
	
	return i->second;
}
