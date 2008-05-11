// Copyright (C) 2008 David Griffiths <dave@pawfal.org>
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

#include "SampleStore.h"
#include "AsyncSampleLoader.h"

SampleStore *SampleStore::m_Singleton=NULL;

SampleStore::SampleStore()
{
}

SampleStore::~SampleStore()
{
}

void SampleStore::AddToQueue(SampleID ID, const string &Filename)
{
	//if (m_SampleMap.find(ID)!=m_SampleMap.end()) return;
	m_SampleMap[ID]=AsyncSampleLoader::Get()->AddToQueue(Filename);
}

void SampleStore::LoadQueue()
{
	AsyncSampleLoader::Get()->LoadQueue();
}
	
void SampleStore::Unload(SampleID ID)
{
	map<SampleID,Sample*>::iterator i = m_SampleMap.find(ID);
	if (i!=m_SampleMap.end())
	{
		m_SampleMap.erase(i);
	}
	//else
	//{
	//	cerr<<"Could not find sample "<<ID<<" to unload"<<endl;
	//}
}

void SampleStore::UnloadAll()
{
	m_SampleMap.clear();
	m_NextSampleID=1;	
}	

Sample *SampleStore::GetSample(SampleID id)
{
	map<SampleID,Sample*>::iterator i = m_SampleMap.find(id);
	if (i!=m_SampleMap.end())
	{
		return i->second;
	}
	
	return NULL;
}
