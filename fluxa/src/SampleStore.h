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

#include <map>
#include "Sample.h"

using namespace spiralcore;
using namespace std;

class SampleStore
{
public:
	static SampleStore* Get()
	{
		if (m_Singleton==NULL) m_Singleton=new SampleStore;
		return m_Singleton;
	}

	void AddToQueue(SampleID ID, const string &Filename);
	void LoadQueue();
	void Unload(SampleID ID);
	void UnloadAll();

	Sample* GetSample(SampleID ID);

private:
	SampleStore();
	~SampleStore();

 	map<SampleID,Sample*> m_SampleMap;
 	int m_NextSampleID;
	
	static SampleStore *m_Singleton;
};
