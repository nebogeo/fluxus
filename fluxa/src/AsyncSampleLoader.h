// Copyright (C) 2004 David Griffiths <dave@pawfal.org>
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

#include <string>
#include <pthread.h>
#include <deque>
#include <map>
#include "Types.h"
#include "Sample.h"

using namespace std;

#ifndef SAMPLELOADER_CLOCK
#define SAMPLELOADER_CLOCK

namespace spiralcore
{

// a sample loader that does it's loading in another thread. should be suitable
// for realtime use. caches samples (forever) and seems to work, but needs a 
// little fixing up to be safer
class AsyncSampleLoader
{
public:
	static AsyncSampleLoader* Get();
	static void Shutdown();
	
	// this sample will be filled later. *should* be ok to play it, as
	// it will be allocated on one chunk, and the size updated after
	// might get some noise though - and you probably shouldnt - need to
	// have some info on whether it's loaded or not :)
	// ownership of the sample remains in control of this class - do not
	// delete!
	Sample *AddToQueue(const string &Filename);
	// batches em up to save time
	void LoadQueue();
	
private:
	AsyncSampleLoader();
	~AsyncSampleLoader();
	
	static void LoadLoop();

	pthread_t  m_LoaderThread;
	static pthread_mutex_t* m_Mutex;
	
	struct LoadItem
	{
		string Name;
		Sample *SamplePtr;
	};
	
	static map<string,Sample*> m_Cache;
	
	// two loaderstacks, so we can get a lock on at least one of them at any time
	static deque<LoadItem> m_LoadQueue;
	static AsyncSampleLoader *m_Singleton;
};

}

#endif



