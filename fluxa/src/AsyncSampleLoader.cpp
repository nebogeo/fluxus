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

#include <unistd.h>
#include <sndfile.h>
#include "AsyncSampleLoader.h"
#include "SearchPaths.h"

using namespace spiralcore;

AsyncSampleLoader *AsyncSampleLoader::m_Singleton=NULL;
deque<AsyncSampleLoader::LoadItem> AsyncSampleLoader::m_LoadQueue;
pthread_mutex_t* AsyncSampleLoader::m_Mutex;
map<string,Sample*> AsyncSampleLoader::m_Cache;

short *LoadWav(FILE *file, unsigned int &size, unsigned short &channels);

AsyncSampleLoader* AsyncSampleLoader::Get()
{
	if (!m_Singleton) m_Singleton = new AsyncSampleLoader();
	return m_Singleton;
}

void AsyncSampleLoader::Shutdown()
{
	if (m_Singleton) delete m_Singleton;
}

AsyncSampleLoader::AsyncSampleLoader()
{
	m_Mutex = new pthread_mutex_t;
	pthread_mutex_init(m_Mutex,NULL);
}

AsyncSampleLoader::~AsyncSampleLoader()
{
	for (map<string,Sample*>::iterator i=m_Cache.begin(); i!=m_Cache.end(); i++)
	{
		cerr<<"deleting cache "<<i->first<<endl;
		delete i->second;
	}
	
	Shutdown();
}

Sample *AsyncSampleLoader::AddToQueue(const string &Filename)
{
	map<string,Sample*>::iterator i=m_Cache.find(Filename);
	if (i!=m_Cache.end())
	{
		return i->second; // already loaded this one
	}
	
	LoadItem NewItem;
	NewItem.Name=Filename;
	NewItem.SamplePtr=new Sample;
	
	// add to the cache
	m_Cache[Filename]=NewItem.SamplePtr;

	// spinlock
	for (int n=0; n<5; n++) // why?
	{
		if (pthread_mutex_trylock(m_Mutex))
		{
			m_LoadQueue.push_back(NewItem);
			pthread_mutex_unlock(m_Mutex);
			return NewItem.SamplePtr;
		}
	}
	cerr<<"Could not get a lock on the loaderqueue, not loading ["<<Filename<<"]"<<endl;
	return NewItem.SamplePtr;
}

void AsyncSampleLoader::LoadQueue()
{
	if (pthread_mutex_trylock(m_Mutex))
	{
		if (m_LoadQueue.size()>0)
		{
			pthread_create(&m_LoaderThread,NULL,(void*(*)(void*))LoadLoop,NULL);
		}
		pthread_mutex_unlock(m_Mutex);
	}
	else
	{
		//cerr<<"Could not get a lock on the loaderqueue from LoadQueue()"<<endl;
	}
}

/*
void AsyncSampleLoader::LoadLoop()
{		
	pthread_mutex_lock(m_Mutex);
	while (m_LoadQueue.size())
	{
		LoadItem Item = *m_LoadQueue.begin();
		m_LoadQueue.pop_front();
		pthread_mutex_unlock(m_Mutex);
			
		SF_INFO info;
		info.format=0;
		string filename=SearchPaths::Get()->GetFullPath(Item.Name);

		cerr<<"async loading: "<<filename<<endl;
		
		FILE* file = fopen (filename.c_str(), "rb") ;
		if (!file)
		{
			cerr<<"Error opening ["<<Item.Name<<"]"<<endl;
		}
		else
		{
			unsigned short channels=0;
			unsigned int size=0;
			short *data = LoadWav(file,size,channels);
			size/=2; // bytes -> samples
			
			if (data)
			{
				unsigned int samples=size/channels;
				Item.SamplePtr->Allocate(samples);
				
				// mix down to mono if need be
				if (channels>1)
				{
					int from=0;
					for (unsigned int n=0; n<samples; n++)
					{
						for (int c=0; c<channels; c++)
						{
							//cerr<<n<<endl; // 60414
							Item.SamplePtr->Set(n,((*Item.SamplePtr)[n]+(data[from++]/32767.0f))/(float)channels);
						}
					}
				}
				else
				{
					for (unsigned int n=0; n<size; n++)
					{
						Item.SamplePtr->Set(n,data[n]/32767.0f);
					}
				}
				delete[] data;
			}
			fclose(file);
		}
		sleep(1);
	}		
	pthread_mutex_unlock(m_Mutex);
}
*/

void AsyncSampleLoader::LoadLoop()
{		
	pthread_mutex_lock(m_Mutex);
	while (m_LoadQueue.size())
	{
		LoadItem Item = *m_LoadQueue.begin();
		m_LoadQueue.pop_front();
			
		SF_INFO info;
		info.format=0;
		string filename=SearchPaths::Get()->GetFullPath(Item.Name);

		cerr<<"async loading: "<<filename<<endl;
		
		SNDFILE* file = sf_open (filename.c_str(), SFM_READ, &info) ;
		if (!file)
		{
			cerr<<"Error opening ["<<Item.Name<<"] : "<<sf_strerror (file)<<endl;
		}
		Item.SamplePtr->Allocate(info.frames);
			
		// mix down to mono if need be
		if (info.channels>1)
		{
			float *Buffer = new float[info.frames*info.channels];
		
			sf_readf_float(file,Buffer,info.frames*info.channels);
			int from=0;
			for (unsigned int n=0; n<info.frames; n++)
			{
				for (int c=0; c<info.channels; c++)
				{
					Item.SamplePtr->Set(n,((*Item.SamplePtr)[n]+Buffer[from++])/2.0f);
				}
			}
		}
		else
		{
			sf_readf_float(file, Item.SamplePtr->GetNonConstBuffer(), info.frames);	
		}
		sf_close(file);
		
		sleep(1);
        }		
	pthread_mutex_unlock(m_Mutex);
}

/*
// having problems with libsndfile crashing in this thread. (nm/eb.wav)
// need to look into it more, but need this working for a gig
// so writing a quick dirty wav loader here 
short *LoadWav(FILE *file, unsigned int &size, unsigned short &channels)
{
	char id[5];
	id[4]='\0';
	fread(id,1,4,file);
	if (strcmp(id,"RIFF")!=0) 
	{
		cerr<<"WAV format error (RIFF): "<<id<<endl;
		return NULL;
	}
	fread(&size,1,4,file);
	fread(id,1,4,file);
	if (strcmp(id,"WAVE")!=0)
	{
		cerr<<"WAV format error (WAVE): "<<id<<endl;
		return NULL;
	}
	fread(id,1,4,file);
	if (strcmp(id,"fmt ")!=0)
	{
		cerr<<"WAV format error (fmt ): "<<id<<endl;
		return NULL;
	}
	fread(&size,1,4,file);
	unsigned int datastart=size+ftell(file);
	short compression;
	fread(&compression,1,2,file);
	if (compression!=1)
	{
		cerr<<"WAV data is compressed"<<endl;
		return NULL;
	}
	fread(&channels,1,2,file);
	if (!(channels==1 || channels==2))
	{
		cerr<<"WAV data is not mono or stereo"<<endl;
		return NULL;
	}
	fseek(file,datastart,SEEK_SET);		
	fread(id,1,4,file);
	if (strcmp(id,"data")!=0)
	{
		cerr<<"WAV format error (data): "<<id<<endl;
		return NULL;
	}
	fread(&size,1,4,file);
	char *data=new char[size];
	fread(data,1,size,file);
	return (short*)data;
};

*/
