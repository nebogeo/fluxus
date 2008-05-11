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

#include <stdio.h>
#include <math.h>
#include <algorithm>
#include "Sampler.h"
#include "SampleStore.h"
#include "AsyncSampleLoader.h"

static const unsigned int SAFETY_MAX_CHANNELS=30;

Sampler::Sampler(unsigned int samplerate) :
m_SampleRate(samplerate),
m_Poly(true),
m_Reverse(false),
m_StartTime(0),
m_NextEventID(1)
{
}

Sampler::~Sampler()
{
}

EventID Sampler::Play(float timeoffset, const Event &event)
{
	Sample* sample = SampleStore::Get()->GetSample(event.ID);
	if (sample!=NULL)
	{
		Event Copy = event;
		if (Copy.Frequency==0)
		{
			cerr<<"Cancelling zero speed sample"<<endl;
			return 0;
		}
		
		// start playing from first non-zero sample
		/*if (0)//m_AutoCue)
		{
			float s=1;
			int c=0;
			while (s==0 && c<sample->GetLength())
			{
				s=(*sample)[c++];
			}
			Copy.Position=c;  
		}*/
		
		while (m_ChannelMap.size()>SAFETY_MAX_CHANNELS)
		{	
			Trace(RED,BLACK,"channels exceeded %d, culling!",SAFETY_MAX_CHANNELS);
			m_ChannelMap.erase(m_ChannelMap.begin());
		}


		Copy.Position+=((m_StartTime+timeoffset)*(float)m_SampleRate)*(Copy.Frequency/440.0)*
			(m_Globals.Frequency/440.0);
		m_ChannelMap[m_NextEventID++]=Copy;
		
		// if poly mode is turned off, remove the last playing sample
		if (!m_Poly)
		{
			map<EventID,Event>::iterator i=m_ChannelMap.find(m_PlayingOn);
			while(i!=m_ChannelMap.end())
			{
	       		m_ChannelMap.erase(i);
				i=m_ChannelMap.find(m_PlayingOn);
			}
		}
		
		m_PlayingOn=m_NextEventID-1;
		return m_NextEventID-1;
	}
	else
	{
		cerr<<"Could not find sample "<<event.ID<<" to play"<<endl;
	}

	return 0;
}

void Sampler::Process(uint32 BufSize, Sample &left, Sample &right)
{
	static uint32 highwater=0;
	if (m_ChannelMap.size()>highwater)
	{
		highwater=m_ChannelMap.size();
		//cerr<<"Channels highwater mark now at : "<<highwater<<endl;
	}

	map<EventID,Event>::iterator nexti=m_ChannelMap.begin();
	for (map<EventID,Event>::iterator i=m_ChannelMap.begin();
	       i!=m_ChannelMap.end();)
	{
		nexti++; // used so we can delete the current channel
		Event *ch = &i->second;
		Sample *sample = SampleStore::Get()->GetSample(ch->ID);
		// check we still have the sample
		if (sample != NULL)
		{			
			float Volume = ch->Volume*m_Globals.Volume*0.3;
			float Speed =  (ch->Frequency/440.0)*(m_Globals.Frequency/440.0);
			
			float Pan = 0;
			
			if (m_Globals.Pan!=0) Pan = (ch->Pan+m_Globals.Pan)/2.0f; // average
			else Pan = ch->Pan; // just channel pan
				
			Pan = 0.5f+Pan/2.0f; // 0 -> 1
			float Left = Pan;		
			float Right = 1-Pan;
					
			float rev = 0;
			if (m_Reverse) rev = sample->GetLength();
					
			for (uint32 n=0; n<BufSize; n++)
			{	
				if (ch->Position<sample->GetLength() && ch->Position>=0)
				{
					left.Set(n,left[(int)n]+(*sample)[(float)fabs(ch->Position-rev)]*Volume*Left);
					right.Set(n,right[(int)n]+(*sample)[(float)fabs(ch->Position-rev)]*Volume*Right);
				}
				
				ch->Position+=Speed;
							
				if (ch->Position>=sample->GetLength())
				{
					m_ChannelMap.erase(i);
					break;
				}
			}
		}
		else // sample deleted, so free the channel
		{
			m_ChannelMap.erase(i);
		}
		i=nexti;
	}
}

