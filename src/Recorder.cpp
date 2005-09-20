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

#include <sys/time.h>
#include <iostream>
#include <fstream>
#include "Recorder.h"

using namespace fluxus;

EventRecorder::EventRecorder() :
m_Mode(OFF),
m_LastTimeSeconds(0),
m_TimeSeconds(0)
{
}

EventRecorder::~EventRecorder()
{
}

bool EventRecorder::Get(vector<Event> &events)
{
	bool found=false;
	for (vector<Event>::iterator i=m_EventVec.begin(); i!=m_EventVec.end(); i++)
	{
		if (i->Time>m_LastTimeSeconds && i->Time<=m_TimeSeconds)
		{
			events.push_back(*i);
			found=true;
		}		
	}
	return found;
}

void EventRecorder::Record(Event &event)
{
	event.Time=m_TimeSeconds;
	m_EventVec.push_back(event);
}

void EventRecorder::Reset()
{
	m_EventVec.clear();
}

void EventRecorder::ResetClock()
{
	m_TimeSeconds=0;
	m_LastTimeSeconds=0;
}

void EventRecorder::UpdateClock()
{
	double Delta;
	timeval ThisTime;
	gettimeofday(&ThisTime,NULL);
	Delta=(ThisTime.tv_sec-m_LastTime.tv_sec)+
		  (ThisTime.tv_usec-m_LastTime.tv_usec)*0.000001f;
	m_LastTime=ThisTime;
	IncClock(Delta);
}

void EventRecorder::IncClock(double delta)
{
	m_LastTimeSeconds=m_TimeSeconds;
	if (delta>0 && delta<1000) m_TimeSeconds+=delta;
	//cerr<<m_TimeSeconds<<" "<<delta<<endl;
}

void EventRecorder::Save(const string &filename)
{
	ofstream os(filename.c_str());
	
	for (vector<Event>::iterator i=m_EventVec.begin(); i!=m_EventVec.end(); i++)
	{
		os<<i->Time<<" "<<i->Key<<" "<<i->Button<<" "<<i->Special<<" "<<i->State<<" "<<i->X<<" "<<i->Y<<endl;
	}
	
	os.close();		
}

void EventRecorder::Load(const string &filename)
{
	ifstream is(filename.c_str());
	
	while(!is.eof())
	{
		Event event;
		is>>event.Time;
		is>>event.Key;
		is>>event.Button;
		is>>event.Special;
		is>>event.State;
		is>>event.X;
		is>>event.Y;
		
		m_EventVec.push_back(event);
	}
}
