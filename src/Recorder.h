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

#ifndef FLUXUS_RECORDER
#define FLUXUS_RECORDER

#include <sys/time.h>
#include <vector>

using namespace std;

namespace fluxus
{

class RecorderMessage
{
public:
	RecorderMessage() : Time(0){}
	
	string Name;
	double Time;
	int Data;
};

class EventRecorder
{
public:
	EventRecorder();
	~EventRecorder();
	
	enum Mode{OFF,RECORD,PLAYBACK};

	void SetMode(Mode mode) { m_Mode=mode; }
	Mode GetMode() { return m_Mode; }
	
	bool Get(vector<RecorderMessage*> &events);
	void Record(RecorderMessage *event);

	void Reset();
	void ResetClock();
	void UpdateClock();
	void IncClock(double delta);
	
	void Save(const string &filename);
	void Load(const string &filename);
	
private:
	
	Mode m_Mode;
	timeval m_LastTime;
	double m_LastTimeSeconds;
	double m_TimeSeconds;
	
	vector<RecorderMessage*> m_EventVec;
};

}

#endif

