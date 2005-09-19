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
#include <vector>

using namespace std;

namespace fluxus
{

class Event
{
public:
	Event(unsigned char key, int button, int special, int state, int x, int y) :
	Time(0),
	Key(key),
	Button(button),
	Special(special),
	State(state),
	X(x),
	Y(y)
	{}
	
	Event() {}
	
	double Time;
	unsigned char Key; 
	int Button;
	int Special;
	int State;
	int X;
	int Y;
};

class EventRecorder
{
public:
	EventRecorder();
	~EventRecorder();
	
	enum Mode{OFF,RECORD,PLAYBACK};

	void SetMode(Mode mode) { m_Mode=mode; }
	
	// if update returns true, there may be further events, call it repeatedly until it returns false
	bool Update(unsigned char &key, int &button, int &special, int &state, int &x, int &y);	
	void Reset();
	void ResetClock();
	void UpdateClock();
	void IncClock(double delta);
	
	void Save(const string &filename);
	void Load(const string &filename);
	
private:
	bool Get(Event &event);
	void Record(Event &event);
	
	Mode m_Mode;
	timeval m_LastTime;
	double m_LastTimeSeconds;
	double m_TimeSeconds;
	
	vector<Event> m_EventVec;
};

}
