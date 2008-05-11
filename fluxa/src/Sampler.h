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

#include <map>
#include <string>
#include <vector>
#include "Types.h"
#include "Event.h"
#include "Sample.h"
#include "Trace.h"

#ifndef NE_SAMPLER
#define NE_SAMPLER

using namespace spiralcore;
using namespace std;

struct OutBuffer
{
	Sample Left;
	Sample Right;
};

class Sampler
{
public:
	Sampler(unsigned int samplerate);
	virtual ~Sampler();

	EventID Play(float timeoffset, const Event &Channel);	
	virtual void Process(uint32 BufSize, Sample &left, Sample &right);
	
	void SetPoly(bool s) { m_Poly=s; }
	void SetReverse(bool s) { m_Reverse=s; }
	
private:
	unsigned int m_SampleRate;
	
	bool m_Poly;
	bool m_Reverse;
	float m_StartTime;
	Event m_Globals;
	EventID m_PlayingOn;
	
 	map<EventID,Event> m_ChannelMap;
 	int m_NextEventID;
};

#endif

