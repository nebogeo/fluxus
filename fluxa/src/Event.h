// Copyright (C) 2003 David Griffiths <dave@pawfal.org>
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

#include "Types.h"
#include "Time.h"

#ifndef NE_EVENT
#define NE_EVENT

namespace spiralcore
{

class Event
{
public:
	Event() :
	ID(0),
	Frequency(440.0f),
	SlideFrequency(0.0f),
	Volume(1.0f),
	Pan(0.0f),
	Position(0),
	Channel(0),
	NoteNum(0),
	Message(0)
	{}
	
	int ID;            // the currently playing sample, or voice
	float32 Frequency; // freq
	float32 SlideFrequency; // slide dest freq
	float32 Volume;    // or velocity
	float32 Pan;       // stereo pan
	float32 Position;  // sample position start->end 0->1
	int Channel;       // output channel for this event
	int NoteNum;
	char Message;      // used for charscore message passing
	Time TimeStamp;    // when to do this event
};

}

#endif
