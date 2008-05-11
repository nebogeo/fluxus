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

#include "Event.h"

#ifndef SPIRALCORE_EVENT_QUEUE
#define SPIRALCORE_EVENT_QUEUE

static const int EVENT_QUEUE_SIZE = 256;

namespace spiralcore
{

// no mallocs, so a bit of diy memory allocation

class EventQueue
{
public:
	EventQueue();
	~EventQueue();
	
	bool Add(const Event &e);
	
	// you should keep calling this function for the specified
	// time slice until it returns false
	bool Get(Time from, Time till, Event &e);
	
private:

	struct QueueItem
	{
		QueueItem() : m_IsEmpty(true) {}
		bool m_IsEmpty;
		Event m_Event;
	};

	QueueItem m_Queue[EVENT_QUEUE_SIZE]; 
};

}

#endif
