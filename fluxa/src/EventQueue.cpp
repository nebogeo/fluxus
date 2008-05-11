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

#include "EventQueue.h"

using namespace spiralcore;

EventQueue::EventQueue()
{
}

EventQueue::~EventQueue()	
{
}

bool EventQueue::Add(const Event &e)
{
	for (int i=0; i<EVENT_QUEUE_SIZE; i++)
	{
		if (m_Queue[i].m_IsEmpty)
		{
			m_Queue[i].m_Event=e;
			m_Queue[i].m_IsEmpty=false;
			return true;
		}
	}
	
	return false;
}

bool EventQueue::Get(Time from, Time till, Event &e)
{
	for (int i=0; i<EVENT_QUEUE_SIZE; i++)
	{
		if (!m_Queue[i].m_IsEmpty && m_Queue[i].m_Event.TimeStamp>from &&
		     m_Queue[i].m_Event.TimeStamp<till)
		{
			e=m_Queue[i].m_Event;      // return this one
			m_Queue[i].m_IsEmpty=true; // delete from the queue
			return true;
		}
	}
	return false;
}

