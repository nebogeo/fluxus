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

#include "Types.h"
#include "Time.h"
#include "EventQueue.h"
#include "Trace.h"
#include "OSCServer.h"
#include "Sampler.h"
#include "Graph.h"
#include "JackClient.h"

#ifndef FLEEP
#define FLEEP

static const int EVENT_BUFFER_SIZE=1024;

class Fluxa
{
public:
	Fluxa(OSCServer *server, JackClient* jack, const string &leftport, const string &rightport);
	~Fluxa() {}
	
private:
	static void Run(void *RunContext, unsigned int BufSize);
	void Process(unsigned int BufSize);
	void ProcessCommands();
	
	unsigned int m_SampleRate;
		
	Graph m_Graph;
	Sampler m_Sampler;
	Sample m_LeftBuffer;
	Sample m_RightBuffer;
	int    m_LeftJack;
	int    m_RightJack;
	bool 	m_Running;
	Time	m_CurrentTime;
	OSCServer *m_Server;
	EventQueue m_EventQueue;
	float m_GlobalVolume;
	float m_Pan;
	bool m_Debug;
	
	Eq m_LeftEq;
    Eq m_RightEq;
	Compressor m_Comp;
};

#endif
