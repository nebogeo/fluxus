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

#include <limits.h>
#include "SearchPaths.h"
#include "Fluxa.h"
#include "SampleStore.h"
#include "Modules.h"

using namespace spiralcore;

Fluxa::Fluxa(OSCServer *server, JackClient* jack, const string &leftport, const string &rightport) :
m_SampleRate(jack->GetSamplerate()),
m_Graph(70,jack->GetSamplerate()),
m_Sampler(jack->GetSamplerate()),
m_Running(false),
m_Server(server),
m_GlobalVolume(1.0f),
m_Pan(0.0f),
m_Debug(false),
m_Eq(jack->GetSamplerate()),
m_Comp(jack->GetSamplerate())
{		
	WaveTable::WriteWaves();
 	jack->SetCallback(Run,(void*)this);

	//PortAudioClient* Audio=PortAudioClient::Get();
	//Audio->SetCallback(Run,(void*)this);
	//PortAudioClient::DeviceOptions Options;
	//Options.Samplerate=m_SampleRate;
	//Options.NumBuffers=2;
	//Options.BufferSize=512;
	//Audio->Attach("Fluxa",Options);	
	
	m_LeftBuffer.Allocate(1024);
	m_RightBuffer.Allocate(1024);
	m_LeftBuffer.Zero();
	m_RightBuffer.Zero();
	
	if (jack->IsAttached())
	{	
		//Audio->SetOutputs(m_LeftBuffer.GetNonConstBuffer(),m_RightBuffer.GetNonConstBuffer());
		
		m_LeftJack = jack->AddOutputPort();
 		jack->SetOutputBuf(m_LeftJack, m_LeftBuffer.GetNonConstBuffer());
 	    jack->ConnectOutput(m_LeftJack,leftport);
  	    m_RightJack = jack->AddOutputPort();
 		jack->SetOutputBuf(m_RightJack, m_RightBuffer.GetNonConstBuffer());
  	    jack->ConnectOutput(m_RightJack,rightport); 	
 		m_Running=true;
	}
	//Sample::SetAllocator(new RealtimeAllocator(1024*1024*40));
	
	Time Now;
	Now.SetToNow();
	m_CurrentTime.Seconds=Now.Seconds;
	m_CurrentTime.Fraction=Now.Fraction;
	
	cerr<<"fluxa server ready... "<<endl;
}

void Fluxa::Run(void *RunContext, unsigned int BufSize)
{ 
	((Fluxa*)RunContext)->ProcessCommands();
	((Fluxa*)RunContext)->Process(BufSize);
}

void Fluxa::ProcessCommands()
{
	CommandRingBuffer::Command cmd;
	while (m_Server->Get(cmd))
	{
		string name = cmd.Name;
		//cerr<<name<<endl;		
		
		if (name=="/setclock")	
		{ 
			// baddddd :P
			Time Now;
			Now.SetToNow();
			m_CurrentTime.Seconds=Now.Seconds;
			m_CurrentTime.Fraction=Now.Fraction;		
		}
		else if (name=="/create")	
		{ 		
			unsigned int pos=0;
			while (pos<cmd.Size())
			{
				if (cmd.Size()<pos+1) 
				{
					cerr<<"/create - malformed arguments..."<<endl;
				}
				else
				{
					Graph::Type type=(Graph::Type)cmd.GetInt(pos+1);
				
					if (type==Graph::TERMINAL) 
					{
						m_Graph.Create(cmd.GetInt(pos),type,cmd.GetFloat(pos+2));
						pos+=3;
					}
					else
					{
						m_Graph.Create(cmd.GetInt(pos),type,0);
						pos+=2;
					}
				}
			}
		}
		else if (name=="/connect")	
		{ 		
			for (unsigned int n=0; n<cmd.Size(); n+=3)
			{
				if (cmd.Size()<n+1) 
				{
					cerr<<"/connect - malformed arguments..."<<endl;
				}
				else
				{
					m_Graph.Connect(cmd.GetInt(n),cmd.GetInt(n+1),cmd.GetInt(n+2));
				}
			}
		}
		else if (name=="/play")	
		{
			Event e;
			e.TimeStamp.Seconds=(unsigned int)cmd.GetInt(0);
			e.TimeStamp.Fraction=(unsigned int)cmd.GetInt(1);
			e.ID=cmd.GetInt(2);
			
			if (e.TimeStamp.Seconds==0 && e.TimeStamp.Fraction==0)
			{
				e.TimeStamp=m_CurrentTime;
				e.TimeStamp+=0.1;
			}
			if (e.TimeStamp>=m_CurrentTime) 
			{
				m_EventQueue.Add(e);

				if (e.TimeStamp.GetDifference(m_CurrentTime)>30)
				{
					Trace(RED,YELLOW,"Reset clock? Event far in future %f seconds",e.TimeStamp.GetDifference(m_CurrentTime));
				} 			
			}
			else 
			{
				Trace(RED,YELLOW,"Event arrived too late [%f secs], playing now anyway!",m_CurrentTime.GetDifference(e.TimeStamp));
				e.TimeStamp=m_CurrentTime;
				e.TimeStamp+=0.1;
				m_EventQueue.Add(e);
			}
			
			if (m_Debug)
			{
				Trace(RED,YELLOW,"/play received ID=%d secs=%d frac=%d",e.ID,(unsigned int)e.TimeStamp.Seconds,
																			 (unsigned int)e.TimeStamp.Fraction);
			}
		}
		else if (name=="/maxsynths")	
		{ 		
			m_Graph.SetMaxPlaying(cmd.GetInt(0));
		}
		else if (name=="/reset")	
		{ 		
			m_Graph.Clear();
			m_Graph.Init();
		}
		else if (name=="/globalvolume")	
		{ 		
			m_GlobalVolume=cmd.GetFloat(0);
		}
		else if (name=="/pan")	
		{ 		
			m_Pan=cmd.GetFloat(0);
		}
		else if (name=="/eq")	
		{ 		
			m_Eq.SetLow(cmd.GetFloat(0));
			m_Eq.SetMid(cmd.GetFloat(1));
			m_Eq.SetHigh(cmd.GetFloat(2));
		}
		else if (name=="/comp")	
		{ 		
			m_Comp.SetAttack(cmd.GetFloat(0));
			m_Comp.SetRelease(cmd.GetFloat(1));
			m_Comp.SetThreshold(cmd.GetFloat(2));
			m_Comp.SetSlope(cmd.GetFloat(3));
		}
		else if (name=="/addtoqueue")
		{
			char *filename = cmd.GetString(1);
			if (filename!=NULL)
			{
				SampleStore::Get()->AddToQueue(cmd.GetInt(0), filename);
			}
		}
		else if (name=="/loadqueue")
		{
			SampleStore::Get()->LoadQueue();
		}
		else if (name=="/unload")
		{
			SampleStore::Get()->Unload(cmd.GetInt(0));
		}
		else if (name=="/debug")
		{
			m_Debug=cmd.GetInt(0);
		}
		else if (name=="/addsearchpath")
		{
			SearchPaths::Get()->AddPath(cmd.GetString(0));
		}
	}	
}

void Fluxa::Process(unsigned int BufSize)
{	
	if (BufSize==0)
	{
		cerr<<"BufSize is "<<BufSize<<" ??"<<endl;
		return;
	}

	if (BufSize>(unsigned int)m_LeftBuffer.GetLength())
	{
		m_LeftBuffer.Allocate(BufSize);
		m_RightBuffer.Allocate(BufSize);
		//PortAudioClient::Get()->SetOutputs(m_LeftBuffer.GetNonConstBuffer(),m_RightBuffer.GetNonConstBuffer());
 		JackClient::Get()->SetOutputBuf(m_LeftJack, m_LeftBuffer.GetNonConstBuffer());
 		JackClient::Get()->SetOutputBuf(m_RightJack, m_RightBuffer.GetNonConstBuffer());
	}
	
	m_LeftBuffer.Zero();
	m_RightBuffer.Zero();
	
	Time LastTime = m_CurrentTime;
	m_CurrentTime.IncBySample(BufSize,m_SampleRate);
	
	Event e;
	while (m_EventQueue.Get(LastTime, m_CurrentTime, e))
	{
		float t = LastTime.GetDifference(e.TimeStamp);
		// hack to get round bug with GetDifference throwing big numbers
		if (t<=0) 
		{
			m_Graph.Play(t,e.ID);
		}
		else
		{
			cerr<<"----------------"<<endl;
			cerr<<t<<endl;
			LastTime.Print();
			e.TimeStamp.Print();
		}
	}
	
	m_Graph.Process(BufSize,m_LeftBuffer);
	m_Eq.Process(BufSize,m_LeftBuffer);
	//m_Comp.Process(BufSize,m_LeftBuffer);
	m_RightBuffer=m_LeftBuffer;
	
	//cerr<<m_RightBuffer[0]<<endl;
		
	// panning
	float leftpan=1,rightpan=1;
	if (m_Pan<0) leftpan=1-m_Pan;
	else rightpan=1+m_Pan;
	
	// global volume + clip
	bool clip=false;
	for (unsigned int i=0; i<BufSize; i++)
	{
		m_LeftBuffer[i]*=m_GlobalVolume*leftpan;
		m_RightBuffer[i]*=m_GlobalVolume*rightpan;
		
		if (m_LeftBuffer[i]<-1) { m_LeftBuffer[i]=-1; clip=true; }
		if (m_LeftBuffer[i]>1) { m_LeftBuffer[i]=1; clip=true; }
		if (m_RightBuffer[i]<-1) { m_RightBuffer[i]=-1; clip=true; }
		if (m_RightBuffer[i]>1) { m_RightBuffer[i]=1; clip=true; }		
	}
	//if (clip) cerr<<"clip!"<<endl;
}
