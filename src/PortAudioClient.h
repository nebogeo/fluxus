// Copyright (C) 2005 David Griffiths <dave@pawfal.org>
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
#include <vector>
#include <string>
#include <iostream>
#include <portaudio.h>

using namespace std;

#ifndef PA_CLIENT
#define PA_CLIENT

class PortAudioClient
{
public:
	static void Initialise(unsigned int s, int bs) { m_SampleRate=s; m_BufferSize=bs; }
	static PortAudioClient *Get()      { if(!m_Singleton) m_Singleton=new PortAudioClient; return m_Singleton; }
	static void PackUpAndGoHome() { if(m_Singleton)  { delete m_Singleton; m_Singleton=NULL; } }
	
	bool   Attach(const string &ClientName);
	void   Detach();
	bool   IsAttached()                   { return m_Attached; }
	void   SetCallback(void(*Run)(void*, unsigned int),void *Context) { RunCallback=Run; RunContext=Context; }					
	void   SetOutputs(float *l, float *r) { m_LeftData=l; m_RightData=r; }
	void   SetInputs(float *l, float *r) { m_LeftInData=l; m_RightInData=r; }
	
protected:
	PortAudioClient();
	~PortAudioClient();
	
	static int  Process(void *inputBuffer, void *outputBuffer,
                           unsigned long framesPerBuffer,
                           PaTimestamp outTime, void *userData);

private:

	static PortAudioClient*   m_Singleton;
	static PortAudioStream*   m_Client;	
	static long unsigned int  m_BufferSize;
	static long unsigned int  m_SampleRate;	
	static bool               m_Attached;
	
	static float *m_RightData;
	static float *m_LeftData;
	static float *m_RightInData;
	static float *m_LeftInData;
	
	static void(*RunCallback)(void *, unsigned int);
	static void *RunContext;
};

#endif
