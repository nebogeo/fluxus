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

#include <map>
#include <vector>
#include <string>
#include <iostream>
#include <jack/jack.h>

using namespace std;

typedef jack_default_audio_sample_t sample_t;

#ifndef JACK_CLIENT
#define JACK_CLIENT

const int MAX_INPUTPORTS = 256;
const int MAX_OUTPUTPORTS = 256;

class JackClient
{
public:
	static JackClient *Get()      { if(!m_Singleton) m_Singleton=new JackClient; return m_Singleton; }
	static void PackUpAndGoHome() { if(m_Singleton)  { delete m_Singleton; m_Singleton=NULL; } }
	
	bool   Attach(const string &ClientName);
	void   Detach();
	bool   IsAttached()                   { return m_Attached; }
	void   SetCallback(void(*Run)(void*, unsigned int),void *Context) { RunCallback=Run; RunContext=Context; }					
	void   GetPortNames(vector<string> &InputNames,vector<string> &OutputNames);
	void   ConnectInput(int n, const string &JackPort);
	void   ConnectOutput(int n, const string &JackPort);
	void   DisconnectInput(int n);
	void   DisconnectOutput(int n);
	string GetInputName(int ID)           { return m_InputPortMap[ID]->Name; }
	string GetOutputName(int ID)          { return m_OutputPortMap[ID]->Name; }
	void   SetInputBuf(int ID, float* s);
	void   SetOutputBuf(int ID, float* s);
         int 	 AddInputPort();
         int 	 AddOutputPort();

protected:
	JackClient();
	~JackClient();
	
	static int  Process(jack_nframes_t nframes, void *o);
	static int  OnSRateChange(jack_nframes_t n, void *o);
	static void OnJackShutdown(void *o);

private:

	class JackPort
	{		
		public:
		JackPort::JackPort() :
			Connected(false),Buf(NULL),Port(NULL) {}
		
		string         Name;
		bool           Connected;
		float*         Buf;
		jack_port_t*   Port;
		string         ConnectedTo;
	};

	static JackClient*        m_Singleton;
	static jack_client_t*     m_Client;
	static map<int,JackPort*> m_InputPortMap;	
	static map<int,JackPort*> m_OutputPortMap;	
	static long unsigned int  m_BufferSize;
	static long unsigned int  m_SampleRate;	
	static bool               m_Attached;
	int m_NextInputID;
	int m_NextOutputID;
	
	static void(*RunCallback)(void*, unsigned int bufsize);
	static void *RunContext;
};

#endif
