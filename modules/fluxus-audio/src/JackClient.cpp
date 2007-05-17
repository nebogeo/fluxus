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

#include <stdio.h>
#include <limits.h>

#include "JackClient.h"

JackClient*       JackClient::m_Singleton  = NULL;
bool              JackClient::m_Attached   = false;
long unsigned int JackClient::m_BufferSize = 0;
long unsigned int JackClient::m_SampleRate = 0;
void            (*JackClient::RunCallback)(void*, unsigned int BufSize)=NULL;
void             *JackClient::RunContext   = NULL;	
jack_client_t    *JackClient::m_Client     = NULL;
map<int,JackClient::JackPort*> JackClient::m_InputPortMap;	
map<int,JackClient::JackPort*> JackClient::m_OutputPortMap;	

///////////////////////////////////////////////////////

JackClient::JackClient() :
m_NextInputID(0),
m_NextOutputID(0)
{
}

/////////////////////////////////////////////////////////////////////////////////////////////

JackClient::~JackClient()	
{	
	Detach();
}

/////////////////////////////////////////////////////////////////////////////////////////////

bool JackClient::Attach(const string &ClientName)
{
	if (m_Attached) return true;

	if (!(m_Client = jack_client_new(ClientName.c_str())))
	{
		cerr<<"jack server not running?"<<endl;
		return false;
	}

	jack_set_process_callback(m_Client, JackClient::Process, 0);
	jack_set_sample_rate_callback (m_Client, JackClient::OnSRateChange, 0);
	jack_on_shutdown (m_Client, JackClient::OnJackShutdown, this);

	m_InputPortMap.clear();
	m_OutputPortMap.clear();
	
    // tell the JACK server that we are ready to roll
	if (jack_activate (m_Client))
	{
		cerr<<"cannot activate client"<<endl;
		return false;
	}

	m_Attached=true;
			
	return true;
}

/////////////////////////////////////////////////////////////////////////////////////////////

void JackClient::Detach()
{
	if (m_Client)
	{
		cerr<<"Detaching from JACK"<<endl;
		jack_client_close(m_Client);
		m_Client=NULL;
		m_Attached=false;
	}
	
	// tells ssm to go back to non callback mode
	//if (RunCallback) RunCallback(RunContext, false);
}

/////////////////////////////////////////////////////////////////////////////////////////////

int JackClient::Process(jack_nframes_t nframes, void *o)
{	
	for (map<int,JackPort*>::iterator i=m_InputPortMap.begin();
		i!=m_InputPortMap.end(); i++)
	{
		if (jack_port_connected(i->second->Port))
		{
			sample_t *in = (sample_t *) jack_port_get_buffer(i->second->Port, nframes);
			memcpy (i->second->Buf, in, sizeof (sample_t) * m_BufferSize);
		}			
	}
	
	for (map<int,JackPort*>::iterator i=m_OutputPortMap.begin();
		i!=m_OutputPortMap.end(); i++)
	{
		if (i->second->Buf)
		{
			sample_t *out = (sample_t *) jack_port_get_buffer(i->second->Port, nframes);
			memcpy (out, i->second->Buf, sizeof (sample_t) * m_BufferSize);
		}
		else // no output availible, clear
		{
			sample_t *out = (sample_t *) jack_port_get_buffer(i->second->Port, nframes);
			memset (out, 0, sizeof (sample_t) * m_BufferSize);
		}
	}
	
	m_BufferSize=nframes;
			
	if(RunCallback&&RunContext)
	{
		// do the work
		RunCallback(RunContext, nframes);
	}
	
	return 0;
}

/////////////////////////////////////////////////////////////////////////////////////////////

int JackClient::AddInputPort()
{
	char Name[256];
	sprintf(Name,"In%d",m_NextInputID);
	
	JackPort *NewPort = new JackPort;
	NewPort->Name=Name;
	NewPort->Buf=NULL;		
	NewPort->Port = jack_port_register (m_Client, Name, JACK_DEFAULT_AUDIO_TYPE, JackPortIsInput, 0);
	m_InputPortMap[m_NextInputID]=NewPort;
	
	m_NextInputID++;
	return m_NextInputID-1;
}

/////////////////////////////////////////////////////////////////////////////////////////////

int JackClient::AddOutputPort()
{
	char Name[256];
	sprintf(Name,"Out%d",m_NextOutputID);
	
	JackPort *NewPort = new JackPort;
	NewPort->Name=Name;
	NewPort->Buf=NULL;		
	NewPort->Port = jack_port_register (m_Client, Name, JACK_DEFAULT_AUDIO_TYPE, JackPortIsOutput, 0);
	m_OutputPortMap[m_NextOutputID]=NewPort;
	
	m_NextOutputID++;
	return m_NextOutputID-1;
}

/////////////////////////////////////////////////////////////////////////////////////////////

int JackClient::OnSRateChange(jack_nframes_t n, void *o)
{
	m_SampleRate=n;
	return 0;
}

/////////////////////////////////////////////////////////////////////////////////////////////

void JackClient::OnJackShutdown(void *o)
{
	cerr<<"Shutdown"<<endl;
	m_Attached=false;
	// tells ssm to go back to non callback mode
	RunCallback(RunContext, false);
	return;
}

/////////////////////////////////////////////////////////////////////////////////////////////

void JackClient::GetPortNames(vector<string> &InputNames, vector<string> &OutputNames)
{
	InputNames.clear();
	OutputNames.clear();

	if (!m_Attached) return;

	//Outputs first
	const char **PortNameList=jack_get_ports(m_Client,NULL,NULL,JackPortIsOutput);	
	
	int n=0;
	while(PortNameList[n]!=NULL)
	{		
		OutputNames.push_back(PortNameList[n]);
		n++;
	}	
	
	delete PortNameList;
	
	//Inputs second
	PortNameList=jack_get_ports(m_Client,NULL,NULL,JackPortIsInput);
	
	n=0;
	while(PortNameList[n]!=NULL)
	{		
		InputNames.push_back(PortNameList[n]);
		n++;
	}
	
	delete PortNameList;		
}

/////////////////////////////////////////////////////////////////////////////////////////////
// Input means input of SSM, so this connects jack sources to the plugin outputs
void JackClient::ConnectInput(int n, const string &JackPort)
{
	if (!IsAttached()) return;

	//cerr<<"JackClient::ConnectInput: connecting source ["<<JackPort<<"] to dest ["<<m_InputPortMap[n]->Name<<"]"<<endl;

	if (m_InputPortMap[n]->ConnectedTo!="")
	{
		if (jack_disconnect (m_Client, m_InputPortMap[n]->ConnectedTo.c_str(), jack_port_name(m_InputPortMap[n]->Port)))
			cerr<<"JackClient::ConnectInput: cannot disconnect input port ["
				<<m_InputPortMap[n]->ConnectedTo<<"] from ["<<m_InputPortMap[n]->Name<<"]"<<endl;
	}
	
	m_InputPortMap[n]->ConnectedTo = JackPort;
		
	if (jack_connect (m_Client, JackPort.c_str(), jack_port_name(m_InputPortMap[n]->Port)))
		cerr<<"JackClient::ConnectInput: cannot connect input port ["
			<<JackPort<<"] to ["<<m_InputPortMap[n]->Name<<"]"<<endl;
			
	m_InputPortMap[n]->Connected=true;
}

/////////////////////////////////////////////////////////////////////////////////////////////
// Output means output of SSM, so this connects plugin inputs to a jack destination
void JackClient::ConnectOutput(int n, const string &JackPort)
{
	if (!IsAttached()) return;
	//cerr<<"JackClient::ConnectOutput: connecting source ["<<m_OutputPortMap[n]->Name<<"] to dest ["<<JackPort<<"]"<<endl;

	if (m_OutputPortMap[n]->ConnectedTo!="")
	{
		if (jack_disconnect (m_Client, jack_port_name(m_OutputPortMap[n]->Port), m_OutputPortMap[n]->ConnectedTo.c_str()))
			cerr<<"JackClient::ConnectOutput: cannot disconnect output port ["
				<<m_OutputPortMap[n]->ConnectedTo<<"] from ["<<m_OutputPortMap[n]->Name<<"]"<<endl;
	}
	
	m_OutputPortMap[n]->ConnectedTo = JackPort;
	if (jack_connect (m_Client, jack_port_name(m_OutputPortMap[n]->Port), JackPort.c_str()))
		cerr<<"JackClient::ConnectOutput: cannot connect output port ["
			<<m_OutputPortMap[n]->Name<<"] to ["<<JackPort<<"]"<<endl;
	m_OutputPortMap[n]->Connected=true;
}

/////////////////////////////////////////////////////////////////////////////////////////////
// Input means input of SSM, so this connects jack sources to the plugin outputs
void JackClient::DisconnectInput(int n)
{
	if (!IsAttached()) return;
	//cerr<<"JackClient::DisconnectInput: Disconnecting input "<<n<<endl;

	if (m_InputPortMap[n]->ConnectedTo!="")
	{
		if (jack_disconnect (m_Client, m_InputPortMap[n]->ConnectedTo.c_str(), jack_port_name(m_InputPortMap[n]->Port)))
			cerr<<"JackClient::ConnectInput: cannot disconnect input port ["
				<<m_InputPortMap[n]->ConnectedTo<<"] from ["<<m_InputPortMap[n]->Name<<"]"<<endl;
	}

	m_InputPortMap[n]->Connected=false;
}

/////////////////////////////////////////////////////////////////////////////////////////////
// Output means output of SSM, so this connects plugin inputs to a jack destination
void JackClient::DisconnectOutput(int n)
{
	if (!IsAttached()) return;
	//cerr<<"JackClient::DisconnectInput: Disconnecting input "<<n<<endl;

	if (m_OutputPortMap[n]->ConnectedTo!="")
	{
		if (jack_disconnect (m_Client, jack_port_name(m_OutputPortMap[n]->Port), m_OutputPortMap[n]->ConnectedTo.c_str()))
			cerr<<"JackClient::ConnectOutput: cannot disconnect output port ["
				<<m_OutputPortMap[n]->ConnectedTo<<"] from ["<<m_OutputPortMap[n]->Name<<"]"<<endl;
	}

	m_OutputPortMap[n]->Connected=false;
}
/////////////////////////////////////////////////////////////////////////////////////////////

void JackClient::SetInputBuf(int ID, float* s)
{
	if(m_InputPortMap.find(ID)!=m_InputPortMap.end()) m_InputPortMap[ID]->Buf=s;
	else cerr<<"Could not find port ID "<<ID<<endl;
}

/////////////////////////////////////////////////////////////////////////////////////////////
	
void JackClient::SetOutputBuf(int ID, float* s)
{
	if(m_OutputPortMap.find(ID)!=m_OutputPortMap.end()) m_OutputPortMap[ID]->Buf=s;
	else cerr<<"Could not find port ID "<<ID<<endl;
}
