// Copyright (C) 2006 David Griffiths <dave@pawfal.org>
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

#include <cstring>
#include <cstdio>
#include <cstdlib>
#include <unistd.h>
#include <iostream>

#include "OSCServer.h"

using namespace std;
using namespace fluxus;

void Client::SetDestination(const string &Port) 
{ 
	if (m_Initialised) lo_address_free(m_Destination);
	m_Destination=lo_address_new_from_url(Port.c_str()); 
	m_Initialised=true;
}	

void Client::Send(const string &msg, const vector<OSCData*> &args)
{
	if (!m_Initialised) return;
	
	lo_message oscmsg=lo_message_new();
		
	for (vector<OSCData*>::const_iterator i=args.begin(); i!=args.end(); i++)
	{					
		switch((*i)->Type())
		{
			case 's': lo_message_add_string(oscmsg,static_cast<OSCString*>(*i)->Value.c_str()); break;
			case 'f': lo_message_add_float(oscmsg,static_cast<OSCFloat*>(*i)->Value); break;
			case 'i': lo_message_add_int32(oscmsg,static_cast<OSCInt*>(*i)->Value); break;
			case 'l': lo_message_add_int64(oscmsg,static_cast<OSCLong*>(*i)->Value); break;
			default: break;
		}
	} 
	
	lo_send_message(m_Destination, msg.c_str(), oscmsg);
	lo_message_free(oscmsg);
}
	

static const unsigned int MAX_MSGS_STORED=2048;
static const unsigned int DATA_PER_MESSAGE=256;

bool Server::m_Exit=false;
map<string,list<OSCMsgData*> > Server::m_Map;
pthread_mutex_t* Server::m_Mutex;
string Server::m_LastMsg="no message yet...";
//EventRecorder *Server::m_Recorder=NULL;
bool Server::m_Error=false;

Server::Server(const string &Port) :
m_ServerStarted(false)
{
	SetPort(Port);
	m_Mutex = new pthread_mutex_t;
	pthread_mutex_init(m_Mutex,NULL);
}


Server::~Server()
{
	m_Exit=true;
}

void Server::SetPort(const string &Port)
{
	if (m_Port!=Port)
	{
		if (m_ServerStarted) 
		{
			lo_server_thread_stop(m_Server);
			lo_server_thread_free(m_Server);
			m_ServerStarted=false;
		}

		m_Server = lo_server_thread_new(Port.c_str(), ErrorHandler);
   		if (!m_Error) 
		{
			m_Port=Port;
			lo_server_thread_add_method(m_Server, NULL, NULL, DefaultHandler, NULL);
			m_ServerStarted=true;
		}
	}
}

void Server::Run()
{
	if (!m_Error) lo_server_thread_start(m_Server);
}

void Server::ErrorHandler(int num, const char *msg, const char *path)
{
    cerr<<"liblo server error "<<num<<endl;//" in path "<<path<<": "<<msg<<endl;
	m_Error=true;
}

int Server::DefaultHandler(const char *path, const char *types, lo_arg **argv,
		    int argc, void *data, void *user_data)
{
	if (m_Map.size()>MAX_MSGS_STORED)
	{
		// stop us filling up mem with messages! :)
		m_Map.erase(m_Map.begin());
		// todo - this is a potential mem leak...
	}
	
	// record the message name
	pthread_mutex_lock(m_Mutex);
	m_LastMsg=string(path)+" "+string(types)+" ";
	pthread_mutex_unlock(m_Mutex);
	
	// put the data in a vector
	vector<OSCData*> argsdata;
	char buf[256];
	
	for (unsigned int i=0; i<strlen(types); i++)
	{
		switch (types[i]) 
		{
			case 'f': 
			{
				argsdata.push_back(new OSCFloat(argv[i]->f)); 
				snprintf(buf,256,"%f",argv[i]->f);
				m_LastMsg+=string(buf)+" "; 
			}
			break;
			case 'i': 
			{
				argsdata.push_back(new OSCInt(argv[i]->i)); 
				snprintf(buf,256,"%i",argv[i]->i);
				m_LastMsg+=string(buf)+" "; 			
			}
			break;
			case 'h': 
			{ 
				argsdata.push_back(new OSCLong(argv[i]->h)); 
				snprintf(buf,256,"%ld",argv[i]->h);
				m_LastMsg+=string(buf)+" "; 			
			}
			break;
			case 's': argsdata.push_back(new OSCString(&argv[i]->s)); m_LastMsg+=string(&argv[i]->s)+" "; break;
			default :
                //cerr<<types[i]<<" not supported"<<endl;
                argsdata.push_back(new OSCData); break; // put in a null data type
                break;
		}
	}
	
	pthread_mutex_lock(m_Mutex);
	if (m_Map[path].size()<DATA_PER_MESSAGE) 
	{	
		m_Map[path].push_back(new OSCMsgData(argsdata));	
	}
/*	if (m_Recorder && m_Recorder->GetMode()==EventRecorder::RECORD)
	{
		cerr<<"saving an osc message"<<endl;
		RecorderMessage *event = new RecorderMessage;
		event->Name=path;
		event->Data.Copy(argsdata);
		m_Recorder->Record(event);
	}*/
	pthread_mutex_unlock(m_Mutex);
		
    return 1;
}
/*
void Server::PollRecorder()
{
	if (m_Recorder && m_Recorder->GetMode()==EventRecorder::PLAYBACK)
	{
		vector<RecorderMessage*> events;
		m_Recorder->Get(events);
		for (vector<RecorderMessage*>::iterator i=events.begin(); i!=events.end(); i++)
		{
			//cerr<<"polled an osc message from recorder"<<endl;
			if (m_Map[(*i)->Name].size()<DATA_PER_MESSAGE) 
			{
				OSCMsgData *msg = new OSCMsgData;
				msg->Copy((*i)->Data.m_Data);
				m_Map[(*i)->Name].push_back(msg);
			}
		}
	}
}
*/
bool Server::GetArgs(vector<OSCData*> &args) 
{ 
	if (m_CurrentOSCMsg!="")
	{
		pthread_mutex_lock(m_Mutex);
		args = (*m_CurrentOSCData)->m_Data;
		pthread_mutex_unlock(m_Mutex);
		return true;
	}
	return false;
}

bool Server::SetMsg(const string &name) 
{	
	if (m_CurrentOSCMsg!="")
	{
		// get rid of the old data
		delete *m_CurrentOSCData;
		m_Map[m_CurrentOSCMsg].erase(m_CurrentOSCData);
	}
	
	m_CurrentOSCMsg="";
	map<string,list<OSCMsgData*> >::iterator i=m_Map.find(name);
	if (i!=m_Map.end() && !i->second.empty())
	{
		m_CurrentOSCMsg=name;
		m_CurrentOSCData=i->second.begin();
		return true;
	}
	return false;
}

