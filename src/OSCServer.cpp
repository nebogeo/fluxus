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

#include <cstdio>
#include <cstdlib>
#include <unistd.h>
#include <iostream>

#include "OSCServer.h"

using namespace std;

void Client::Send(const string &msg, const vector<OSCData*> &args)
{
	lo_message oscmsg=lo_message_new();
		
	for (vector<OSCData*>::const_iterator i=args.begin(); i!=args.end(); i++)
	{					
		switch((*i)->Type())
		{
			case 's': lo_message_add_string(oscmsg,static_cast<OSCString*>(*i)->Value.c_str()); break;
			case 'f': lo_message_add_float(oscmsg,static_cast<OSCFloat*>(*i)->Value); break;
			case 'i': lo_message_add_int32(oscmsg,static_cast<OSCInt*>(*i)->Value); break;
			default: break;
		}
	} 
	
	lo_send_message(m_Destination, msg.c_str(), oscmsg);
	lo_message_free(oscmsg);
}
	

static const unsigned int MAX_MSGS_STORED=2048;

bool Server::m_Exit=false;
map<string,vector<OSCData*> > Server::m_Map;
pthread_mutex_t* Server::m_Mutex;
string Server::m_LastMsg="no message yet...";
set<string> Server::m_MessageHistory;

Server::Server(const string &Port)
{
	cerr<<"osc port ["<<Port<<"]"<<endl;
    m_Server = lo_server_thread_new(Port.c_str(), ErrorHandler);
    lo_server_thread_add_method(m_Server, NULL, NULL, DefaultHandler, NULL);
	m_Mutex = new pthread_mutex_t;
	pthread_mutex_init(m_Mutex,NULL);
}

Server::~Server()
{
	m_Exit=true;
}

void Server::Run()
{
	cerr<<"Starting osc server"<<endl;
	lo_server_thread_start(m_Server);
}

void Server::ErrorHandler(int num, const char *msg, const char *path)
{
    cerr<<"liblo server error "<<num<<" in path "<<path<<": "<<msg<<endl;
}

int Server::DefaultHandler(const char *path, const char *types, lo_arg **argv,
		    int argc, void *data, void *user_data)
{
	if (m_Map.size()>MAX_MSGS_STORED)
	{
		// stop us filling up mem with messages! :)
		m_Map.erase(m_Map.begin());
	}
	
	// record the message name
	pthread_mutex_lock(m_Mutex);
	m_LastMsg=string(path)+" "+string(types)+" ";
	m_MessageHistory.insert(path);
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
			case 's': argsdata.push_back(new OSCString(&argv[i]->s)); m_LastMsg+=string(&argv[i]->s)+" "; break;
			default : argsdata.push_back(new OSCData); break; // put in a null data type
		}
	}
	
	map<string,vector<OSCData*> >::iterator i=m_Map.find(path);
	
	if(i!=m_Map.end())
	{
		// delete the old message arguments
		for (vector<OSCData*>::iterator arg=i->second.begin(); arg!=i->second.end(); arg++)
		{
			delete *arg;
		}
	}
	
	pthread_mutex_lock(m_Mutex);
	m_Map[path]=argsdata;
	pthread_mutex_unlock(m_Mutex);
	
    return 1;
}

bool Server::GetArgs(vector<OSCData*> &args) 
{ 
	if (m_CurrentOSCMsg!="")
	{
		pthread_mutex_lock(m_Mutex);
		map<string,vector<OSCData*> >::iterator i=m_Map.find(m_CurrentOSCMsg);
		if (i!=m_Map.end())
		{
			args = i->second;
			pthread_mutex_unlock(m_Mutex);
			return true;
		}
		pthread_mutex_unlock(m_Mutex);
	}
	return false;
}

bool Server::SetMsg(const string &name) 
{
	if (m_MessageHistory.find(name)!=m_MessageHistory.end())
	{
		m_CurrentOSCMsg=name;
		return true;
	}
	return false;
}

void Server::ClearHistory()
{
	m_MessageHistory.clear();
}
