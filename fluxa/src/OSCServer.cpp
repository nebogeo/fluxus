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

OSCServer::OSCServer(const string &Port) :
m_Port(Port),
m_Exit(false),
m_CommandRingBuffer(262144)
{
	//cerr<<"Using port: ["<<Port<<"]"<<endl;
    m_Server = lo_server_thread_new(Port.c_str(), ErrorHandler);
    lo_server_thread_add_method(m_Server, NULL, NULL, DefaultHandler, this);
}

OSCServer::~OSCServer()
{
	m_Exit=true;
}

void OSCServer::Run()
{
	lo_server_thread_start(m_Server);
    while (!m_Exit) usleep(1000);   
}

void OSCServer::ErrorHandler(int num, const char *msg, const char *path)
{
    //cerr<<"liblo server error "<<num<<" in path "<<path<<": "<<msg<<endl;
    cerr<<"liblo server error "<<num<<endl;
}

int OSCServer::DefaultHandler(const char *path, const char *types, lo_arg **argv,
		    int argc, void *data, void *user_data)
{
	OSCServer *server = (OSCServer*)user_data;
	
	unsigned int size = 0;
	for (int i=0; i<argc; i++)
	{
		size+=lo_arg_size((lo_type)types[i],argv[i]);
		// add one for the null terminator
		if (types[i]=='s') size++;
	}
	
	char *newdata=new char[size];
	unsigned int pos=0;
	for (int i=0; i<argc; i++)
	{
		switch (types[i])
		{
			case LO_INT32:
			{
				if (pos+4>COMMAND_DATA_SIZE)
				{
					cerr<<"osc data too big for ringbuffer command"<<endl;
					delete[] newdata;
					return 1;
				}
				
				memcpy(newdata+pos,(char*)argv[i],4);
				pos+=4;
			}
			break;			
			case LO_FLOAT:
			{
				if (pos+4>COMMAND_DATA_SIZE)
				{
					cerr<<"osc data too big for ringbuffer command"<<endl;
					delete[] newdata;
					return 1;
				}
				
				memcpy(newdata+pos,(char*)argv[i],4);
				pos+=4;
			}
			break;
			case LO_STRING:
			{
				int size=strlen(&argv[i]->s);
				
				if (pos+size+1>COMMAND_DATA_SIZE)
				{
					cerr<<"osc data too big for ringbuffer command"<<endl;
					delete[] newdata;
					return 1;
				}
				
				memcpy(newdata+pos,&argv[i]->s,size);
				newdata[pos+size]='\0';
				pos+=size+1;
			}
			break;
			default:
			{
				cerr<<"unsupported type: "<<types[i]<<endl;
				delete[] newdata;
				return 1;
			}
			break;
		}
	}
	
	if (1)//pos==size) hmm
	{	
		CommandRingBuffer::Command command(path,types,newdata,pos);
		if (!server->m_CommandRingBuffer.Send(command))
		{
			//cerr<<"OSCServer - ringbuffer full!"<<endl;
		}
	}
	else
	{
		cerr<<"OSCServer::DefaultHandler: size mismatch ["<<pos<<":"<<size<<"], not sending message"<<endl;
	}
	
	delete[] newdata;
    return 1;
}
