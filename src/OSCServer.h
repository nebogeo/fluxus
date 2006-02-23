
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

#ifndef FLUXUS_OSC_SERVER
#define FLUXUS_OSC_SERVER

#include <string>
#include <lo/lo.h>
#include <set>
#include <map>
#include <list>
#include <vector>
#include <pthread.h>
#include "OSCCore.h"
#include "Recorder.h"

using namespace std;

namespace fluxus
{

class Client
{
public:
	Client() : m_Initialised(false) {}
	~Client() {}
	void SetDestination(const string &Port);	
	void Send(const string &msg,const vector<OSCData*> &args);
	
private:
	lo_address m_Destination;
	bool m_Initialised;
};

class Server
{
public:
	Server(const string &Port);
	~Server();
	
	void Run();
	bool SetMsg(const string &name);
	bool GetArgs(vector<OSCData*> &args);
	string GetLastMsg() { return m_LastMsg; }
	
	static void SetRecorder(EventRecorder *s) { m_Recorder = s; }
	
	// call once per frame
	void PollRecorder();
	
private:

	static int DefaultHandler(const char *path, const char *types, lo_arg **argv, int argc, void *data, void *user_data);
	static void ErrorHandler(int num, const char *m, const char *path);	
	
	static map<string,list<OSCMsgData*> > m_Map;
	static string m_LastMsg;
	
	lo_server_thread m_Server;
	static bool m_Exit;
	static pthread_mutex_t* m_Mutex;
	string m_CurrentOSCMsg;
	list<OSCMsgData*>::iterator m_CurrentOSCData;
	static EventRecorder *m_Recorder;
};

}

#endif
