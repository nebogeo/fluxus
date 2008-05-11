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

#include <string>
#include <lo/lo.h>
#include "CommandRingBuffer.h"

using namespace std;

class OSCServer
{
public:
	OSCServer(const string &Port);
	~OSCServer();
	
	void Run();
	bool Get(CommandRingBuffer::Command& command) { return m_CommandRingBuffer.Get(command);}
	
private:
	static int DefaultHandler(const char *path, const char *types, lo_arg **argv, int argc, void *data, void *user_data);
	static void ErrorHandler(int num, const char *m, const char *path);

	lo_server_thread m_Server;
	string m_Port;
	bool m_Exit;
	CommandRingBuffer m_CommandRingBuffer; 
};
