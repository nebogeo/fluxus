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

#include <iostream>
#include "Fluxa.h"
#include "JackClient.h"

void printusage()
{
	cerr<<"usage: fluxa [-osc oscportnumber] [-jackports leftport rightport]"<<endl;
	exit(-1);
}

int main(int argc, char **argv)
{
	string leftport("alsa_pcm:playback_1");
	string rightport("alsa_pcm:playback_2");
	string port("4004");
	
	int arg=1;
	while(arg<argc)
	{		
		if (!strcmp(argv[arg],"-osc"))
		{
			if (arg+1 < argc) port=argv[arg+1];
			else printusage();
		}
		if (!strcmp(argv[arg],"-jackports"))
		{
			if (arg+2 < argc) 
			{
				leftport=argv[arg+1];
				rightport=argv[arg+2];
			}
			else printusage();
		}
		arg++;	
	}
				
	OSCServer server(port);
	JackClient* jack=JackClient::Get();
 	jack->Attach("fluxa");	
	Fluxa engine(&server,jack,leftport,rightport);
	server.Run();
	return 0;
}
