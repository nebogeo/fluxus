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

int main(int argc, char **argv)
{
	if (argc!=1 && argc!=2)
	{
		cerr<<"usage: fluxa oscportnumber"<<endl;
		return -1;
	}
	string port("4004");
	if (argc==2) port=argv[1];
	OSCServer server(port);
	Fluxa engine(&server,44100);
	server.Run();
	return 0;
}
