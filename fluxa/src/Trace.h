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
#include <cstdarg>
#include <iostream>
#include <cstdlib>
#include <cstring>

#ifndef SPIRALCORE_TRACE
#define SPIRALCORE_TRACE

using namespace std;

#define BLACK           0
#define RED             1
#define GREEN           2
#define YELLOW          3
#define BLUE            4
#define MAGENTA         5
#define CYAN            6
#define WHITE           7

inline void Trace(int FGColour, int BGColour, const char *msg, ...)
{
    va_list ap;
    va_start(ap, msg);
    char buffer[1024];
    if (!strcmp(msg,"%s")) 
    {
        strcpy(buffer,va_arg(ap, const char*));
    } 
    else 
    {
        vsnprintf(buffer, 1024, msg, ap);
    }
    va_end(ap);
 
    //char command[13];               
    //sprintf(command, "%c[%d;%d;%dm", 0x1B, 2, FGColour + 30, BGColour + 40);        
    //cerr<<command<<buffer;
    //sprintf(command, "%c[%d;%d;%dm", 0x1B, 2, WHITE + 30, BLACK + 40);      
    //cerr<<command<<endl;
	
    cerr<<buffer<<endl;
}

#endif
