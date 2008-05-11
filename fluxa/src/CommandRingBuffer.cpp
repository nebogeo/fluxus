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

#include "CommandRingBuffer.h"
#include <string.h>
#include <iostream>

using namespace std;

CommandRingBuffer::Command::Command(const char *name, const char *types, const char *data, unsigned int datasize)
{
	strcpy(Name,name);
	strcpy(Types,types);
	memcpy(Data,data,datasize);
	
	m_NumArgs=strlen(Types);
	
	// figure out the offsets into the data to use later
	int pos=0;
	for(unsigned int i=0; i<m_NumArgs; i++)
	{
		m_Offsets[i]=pos;
		switch(Types[i])
		{
			case 'i': pos+=4; break;
			case 'f': pos+=4; break;
			case 's': pos+=strlen(Data+pos)+1; break;
			case 'b': pos+=sizeof(char*); break;
			
			default: 
				// invalidate the command
				for(unsigned int n=0; n<strlen(Types); n++) m_Offsets[n]=-1;
				cerr<<"CommandRingBuffer::Command::Command: erk! unknown type: "<<Types[i]<<endl; 				
				return;				
			break;
		}
	}
}

int CommandRingBuffer::Command::GetInt(unsigned int index)
{
	if (m_Offsets[index]!=-1 && Types[index]=='i') return *((int*)(Data+m_Offsets[index]));
	return 0;
}

float CommandRingBuffer::Command::GetFloat(unsigned int index)
{
	if (m_Offsets[index]!=-1 && Types[index]=='f') return *((float*)(Data+m_Offsets[index]));
	return 0;
}

char *CommandRingBuffer::Command::GetString(unsigned int index)
{
	if (m_Offsets[index]!=-1 && Types[index]=='s') return ((char*)(Data+m_Offsets[index]));
	return 0;
}

////////////////////////////////////////////////////////////////

CommandRingBuffer::CommandRingBuffer(unsigned int size): 
RingBuffer(size) 
{
}

CommandRingBuffer::~CommandRingBuffer()
{
}
	
bool CommandRingBuffer::Send(const Command& command)
{
	return Write((char*)&command,sizeof(Command));
}

bool CommandRingBuffer::Get(Command& command)
{
	return Read((char*)&command, sizeof(Command));
}

