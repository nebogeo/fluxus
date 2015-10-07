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

#include "RingBuffer.h"

static const unsigned int COMMAND_DATA_SIZE = 128;

class CommandRingBuffer : public RingBuffer
{
public:
	CommandRingBuffer(unsigned int size);
	~CommandRingBuffer();
	
	class Command
	{
	public:
		Command() {}
		Command(const char *name, const char *types, const char *data, unsigned int datasize);
		~Command() {}
		
		int GetInt(unsigned int index);
		float GetFloat(unsigned int index);
		char *GetString(unsigned int index);
		// unlike the string - ownership of the blob is yours
		// you must delete it when you're done...
		char *GetBlob(unsigned int index);
		unsigned int Size() { return m_NumArgs; }
		char Name[256];
		char Types[64];
		
	private:
		char Data[COMMAND_DATA_SIZE];
		int m_Offsets[64]; 
		unsigned int m_NumArgs; 
		
	};	
	
	bool Send(const Command& command);
	bool Get(Command& command);
	
private:
	Command m_Current;
};
