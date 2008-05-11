// Copyright (C) 2003 David Griffiths <dave@pawfal.org>
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

#include "Allocator.h"

char *MallocAllocator::New(unsigned int size)
{
	return new char[size];
}

void MallocAllocator::Delete(char *mem)
{
	delete[] mem;
}

///////////////////////////////////////////////////////////

RealtimeAllocator::RealtimeAllocator(unsigned int size) :
m_Position(0),
m_Size(size)
{
	m_Buffer = new char[m_Size];
}

void RealtimeAllocator::Reset()
{
	m_Position=0;
}

char *RealtimeAllocator::New(unsigned int size)
{
	//cerr<<"new "<<size<<endl;
	char *ret = m_Buffer+m_Position;
	m_Position+=size;
	

	if (m_Position>m_Size)
	{
		cerr<<"out of realtime buffer mem, here we go!!! :("<<endl;
		m_Position=0;
		ret = m_Buffer;
	}
	
	return ret;
}

void RealtimeAllocator::Delete(char *mem)
{
	//cerr<<"delete"<<endl;
	// we don't need no stinking delete!
}
