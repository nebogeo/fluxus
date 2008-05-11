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
#include <cstdio>
#include <cstring>
#include <string>
#include "RingBuffer.h"

using namespace std;

RingBuffer::RingBuffer(unsigned int size):
m_ReadPos(0),
m_WritePos(0),
m_Size(size),
m_SizeMask(size-1),
m_Buffer(NULL)
{
	m_Buffer = new char[m_Size];
	memset(m_Buffer,'Z',m_Size);
}

RingBuffer::~RingBuffer()	
{
	delete[] m_Buffer;
}

bool RingBuffer::Write(char *src, unsigned int size)
{
	//cerr<<"write pos: "<<m_WritePos<<endl;
	unsigned int space=WriteSpace();
	if (space<size) return false;
	
	//cerr<<size<<" "<<space<<endl;
	
	if (size<m_Size-m_WritePos)
	{
		//cerr<<"written to: "<<m_WritePos<<endl;
		memcpy(&(m_Buffer[m_WritePos]), src, size);
		m_WritePos += size;
		m_WritePos &= m_SizeMask;
	}
	else // have to split data over boundary
	{
		unsigned int first = m_Size-m_WritePos;
		unsigned int second = (m_WritePos+size) & m_SizeMask;
		
		memcpy(&(m_Buffer[m_WritePos]), src, first);
		m_WritePos += first;
		m_WritePos &= m_SizeMask;
		
		memcpy(&(m_Buffer[m_WritePos]), &src[first], second);
		m_WritePos += second;
		m_WritePos &= m_SizeMask;
	}
	
	return true;
}

bool RingBuffer::Read(char *dest, unsigned int size)
{
	//cerr<<"read pos: "<<m_ReadPos<<endl;
	unsigned int space=ReadSpace();
	if (space==0 || size>m_Size) return false;
	
	if (size<m_Size-m_ReadPos)
	{
		//cerr<<"reading from: "<<m_ReadPos<<endl;
		memcpy(dest, &(m_Buffer[m_ReadPos]), size);
		m_ReadPos += size;
		m_ReadPos &= m_SizeMask;
	}
	else // have to split data over boundary
	{
		unsigned int first = m_Size-m_ReadPos;
		unsigned int second = (m_ReadPos+size) & m_SizeMask;
		
		memcpy(dest, &(m_Buffer[m_ReadPos]), first);
		m_ReadPos += first;
		m_ReadPos &= m_SizeMask;
		
		memcpy(&dest[first], &(m_Buffer[m_ReadPos]), second);
		m_ReadPos += second;
		m_ReadPos &= m_SizeMask;
	}
	
	return true;
}

void RingBuffer::Dump()
{
	for (unsigned int i=0; i<m_Size; i++) cerr<<m_Buffer[i];
	cerr<<endl;
}

unsigned int RingBuffer::WriteSpace()
{
	unsigned int read = m_ReadPos;
	unsigned int write = m_WritePos;
	
	if (write > read) return (read - write + m_Size) & m_SizeMask - 1;
	if (write < read) return (read - write) - 1;
	return m_Size - 1;
}

unsigned int RingBuffer::ReadSpace()
{
	unsigned int read = m_ReadPos;
	unsigned int write = m_WritePos;

	if (write > read) return write - read;
	else return (write - read + m_Size) & m_SizeMask;
}
