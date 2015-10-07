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

// ringbuffer for processing commands between asycronous threads, either may be
// realtime and non blocking, so all code should be realtime capable

class RingBuffer
{
public:
	RingBuffer(unsigned int size);
	~RingBuffer();
	
	//bool Lock();
	//bool Unlock();
	bool Write(char *src, unsigned int size);
	bool Read(char *dest, unsigned int size);
	void Dump();

private:
	unsigned int WriteSpace();
	unsigned int ReadSpace();
	
	unsigned int m_ReadPos;
	unsigned int m_WritePos;
	unsigned int m_Size;
	unsigned int m_SizeMask;	
	char *m_Buffer;	
};
