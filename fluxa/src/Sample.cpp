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

#include <string.h>
#include "Types.h"
#include "Sample.h"
#include <iostream>

using namespace spiralcore;

Allocator *Sample::m_Allocator = new MallocAllocator();

Sample::Sample(unsigned int Len) :
m_Data(NULL),
m_Length(0)
{	
	if (Len) 
	{
		Allocate(Len);
	}
}


Sample::Sample(const Sample &rhs):
m_Data(NULL),
m_Length(0)
{
	*this=rhs;
}


Sample::Sample(const AudioType *S, unsigned int Len):
m_Data(NULL),
m_Length(0)
{
	assert(S);
	Allocate(Len);		
	memcpy(m_Data,S,GetLengthInBytes());
}


Sample::~Sample()
{
	Clear();
}


	
bool Sample::Allocate(unsigned int Size)
{
	Clear();
	
	m_Data = (AudioType*) m_Allocator->New(Size*sizeof(AudioType));
	m_Length=Size;
	
	memset(m_Data,0,GetLengthInBytes());
	
	return (m_Data);
}

void Sample::Clear()
{
	if (m_Data)
	{
		m_Allocator->Delete((char*)m_Data);
		m_Length=0;
		m_Data=NULL;
	}
}

void Sample::Zero()
{
	memset(m_Data,0,GetLengthInBytes());
}

void Sample::Set(AudioType Val)
{
	for (unsigned int n=0; n<m_Length; n++)
	{
		m_Data[n]=Val;
	}
}

void Sample::Insert(const Sample &S, unsigned int Pos)
{
	// do some checking	
	assert(Pos<=GetLength());

	unsigned int NewLen = GetLength()+S.GetLength();
	AudioType *NewBuf = (AudioType*) m_Allocator->New(NewLen*sizeof(AudioType));
	unsigned int FromPos=0, ToPos=0, TempBufPos=0;
	
	while (FromPos<=GetLength())
	{
		if (FromPos==Pos)
		{
			for (TempBufPos=0; TempBufPos<S.GetLength(); TempBufPos++)
			{
				NewBuf[ToPos]=S[TempBufPos];
				ToPos++;
			}
		}
		else
		{
			// this test is needed so the loop can deal
			// with samples being "inserted" on to the
			// very end of the buffer
			if (FromPos<GetLength())
			{
				NewBuf[ToPos]=m_Data[FromPos];
			}
		}
		FromPos++;
		ToPos++;
	}
	
	Clear();
	m_Data=NewBuf;
	m_Length=NewLen;
}

void Sample::Add(const Sample &S)
{
	Insert(S,GetLength());
}

void Sample::Mix(const Sample &S, unsigned int Pos)
{
	// do some checking	
	assert(Pos<GetLength());
	
	unsigned int ToPos=Pos;
	
	for (unsigned int FromPos=0; FromPos<S.GetLength(); FromPos++)
	{
		m_Data[ToPos]=m_Data[ToPos]+S[FromPos];
		
		if (ToPos>GetLength()) ToPos=0;
		ToPos++;
	}
}

void Sample::MulClipMix(const Sample &S, float m)
{
	unsigned int ToPos=0;
	
	for (unsigned int FromPos=0; FromPos<S.GetLength(); FromPos++)
	{
		float t=S[FromPos]*m;
		if (t>m) t=m;
		else if (t<-m) t=-m;
	
		m_Data[ToPos]=m_Data[ToPos]+t;
		
		if (ToPos>GetLength()) ToPos=0;
		ToPos++;
	}
}
void Sample::Remove(unsigned int Start, unsigned int End)
{
	// do some checking
	assert(End<GetLength() && Start<GetLength());
	assert(Start<=End);
	
	// check the range
	if (End>GetLength()) End=GetLength();
	if (Start<0) Start=0;
	
	// calc lengths and allocate memory
	unsigned int CutLen = End - Start;
	// has to be granulated by the buffer size
	
	unsigned int NewLen = GetLength()-CutLen;

	AudioType *TempBuf = (AudioType*) m_Allocator->New(NewLen*sizeof(AudioType));
		
	unsigned int ToPos=0;
	
	for (unsigned int FromPos=0; FromPos<GetLength(); FromPos++)
	{
		// copy the areas outside of the cut range
		if (FromPos<Start || FromPos>End)
		{
			TempBuf[ToPos]=m_Data[FromPos];
			ToPos++;			
			// check the position is in range of the calculated length
			assert(ToPos<=NewLen);
		}
	}
	
	Clear();
	m_Data=TempBuf;
	m_Length=NewLen;
}

void Sample::Reverse(unsigned int Start, unsigned int End)
{
	// do some checking
	assert(End<GetLength() && Start<GetLength());
	assert(Start<=End);
	
	// check the range
	if (End>GetLength()) End=GetLength();
	
	unsigned int NewLen = End-Start;
	AudioType *TempBuf = (AudioType*) m_Allocator->New(NewLen*sizeof(AudioType));
	unsigned int ToPos=0;
	unsigned int FromPos=0;
	
	// get the reversed sample
	for (FromPos=End; FromPos>Start; FromPos--)
	{
		TempBuf[ToPos]=m_Data[FromPos];
		ToPos++;
		assert(ToPos<=NewLen);
	}
	
	FromPos=0;
	
	// overwrite back into place
	for (ToPos=Start; ToPos<End; ToPos++)
	{
		m_Data[ToPos]=TempBuf[FromPos];
		FromPos++;
	}
	
}

void Sample::Move(unsigned int Dist)
{
	unsigned int Length=GetLength();
	AudioType *TempBuf = (AudioType*) m_Allocator->New(Length*sizeof(AudioType));
	unsigned int ToPos=0;
	unsigned int FromPos=Dist;
	
	if (FromPos<0) FromPos+=Length;
	if (FromPos>Length) FromPos-=Length;	
	
	// get the offset sample
	for (ToPos=0; ToPos<Length; ToPos++)
	{	
		TempBuf[ToPos]=m_Data[FromPos];
		FromPos++;
		if (FromPos>=Length) FromPos=0;
	}
	
	Clear();
	m_Data=TempBuf;
	m_Length=Length;	
}

void Sample::GetRegion(Sample &S, unsigned int Start, unsigned int End) const
{
	// do some checking
	assert(End<GetLength() && Start<GetLength());
	assert(Start<=End);
	
	unsigned int Length=End-Start;
	S.Allocate(Length);
	
	unsigned int FromPos=Start;
	
	for (unsigned int ToPos=0; ToPos<Length; ToPos++)
	{
		S.Set(ToPos,m_Data[FromPos]);
		FromPos++;
	}
}

void Sample::CropTo(unsigned int NewLength)
{
	assert (NewLength<GetLength());
	
	AudioType *temp = (AudioType*) m_Allocator->New(NewLength*sizeof(AudioType));
		
	for(unsigned int n=0; n<NewLength; n++)
	{
		temp[n]=m_Data[n];
	}
	
	Clear();
	m_Data=temp;	
	m_Length=NewLength;
}

// adds length amount of blank space
void Sample::Expand(unsigned int Length)
{
	Sample Temp(Length);
	Temp.Zero();
	
	Add(Temp);
}

// shrink the samle by length amount
void Sample::Shrink(unsigned int Length)
{
	unsigned int NewLength=GetLength()-Length;
	assert(NewLength>0 && NewLength<=GetLength());
	
	AudioType *temp = (AudioType*) m_Allocator->New(NewLength*sizeof(AudioType));
	
	for(unsigned int n=0; n<NewLength; n++)
	{
		temp[n]=m_Data[n];
	}
		
	Clear();
	m_Data=temp;	
	m_Length=NewLength;
}
