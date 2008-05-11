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

#ifndef SAMPLE
#define SAMPLE

#include <memory.h>
#include <assert.h>
#include <limits.h>
#include <iostream>
#include "Types.h"
#include "Allocator.h"

namespace spiralcore
{
//#define DEBUG

inline float Linear(float bot,float top,float pos,float val1,float val2) 
{ 
    float t=(pos-bot)/(top-bot); 
    return val1*t + val2*(1.0f-t); 
}

inline bool feq(float a, float b, float tol)
{
	return (a>b-tol && a<b+tol);
}

class Sample
{
public:
	enum SampleType {AUDIO=0, IMAGE, MIDI};
	
	Sample(int Len=0);
	Sample(const Sample &rhs);
	Sample(const AudioType *S, int Len);
	~Sample();

	static void SetAllocator(Allocator *s) { m_Allocator=s; }
	static Allocator *GetAllocator() { return m_Allocator; }

	bool Allocate(int Size);
	void Clear();
	void Zero();
	void Set(AudioType Val);
	void Insert(const Sample &S, int Pos);
	void Add(const Sample &S);
	void Mix(const Sample &S, int Pos=0);
	void MulClipMix(const Sample &S, float m);
	void Remove(int Start, int End);
	void Reverse(int Start, int End);
	void Move(int Dist);
	void GetRegion(Sample &S, int Start, int End) const;
	const AudioType *GetBuffer() const {return m_Data;}
	AudioType *GetNonConstBuffer() {return m_Data;}
	int  GetLength() const {return m_Length;}
	int  GetLengthInBytes() const {return m_Length*sizeof(AudioType);}
	void Expand(int Length);
	void Shrink(int Length);
	void CropTo(int NewLength);

	AudioType &operator[](unsigned int i) const
	{		
		#ifdef DEBUG
			assert(i>=0 && i<m_Length);
		#endif
		return m_Data[i];
	}
	
	AudioType &operator[](int i) const
	{
		//return (*this)[(int)i];
		return m_Data[i];
	}
	
	// Linear interpolated
	inline AudioType operator[](float i) const
	{		
		int ii=(int)i;
		
		#ifdef DEBUG
			assert(ii>=0 && ii<m_Length);
		#endif
		
		if (ii==m_Length-1) return m_Data[ii];	
		AudioType t=i-ii;		
		return ((m_Data[ii]*(1-t))+(m_Data[ii+1])*t);
	}


	void Set(int i, AudioType v)
	{	
		#ifdef DEBUG
			assert(i>=0 && i<m_Length);
		#endif							
		m_Data[i]=v;
	}	
	
	Sample &operator=(const Sample &rhs)
	{
		if (GetLength()!=rhs.GetLength()) Allocate(rhs.GetLength());		
		memcpy(m_Data,rhs.GetBuffer(),GetLengthInBytes());
		return *this;
	}
		
private:
	AudioType *m_Data;
	long  int  m_Length;
	
    SampleType m_SampleType;
	static Allocator *m_Allocator;
};
}
#endif
