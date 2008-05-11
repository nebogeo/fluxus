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

#include <sys/time.h>
#include <iostream>
#include <limits.h>

using namespace std;

#ifndef SPIRALCORE_TIME
#define SPIRALCORE_TIME

static const double ONE_OVER_UINT_MAX = 1.0/UINT_MAX;

namespace spiralcore
{

class Time
{
public:
	Time(); 
	Time(int s, int f) : Seconds(s),Fraction(f) {}
	void SetToNow();
	void SetFromPosix(timeval tv);
	void IncBySample(unsigned long samples, unsigned long samplerate);
	bool operator<(const Time& other);
	bool operator>(const Time& other);
	bool operator<=(const Time& other);
	bool operator>=(const Time& other);
	bool operator==(const Time& other);
	Time &operator+=(double s);
	void Print() const;
	double GetFraction() const { return Fraction*ONE_OVER_UINT_MAX; }
	void SetFraction(double s) { Fraction = (int)(s*(double)UINT_MAX); }
	bool IsEmpty() { return (!Seconds && !Fraction); }
	double GetDifference(const Time& other);
	
	unsigned int Seconds;
	unsigned int Fraction;
};

}

#endif
