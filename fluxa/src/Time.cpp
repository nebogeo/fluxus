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

#include <assert.h>
#include <math.h>
#include "Time.h"

using namespace spiralcore;

// got this and usec2ntp from http://www.openmash.org/lxr/source/rtp/ntp-time.h
static const unsigned long GETTIMEOFDAY_TO_NTP_OFFSET = 2208988800UL;

// convert microseconds to fraction of second * 2^32 (i.e., the lsw of
// a 64-bit ntp timestamp).  This routine uses the factorization
// 2^32/10^6 = 4096 + 256 - 1825/32 which results in a max conversion
// error of 3 * 10^-7 and an average error of half that.
unsigned int usec2ntp(unsigned int usec)
{
	unsigned int t = (usec * 1825) >> 5;
	return ((usec << 12) + (usec << 8) - t);
}
  
Time::Time() :
Seconds(0),
Fraction(0)
{	
	
}

void Time::SetToNow()
{
	timeval tv;
	gettimeofday(&tv,0);
	SetFromPosix(tv);
}

void Time::SetFromPosix(timeval tv) 
{
	// gettimeofday epoch is 00:00:00 UTC, January 1, 1970
	// ntp (what we're basing time on for OSC compat) epoch is 
	// 00:00:00 UTC, January 1, 1900, so we need to convert...
	Seconds = (unsigned int)tv.tv_sec + GETTIMEOFDAY_TO_NTP_OFFSET;
	Fraction = usec2ntp(tv.tv_usec);
}

void Time::IncBySample(unsigned long samples, unsigned long samplerate)
{
	(*this)+=samples/(double)samplerate;
}

Time &Time::operator+=(double s)
{
	unsigned int Secs = (unsigned int)floor(s);
	Seconds += Secs;
	double Frac = s-Secs;
	// overflow? (must do this better)
	if (Frac+Fraction*ONE_OVER_UINT_MAX>1.0f) Seconds++;	
	Fraction += (unsigned int)(Frac*UINT_MAX);
	return *this;
}

double Time::GetDifference(const Time& other)
{
	double SecsDiff = (long)Seconds-(long)other.Seconds;
	double SecsFrac = Fraction*ONE_OVER_UINT_MAX;
	SecsFrac-=other.Fraction*ONE_OVER_UINT_MAX;
	return SecsDiff+SecsFrac;
}

bool Time::operator<(const Time& other)
{
	if (Seconds<other.Seconds) return true;
	else if (Seconds==other.Seconds && Fraction<other.Fraction) return true;
	return false;
}

bool Time::operator>(const Time& other)
{
	if (Seconds>other.Seconds) return true;
	else if (Seconds==other.Seconds && Fraction>other.Fraction) return true;
	return false;
}

bool Time::operator<=(const Time& other)
{
	if (Seconds<other.Seconds|| (Seconds==other.Seconds && Fraction==other.Fraction)) return true;
	else if (Seconds==other.Seconds && Fraction<other.Fraction) return true;
	return false;
}

bool Time::operator>=(const Time& other)
{
	if (Seconds>other.Seconds || (Seconds==other.Seconds && Fraction==other.Fraction)) return true;
	else if (Seconds==other.Seconds && Fraction>other.Fraction) return true;
	return false;
}

bool Time::operator==(const Time& other)
{
	if (Seconds==other.Seconds && Fraction==other.Fraction) return true;
	return false;
}

void Time::Print() const
{
	cerr<<Seconds<<":"<<GetFraction()<<" ("<<Fraction<<")"<<endl;
}

