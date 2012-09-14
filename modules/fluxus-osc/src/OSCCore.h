// Copyright (C) 2006 David Griffiths <dave@pawfal.org>
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

#ifndef FLUXUS_OSC_CORE
#define FLUXUS_OSC_CORE

#include <string>
#include <vector>

using namespace std;
namespace fluxus
{

class OSCData
{
	public:
	virtual ~OSCData() {}
	virtual char Type() { return '0'; }
};

class OSCInt : public OSCData
{
	public:
	OSCInt(int s) { Value=s; }
	virtual ~OSCInt() {}
	virtual char Type() { return 'i'; }
	int Value;
};

class OSCLong : public OSCData
{
	public:
	OSCLong(int s) { Value=s; }
	virtual ~OSCLong() {}
	virtual char Type() { return 'l'; }
	long int Value;
};

class OSCFloat : public OSCData
{
	public:
	OSCFloat(float s) { Value=s; }
	virtual ~OSCFloat() {}
	virtual char Type() { return 'f'; }
	float Value;
};

class OSCString : public OSCData
{
	public:
	OSCString(const string &s) { Value=s; }
	virtual ~OSCString() {}
	virtual char Type() { return 's'; }
	string Value;
};


class OSCMsgData
{
public:
	OSCMsgData() {}
	OSCMsgData(vector<OSCData*> a) : m_Data(a) {}
	~OSCMsgData();
	void Copy(const vector<OSCData*> &other);
	vector<OSCData*> m_Data;
};

ostream &operator<<(ostream &os, const OSCMsgData &msg);
istream &operator>>(istream &os, OSCMsgData &msg);

}

#endif
