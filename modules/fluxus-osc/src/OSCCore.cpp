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

#include <cstdio>
#include <cstdlib>
#include <unistd.h>
#include <iostream>

#include "OSCCore.h"

using namespace std;
using namespace fluxus;

OSCMsgData::~OSCMsgData() 
{
	for (vector<OSCData*>::iterator arg=m_Data.begin(); arg!=m_Data.end(); arg++)
	{
		delete *arg;
	}
}

void OSCMsgData::Copy(const vector<OSCData*> &other)
{
	for (vector<OSCData*>::const_iterator i=other.begin(); i!=other.end(); ++i)
	{
		switch((*i)->Type())
		{
			case 's': m_Data.push_back(new OSCString(static_cast<OSCString*>(*i)->Value)); break;
			case 'f': m_Data.push_back(new OSCFloat(static_cast<OSCFloat*>(*i)->Value)); break;
			case 'i': m_Data.push_back(new OSCInt(static_cast<OSCInt*>(*i)->Value)); break;
			case 'l': m_Data.push_back(new OSCLong(static_cast<OSCInt*>(*i)->Value)); break;
		}
	}
}

ostream &fluxus::operator<<(ostream &os, const OSCMsgData &msg)
{
	int version=1;
	os<<version<<" ";;
	
	// build a format string
	string format("");
	for (vector<OSCData*>::const_iterator i=msg.m_Data.begin(); i!=msg.m_Data.end(); ++i)
	{
		format+=(*i)->Type();
	}
	os<<format<<" ";
	
	for (vector<OSCData*>::const_iterator i=msg.m_Data.begin(); i!=msg.m_Data.end(); ++i)
	{					
		switch((*i)->Type())
		{
			case 's': os<<static_cast<OSCString*>(*i)->Value<<" "; break;
			case 'f': os<<static_cast<OSCFloat*>(*i)->Value<<" "; break;
			case 'i': os<<static_cast<OSCInt*>(*i)->Value<<" "; break;
			case 'l': os<<static_cast<OSCLong*>(*i)->Value<<" "; break;
			default: break;
		}
	} 
	return os;
}

istream &fluxus::operator>>(istream &is, OSCMsgData &msg)
{	
	int version;
	is>>version;
	
	string format("");
	is>>format;
	
	for (string::iterator i=format.begin(); i!=format.end(); ++i)
	{					
		switch(*i)
		{
			case 's': 
			{
				string value; 
				is>>value; 
				msg.m_Data.push_back(new OSCString(value)); 
			}
			break;
			case 'f': 
			{
				float value; 
				is>>value; 
				msg.m_Data.push_back(new OSCFloat(value));  
			}
			break;
			case 'i':
			{
				int value; 
				is>>value; 
				msg.m_Data.push_back(new OSCInt(value)); 
			}
			break;
			case 'l':
			{
				long int value; 
				is>>value; 
				msg.m_Data.push_back(new OSCLong(value)); 
			}
			break;
			default: break;
		}
	} 
	return is;
}


