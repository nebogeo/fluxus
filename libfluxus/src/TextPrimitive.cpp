// Copyright (C) 2005 Dave Griffiths
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

#include "Renderer.h"
#include "TextPrimitive.h"
#include "State.h"

using namespace Fluxus;
	
TextPrimitive::TextPrimitive(float charw, float charh, int charstride, int wrapchars) :
PolyPrimitive(PolyPrimitive::QUADS),
m_CharWidth(charw),
m_CharHeight(charh),
m_CharStride(charstride),
m_WrapChars(wrapchars),
m_XOff(0),
m_YOff(0),
m_Crowd(0)
{
}

TextPrimitive::TextPrimitive(const TextPrimitive &other) :
PolyPrimitive(other),
m_CharWidth(other.m_CharWidth),
m_CharHeight(other.m_CharHeight),
m_CharStride(other.m_CharStride),
m_TextWidth(other.m_TextWidth),
m_TextHeight(other.m_TextHeight),
m_WrapChars(other.m_WrapChars),
m_XOff(other.m_XOff),
m_YOff(other.m_YOff),
m_Crowd(other.m_Crowd)
{
}

TextPrimitive* TextPrimitive::Clone() const 
{
	return new TextPrimitive(*this); 
}

void TextPrimitive::SetText(const string &s, float Width, float Height, float Zoom)
{
	float x=0,y=0;
	dVector Normal(0,0,1);
	
	float w=m_CharWidth*Width;
	float h=m_CharHeight*Height;
	
	m_TextWidth=w*s.size();
	m_TextHeight=h;
	int wrapcount=0;
		
	w-=Zoom*50; //??? some constant scaling to covert from texture 
                // coordinates to world space

	Clear();	

	for (unsigned int n=0; n<s.size(); n++)
	{
		int pos=(int)s[n];

		float S=(pos%m_CharStride)*m_CharWidth+m_XOff;
		float T=(pos/m_CharStride)*m_CharHeight+m_YOff;
		
		dVector min(S,T,0);
		dVector max(S+m_CharWidth,T+m_CharHeight,0);
				
		min.x+=Zoom;
		max.x-=Zoom;
		
		AddVertex(dVertex(dVector(x,y,0),Normal,min.x,1-min.y));
		AddVertex(dVertex(dVector(x+w,y,0),Normal,max.x,1-min.y));
		AddVertex(dVertex(dVector(x+w,y+h,0),Normal,max.x,1-max.y));
		AddVertex(dVertex(dVector(x,y+h,0),Normal,min.x,1-max.y));
		if (m_WrapChars) wrapcount++;
		
		if (s[n]=='\n' || (m_WrapChars && wrapcount>m_WrapChars))
		{
			y+=h;
			m_TextHeight+=h;
			x=0;
			wrapcount=0;
		}
		else
		{
			x+=(w - m_Crowd);
		}
	}
}

void TextPrimitive::SetTextParams(float w, float h, int stride, int wrap, float xoff, float yoff, float crowd)
{
	m_CharWidth=w;
	m_CharHeight=h;
	m_CharStride=stride;
	m_WrapChars=wrap;
	m_XOff=xoff;
	m_YOff=yoff;
	m_Crowd=crowd;
}

void TextPrimitive::Render()
{
	glDisable(GL_CULL_FACE);
	PolyPrimitive::Render();
	glEnable(GL_CULL_FACE);
}

istream &Fluxus::operator>>(istream &s, TextPrimitive &o)
{
	return s;
}

ostream &Fluxus::operator<<(ostream &s, TextPrimitive &o)
{
	return s;
}
