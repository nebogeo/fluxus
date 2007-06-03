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
m_WrapChars(wrapchars)
{
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
		
	for (unsigned int n=0; n<s.size(); n++)
	{
		int pos=(int)s[n];

		float S=(pos%m_CharStride)*m_CharWidth;
		float T=(pos/m_CharStride)*m_CharHeight;
		
		dVector min(S,T,0);
		dVector max(S+m_CharWidth,T+m_CharHeight,0);
				
		min.x+=Zoom;
		max.x-=Zoom;
		
		AddVertex(dVertex(dVector(x,y,0),Normal,min.x,min.y));
		AddVertex(dVertex(dVector(x+w,y,0),Normal,max.x,min.y));
		AddVertex(dVertex(dVector(x+w,y+h,0),Normal,max.x,max.y));
		AddVertex(dVertex(dVector(x,y+h,0),Normal,min.x,max.y));
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
			x+=w;
		}
	}
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
