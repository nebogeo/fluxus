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

using namespace fluxus;

// assumes font has chars from ascii 32 onwards:
//   ! " # $ % & ' ( ) * + ' - . / 
// 0 1 2 3 4 5 6 7 8 9 : ; < = > ?
// @ A B C D E F G H I J K L M N O
// P Q R S T U V W X Y Z [ \ ] ^ _
// ` a b c d e f g h i j k l m n o
// p q r s t u v w x y z { | } ~
	
TextPrimitive::TextPrimitive(float charw, float charh, int charstride, int wrapchars) :
PolyPrimitive(PolyPrimitive::QUADS),
m_CharWidth(charw),
m_CharHeight(charh),
m_CharStride(charstride),
m_WrapChars(wrapchars)
{
}

void TextPrimitive::SetText(const string &s, float Width, float Height)
{
	float x=0,y=0;
	dVector Normal(0,0,1);
	
	float w=m_CharWidth*Width;
	float h=m_CharHeight*Height;
	
	m_TextWidth=w*s.size();
	m_TextHeight=h;
	int wrapcount=0;
	
	for (unsigned int n=0; n<s.size(); n++)
	{
		if (s[n]>=32 && s[n]<127)
		{
			int pos=(int)s[n]-32;
		
			float s=(pos%m_CharStride)*m_CharWidth;
			float t=(pos/m_CharStride)*m_CharHeight;

			AddVertex(dVertex(dVector(x,y,0),Normal,s,t));
			AddVertex(dVertex(dVector(x+w,y,0),Normal,s+m_CharWidth,t));
			AddVertex(dVertex(dVector(x+w,y+h,0),Normal,s+m_CharWidth,t+m_CharHeight));
			AddVertex(dVertex(dVector(x,y+h,0),Normal,s,t+m_CharHeight));
			if (m_WrapChars) wrapcount++;
		}
		
		if (s[n]=='\n' || wrapcount>m_WrapChars)
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

istream &fluxus::operator>>(istream &s, TextPrimitive &o)
{
	return s;
}

ostream &fluxus::operator<<(ostream &s, TextPrimitive &o)
{
	return s;
}
