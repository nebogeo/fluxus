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

#include "PolyPrimitive.h"

#ifndef N_TEXTPRIM
#define N_TEXTPRIM

namespace fluxus
{

class TextPrimitive : public PolyPrimitive
{
public:
	// charw,h are in _texture_ coords not pixels
	TextPrimitive(float charw, float charh, int charstride, int wrapchars=0);
	virtual ~TextPrimitive() {}
	virtual void Render();
	virtual string GetTypeName() { return "TextPrimitive"; }
	
	void SetText(const string &s, float Width=10, float Height=10, float Zoom=0);
	float GetTextWidth() { return m_TextWidth; }
	float GetTextHeight() { return m_TextHeight; }
	
protected:
	
	float m_CharWidth;
	float m_CharHeight;
	int m_CharStride;
	float m_TextWidth;
	float m_TextHeight;
	int m_WrapChars;
	
	friend istream &operator>>(istream &s, TextPrimitive &o);
	friend ostream &operator<<(ostream &s, TextPrimitive &o);
};

istream &operator>>(istream &s, TextPrimitive &o);
ostream &operator<<(ostream &s, TextPrimitive &o);

};

#endif
