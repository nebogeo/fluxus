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
#include "Primitive.h"

#ifndef N_COMPPRIM
#define N_COMPPRIM

namespace fluxus
{

class CompiledPrimitive : public Primitive
{
public:	
	CompiledPrimitive(const Renderer::LibraryEntry &entry);
	virtual  ~CompiledPrimitive();
	
	virtual void Render();
	virtual dBoundingBox GetBoundingBox();
	virtual string GetTypeName() { return "CompiledPrimitive"; }
	virtual void ApplyTransform(bool ScaleRotOnly);

protected:
	int m_ID;
	dBoundingBox m_BBox;
	
	friend istream &operator>>(istream &s, CompiledPrimitive &o);
	friend ostream &operator<<(ostream &s, CompiledPrimitive &o);
};

istream &operator>>(istream &s, CompiledPrimitive &o);
ostream &operator<<(ostream &s, CompiledPrimitive &o);

};

#endif
