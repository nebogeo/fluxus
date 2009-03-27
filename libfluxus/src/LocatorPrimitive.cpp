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
#include "LocatorPrimitive.h"
#include "State.h"

using namespace Fluxus;
	
LocatorPrimitive::LocatorPrimitive() :
m_BoundingBoxRadius(0.5f)
{
}

LocatorPrimitive::LocatorPrimitive(const LocatorPrimitive &other) :
Primitive(other) 
{
}

LocatorPrimitive::~LocatorPrimitive()
{
}

LocatorPrimitive* LocatorPrimitive::Clone() const 
{
	return new LocatorPrimitive(*this); 
}

void LocatorPrimitive::PDataDirty()
{	
}

void LocatorPrimitive::Render()
{
}

///\todo need some way of setting a locator's bounding volume
dBoundingBox LocatorPrimitive::GetBoundingBox(const dMatrix &space)
{	
	return dBoundingBox(space.transform(dVector(-m_BoundingBoxRadius,-m_BoundingBoxRadius,-m_BoundingBoxRadius)),
						space.transform(dVector(m_BoundingBoxRadius,m_BoundingBoxRadius,m_BoundingBoxRadius)));
}

void LocatorPrimitive::ApplyTransform(bool ScaleRotOnly)
{
	GetState()->Transform.init();
}
