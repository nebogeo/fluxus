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

#include "DepthSorter.h"

using namespace Fluxus;

DepthSorter::DepthSorter()
{
}

DepthSorter::~DepthSorter()
{
}

void DepthSorter::Clear()
{
	m_RenderList.clear();
}

void DepthSorter::Add(const dMatrix &globaltransform, Primitive *prim, int id)
{
	Item item;
	item.Prim=prim;
	item.GlobalTransform=globaltransform;
	item.ID=id;
	
	dMatrix all=globaltransform*prim->GetState()->Transform;
	dVector pos=all.transform(dVector(0,0,0));
	item.Depth=pos.z;
	m_RenderList.push_back(item);
}

void DepthSorter::Render()
{
	m_RenderList.sort();

	for(list<Item>::iterator i=m_RenderList.begin(); i!=m_RenderList.end(); i++)
	{
		glPushMatrix();	
		glPushName(i->ID);
		glLoadIdentity();
		glMultMatrixf(i->GlobalTransform.arr());
		i->Prim->ApplyState();
		i->Prim->Prerender();
		i->Prim->Render();
		glPopName();
		glPopMatrix();
	}
}
