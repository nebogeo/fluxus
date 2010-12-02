// Copyright (C) 2007 Dave Griffiths
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

#include "TurtleBuilder.h"

using namespace Fluxus;

TurtleBuilder::TurtleBuilder() :
m_BuildingPrim(NULL),
m_AttachedPoints(NULL),
m_Position(0)
{
	Reset();
}

void TurtleBuilder::Reset()
{
	m_State.clear();
	Push();
	m_State.begin()->m_Pos=dVector(0,0,0);
	m_State.begin()->m_Rot=dVector(0,0,0);
	m_State.begin()->m_Colour=dColour(1,1,1);
	m_Position=0;
}

void TurtleBuilder::Initialise()
{
	if(m_BuildingPrim) delete m_BuildingPrim;
	m_AttachedPoints=NULL;
	m_BuildingPrim=NULL;
	m_Position=0;
}

void TurtleBuilder::Prim(PolyPrimitive::Type Type)
{
	Initialise();
	m_BuildingPrim=new PolyPrimitive(Type);
}

void TurtleBuilder::Attach(PolyPrimitive *p)
{
	Initialise();
	TypedPData<dVector> *points = dynamic_cast<TypedPData<dVector>* >(p->GetDataRaw("p"));
	m_AttachedPoints = &points->m_Data;
}


void TurtleBuilder::Vert()
{
	if (m_BuildingPrim)
	{
		m_BuildingPrim->AddVertex(dVertex(m_State.begin()->m_Pos,dVector(0,1,0)));
	}
	else if (m_AttachedPoints && !m_AttachedPoints->empty() )
	{
		(*m_AttachedPoints)[m_Position%m_AttachedPoints->size()]=m_State.begin()->m_Pos;
	}

	m_Position++;
}

void TurtleBuilder::Skip(int n)
{
	m_Position+=n;
}

int TurtleBuilder::Build(Renderer *renderer)
{
	if (m_BuildingPrim)
	{
		int id = renderer->AddPrimitive(m_BuildingPrim);
		m_BuildingPrim=NULL;
		return id;
	}
	return -1;
}

void TurtleBuilder::Move(float d)
{
	dVector offset(d,0,0);
	dMatrix mat;
	mat.rotxyz(m_State.begin()->m_Rot.x,m_State.begin()->m_Rot.y,m_State.begin()->m_Rot.z);
	offset=mat.transform(offset);
	m_State.begin()->m_Pos+=offset;
}

void TurtleBuilder::Turn(dVector a)
{
	m_State.begin()->m_Rot+=a;
}

void TurtleBuilder::Push()
{
	if (m_State.size()<1)
	{
		State state;
		m_State.push_front(state);
	}
	else
	{
		m_State.push_front(*m_State.begin());
	}
}

void TurtleBuilder::Pop()
{
	if (m_State.size()>1);
	{
		m_State.pop_front();
	}
}

dMatrix TurtleBuilder::GetTransform()
{
	dMatrix m;
	m.rotxyz(m_State.begin()->m_Rot.x,
			 m_State.begin()->m_Rot.y,
			 m_State.begin()->m_Rot.z);
	m.settranslate(m_State.begin()->m_Pos);
	return m;
}

