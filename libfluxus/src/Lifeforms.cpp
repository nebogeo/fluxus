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

#include "Lifeforms.h"
#include "Renderer.h"
#include "float.h"

using namespace fluxus;

Lifeform::Lifeform()
{
}

Lifeform::~Lifeform()
{
}
	
//////////////////////////////////////////////////////////////////

Lifeforms::Lifeforms() :
m_Avoidance(4),
m_FlockCentering(1),
m_Inertia(0.99),
m_Centering(0.2),
m_MaxSpeed(1)
{
}

Lifeforms::~Lifeforms()
{
	for (map<int,Lifeform*>::iterator i=m_Population.begin(); i!=m_Population.end(); i++)
	{
		delete i->second;
	}
}

void Lifeforms::AddLifeform(int id, dMatrix m)
{
	Lifeform *e = new Lifeform;
	e->Position=m.transform(e->Position);
	e->Direction=m.transform_no_trans(e->Direction);
	m_Population[id]=e;
}

Lifeform *Lifeforms::GetNearest(Lifeform *o)
{
	float closest=FLT_MAX;
	Lifeform *ent=NULL;
	float dist;
    for (map<int,Lifeform*>::iterator i=m_Population.begin(); i!=m_Population.end(); i++)
	{
		dist=o->Position.dist(i->second->Position);
		
		if (o!=i->second && dist<closest)
		{
			closest=dist;
			ent=i->second;
		}
	}
	return ent;
}

dVector Lifeforms::GetCentre()
{
	dVector Centre;
    for (map<int,Lifeform*>::iterator i=m_Population.begin(); i!=m_Population.end(); i++)
	{
		Centre+=i->second->Position;
	}
	Centre/=m_Population.size();
	
	return Centre;
}

void Lifeforms::Update()
{
	dVector Centre=GetCentre();
	
	for (map<int,Lifeform*>::iterator i=m_Population.begin(); i!=m_Population.end(); i++)
	{
		Lifeform *closest = GetNearest(i->second);
		Lifeform *cur=i->second;
		dVector closestrepel;
		float repulsion=0;
		
		// if the are any other lifeforms, avoid them
		if (closest)
		{
			closestrepel=cur->Position-closest->Position;
			repulsion=1/closestrepel.mag();
			closestrepel.normalise();
			closestrepel*=m_Avoidance;
		}
		
		// calculate the flock centering vector
		dVector flockcentrevec=cur->Position-Centre;
		flockcentrevec.normalise();
		flockcentrevec*=m_FlockCentering;
		
		// calculate the scene centering vector
		dVector centrevec=cur->Position-m_Centre;
		centrevec.normalise();
		centrevec*=m_Centering;
		
		// calculate the final direction
		cur->Direction=(cur->Direction*m_Inertia)+
		               ((closestrepel*repulsion-(flockcentrevec+centrevec))*(1-m_Inertia));
		
		// clamp the speed
		if (cur->Direction.mag()>m_MaxSpeed)
		{
			cur->Direction.normalise();
			cur->Direction*=m_MaxSpeed;
		}
		
		cur->Position+=cur->Direction;	
		
		dMatrix *m = &m_Renderer->GetPrimitive(i->first)->GetState()->Transform;
		m->init();
		m->translate(cur->Position);
		m->aim(cur->Direction);
	}
}

