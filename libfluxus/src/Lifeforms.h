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

#include "dada.h"
#include <map>
#include "Renderer.h"

#ifndef LIFEFORMS
#define LIFEFORMS

namespace fluxus
{

class Lifeform
{
public:
	Lifeform();
	~Lifeform();
	
	dVector Direction;
	dVector Position;
};

class Lifeforms
{
public:
	Lifeforms();
	~Lifeforms();
	
	void RegisterRenderer(Renderer* s) { m_Renderer=s; }
	void AddLifeform(int id, dMatrix m);
	void Update();
	dVector GetCentre();
	
	void SetAvoidance(float s) { m_Avoidance=s; }
	void SetFlockCentering(float s) { m_FlockCentering=s; }
	void SetSceneCentering(float s) { m_Centering=s; }
	void SetSceneCentre(dVector s) { m_Centre=s; }
	void SetInertia(float s) { m_Inertia=s; }
	void SetMaxSpeed(float s) { m_MaxSpeed=s; }

private:
	Lifeform *GetNearest(Lifeform *o);
	Renderer* m_Renderer;
	
	float   m_Avoidance;
	float   m_FlockCentering;
	float   m_Inertia;
	float   m_Centering;
	float   m_MaxSpeed;
	dVector m_Centre;
	
	map<int,Lifeform*> m_Population;
};

}

#endif
