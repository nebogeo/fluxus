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

#include <assert.h>
#include "Engine.h"
#include "GraphicsUtils.h"
#include "PixelPrimitive.h"

using namespace Fluxus;

Engine Engine::m_Engine;

PolyPrimitive* Engine::StaticCube=NULL;
PolyPrimitive* Engine::StaticPlane=NULL;
PolyPrimitive* Engine::StaticSphere=NULL;
PolyPrimitive* Engine::StaticCylinder=NULL;
PolyPrimitive* Engine::StaticTorus=NULL;
PolyPrimitive* Engine::StaticTeapot=NULL;

Engine::Engine()
{
	StaticCube = new PolyPrimitive(PolyPrimitive::QUADS);
	MakeCube(StaticCube);

	StaticPlane = new PolyPrimitive(PolyPrimitive::QUADS);
	MakePlane(StaticPlane);

	StaticSphere = new PolyPrimitive(PolyPrimitive::TRILIST);
	MakeSphere(StaticSphere,1,5,10);

	StaticCylinder = new PolyPrimitive(PolyPrimitive::TRILIST);
	MakeCylinder(StaticCylinder,1,1,5,10);

	StaticTorus = new PolyPrimitive(PolyPrimitive::QUADS);
	MakeTorus(StaticTorus,0.5,1,12,12);

	StaticTeapot = new PolyPrimitive(PolyPrimitive::TRILIST);
	MakeTeapot(StaticTeapot);

	Fluxus::Renderer *renderer = new Fluxus::Renderer(true);
	Fluxus::Physics *physics = new Fluxus::Physics(renderer);
	PushRenderer(StackItem(renderer, physics));
}

Engine::~Engine()
{
	for (deque<StackItem>::iterator i=m_RendererStack.begin();
			i!=m_RendererStack.end(); ++i)
	{
		delete i->m_Renderer;
		delete i->m_Physics;
	}

	delete StaticCube;
	delete StaticPlane;
	delete StaticSphere;
	delete StaticCylinder;
	delete StaticTorus;
	delete StaticTeapot;
}

bool Engine::PushRenderer(const StackItem &si)
{
	m_RendererStack.push_back(si);
	ClearGrabStack();
	return true;
}

void Engine::PopRenderer()
{
	// make sure we don't pop the default renderer
	if (m_RendererStack.size()>1)
	{
		m_RendererStack.pop_back();
		ClearGrabStack();
	}
}

Engine::StackItem *Engine::StackTop()
{
	// the renderer stack should never be empty
	assert(!m_RendererStack.empty());
	return &(*m_RendererStack.rbegin());
}

Renderer *Engine::Renderer()
{
	return StackTop()->m_Renderer;
}

Physics *Engine::Physics()
{
	return StackTop()->m_Physics;
}
void Engine::Render()
{
	Renderer()->Render();
}

void Engine::Reinitialise()     
{
	Renderer()->Reinitialise();    
	Renderer()->GetTexturePainter()->Initialise();
	Renderer()->ClearLights();
}

// todo - move the grabstack into the renderer?
void Engine::PushGrab(int id)
{	
	// id of zero means this is a state push - but we 
	// still want to record these here, as then we 
	// can interleave the state and primitive push/pops
	if (id==0) 
	{
		StackTop()->m_Grabbed=NULL;
		Renderer()->UnGrab();
		StackTop()->m_GrabStack.push_front(id);
		return;
	}
	
	StackTop()->m_Grabbed=Renderer()->GetPrimitive(id);
	
	if (StackTop()->m_Grabbed)
	{
		StackTop()->m_GrabStack.push_front(id);
		Renderer()->Grab(id);
	}
	else
	{
		Trace::Stream<<"grab: primitive id "<<id<<" not found"<<endl;
	}
}

void Engine::PopGrab()
{
	StackTop()->m_Grabbed=NULL;
	if (!StackTop()->m_GrabStack.empty())
	{
		Renderer()->UnGrab();
		StackTop()->m_GrabStack.pop_front();
		if (!StackTop()->m_GrabStack.empty() && 
			StackTop()->m_GrabStack[0]>0)
		{
			StackTop()->m_Grabbed=Renderer()->GetPrimitive(*StackTop()->m_GrabStack.begin());
			Renderer()->Grab(*StackTop()->m_GrabStack.begin());
		}
	}
}
	
unsigned int Engine::GrabbedID()
{
	if (!StackTop()->m_GrabStack.empty())
	{
		return *StackTop()->m_GrabStack.begin();
	}
	return 0;
}

bool Engine::GrabCamera(unsigned int cam)
{
	if (cam<Renderer()->GetCameraVec().size())
	{
		StackTop()->m_CurrentCamera=cam;
		return true;
	}
	return false;
}

Camera *Engine::GetCamera()
{
	assert(StackTop()->m_CurrentCamera<Renderer()->GetCameraVec().size());	
	return &Renderer()->GetCameraVec()[StackTop()->m_CurrentCamera];
}

void Engine::ClearGrabStack()
{
	StackTop()->m_GrabStack.clear();
	StackTop()->m_Grabbed=NULL;
	StackTop()->m_CurrentCamera=0;
}

Fluxus::State *Engine::State()
{
    if (Grabbed()) 
	{
		return Grabbed()->GetState();
	}
	return Renderer()->GetState();
}

