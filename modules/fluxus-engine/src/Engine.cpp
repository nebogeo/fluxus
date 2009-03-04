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

Engine *Engine::m_Engine=NULL;

PolyPrimitive*  Engine::StaticCube=NULL;
PolyPrimitive*  Engine::StaticPlane=NULL;
PolyPrimitive*  Engine::StaticSphere=NULL;
PolyPrimitive*  Engine::StaticCylinder=NULL;
PolyPrimitive*  Engine::StaticTorus=NULL;

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

	Fluxus::Renderer *renderer = new Fluxus::Renderer();
	Fluxus::Physics *physics = new Fluxus::Physics(renderer);
	PushRenderer(StackItem(renderer, physics));
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

Renderer *Engine::Renderer()
{
	// the renderer stack should never be empty
	assert(!m_RendererStack.empty());
	return m_RendererStack.rbegin()->m_Renderer;
}

Physics *Engine::Physics()
{
	// the renderer stack should never be empty
	assert(!m_RendererStack.empty());
	return m_RendererStack.rbegin()->m_Physics;
}
void Engine::Render()
{
	Renderer()->Render();
}

void Engine::Reinitialise()     
{
	m_RendererStack.rbegin()->m_Renderer->Reinitialise();    
	m_RendererStack.rbegin()->m_Renderer->GetTexturePainter()->Initialise();
	m_RendererStack.rbegin()->m_Renderer->ClearLights();
}

// todo - move the grabstack into the renderer?
void Engine::PushGrab(int id)
{	
	// id of zero means this is a state push - but we 
	// still want to record these here, as then we 
	// can interleave the state and primitive push/pops
	if (id==0) 
	{
		m_RendererStack.rbegin()->m_Grabbed=NULL;
		Renderer()->UnGrab();
		m_RendererStack.rbegin()->m_GrabStack.push_front(id);
		return;
	}
	
	m_RendererStack.rbegin()->m_Grabbed=Renderer()->GetPrimitive(id);
	
	if (m_RendererStack.rbegin()->m_Grabbed)
	{
		m_RendererStack.rbegin()->m_GrabStack.push_front(id);
		Renderer()->Grab(id);
	}
	else
	{
		Trace::Stream<<"grab: primitive id "<<id<<" not found"<<endl;
	}
}

void Engine::PopGrab()
{
	m_RendererStack.rbegin()->m_Grabbed=NULL;
	if (!m_RendererStack.rbegin()->m_GrabStack.empty())
	{
		Renderer()->UnGrab();
		m_RendererStack.rbegin()->m_GrabStack.pop_front();
		if (!m_RendererStack.rbegin()->m_GrabStack.empty() && 
			m_RendererStack.rbegin()->m_GrabStack[0]>0)
		{
			m_RendererStack.rbegin()->m_Grabbed=Renderer()->GetPrimitive(*m_RendererStack.rbegin()->m_GrabStack.begin());
			Renderer()->Grab(*m_RendererStack.rbegin()->m_GrabStack.begin());
		}
	}
}
	
unsigned int Engine::GrabbedID()
{
	if (!m_RendererStack.rbegin()->m_GrabStack.empty())
	{
		return *m_RendererStack.rbegin()->m_GrabStack.begin();
	}
	return 0;
}

bool Engine::GrabCamera(unsigned int cam)
{
	if (cam<Renderer()->GetCameraVec().size())
	{
		m_RendererStack.rbegin()->m_CurrentCamera=cam;
		return true;
	}
	return false;
}

Camera *Engine::GetCamera()
{
	assert(m_RendererStack.rbegin()->m_CurrentCamera<Renderer()->GetCameraVec().size());
	return &Renderer()->GetCameraVec()[m_RendererStack.rbegin()->m_CurrentCamera];
}

void Engine::ClearGrabStack()
{
	m_RendererStack.rbegin()->m_GrabStack.clear();
	m_RendererStack.rbegin()->m_Grabbed=NULL;
	m_RendererStack.rbegin()->m_CurrentCamera=0;
}

Fluxus::State *Engine::State()
{
    if (Grabbed()) 
	{
		return Grabbed()->GetState();
	}
	return Renderer()->GetState();
}

