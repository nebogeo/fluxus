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

#include "Allocator.h"
#include "Renderer.h"
#include "Camera.h"
#include "Physics.h"
#include "PolyPrimitive.h"
#include "TurtleBuilder.h"
#include "PFuncContainer.h"

#ifndef FLUXUS_EENGINE
#define FLUXUS_EENGINE

class Engine
{
public:
	Engine();
	~Engine();
	
	static Engine *Get() 
	{
		return &m_Engine;
	}
	
	class StackItem
	{
	public:
		StackItem(Fluxus::Renderer *r, Fluxus::Physics *p) :
		m_Renderer(r),
		m_Physics(p),
		m_Grabbed(NULL),
		m_CurrentCamera(0)
		{}
			
		Fluxus::Renderer *m_Renderer;
		Fluxus::Physics *m_Physics;
		deque<unsigned int> m_GrabStack;
		Fluxus::Primitive *m_Grabbed;
		unsigned int m_CurrentCamera;
	};	
	
	bool PushRenderer(const StackItem &si);
	void PopRenderer();
	void Reinitialise();  
	Fluxus::Renderer *Renderer();
	Fluxus::Physics *Physics();
	void Render(); 

	void PushGrab(int id);
	void PopGrab();
	void ClearGrabStack();
	Fluxus::Primitive *Grabbed() { return m_RendererStack.rbegin()->m_Grabbed; }
	unsigned int GrabbedID();
	
	bool GrabCamera(unsigned int cam);
	unsigned int GrabbedCamera() { return m_RendererStack.rbegin()->m_CurrentCamera; }
	Fluxus::Camera *GetCamera();
	
	static Fluxus::PolyPrimitive* StaticCube;
	static Fluxus::PolyPrimitive* StaticPlane;
	static Fluxus::PolyPrimitive* StaticSphere;
	static Fluxus::PolyPrimitive* StaticCylinder;
	static Fluxus::PolyPrimitive* StaticTorus;
	static Fluxus::PolyPrimitive* StaticTeapot;

	Fluxus::TurtleBuilder *GetTurtle() { return &m_Turtle; }
	Fluxus::PFuncContainer *GetPFuncContainer() { return &m_PFuncContainer; }

	// helper for the bindings
	Fluxus::State *State();

private:
	
	static Engine m_Engine;
	
	StackItem *StackTop();
		
	deque<StackItem> m_RendererStack;
	Fluxus::TurtleBuilder m_Turtle;
	Fluxus::PFuncContainer m_PFuncContainer;
};

#endif
