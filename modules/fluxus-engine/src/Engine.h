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

#include "Renderer.h"
#include "Physics.h"
#include "PolyPrimitive.h"
#include "TurtleBuilder.h"
#include "PFuncContainer.h"

class Engine
{
public:
	static Engine *Get() 
	{
		if (!m_Engine) m_Engine=new Engine;
		return m_Engine;
	}
	
	unsigned int MakeRenderer();
	bool PushRenderer(unsigned int);
	void PopRenderer();
	void ResetRenderers();
	void Reinitialise();  
	Fluxus::Renderer *Renderer();
	Fluxus::Physics *Physics();
	void Render(); 
	
	void PushGrab(int id);
	void PopGrab();
	void ClearGrabStack();
	Fluxus::Primitive *Grabbed() { return m_Grabbed; }
	
	static Fluxus::PolyPrimitive* StaticCube;
	static Fluxus::PolyPrimitive* StaticPlane;
	static Fluxus::PolyPrimitive* StaticSphere;
	static Fluxus::PolyPrimitive* StaticCylinder;
	static Fluxus::PolyPrimitive* StaticTorus;
	
	Fluxus::TurtleBuilder *GetTurtle() { return &m_Turtle; }
	Fluxus::PFuncContainer *GetPFuncContainer() { return &m_PFuncContainer; }
	
	// helper for the bindings
	Fluxus::State *State();

private:
	Engine();
	~Engine();
	
	static Engine *m_Engine;
	
	vector<pair<Fluxus::Renderer *, Fluxus::Physics *> > m_RendererVec;
	deque<unsigned int> m_RendererStack;
	deque<unsigned int> m_GrabStack;
	Fluxus::Primitive *m_Grabbed;
	Fluxus::TurtleBuilder m_Turtle;
	Fluxus::PFuncContainer m_PFuncContainer;
};
