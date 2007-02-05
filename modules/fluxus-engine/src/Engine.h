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
	fluxus::Renderer *Renderer();
	fluxus::Physics *Physics();
	void BeginScene(); 
	void EndScene(); 
	
	void PushGrab(int id);
	void PopGrab();
	void ClearGrabStack();
	fluxus::Primitive *Grabbed() { return m_Grabbed; }
	
	static fluxus::PolyPrimitive* StaticCube;
	static fluxus::PolyPrimitive* StaticPlane;
	static fluxus::PolyPrimitive* StaticSphere;
	static fluxus::PolyPrimitive* StaticCylinder;
	
	fluxus::TurtleBuilder *GetTurtle() { return &m_Turtle; }
	
	// helper for the bindings
	fluxus::State *State();

private:
	Engine();
	~Engine();
	
	static Engine *m_Engine;
	
	vector<pair<fluxus::Renderer *, fluxus::Physics *> > m_RendererVec;
	deque<unsigned int> m_RendererStack;
	deque<unsigned int> m_GrabStack;
	fluxus::Primitive *m_Grabbed;
	fluxus::TurtleBuilder m_Turtle;
};
