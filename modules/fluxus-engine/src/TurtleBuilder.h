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

#include <deque>
#include "PolyPrimitive.h"
#include "Renderer.h"

#ifndef N_TURTLE_BUILDER
#define N_TURTLE_BUILDER

namespace Fluxus
{

class TurtleBuilder
{
public:
	TurtleBuilder();
	~TurtleBuilder() {}
	
	void Initialise();
	
	void Prim(int Type=1);
	int Build(Renderer *renderer);
	
	void Attach(PolyPrimitive *p);
	
	void Vert();
	void Move(float d);
	void Turn(dVector a);
	void Reset();
	void Push();
	void Pop();
	int Position() { return m_Position; }
	void SetPosition(unsigned int s) { m_Position=s; }
	
	// for attached mode only
	void Skip(int n);
	
	
private:
	
	PolyPrimitive* m_BuildingPrim;	
	vector<dVector> *m_AttachedPoints;
	unsigned int m_Position;
	
	struct State
	{
		dVector m_Pos;
		dVector m_Rot;
		dColour m_Colour;
	};
	
	deque<State> m_State;
};

}

#endif
