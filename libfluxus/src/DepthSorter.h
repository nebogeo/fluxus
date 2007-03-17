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


// Generates a shadow volume poly primitive for the supplied 
// primitives and light position

// Can we accelerate the process by caching as much as possible, and 
// changing only when light positions/primitive transforms/deform?

#include "Primitive.h"

#ifndef N_DEPTHSORTER
#define N_DEPTHSORTER

namespace fluxus
{

class DepthSorter
{
public:
	DepthSorter();
	~DepthSorter();	

	void Clear();
	void Add(const dMatrix &globaltransform, Primitive *prim, int id);
	void Render();
	
private:

	class Item
	{
	public:
		Primitive *Prim;
		dMatrix GlobalTransform;
		float Depth;
		int ID;
		
		bool operator<(const Item &other) const
		{
			return Depth<other.Depth;
		}
	};

	list<Item> m_RenderList;
};

};

#endif
