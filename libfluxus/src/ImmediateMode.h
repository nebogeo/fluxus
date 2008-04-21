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
 
#ifndef FLUX_IMMEDIATE_MODE
#define FLUX_IMMEDIATE_MODE

#include "Primitive.h"
#include "ShadowVolumeGen.h"
#include "State.h"

namespace Fluxus
{

//////////////////////////////////////////////////////
/// Immediate Mode
/// A store for immediate mode primitives, which we can
/// be given at any time, we keep pointers to them and 
/// render them all in one when the renderer is ready
class ImmediateMode
{
public:
	
	ImmediateMode();
	~ImmediateMode();
	
	void Add(Primitive *p, State *s);
    void Render(ShadowVolumeGen *shadowgen = NULL);
    void Clear();
	
private:
    struct IMItem
    {
    	State m_State;
    	Primitive *m_Primitive;
   	};
	
   	vector<IMItem*> m_IMRecord;
};

}

#endif
