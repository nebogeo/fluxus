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

#include "FluxusBinding.h"

#ifndef FLUXUS_TURTLE_BINDING
#define FLUXUS_TURTLE_BINDING

class FluxusTurtleBinding : public FluxusBinding
{
public:
	void RegisterProcs();
	
	static SCM turtle_prim(SCM type);
	static SCM turtle_vert();
	static SCM turtle_build();
	static SCM turtle_move(SCM dist);
	static SCM turtle_turn(SCM s_vec);
	static SCM turtle_reset();
	static SCM turtle_push();
	static SCM turtle_pop();
	static SCM turtle_attach(SCM s_obj);
	static SCM turtle_skip(SCM s_count);
	static SCM turtle_position();

};
#endif
