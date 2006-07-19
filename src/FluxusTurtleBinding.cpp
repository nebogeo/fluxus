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

#include <fstream>
#include <deque>
#include <libguile.h>
#include "FluxusTurtleBinding.h"
#include "SearchPaths.h"
#include "Repl.h"
#include <LinePrimitive.h>
#include <ParticlePrimitive.h>
#include <PixelPrimitive.h>
#include <LocatorPrimitive.h>

void FluxusTurtleBinding::RegisterProcs()
{
	SchemePrim::Init();
	
	// turtle
	scm_c_define_gsubr("turtle-vert",0,0,0,(CALLBACK_CAST)  		  turtle_vert);
	scm_c_define_gsubr("turtle-build",0,0,0,(CALLBACK_CAST) 		  turtle_build);
	scm_c_define_gsubr("turtle-reset",0,0,0,(CALLBACK_CAST) 		  turtle_reset);
	scm_c_define_gsubr("turtle-push",0,0,0,(CALLBACK_CAST)  		turtle_push);
	scm_c_define_gsubr("turtle-pop",0,0,0,(CALLBACK_CAST)			turtle_pop);
	scm_c_define_gsubr("turtle-move",1,0,0,(CALLBACK_CAST)  	   turtle_move);
	scm_c_define_gsubr("turtle-turn",1,0,0,(CALLBACK_CAST)  	   turtle_turn);
	scm_c_define_gsubr("turtle-prim",1,0,0,(CALLBACK_CAST)  	   turtle_prim);
	scm_c_define_gsubr("turtle-attach",1,0,0,(CALLBACK_CAST)  	   turtle_attach);
	scm_c_define_gsubr("turtle-skip",1,0,0,(CALLBACK_CAST)  	   turtle_skip);
	scm_c_define_gsubr("turtle-position",0,0,0,(CALLBACK_CAST)  	   turtle_position);
}



SCM FluxusTurtleBinding::turtle_prim(SCM type)
{
	SCM_ASSERT(scm_is_number(type), type, SCM_ARG1, "turtle-prim");
	turtle.Prim(scm_to_int(type));
	return SCM_UNSPECIFIED;
}

SCM FluxusTurtleBinding::turtle_vert()
{
	turtle.Vert();
	return SCM_UNSPECIFIED;
}

SCM FluxusTurtleBinding::turtle_build()
{
	return Prim2Smob(turtle.Build(Fluxus->GetRenderer()));
}

SCM FluxusTurtleBinding::turtle_move(SCM dist)
{
	SCM_ASSERT(scm_is_number(dist), dist, SCM_ARG1, "turtle-move");
	turtle.Move(scm_to_double(dist));
	return SCM_UNSPECIFIED;
}

SCM FluxusTurtleBinding::turtle_push()
{
	turtle.Push();
	return SCM_UNSPECIFIED;
}

SCM FluxusTurtleBinding::turtle_pop()
{
	turtle.Pop();
	return SCM_UNSPECIFIED;
}

SCM FluxusTurtleBinding::turtle_turn(SCM s_vec)
{
    SCM_ASSERT(scm_is_generalized_vector(s_vec), s_vec, SCM_ARG1, "turtle-turn");
	SCM_ASSERT(scm_c_generalized_vector_length(s_vec)==3, s_vec, SCM_ARG1, "turtle-turn");
	float rot[3];
	flx_floats_from_scm(s_vec,rot);
	turtle.Turn(dVector(rot[0],rot[1],rot[2]));
	return SCM_UNSPECIFIED;	
}

SCM FluxusTurtleBinding::turtle_reset()
{
	turtle.Reset();
	return SCM_UNSPECIFIED;	
}

SCM FluxusTurtleBinding::turtle_attach(SCM s_id)
{
	PolyPrimitive *poly = dynamic_cast<PolyPrimitive*>(Fluxus->GetRenderer()->GetPrimitive(Smob2Prim(s_id)));
	if (poly)
	{
		turtle.Attach(poly);
	}
	else
	{
		cerr<<"turtle-attach only works on polys"<<endl;
	}
	
	return SCM_UNSPECIFIED;	
}

SCM FluxusTurtleBinding::turtle_skip(SCM s_count)
{
	turtle.Skip(scm_to_int(s_count));
	return SCM_UNSPECIFIED;	
}

SCM FluxusTurtleBinding::turtle_position()
{
	return scm_from_int(turtle.Position());
}
