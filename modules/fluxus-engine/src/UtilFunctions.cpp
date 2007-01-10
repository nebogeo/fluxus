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
#include <plt/escheme.h>
#include "SchemeHelper.h"
#include "Engine.h"
#include "UtilFunctions.h"
#include "Renderer.h"

using namespace UtilFunctions;
using namespace SchemeHelper;

Scheme_Object *time(int argc, Scheme_Object **argv)
{
	return scheme_make_double(Engine::Get()->Renderer()->GetTime());
}

Scheme_Object *delta(int argc, Scheme_Object **argv)
{
	return scheme_make_double(Engine::Get()->Renderer()->GetDelta());
}

Scheme_Object *flxrnd(int argc, Scheme_Object **argv)
{
	return scheme_make_double(RandFloat());
}

Scheme_Object *flxseed(int argc, Scheme_Object **argv)
{
  	ArgCheck("flxseed", "i", argc, argv);
	srand(IntFromScheme(argv[0]));
	return scheme_void;
}

void UtilFunctions::AddGlobals(Scheme_Env *env)
{	
	// renderstate operations
	scheme_add_global("flxtime",scheme_make_prim_w_arity(time,"flxtime",0,0), env);
	scheme_add_global("delta",scheme_make_prim_w_arity(delta,"delta",0,0), env);
	scheme_add_global("flxrnd",scheme_make_prim_w_arity(flxrnd,"flxrnd",0,0), env);
	scheme_add_global("flxseed",scheme_make_prim_w_arity(flxseed,"flxseed",1,1), env);	
}
