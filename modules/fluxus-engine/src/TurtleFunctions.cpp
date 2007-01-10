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
#include "TurtleFunctions.h"
#include "Renderer.h"

using namespace SchemeHelper;
using namespace fluxus;

Scheme_Object *turtle_prim(int argc, Scheme_Object **argv)
{
	ArgCheck("turtle-prim", "i", argc, argv);		
	Engine::Get()->GetTurtle()->Prim(IntFromScheme(argv[0]));
	return scheme_void;
}

Scheme_Object *turtle_vert(int argc, Scheme_Object **argv)
{
	Engine::Get()->GetTurtle()->Vert();
	return scheme_void;
}

Scheme_Object *turtle_build(int argc, Scheme_Object **argv)
{
	return scheme_make_integer_value(Engine::Get()->GetTurtle()->Build(Engine::Get()->Renderer()));
}

Scheme_Object *turtle_move(int argc, Scheme_Object **argv)
{
	ArgCheck("turtle-move", "f", argc, argv);		
	Engine::Get()->GetTurtle()->Move(FloatFromScheme(argv[0]));
	return scheme_void;
}

Scheme_Object *turtle_push(int argc, Scheme_Object **argv)
{
	Engine::Get()->GetTurtle()->Push();
	return scheme_void;
}

Scheme_Object *turtle_pop(int argc, Scheme_Object **argv)
{
	Engine::Get()->GetTurtle()->Pop();
	return scheme_void;
}

Scheme_Object *turtle_turn(int argc, Scheme_Object **argv)
{
	ArgCheck("turtle-turn", "v", argc, argv);		
	float rot[3];
	FloatsFromScheme(argv[0],rot,3);
	Engine::Get()->GetTurtle()->Turn(dVector(rot[0],rot[1],rot[2]));
	return scheme_void;	
}

Scheme_Object *turtle_reset(int argc, Scheme_Object **argv)
{
	Engine::Get()->GetTurtle()->Reset();
	return scheme_void;	
}

Scheme_Object *turtle_attach(int argc, Scheme_Object **argv)
{
	ArgCheck("turtle-attach", "i", argc, argv);		
	PolyPrimitive *poly = dynamic_cast<PolyPrimitive*>(Engine::Get()->Renderer()->GetPrimitive(IntFromScheme(argv[0])));
	if (poly)
	{
		Engine::Get()->GetTurtle()->Attach(poly);
	}
	else
	{
		cerr<<"turtle-attach only works on polys"<<endl;
	}
	
	return scheme_void;	
}

Scheme_Object *turtle_skip(int argc, Scheme_Object **argv)
{
	ArgCheck("turtle-skip", "i", argc, argv);		
	Engine::Get()->GetTurtle()->Skip(IntFromScheme(argv[0]));
	return scheme_void;	
}

Scheme_Object *turtle_position(int argc, Scheme_Object **argv)
{
	return scheme_make_integer_value(Engine::Get()->GetTurtle()->Position());
}

Scheme_Object *turtle_seek(int argc, Scheme_Object **argv)
{
	ArgCheck("turtle-seek", "i", argc, argv);		
	Engine::Get()->GetTurtle()->SetPosition(IntFromScheme(argv[0]));
	return scheme_void;	
}

void TurtleFunctions::AddGlobals(Scheme_Env *env)
{	
	scheme_add_global("turtle-prim", scheme_make_prim_w_arity(turtle_prim, "turtle-prim", 1, 1), env);
	scheme_add_global("turtle-vert", scheme_make_prim_w_arity(turtle_vert, "turtle-vert", 0, 0), env);
	scheme_add_global("turtle-build", scheme_make_prim_w_arity(turtle_build, "turtle-build", 0, 0), env);
	scheme_add_global("turtle-move", scheme_make_prim_w_arity(turtle_move, "turtle-move", 1, 1), env);
	scheme_add_global("turtle-push", scheme_make_prim_w_arity(turtle_push, "turtle-push", 0, 0), env);
	scheme_add_global("turtle-pop", scheme_make_prim_w_arity(turtle_pop, "turtle-pop", 0, 0), env);
	scheme_add_global("turtle-turn", scheme_make_prim_w_arity(turtle_turn, "turtle-turn", 1, 1), env);
	scheme_add_global("turtle-reset", scheme_make_prim_w_arity(turtle_reset, "turtle-reset", 0, 0), env);
	scheme_add_global("turtle-attach", scheme_make_prim_w_arity(turtle_attach, "turtle-attach", 1, 1), env);
	scheme_add_global("turtle-skip", scheme_make_prim_w_arity(turtle_skip, "turtle-skip", 1, 1), env);
	scheme_add_global("turtle-position", scheme_make_prim_w_arity(turtle_position, "turtle-position", 0, 0), env);
	scheme_add_global("turtle-seek", scheme_make_prim_w_arity(turtle_seek, "turtle-seek", 1, 1), env);
}
