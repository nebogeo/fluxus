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
#include "Engine.h"
#include "Light.h"
#include "SchemeHelper.h"
#include "LightFunctions.h"

using namespace LightFunctions;
using namespace fluxus;
using namespace SchemeHelper;

Scheme_Object *make_light(int argc, Scheme_Object **argv)
{
	ArgCheck("make-light", "ss", argc, argv);
	
	char *type=StringFromScheme(argv[0]);
	char *cameralocked=StringFromScheme(argv[1]);
	
	Light *l=new Light;
	
	if (!strcmp(type,"point"))
	{
		l->SetType(Light::POINT);
	}
	else if (!strcmp(type,"directional"))
	{
		l->SetType(Light::DIRECTIONAL);
	}
	else if (!strcmp(type,"spot"))
	{
		l->SetType(Light::SPOT);
	}

	if (!strcmp(cameralocked,"free"))
	{
		l->SetCameraLock(0);
	}
	else
	{
		l->SetCameraLock(1);
	}
	
	return scheme_make_integer_value(Engine::Get()->Renderer()->AddLight(l));
}

Scheme_Object *light_ambient(int argc, Scheme_Object **argv)
{
	ArgCheck("light-ambient", "iv", argc, argv);
	float vec[3];
	FloatsFromScheme(argv[1],vec,3);
	Light *light = Engine::Get()->Renderer()->GetLight(IntFromScheme(argv[0]));
	if (light) light->SetAmbient(dColour(vec[0],vec[1],vec[2]));
	return scheme_void;
}

Scheme_Object *light_diffuse(int argc, Scheme_Object **argv)
{
	ArgCheck("light-diffuse", "iv", argc, argv);
	float vec[3];
	FloatsFromScheme(argv[1],vec,3);
	Light *light = Engine::Get()->Renderer()->GetLight(IntFromScheme(argv[0]));
	if (light) light->SetDiffuse(dColour(vec[0],vec[1],vec[2]));
	return scheme_void;
}

Scheme_Object *light_specular(int argc, Scheme_Object **argv)
{
	ArgCheck("light-specular", "iv", argc, argv);
	float vec[3];
	FloatsFromScheme(argv[1],vec,3);
	Light *light = Engine::Get()->Renderer()->GetLight(IntFromScheme(argv[0]));
	if (light) light->SetSpecular(dColour(vec[0],vec[1],vec[2]));
	return scheme_void;
}

Scheme_Object *light_position(int argc, Scheme_Object **argv)
{
	ArgCheck("light-position", "iv", argc, argv);
	float vec[3];
	FloatsFromScheme(argv[1],vec,3);
	Light *light = Engine::Get()->Renderer()->GetLight(IntFromScheme(argv[0]));
	if (light) light->SetPosition(dVector(vec[0],vec[1],vec[2]));
	return scheme_void;
}
	
Scheme_Object *light_spot_angle(int argc, Scheme_Object **argv)
{
	ArgCheck("light-spot-angle", "if", argc, argv);
	Light *light = Engine::Get()->Renderer()->GetLight(IntFromScheme(argv[0]));
	if (light) light->SetSpotAngle(FloatFromScheme(argv[1]));
	return scheme_void;
}	

Scheme_Object *light_spot_exponent(int argc, Scheme_Object **argv)
{
	ArgCheck("light-spot-exponent", "if", argc, argv);
	Light *light = Engine::Get()->Renderer()->GetLight(IntFromScheme(argv[0]));
	if (light) light->SetSpotExponent(FloatFromScheme(argv[1]));
	return scheme_void;
}

Scheme_Object *light_attenuation(int argc, Scheme_Object **argv)
{
	ArgCheck("light-attenuation", "isf", argc, argv);
	Light *light = Engine::Get()->Renderer()->GetLight(IntFromScheme(argv[0]));
	if (light) 
	{
		char *type=StringFromScheme(argv[1]);	
		if (!strcmp(type,"constant"))
		{
			light->SetAttenuation(0,FloatFromScheme(argv[2]));
		}
		else if (!strcmp(type,"linear"))
		{
			light->SetAttenuation(1,FloatFromScheme(argv[2]));
		}
		else if (!strcmp(type,"quadratic"))
		{
			light->SetAttenuation(2,FloatFromScheme(argv[2]));
		}
	}
	
	return scheme_void;
}

Scheme_Object *light_direction(int argc, Scheme_Object **argv)
{
	ArgCheck("light-direction", "iv", argc, argv);
	float vec[3];
	FloatsFromScheme(argv[1],vec,3);
	Light *light = Engine::Get()->Renderer()->GetLight(IntFromScheme(argv[0]));
	if (light) light->SetDirection(dVector(vec[0],vec[1],vec[2]));
	return scheme_void;
}

void LightFunctions::AddGlobals(Scheme_Env *env)
{	
	scheme_add_global("make-light", scheme_make_prim_w_arity(make_light, "make-light", 2, 2), env);
	scheme_add_global("light-ambient", scheme_make_prim_w_arity(light_ambient, "light-ambient", 2, 2), env);
	scheme_add_global("light-diffuse", scheme_make_prim_w_arity(light_diffuse, "light-diffuse", 2, 2), env);
	scheme_add_global("light-specular", scheme_make_prim_w_arity(light_specular, "light-specular", 2, 2), env);
	scheme_add_global("light-position", scheme_make_prim_w_arity(light_position, "light-position", 2, 2), env);
	scheme_add_global("light-spot-angle", scheme_make_prim_w_arity(light_spot_angle, "light-spot-angle", 2, 2), env);
	scheme_add_global("light-spot-exponent", scheme_make_prim_w_arity(light_spot_exponent, "light-spot-exponent", 2, 2), env);
	scheme_add_global("light-attenuation", scheme_make_prim_w_arity(light_attenuation, "light-attenuation", 3, 3), env);
	scheme_add_global("light-direction", scheme_make_prim_w_arity(light_direction, "light-direction", 2, 2), env);
}
