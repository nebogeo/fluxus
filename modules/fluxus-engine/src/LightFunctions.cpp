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

// StartSectionDoc-en
// Lights
// Without lights you wouldn't be able to see anything. Luckily fluxus gives you one for free by default, a white 
// diffuse point light attached to the camera. For more interesting lighting, you'll need these functions. Using the 
// standard fixed function graphics pipeline, simplistically speaking, OpenGL multiplies these values with the surface 
// material (set with local state commands like ambient and diffuse) and the texture colour value to give the final 
// colour.
// Example:
// EndSectionDoc 

// StartFunctionDoc-en
// make-light type-string cameralocked-string
// Returns: lightid-number
// Description:
// Makes a new light. The type can be one of: "point", "directional" or "spot". If the cameralocked string is not
// "free" then it will be attached to the camera, and move around when you move the camera.
// Example:
// (make-light "spot" "locked")
// EndFunctionDoc

Scheme_Object *make_light(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("make-light", "ss", argc, argv);
	
	string type=StringFromScheme(argv[0]);
	string cameralocked=StringFromScheme(argv[1]);
	
	Light *l=new Light;
	
	if (type=="point")
	{
		l->SetType(Light::POINT);
	}
	else if (type=="directional")
	{
		l->SetType(Light::DIRECTIONAL);
	}
	else if (type=="spot")
	{
		l->SetType(Light::SPOT);
	}

	if (cameralocked=="free")
	{
		l->SetCameraLock(0);
	}
	else
	{
		l->SetCameraLock(1);
	}
	
	MZ_GC_UNREG(); 
	return scheme_make_integer_value(Engine::Get()->Renderer()->AddLight(l));
}

// StartFunctionDoc-en
// light-ambient lightid-number colour
// Returns: void
// Description:
// Sets the ambient contribution for the specified light.
// Example:
// (light-ambient mylight (vector 1 1 1)) ; a boring light
// EndFunctionDoc

Scheme_Object *light_ambient(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("light-ambient", "iv", argc, argv);
	float vec[3];
	FloatsFromScheme(argv[1],vec,3);
	Light *light = Engine::Get()->Renderer()->GetLight(IntFromScheme(argv[0]));
	if (light) light->SetAmbient(dColour(vec[0],vec[1],vec[2]));
	MZ_GC_UNREG(); 
	return scheme_void;
}

// StartFunctionDoc-en
// light-diffuse lightid-number colour
// Returns: void
// Description:
// Sets the diffuse contribution for the specified light.
// Example:
// (light-diffuse mylight (vector 1 1 1)) 
// EndFunctionDoc

Scheme_Object *light_diffuse(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("light-diffuse", "iv", argc, argv);
	float vec[3];
	FloatsFromScheme(argv[1],vec,3);
	Light *light = Engine::Get()->Renderer()->GetLight(IntFromScheme(argv[0]));
	if (light) light->SetDiffuse(dColour(vec[0],vec[1],vec[2]));
	MZ_GC_UNREG(); 
	return scheme_void;
}

// StartFunctionDoc-en
// light-specular lightid-number colour
// Returns: void
// Description:
// Sets the specular contribution for the specified light.
// Example:
// (light-specular mylight (vector 1 1 1)) 
// EndFunctionDoc

Scheme_Object *light_specular(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("light-specular", "iv", argc, argv);
	float vec[3];
	FloatsFromScheme(argv[1],vec,3);
	Light *light = Engine::Get()->Renderer()->GetLight(IntFromScheme(argv[0]));
	if (light) light->SetSpecular(dColour(vec[0],vec[1],vec[2]));
	MZ_GC_UNREG(); 
	return scheme_void;
}

// StartFunctionDoc-en
// light-position lightid-number position-vector
// Returns: void
// Description:
// Sets the position of the specified light. In worldspace if free, in camera space is attached.
// Example:
// (light-position mylight (vector 0 100 0)) 
// EndFunctionDoc

Scheme_Object *light_position(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("light-position", "iv", argc, argv);
	float vec[3];
	FloatsFromScheme(argv[1],vec,3);
	Light *light = Engine::Get()->Renderer()->GetLight(IntFromScheme(argv[0]));
	if (light) light->SetPosition(dVector(vec[0],vec[1],vec[2]));
	MZ_GC_UNREG(); 
	return scheme_void;
}

// StartFunctionDoc-en
// light-spot-angle lightid-number angle-number
// Returns: void
// Description:
// Sets the spotlight cone angle of the specified light. If it's not a spot light, this command has no effect. 
// Example:
// (light-position mylight (vector 0 100 0)) 
// EndFunctionDoc
	
Scheme_Object *light_spot_angle(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("light-spot-angle", "if", argc, argv);
	Light *light = Engine::Get()->Renderer()->GetLight(IntFromScheme(argv[0]));
	if (light) light->SetSpotAngle(FloatFromScheme(argv[1]));
	MZ_GC_UNREG(); 
	return scheme_void;
}
	
// StartFunctionDoc-en
// light-spot-exponent lightid-number exponent-number
// Returns: void
// Description:
// Sets the spotlight exponent (fuzzyness of the cone) of the specified light. If it's not a spot light, this command has no effect. 
// Example:
// (light-spot-exponent mylight 0.1) 
// EndFunctionDoc

Scheme_Object *light_spot_exponent(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("light-spot-exponent", "if", argc, argv);
	Light *light = Engine::Get()->Renderer()->GetLight(IntFromScheme(argv[0]));
	if (light) light->SetSpotExponent(FloatFromScheme(argv[1]));
	MZ_GC_UNREG(); 
	return scheme_void;
}

// StartFunctionDoc-en
// light-attenuation lightid-number type-string attenuation-number
// Returns: void
// Description:
// Sets the light attenuation (fade off with distance) of the specified light. 
// The type string can be one of: "constant", "linear" or "quadratic".
// Example:
// (light-spot-exponent mylight 0.1) 
// EndFunctionDoc

Scheme_Object *light_attenuation(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("light-attenuation", "isf", argc, argv);
	Light *light = Engine::Get()->Renderer()->GetLight(IntFromScheme(argv[0]));
	if (light) 
	{
		string type=StringFromScheme(argv[1]);	
		if (type=="constant")
		{
			light->SetAttenuation(0,FloatFromScheme(argv[2]));
		}
		else if (type=="linear")
		{
			light->SetAttenuation(1,FloatFromScheme(argv[2]));
		}
		else if (type=="quadratic")
		{
			light->SetAttenuation(2,FloatFromScheme(argv[2]));
		}
	}
	
	MZ_GC_UNREG(); 
	return scheme_void;
}

// StartFunctionDoc-en
// light-direction lightid-number direction-vector
// Returns: void
// Description:
// Sets the direction of a directional light. If it's not a directional light, this command has no effect. 
// Example:
// (light-spot-exponent mylight 0.1) 
// EndFunctionDoc

Scheme_Object *light_direction(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("light-direction", "iv", argc, argv);
	float vec[3];
	FloatsFromScheme(argv[1],vec,3);
	Light *light = Engine::Get()->Renderer()->GetLight(IntFromScheme(argv[0]));
	if (light) light->SetDirection(dVector(vec[0],vec[1],vec[2]));
	MZ_GC_UNREG(); 
	return scheme_void;
}

void LightFunctions::AddGlobals(Scheme_Env *env)
{	
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, env);
	MZ_GC_REG();
	scheme_add_global("make-light", scheme_make_prim_w_arity(make_light, "make-light", 2, 2), env);
	scheme_add_global("light-ambient", scheme_make_prim_w_arity(light_ambient, "light-ambient", 2, 2), env);
	scheme_add_global("light-diffuse", scheme_make_prim_w_arity(light_diffuse, "light-diffuse", 2, 2), env);
	scheme_add_global("light-specular", scheme_make_prim_w_arity(light_specular, "light-specular", 2, 2), env);
	scheme_add_global("light-position", scheme_make_prim_w_arity(light_position, "light-position", 2, 2), env);
	scheme_add_global("light-spot-angle", scheme_make_prim_w_arity(light_spot_angle, "light-spot-angle", 2, 2), env);
	scheme_add_global("light-spot-exponent", scheme_make_prim_w_arity(light_spot_exponent, "light-spot-exponent", 2, 2), env);
	scheme_add_global("light-attenuation", scheme_make_prim_w_arity(light_attenuation, "light-attenuation", 3, 3), env);
	scheme_add_global("light-direction", scheme_make_prim_w_arity(light_direction, "light-direction", 2, 2), env);
 	MZ_GC_UNREG(); 
}
