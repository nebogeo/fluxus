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
using namespace Fluxus;
using namespace SchemeHelper;

// StartSectionDoc-en
// lights
// Without lights you wouldn't be able to see anything. Luckily fluxus gives you one for free by default, a white 
// diffuse point light attached to the camera. For more interesting lighting, you'll need these functions. Using the 
// standard fixed function graphics pipeline, simplistically speaking, OpenGL multiplies these values with the surface 
// material (set with local state commands like ambient and diffuse) and the texture colour value to give the final 
// colour.
// Example:
// EndSectionDoc 

// StartSectionDoc-pt
// lights
// Sem luzes você não seria capaz de ver nada. Por sorte fluxus te dá
// uma de graça por padrão, uma luz pontual difusa e branca fixa à
// camêra. Para iluminações mais interessantes, você vai precisar
// destas funções. Usando as funções fixas de pipeline gráfico padrão,
// falando simplisticamente, OpenGL multiplica estes valores pela
// superficie do material (ajustado com comandos do estado local
// [local state] como ambient e diffuse) e o valor cor da textura
// dando a cor final.
// Exemplo:
// EndSectionDoc

// StartFunctionDoc-en
// make-light type-symbol cameralocked-symbol
// Returns: lightid-number
// Description:
// Makes a new light. The type can be one of: point, directional or spot. If the cameralocked string is not
// free then it will be attached to the camera, and move around when you move the camera.
// Example:
// (make-light 'spot 'locked)
// EndFunctionDoc

// StartFunctionDoc-pt
// make-light simbolo-tipo simbolo-travado-camera[cameralocked]
// Retorna: lightid-number
// Descrição:
// Faz uma luz nova. O tipo pode ser um destes: point, directional ou
// spot. Se a string de cameralocked não estiver livre então esta vai
// ser fixa à câmera, e mexer ao redor quando você mover a câmera.
// Exemplo:
// (make-light 'spot 'locked)
// EndFunctionDoc

Scheme_Object *make_light(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("make-light", "SS", argc, argv);
	
	string type=SymbolName(argv[0]);
	string cameralocked=SymbolName(argv[1]);
	
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

// StartFunctionDoc-pt
// light-ambient número-id-luz colour
// Retorna: void
// Descrição:
// Ajusta a contribuição do ambiente para a luz especificada.
// Exemplo:
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

// StartFunctionDoc-pt
// light-diffuse número-id-luz colour
// Retorna: void
// Descrição:
// Ajusta a contribuição difusa para a luz especificada.
// Exemplo:
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

// StartFunctionDoc-pt
// light-specular número-id-luz colour
// Retorna: void
// Descrição:
// Ajusta a contribuição especular para a luz específicada.
// Exemplo:
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

// StartFunctionDoc-pt
// light-position número-id-luz vetor-posição
// Retorna: void
// Descrição:
// Ajusta a posição da luz especificada. Em espaço global se livre, em
// espaço da câmera se fixa.
// Exemplo:
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

// StartFunctionDoc-pt
// light-spot-angle número-id-luz número-ângulo
// Retorna: void
// Descrição:
// Ajusta o ângulo do cone da luz spot especificada. Se não é uma luz
// spot este comando não tem nenhum efeito.
// Exemplo:
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

// StartFunctionDoc-pt
// light-spot-exponent número-id-luz número-exponente
// Retorna: void
// Descrição:
// Ajusta a exponencial da luz spot (dispersão do cone)
// específicada. Não funciona com outra lâmpada senão a spot.
// Exemplo:
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
// light-attenuation lightid-number type-symbol attenuation-number
// Returns: void
// Description:
// Sets the light attenuation (fade off with distance) of the specified light. 
// The type symbol can be one of: constant, linear or quadratic.
// Example:
// (light-attenuation 'constant 0.1) 
// EndFunctionDoc

// StartFunctionDoc-pt
// light-attenuation número-id-luz símbolo-tipo número-atenuação
// Retorna: void
// Descrição:
// Ajusta a atenuação da luz (decaimento com distância) da luz
// especificada. O símbolo-tipo, pode ser um dos: constant, linear ou quadratic.
// Exemplo:
// (light-attenuation 'constant 0.1) 
// EndFunctionDoc

Scheme_Object *light_attenuation(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("light-attenuation", "iSf", argc, argv);
	Light *light = Engine::Get()->Renderer()->GetLight(IntFromScheme(argv[0]));
	if (light) 
	{
		string type=SymbolName(argv[1]);	
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

// StartFunctionDoc-pt
// light-direction número-id-luz vetor-direção
// Retorna: void
// Descrição:
// Ajusta a direção da luz direcional. Se não é uma luz direcional,
// este comando não tem efeito.
// Exemplo:
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
