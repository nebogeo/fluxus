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
#include "FluxusLightsBinding.h"
#include "SearchPaths.h"
#include "Repl.h"
#include <LinePrimitive.h>
#include <ParticlePrimitive.h>
#include <PixelPrimitive.h>
#include <LocatorPrimitive.h>

void FluxusLightsBinding::RegisterProcs()
{
	SchemePrim::Init();
	
    scm_c_define_gsubr("make-light",2,0,0,(CALLBACK_CAST)		 make_light);
	scm_c_define_gsubr("light-ambient",2,0,0,(CALLBACK_CAST)   light_ambient);
	scm_c_define_gsubr("light-diffuse",2,0,0,(CALLBACK_CAST)   light_diffuse);
	scm_c_define_gsubr("light-specular",2,0,0,(CALLBACK_CAST)  light_specular);
	scm_c_define_gsubr("light-position",2,0,0,(CALLBACK_CAST)  light_position);
	scm_c_define_gsubr("light-spot-angle",2,0,0,(CALLBACK_CAST)  light_spot_angle);
	scm_c_define_gsubr("light-spot-exponent",2,0,0,(CALLBACK_CAST)  light_spot_exponent);
	scm_c_define_gsubr("light-attenuation",3,0,0,(CALLBACK_CAST)  light_attenuation);
	scm_c_define_gsubr("light-direction",2,0,0,(CALLBACK_CAST)  light_direction);

}

SCM FluxusLightsBinding::make_light(SCM s_type, SCM s_cameralocked)
{
	SCM_ASSERT(scm_is_string(s_type), s_type, SCM_ARG1, "make_light");
	SCM_ASSERT(scm_is_string(s_cameralocked), s_cameralocked, SCM_ARG2, "make_light");
	char *type=scm_to_locale_string(s_type);
	char *cameralocked=scm_to_locale_string(s_cameralocked);
	
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
	
	free(type);
	free(cameralocked);
	
	return scm_from_int(Fluxus->GetRenderer()->AddLight(l));
}

SCM FluxusLightsBinding::light_ambient(SCM id, SCM v)
{
	SCM_ASSERT(scm_is_number(id), id, SCM_ARG1, "light_ambient");	
	SCM_ASSERT(scm_is_generalized_vector(v), v, SCM_ARG2, "light_ambient");
	SCM_ASSERT(scm_c_generalized_vector_length(v)==3, v, SCM_ARG2, "light_ambient");
	float vec[3];
	flx_floats_from_scm(v,vec);
	Light *light = Fluxus->GetRenderer()->GetLight(scm_to_int(id));
	if (light) light->SetAmbient(dColour(vec[0],vec[1],vec[2]));
	return SCM_UNSPECIFIED;
}

SCM FluxusLightsBinding::light_diffuse(SCM id, SCM v)
{
	SCM_ASSERT(scm_is_number(id), id, SCM_ARG1, "light_diffuse");	
	SCM_ASSERT(scm_is_generalized_vector(v), v, SCM_ARG2, "light_diffuse");
	SCM_ASSERT(scm_c_generalized_vector_length(v)==3, v, SCM_ARG2, "light_diffuse");
	float vec[3];
	flx_floats_from_scm(v,vec);
	Light *light = Fluxus->GetRenderer()->GetLight(scm_to_int(id));
	if (light) light->SetDiffuse(dColour(vec[0],vec[1],vec[2]));
	return SCM_UNSPECIFIED;
}

SCM FluxusLightsBinding::light_specular(SCM id, SCM v)
{
	SCM_ASSERT(scm_is_number(id), id, SCM_ARG1, "light_specular");	
	SCM_ASSERT(scm_is_generalized_vector(v), v, SCM_ARG2, "light_specular");
	SCM_ASSERT(scm_c_generalized_vector_length(v)==3, v, SCM_ARG2, "light_specular");
	float vec[3];
	flx_floats_from_scm(v,vec);
	Light *light = Fluxus->GetRenderer()->GetLight(scm_to_int(id));
	if (light) light->SetSpecular(dColour(vec[0],vec[1],vec[2]));
	return SCM_UNSPECIFIED;
}

SCM FluxusLightsBinding::light_position(SCM id, SCM v)
{
	SCM_ASSERT(scm_is_number(id), id, SCM_ARG1, "light_position");	
	SCM_ASSERT(scm_is_generalized_vector(v), v, SCM_ARG2, "light_position");
	SCM_ASSERT(scm_c_generalized_vector_length(v)==3, v, SCM_ARG2, "light_position");	
	float vec[3];
	flx_floats_from_scm(v,vec);
	Light *light = Fluxus->GetRenderer()->GetLight(scm_to_int(id));
	if (light) light->SetPosition(dVector(vec[0],vec[1],vec[2]));
	return SCM_UNSPECIFIED;
}
	
SCM FluxusLightsBinding::light_spot_angle(SCM id, SCM s)
{
	SCM_ASSERT(scm_is_number(id), id, SCM_ARG1, "light-spot-angle");	
	SCM_ASSERT(scm_is_number(s), s, SCM_ARG2, "light-spot-angle");
	Light *light = Fluxus->GetRenderer()->GetLight(scm_to_int(id));
	if (light) light->SetSpotAngle(scm_to_double(s));
	return SCM_UNSPECIFIED;
}	

SCM FluxusLightsBinding::light_spot_exponent(SCM id, SCM s)
{
	SCM_ASSERT(scm_is_number(id), id, SCM_ARG1, "light-spot-exponent");	
	SCM_ASSERT(scm_is_number(s), s, SCM_ARG2, "light-spot-exponent");
	Light *light = Fluxus->GetRenderer()->GetLight(scm_to_int(id));
	if (light) light->SetSpotExponent(scm_to_double(s));
	return SCM_UNSPECIFIED;
}

SCM FluxusLightsBinding::light_attenuation(SCM id, SCM t, SCM s)
{
	SCM_ASSERT(scm_is_number(id), id, SCM_ARG1, "light-attenuation");	
	SCM_ASSERT(scm_is_string(t), t, SCM_ARG2, "make_light");
	SCM_ASSERT(scm_is_number(s), s, SCM_ARG3, "light-attenuation");
				
	Light *light = Fluxus->GetRenderer()->GetLight(scm_to_int(id));
	if (light) 
	{
		char *type=scm_to_locale_string(t);	
		if (!strcmp(type,"constant"))
		{
			light->SetAttenuation(0,scm_to_double(s));
		}
		else if (!strcmp(type,"linear"))
		{
			light->SetAttenuation(1,scm_to_double(s));
		}
		else if (!strcmp(type,"quadratic"))
		{
			light->SetAttenuation(2,scm_to_double(s));
		}

		free(type);
	}
	
	return SCM_UNSPECIFIED;
}

SCM FluxusLightsBinding::light_direction(SCM id, SCM v)
{
	SCM_ASSERT(scm_is_number(id), id, SCM_ARG1, "light-direction");	
	SCM_ASSERT(scm_is_generalized_vector(v), v, SCM_ARG2, "light-direction");
	SCM_ASSERT(scm_c_generalized_vector_length(v)==3, v, SCM_ARG2, "light-direction");	
	float vec[3];
	flx_floats_from_scm(v,vec);
	Light *light = Fluxus->GetRenderer()->GetLight(scm_to_int(id));
	if (light) light->SetDirection(dVector(vec[0],vec[1],vec[2]));
	return SCM_UNSPECIFIED;
}

