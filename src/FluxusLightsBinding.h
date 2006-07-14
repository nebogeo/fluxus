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

#ifndef FLUXUS_LIGHTS_BINDING
#define FLUXUS_LIGHTS_BINDING

class FluxusLightsBinding : public FluxusBinding
{
public:
	void RegisterProcs();
	
	static SCM make_light(SCM type, SCM cam);
	static SCM light_ambient(SCM id, SCM v);
	static SCM light_diffuse(SCM id, SCM v);
	static SCM light_specular(SCM id, SCM v);
	static SCM light_position(SCM id, SCM v);
	static SCM light_spot_angle(SCM id, SCM s);
	static SCM light_spot_exponent(SCM id, SCM s);
	static SCM light_attenuation(SCM id, SCM t, SCM s);
	static SCM light_direction(SCM id, SCM v);

};
#endif
