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

#ifndef FLUXUS_RENDERSTATE_BINDING
#define FLUXUS_RENDERSTATE_BINDING

class FluxusRenderstateBinding : public FluxusBinding
{
public:
	void RegisterProcs();
	
	static SCM push();
	static SCM pop();
	static SCM grab(SCM s_id);
	static SCM ungrab();
	static SCM print_scene_graph();
	static SCM apply(SCM s_id);
	static SCM flux_identity();
	static SCM concat(SCM s_m);
	static SCM translate(SCM s_vec);
	static SCM rotate(SCM s_vec);
	static SCM scale(SCM s_vec);
	static SCM colour(SCM s_vec);
	static SCM wire_colour(SCM s_vec);
	static SCM opacity(SCM s_opac);
	static SCM shinyness(SCM s_opac);
	static SCM specular(SCM s_vec);
	static SCM ambient(SCM s_vec);
	static SCM emissive(SCM s_vec);
	static SCM texture(SCM s_id);
	static SCM multitexture(SCM s_t, SCM s_id);
	static SCM hint_solid();
	static SCM hint_wire();
	static SCM hint_normal();
	static SCM hint_points();
	static SCM hint_anti_alias();
	static SCM hint_unlit();
	static SCM hint_vertcols();
	static SCM hint_box();
	static SCM hint_none();
	static SCM hint_multitex();
	static SCM hint_origin();
	static SCM hint_cast_shadow();	
	static SCM hint_ignore_depth();	
	static SCM line_width(SCM s_p);
	static SCM point_width(SCM s_p);
	static SCM blend_mode(SCM s_s, SCM s_d);
	static SCM parent(SCM s_p);
	static SCM hide(SCM s_b);
	static SCM selectable(SCM s_b);
	static SCM shader(SCM s_vert, SCM s_frag);
	static SCM shader_set(SCM s_params);

};
#endif
