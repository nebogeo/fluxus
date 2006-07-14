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

#ifndef FLUXUS_GLOBALSTATE_BINDING
#define FLUXUS_GLOBALSTATE_BINDING

class FluxusGlobalstateBinding : public FluxusBinding
{
public:
	void RegisterProcs();
	
	static SCM clear();	
	static SCM ortho();
	static SCM persp();
	static SCM frustum(SCM s_u, SCM s_d, SCM s_l, SCM s_r);
	static SCM clip(SCM s_f, SCM s_b);
	static SCM blur(SCM s_blur);
	static SCM fog(SCM s_col, SCM s_d, SCM s_s, SCM s_e);
	static SCM feedback(SCM s_fb);
	static SCM feedback_transform(SCM s_fb);
	static SCM load_texture(SCM s_name);
	static SCM force_load_texture(SCM s_name);
	static SCM clear_colour(SCM s_vec);
	static SCM clear_frame(SCM s_gain);
	static SCM lock_camera(SCM s_ob);
	static SCM camera_lag(SCM s_amount);
	static SCM backfacecull(SCM s);
	static SCM show_axis(SCM s_id);
	static SCM show_fps(SCM s_id);
	static SCM get_transform();
	static SCM get_camera_transform();
	static SCM set_camera_transform(SCM s_m);
	static SCM get_projection_transform();
	static SCM get_screen_size();
	static SCM set_screen_size(SCM s_size);
	static SCM reset_camera();
};
#endif
