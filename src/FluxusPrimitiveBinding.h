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

#ifndef FLUXUS_PRIMITIVE_BINDING
#define FLUXUS_PRIMITIVE_BINDING

class FluxusPrimitiveBinding : public FluxusBinding
{
public:
	void RegisterProcs();
	
	static SCM build_polygons(SCM s_size, SCM s_type);
	static SCM build_cube();
	static SCM build_sphere(SCM s_hsegments, SCM s_rsegments);
	static SCM build_plane();
	static SCM build_plane(SCM s_xsegments, SCM s_ysegments);
	static SCM build_cylinder(SCM s_hsegments, SCM s_rsegments);
	static SCM build_line(SCM s_numpoints);
	static SCM build_text(SCM text);
	static SCM build_nurbs(SCM s_size);
	static SCM build_nurbs_sphere(SCM s_hsegments, SCM s_rsegments);
	static SCM build_nurbs_plane(SCM s_usegments, SCM s_vsegments);
	static SCM build_particles(SCM s_count);
	static SCM build_locator();
	static SCM build_pixels(SCM s_w, SCM s_h);
	static SCM upload_pixels();
	static SCM pixels2texture(SCM s_ob);
	static SCM build_blobby(SCM s_count, SCM s_dim, SCM s_size);
	static SCM draw_instance(SCM s_ob);
	static SCM draw_cube();
	static SCM draw_plane();
	static SCM draw_sphere();
	static SCM draw_cylinder();
	static SCM destroy(SCM s_name);

};
#endif
