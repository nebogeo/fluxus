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

#ifndef FLUXUS_PHYSICS_BINDING
#define FLUXUS_PHYSICS_BINDING

class FluxusPhysicsBinding : public FluxusBinding
{
public:
	void RegisterProcs();
	
	static SCM collisions(SCM s);
	static SCM ground_plane(SCM s_ori, SCM s_off);
	static SCM active_box(SCM s_name);
	static SCM active_cylinder(SCM s_name);
	static SCM active_sphere(SCM s_name);
	static SCM passive_box(SCM s_name);
	static SCM passive_cylinder(SCM s_name);
	static SCM passive_sphere(SCM s_name);
	static SCM surface_params(SCM s_slip1, SCM s_slip2, SCM s_softerp, SCM s_softcfm);
	static SCM build_fixedjoint(SCM s_ob1);
	static SCM build_hingejoint(SCM s_ob1, SCM s_ob2, SCM s_anchor, SCM s_hinge);
	static SCM build_balljoint(SCM s_ob1, SCM s_ob2, SCM s_anchor);
	static SCM build_sliderjoint(SCM s_ob1, SCM s_ob2, SCM s_hinge);
	static SCM build_hinge2joint(SCM s_ob1, SCM s_ob2, SCM s_anchor, SCM s_hinge1, SCM s_hinge2);
	static SCM build_amotorjoint(SCM s_ob1, SCM s_ob2, SCM s_axis);
	static SCM joint_param(SCM s_joint, SCM s_param, SCM s_value);
	static SCM joint_angle(SCM s_joint, SCM s_vel, SCM s_angle);
	static SCM set_max_physical(SCM s_value);
	static SCM set_mass(SCM s_obj, SCM s_value);
	static SCM kick(SCM s_obj, SCM s_vec);
	static SCM twist(SCM s_obj, SCM s_vec);
	static SCM gravity(SCM s_vec);
	static SCM has_collided(SCM s_id);

};
#endif
