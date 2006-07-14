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
#include "FluxusPhysicsBinding.h"
#include "SearchPaths.h"
#include "Repl.h"
#include <LinePrimitive.h>
#include <ParticlePrimitive.h>
#include <PixelPrimitive.h>
#include <LocatorPrimitive.h>

void FluxusPhysicsBinding::RegisterProcs()
{
	SchemePrim::Init();
	
	// physics
	scm_c_define_gsubr("collisions",1,0,0,(CALLBACK_CAST) collisions);
	scm_c_define_gsubr("gravity",1,0,0,(CALLBACK_CAST) gravity);
	scm_c_define_gsubr("set-max-physical",1,0,0,(CALLBACK_CAST) set_max_physical);
    scm_c_define_gsubr("active-box",1,0,0,(CALLBACK_CAST)	  active_box);
    scm_c_define_gsubr("active-sphere",1,0,0,(CALLBACK_CAST)  active_sphere);
    scm_c_define_gsubr("active-cylinder",1,0,0,(CALLBACK_CAST) active_cylinder);
    scm_c_define_gsubr("passive-box",1,0,0,(CALLBACK_CAST)     passive_box);
    scm_c_define_gsubr("passive-sphere",1,0,0,(CALLBACK_CAST)  passive_sphere);
    scm_c_define_gsubr("passive-cylinder",1,0,0,(CALLBACK_CAST)passive_cylinder);
    scm_c_define_gsubr("ground-plane",2,0,0,(CALLBACK_CAST) ground_plane);
    scm_c_define_gsubr("build-fixedjoint",1,0,0,(CALLBACK_CAST) build_fixedjoint);
    scm_c_define_gsubr("build-hingejoint",4,0,0,(CALLBACK_CAST) build_hingejoint);
    scm_c_define_gsubr("build-balljoint",3,0,0,(CALLBACK_CAST) build_balljoint);
    scm_c_define_gsubr("build-sliderjoint",3,0,0,(CALLBACK_CAST) build_sliderjoint);
    scm_c_define_gsubr("build-hinge2joint",5,0,0,(CALLBACK_CAST) build_hinge2joint);
    scm_c_define_gsubr("build-amotorjoint",3,0,0,(CALLBACK_CAST) build_amotorjoint);
    scm_c_define_gsubr("surface-params",4,0,0,(CALLBACK_CAST) surface_params);
    scm_c_define_gsubr("joint-param",3,0,0,(CALLBACK_CAST)  joint_param);
    scm_c_define_gsubr("joint-angle",3,0,0,(CALLBACK_CAST)  joint_angle);
    scm_c_define_gsubr("set-mass",2,0,0,(CALLBACK_CAST) 	set_mass);
    scm_c_define_gsubr("kick",2 ,0,0,(CALLBACK_CAST)		kick);
    scm_c_define_gsubr("twist",2,0,0,(CALLBACK_CAST)		twist);
	scm_c_define_gsubr("has-collided",1,0,0,(CALLBACK_CAST) has_collided);
}

SCM FluxusPhysicsBinding::collisions(SCM s)
{
	SCM_ASSERT(scm_is_number(s), s, SCM_ARG1, "collisions");
	Fluxus->GetPhysics()->SetCollisions(scm_to_double(s));
	return SCM_UNSPECIFIED;
}

SCM FluxusPhysicsBinding::ground_plane(SCM s_ori, SCM s_off)
{
    SCM_ASSERT(scm_is_generalized_vector(s_ori), s_ori, SCM_ARG1, "ground_plane");
	SCM_ASSERT(scm_c_generalized_vector_length(s_ori)==3, s_ori, SCM_ARG1, "ground_plane");
    SCM_ASSERT(scm_is_number(s_off), s_off, SCM_ARG2, "ground_plane");
    float ori[3];
	flx_floats_from_scm(s_ori,ori);
	float off = (float) scm_to_double(s_off);
	Fluxus->GetPhysics()->GroundPlane(dVector(ori[0],ori[1],ori[2]),off);
	return SCM_UNSPECIFIED;
}

SCM FluxusPhysicsBinding::active_box(SCM s_name)
{
	int name=Smob2Prim(s_name);	
	Fluxus->GetPhysics()->MakeActive(name,1.0f,Physics::BOX);
	scm_remember_upto_here_1(s_name);
    return SCM_UNSPECIFIED;
}

SCM FluxusPhysicsBinding::active_cylinder(SCM s_name)
{
	int name=Smob2Prim(s_name);	
	Fluxus->GetPhysics()->MakeActive(name,1.0f,Physics::CYLINDER);
	scm_remember_upto_here_1(s_name);
    return SCM_UNSPECIFIED;
}

SCM FluxusPhysicsBinding::active_sphere(SCM s_name)
{
	int name=Smob2Prim(s_name);	
	Fluxus->GetPhysics()->MakeActive(name,1.0f,Physics::SPHERE);
	scm_remember_upto_here_1(s_name);
    return SCM_UNSPECIFIED;
}

SCM FluxusPhysicsBinding::passive_box(SCM s_name)
{
	int name=Smob2Prim(s_name);	
	Fluxus->GetPhysics()->MakePassive(name,1.0f,Physics::BOX);
	scm_remember_upto_here_1(s_name);
    return SCM_UNSPECIFIED;
}

SCM FluxusPhysicsBinding::passive_cylinder(SCM s_name)
{
	int name=Smob2Prim(s_name);	
	Fluxus->GetPhysics()->MakePassive(name,1.0f,Physics::CYLINDER);
	scm_remember_upto_here_1(s_name);
    return SCM_UNSPECIFIED;
}

SCM FluxusPhysicsBinding::passive_sphere(SCM s_name)
{
	int name=Smob2Prim(s_name);	
	Fluxus->GetPhysics()->MakePassive(name,1.0f,Physics::SPHERE);
	scm_remember_upto_here_1(s_name);
	return SCM_UNSPECIFIED;
}

SCM FluxusPhysicsBinding::surface_params(SCM s_slip1, SCM s_slip2, SCM s_softerp, SCM s_softcfm)
{
	SCM_ASSERT(scm_is_number(s_slip1),    s_slip1,    SCM_ARG1, "surface_params");
	SCM_ASSERT(scm_is_number(s_slip2),    s_slip2,    SCM_ARG2, "surface_params");
	SCM_ASSERT(scm_is_number(s_softerp),  s_softerp,  SCM_ARG3, "surface_params");
	SCM_ASSERT(scm_is_number(s_softcfm),  s_softcfm,  SCM_ARG4, "surface_params");
	Fluxus->GetPhysics()->SetGlobalSurfaceParams((float)scm_to_double(s_slip1),(float)scm_to_double(s_slip2),
		(float)scm_to_double(s_softerp),(float)scm_to_double(s_softcfm));
	return SCM_UNSPECIFIED;
}

SCM FluxusPhysicsBinding::build_balljoint(SCM s_ob1, SCM s_ob2, SCM s_anchor)
{
    SCM_ASSERT(scm_is_generalized_vector(s_anchor), s_anchor, SCM_ARG3, "build_balljoint");
	SCM_ASSERT(scm_c_generalized_vector_length(s_anchor)==3, s_anchor, SCM_ARG3, "build_balljoint");
    int name1=Smob2Prim(s_ob1);
	int name2=Smob2Prim(s_ob2);	
	float anchor[3];
	flx_floats_from_scm(s_anchor,anchor);
	scm_remember_upto_here_1(s_ob1);
	scm_remember_upto_here_1(s_ob2);
	return scm_from_int(Fluxus->GetPhysics()->CreateJointBall(name1, name2, dVector(anchor[0],anchor[1],anchor[2])));
}

SCM FluxusPhysicsBinding::build_fixedjoint(SCM s_ob1)
{
    int name1=Smob2Prim(s_ob1);
	return scm_from_int(Fluxus->GetPhysics()->CreateJointFixed(name1));
}

SCM FluxusPhysicsBinding::build_hingejoint(SCM s_ob1, SCM s_ob2, SCM s_anchor, SCM s_hinge)
{
    SCM_ASSERT(scm_is_generalized_vector(s_anchor), s_anchor, SCM_ARG3, "build_hingejoint");
 	SCM_ASSERT(scm_c_generalized_vector_length(s_anchor)==3, s_anchor, SCM_ARG3, "build_hingejoint");
    SCM_ASSERT(scm_is_generalized_vector(s_hinge),  s_hinge,  SCM_ARG4, "build_hingejoint");
	SCM_ASSERT(scm_c_generalized_vector_length(s_hinge)==3, s_hinge, SCM_ARG4, "build_hingejoint");

    int name1=Smob2Prim(s_ob1);
	int name2=Smob2Prim(s_ob2);
	
	float anchor[3];
	flx_floats_from_scm(s_anchor,anchor);
	
	dVector Hinge;
	float temp[3];
	flx_floats_from_scm(s_hinge,temp);
	Hinge.x=temp[0];
	Hinge.y=temp[1];
	Hinge.z=temp[2];

	scm_remember_upto_here_1(s_ob1);
	scm_remember_upto_here_1(s_ob2);

	return scm_from_int(Fluxus->GetPhysics()->CreateJointHinge(name1, name2, dVector(anchor[0],anchor[1],anchor[2]), Hinge));
}

SCM FluxusPhysicsBinding::build_sliderjoint(SCM s_ob1, SCM s_ob2, SCM s_hinge)
{
    SCM_ASSERT(scm_is_generalized_vector(s_hinge),  s_hinge,  SCM_ARG3, "build_sliderjoint");
	SCM_ASSERT(scm_c_generalized_vector_length(s_hinge)==3, s_hinge, SCM_ARG3, "build_sliderjoint");

    int name1=Smob2Prim(s_ob1);
	int name2=Smob2Prim(s_ob2);
		
	dVector Hinge;
	float temp[3];
	flx_floats_from_scm(s_hinge,temp);
	Hinge.x=temp[0];
	Hinge.y=temp[1];
	Hinge.z=temp[2];

	scm_remember_upto_here_1(s_ob1);
	scm_remember_upto_here_1(s_ob2);

	return scm_from_int(Fluxus->GetPhysics()->CreateJointSlider(name1, name2, Hinge));
}

SCM FluxusPhysicsBinding::build_hinge2joint(SCM s_ob1, SCM s_ob2, SCM s_anchor, SCM s_hinge1, SCM s_hinge2)
{
    SCM_ASSERT(scm_is_generalized_vector(s_anchor), s_anchor, SCM_ARG3, "build_hinge2joint");
    SCM_ASSERT(scm_c_generalized_vector_length(s_anchor)==3, s_anchor, SCM_ARG3, "build_hinge2joint");
	SCM_ASSERT(scm_is_generalized_vector(s_hinge1), s_hinge1, SCM_ARG4, "build_hinge2joint");
    SCM_ASSERT(scm_c_generalized_vector_length(s_hinge1)==3, s_hinge1, SCM_ARG4, "build_hinge2joint");
	SCM_ASSERT(scm_is_generalized_vector(s_hinge2), s_hinge2, SCM_ARG5, "build_hinge2joint");
	SCM_ASSERT(scm_c_generalized_vector_length(s_hinge2)==3, s_hinge2, SCM_ARG5, "build_hinge2joint");

    int name1=Smob2Prim(s_ob1);
	int name2=Smob2Prim(s_ob2);
	
	float anchor[3];
	flx_floats_from_scm(s_anchor,anchor);
	
	dVector Hinge[2];
	float temp[3];
	flx_floats_from_scm(s_hinge1,temp);
	Hinge[0].x=temp[0];
	Hinge[0].y=temp[1];
	Hinge[0].z=temp[2];
	
	flx_floats_from_scm(s_hinge2,temp);
	Hinge[1].x=temp[0];
	Hinge[1].y=temp[1];
	Hinge[1].z=temp[2];
	
	scm_remember_upto_here_1(s_ob1);
	scm_remember_upto_here_1(s_ob2);

	return scm_from_int(Fluxus->GetPhysics()->CreateJointHinge2(name1, name2, dVector(anchor[0],anchor[1],anchor[2]), Hinge));
}

SCM FluxusPhysicsBinding::build_amotorjoint(SCM s_ob1, SCM s_ob2, SCM s_axis)
{
    SCM_ASSERT(scm_is_generalized_vector(s_axis),   s_axis,   SCM_ARG3, "build_amotorjoint");
	SCM_ASSERT(scm_c_generalized_vector_length(s_axis)==3, s_axis, SCM_ARG3, "build_amotorjoint");
    int name1=Smob2Prim(s_ob1);
	int name2=Smob2Prim(s_ob2);	
	float axis[3];
	flx_floats_from_scm(s_axis,axis);
	scm_remember_upto_here_1(s_ob1);
	scm_remember_upto_here_1(s_ob2);
	return scm_from_int(Fluxus->GetPhysics()->CreateJointAMotor(name1, name2, dVector(axis[0],axis[1],axis[2])));
}

SCM FluxusPhysicsBinding::joint_param(SCM s_joint, SCM s_param, SCM s_value)
{
    SCM_ASSERT(scm_is_number(s_joint), s_joint, SCM_ARG1, "joint_param");
    SCM_ASSERT(scm_is_string(s_param), s_param, SCM_ARG1, "joint_param");
    SCM_ASSERT(scm_is_number(s_value), s_value, SCM_ARG2, "joint_param");
    int joint=0;
	joint=scm_to_int(s_joint);
	char *param=0;
	param=scm_to_locale_string(s_param);	
    double v = scm_to_double(s_value);
	Fluxus->GetPhysics()->SetJointParam(joint,param,v);
	free(param);
	return SCM_UNSPECIFIED;
}

SCM FluxusPhysicsBinding::joint_angle(SCM s_joint, SCM s_vel, SCM s_angle)
{
    SCM_ASSERT(scm_is_number(s_joint), s_joint, SCM_ARG1, "joint_angle");
    SCM_ASSERT(scm_is_number(s_vel),   s_vel,   SCM_ARG1, "joint_angle");
    SCM_ASSERT(scm_is_number(s_angle), s_angle, SCM_ARG2, "joint_angle");
	Fluxus->GetPhysics()->SetJointAngle(scm_to_int(s_joint),scm_to_double(s_vel),scm_to_double(s_angle));
	return SCM_UNSPECIFIED;
}

SCM FluxusPhysicsBinding::set_max_physical(SCM s_value)
{
    SCM_ASSERT(scm_is_number(s_value), s_value, SCM_ARG2, "set_max_physical");
    Fluxus->GetPhysics()->SetMaxObjectCount(scm_to_int(s_value));
	return SCM_UNSPECIFIED;
}

SCM FluxusPhysicsBinding::set_mass(SCM s_obj, SCM s_mass)
{
    SCM_ASSERT(scm_is_number(s_mass), s_mass, SCM_ARG2, "set_mass");
    int obj=Smob2Prim(s_obj);
    float mass=scm_to_double(s_mass);
	Fluxus->GetPhysics()->SetMass(obj,mass);
	scm_remember_upto_here_1(s_obj);
	return SCM_UNSPECIFIED;
}

SCM FluxusPhysicsBinding::gravity(SCM s_vec)
{
	SCM_ASSERT(scm_is_generalized_vector(s_vec), s_vec, SCM_ARG1, "gravity");
	SCM_ASSERT(scm_c_generalized_vector_length(s_vec)==3, s_vec, SCM_ARG1, "gravity");
	float vec[3];
	flx_floats_from_scm(s_vec,vec);
	Fluxus->GetPhysics()->SetGravity(dVector(vec[0],vec[1],vec[2]));
	return SCM_UNSPECIFIED;
}

SCM FluxusPhysicsBinding::kick(SCM s_obj, SCM s_vec)
{
    SCM_ASSERT(scm_is_generalized_vector(s_vec), s_vec, SCM_ARG2, "kick");
	SCM_ASSERT(scm_c_generalized_vector_length(s_vec)==3, s_vec, SCM_ARG2, "kick");
    int obj=Smob2Prim(s_obj);
    float vec[3];
    flx_floats_from_scm(s_vec,vec);
	Fluxus->GetPhysics()->Kick(obj,dVector(vec[0],vec[1],vec[2]));
	scm_remember_upto_here_1(s_obj);
	return SCM_UNSPECIFIED;
}

SCM FluxusPhysicsBinding::twist(SCM s_obj, SCM s_vec)
{
    SCM_ASSERT(scm_is_generalized_vector(s_vec), s_vec, SCM_ARG2, "twist");
	SCM_ASSERT(scm_c_generalized_vector_length(s_vec)==3, s_vec, SCM_ARG2, "twist");
    int obj=Smob2Prim(s_obj);
    float vec[3];
    flx_floats_from_scm(s_vec,vec);
	Fluxus->GetPhysics()->Twist(obj,dVector(vec[0],vec[1],vec[2]));
	scm_remember_upto_here_1(s_obj);
	return SCM_UNSPECIFIED;
}


SCM FluxusPhysicsBinding::has_collided(SCM s_id)
{
	return scm_from_bool(Fluxus->GetPhysics()->HasCollided(Smob2Prim(s_id)));
}
