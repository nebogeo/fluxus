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
#include <plt/escheme.h>
#include "SchemeHelper.h"
#include "Engine.h"
#include "PhysicsFunctions.h"
#include "Physics.h"
#include "dada.h"

using namespace PhysicsFunctions;
using namespace SchemeHelper;
using namespace fluxus;

Scheme_Object *collisions(int argc, Scheme_Object **argv)
{
	ArgCheck("collisions", "i", argc, argv);
	Engine::Get()->Physics()->SetCollisions(IntFromScheme(argv[0]));
	return scheme_void;
}

Scheme_Object *ground_plane(int argc, Scheme_Object **argv)
{
 	ArgCheck("ground-plane", "vf", argc, argv);
    float ori[3];
	FloatsFromScheme(argv[0],ori,3);
	Engine::Get()->Physics()->GroundPlane(dVector(ori[0],ori[1],ori[2]),FloatFromScheme(argv[1]));
	return scheme_void;
}

Scheme_Object *active_box(int argc, Scheme_Object **argv)
{
 	ArgCheck("active-box", "i", argc, argv);
	int name=IntFromScheme(argv[0]);	
	Engine::Get()->Physics()->MakeActive(name,1.0f,Physics::BOX);
    return scheme_void;
}

Scheme_Object *active_cylinder(int argc, Scheme_Object **argv)
{
 	ArgCheck("active-cylinder", "i", argc, argv);
	int name=IntFromScheme(argv[0]);	
	Engine::Get()->Physics()->MakeActive(name,1.0f,Physics::CYLINDER);
    return scheme_void;
}

Scheme_Object *active_sphere(int argc, Scheme_Object **argv)
{
 	ArgCheck("active-sphere", "i", argc, argv);
	int name=IntFromScheme(argv[0]);	
	Engine::Get()->Physics()->MakeActive(name,1.0f,Physics::SPHERE);
    return scheme_void;
}

Scheme_Object *passive_box(int argc, Scheme_Object **argv)
{
 	ArgCheck("passive-sphere", "i", argc, argv);
	int name=IntFromScheme(argv[0]);	
	Engine::Get()->Physics()->MakePassive(name,1.0f,Physics::BOX);
    return scheme_void;
}

Scheme_Object *passive_cylinder(int argc, Scheme_Object **argv)
{
 	ArgCheck("passive-cylinder", "i", argc, argv);
	int name=IntFromScheme(argv[0]);	
	Engine::Get()->Physics()->MakePassive(name,1.0f,Physics::CYLINDER);
    return scheme_void;
}

Scheme_Object *passive_sphere(int argc, Scheme_Object **argv)
{
 	ArgCheck("passive-sphere", "i", argc, argv);
	int name=IntFromScheme(argv[0]);	
	Engine::Get()->Physics()->MakePassive(name,1.0f,Physics::SPHERE);
	return scheme_void;
}

Scheme_Object *surface_params(int argc, Scheme_Object **argv)
{
 	ArgCheck("surface-params", "ffff", argc, argv);
	Engine::Get()->Physics()->SetGlobalSurfaceParams(FloatFromScheme(argv[0]),FloatFromScheme(argv[1]),
												 FloatFromScheme(argv[2]),FloatFromScheme(argv[3]));
	return scheme_void;
}

Scheme_Object *build_balljoint(int argc, Scheme_Object **argv)
{
 	ArgCheck("build-balljoint", "iiv", argc, argv);
    int name1=IntFromScheme(argv[0]);
	int name2=IntFromScheme(argv[1]);	
	float anchor[3];
	FloatsFromScheme(argv[2],anchor,3);
	return scheme_make_integer_value(Engine::Get()->Physics()->CreateJointBall(name1, name2, dVector(anchor[0],anchor[1],anchor[2])));
}

Scheme_Object *build_fixedjoint(int argc, Scheme_Object **argv)
{
 	ArgCheck("build-fixedjoint", "i", argc, argv);
    int name1=IntFromScheme(argv[0]);
	return scheme_make_integer_value(Engine::Get()->Physics()->CreateJointFixed(name1));
}

Scheme_Object *build_hingejoint(int argc, Scheme_Object **argv)
{
 	ArgCheck("build-hingejoint", "iivv", argc, argv);
    
    int name1=IntFromScheme(argv[0]);
	int name2=IntFromScheme(argv[1]);
	
	float anchor[3];
	FloatsFromScheme(argv[2],anchor,3);
	
	dVector Hinge;
	float temp[3];
	FloatsFromScheme(argv[3],temp,3);
	Hinge.x=temp[0];
	Hinge.y=temp[1];
	Hinge.z=temp[2];

	return scheme_make_integer_value(Engine::Get()->Physics()->CreateJointHinge(name1, name2, dVector(anchor[0],anchor[1],anchor[2]), Hinge));
}

Scheme_Object *build_sliderjoint(int argc, Scheme_Object **argv)
{
  	ArgCheck("build-sliderjoint", "iiv", argc, argv);
   
    int name1=IntFromScheme(argv[0]);
	int name2=IntFromScheme(argv[1]);
		
	dVector Hinge;
	float temp[3];
	FloatsFromScheme(argv[2],temp,3);
	Hinge.x=temp[0];
	Hinge.y=temp[1];
	Hinge.z=temp[2];

	return scheme_make_integer_value(Engine::Get()->Physics()->CreateJointSlider(name1, name2, Hinge));
}

Scheme_Object *build_hinge2joint(int argc, Scheme_Object **argv)
{
   	ArgCheck("build-hinge2joint", "iivvv", argc, argv);
   
    int name1=IntFromScheme(argv[0]);
	int name2=IntFromScheme(argv[1]);
	
	float anchor[3];
	FloatsFromScheme(argv[2],anchor,3);
	
	dVector Hinge[2];
	float temp[3];
	FloatsFromScheme(argv[3],temp,3);
	Hinge[0].x=temp[0];
	Hinge[0].y=temp[1];
	Hinge[0].z=temp[2];
	
	FloatsFromScheme(argv[4],temp,3);
	Hinge[1].x=temp[0];
	Hinge[1].y=temp[1];
	Hinge[1].z=temp[2];
	
	return scheme_make_integer_value(Engine::Get()->Physics()->CreateJointHinge2(name1, name2, dVector(anchor[0],anchor[1],anchor[2]), Hinge));
}

Scheme_Object *build_amotorjoint(int argc, Scheme_Object **argv)
{
   	ArgCheck("build-amotorjoint", "iiv", argc, argv);
    int name1=IntFromScheme(argv[0]);
	int name2=IntFromScheme(argv[1]);	
	float axis[3];
	FloatsFromScheme(argv[2],axis,3);
	return scheme_make_integer_value(Engine::Get()->Physics()->CreateJointAMotor(name1, name2, dVector(axis[0],axis[1],axis[2])));
}

Scheme_Object *joint_param(int argc, Scheme_Object **argv)
{
   	ArgCheck("joint-param", "isf", argc, argv);
    int joint=0;
	joint=IntFromScheme(argv[0]);
	char *param=0;
	param=StringFromScheme(argv[1]);	
    double v = FloatFromScheme(argv[2]);
	Engine::Get()->Physics()->SetJointParam(joint,param,v);
	return scheme_void;
}

Scheme_Object *joint_angle(int argc, Scheme_Object **argv)
{
   	ArgCheck("joint-angle", "iff", argc, argv);
	Engine::Get()->Physics()->SetJointAngle(IntFromScheme(argv[0]),FloatFromScheme(argv[1]),FloatFromScheme(argv[2]));
	return scheme_void;
}

Scheme_Object *set_max_physical(int argc, Scheme_Object **argv)
{
   	ArgCheck("set-max-physical", "i", argc, argv);
    Engine::Get()->Physics()->SetMaxObjectCount(IntFromScheme(argv[0]));
	return scheme_void;
}

Scheme_Object *set_mass(int argc, Scheme_Object **argv)
{
   	ArgCheck("set-mass", "if", argc, argv);
    int obj=IntFromScheme(argv[0]);
    float mass=FloatFromScheme(argv[1]);
	Engine::Get()->Physics()->SetMass(obj,mass);
	return scheme_void;
}

Scheme_Object *gravity(int argc, Scheme_Object **argv)
{
   	ArgCheck("gravity", "v", argc, argv);
	float vec[3];
	FloatsFromScheme(argv[0],vec,3);
	Engine::Get()->Physics()->SetGravity(dVector(vec[0],vec[1],vec[2]));
	return scheme_void;
}

Scheme_Object *kick(int argc, Scheme_Object **argv)
{
    ArgCheck("kick", "iv", argc, argv);
    int obj=IntFromScheme(argv[0]);
    float vec[3];
    FloatsFromScheme(argv[1],vec,3);
	Engine::Get()->Physics()->Kick(obj,dVector(vec[0],vec[1],vec[2]));
	return scheme_void;
}

Scheme_Object *twist(int argc, Scheme_Object **argv)
{
    ArgCheck("twist", "iv", argc, argv);
    int obj=IntFromScheme(argv[0]);
    float vec[3];
    FloatsFromScheme(argv[1],vec,3);
	Engine::Get()->Physics()->Twist(obj,dVector(vec[0],vec[1],vec[2]));
	return scheme_void;
}


Scheme_Object *has_collided(int argc, Scheme_Object **argv)
{
    ArgCheck("has-collided", "i", argc, argv);
	if (Engine::Get()->Physics()->HasCollided(IntFromScheme(argv[0])))
	{
		return scheme_make_true();
	}
	else
	{
		return scheme_make_false();
	}
}


void PhysicsFunctions::AddGlobals(Scheme_Env *env)
{	
	scheme_add_global("collisions", scheme_make_prim_w_arity(collisions, "collisions", 1, 1), env);
	scheme_add_global("ground-plane", scheme_make_prim_w_arity(ground_plane, "ground-plane", 2, 2), env);
	scheme_add_global("active-box", scheme_make_prim_w_arity(active_box, "active-box", 1, 1), env);
	scheme_add_global("active-cylinder", scheme_make_prim_w_arity(active_box, "active-cylinder", 1, 1), env);
	scheme_add_global("active-sphere", scheme_make_prim_w_arity(active_box, "active-sphere", 1, 1), env);
	scheme_add_global("passive-box", scheme_make_prim_w_arity(passive_box, "passive-box", 1, 1), env);
	scheme_add_global("passive-cylinder", scheme_make_prim_w_arity(passive_box, "passive-cylinder", 1, 1), env);
	scheme_add_global("passive-sphere", scheme_make_prim_w_arity(passive_box, "passive-sphere", 1, 1), env);
	scheme_add_global("surface-params", scheme_make_prim_w_arity(surface_params, "surface-params", 4, 4), env);
	scheme_add_global("build-balljoint", scheme_make_prim_w_arity(build_balljoint, "build-balljoint", 3, 3), env);
	scheme_add_global("build-fixedjoint", scheme_make_prim_w_arity(build_fixedjoint, "build-fixedjoint", 1, 1), env);
	scheme_add_global("build-hingejoint", scheme_make_prim_w_arity(build_hingejoint, "build-hingejoint", 4, 4), env);
	scheme_add_global("build-sliderjoint", scheme_make_prim_w_arity(build_sliderjoint, "build-sliderjoint", 3, 3), env);
	scheme_add_global("build-hinge2joint", scheme_make_prim_w_arity(build_hinge2joint, "build-hinge2joint", 5, 5), env);
	scheme_add_global("build-amotorjoint", scheme_make_prim_w_arity(build_amotorjoint, "build-amotorjoint", 3, 3), env);
	scheme_add_global("joint-param", scheme_make_prim_w_arity(joint_param, "joint-param", 3, 3), env);
	scheme_add_global("joint-angle", scheme_make_prim_w_arity(joint_angle, "joint-angle", 3, 3), env);
	scheme_add_global("set-max-physical", scheme_make_prim_w_arity(set_max_physical, "set-max-physical", 1, 1), env);
	scheme_add_global("set-mass", scheme_make_prim_w_arity(set_mass, "set-mass", 2, 2), env);
	scheme_add_global("gravity", scheme_make_prim_w_arity(gravity, "gravity", 1, 1), env);
	scheme_add_global("kick", scheme_make_prim_w_arity(kick, "kick", 2, 2), env);
	scheme_add_global("twist", scheme_make_prim_w_arity(twist, "twist", 2, 2), env);
	scheme_add_global("has-collided", scheme_make_prim_w_arity(has_collided, "has-collided", 1, 1), env);
}
