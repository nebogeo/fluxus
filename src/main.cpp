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
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <sys/time.h>
#include <iostream>
#include <fstream>
#include <cstdlib>
#include <unistd.h>
#include <cstdio>
#include <FL/Fl.h>
#include <fluxus/Renderer.h>
#include <fluxus/GraphicsUtils.h>
#include <fluxus/LinePrimitive.h>
#include "GUI.h"
#include "AudioCollector.h"
#include "TurtleBuilder.h"

#include <guile/gh.h>

using namespace fluxus;

static const string INIT_FILE=".fluxus.scm";

void RenderFlx(const string &Filename, int w, int h);

GUI *Fluxus;
AudioCollector *Audio;
TurtleBuilder turtle;
char *engine_callback_str=NULL;
long frame_count = 0;
ofstream *flx_out;

static const int AUDIO_BUFFER_SIZE = 512;

static PolyPrimitive*
 StaticCube;
static PolyPrimitive* StaticPlane;
static PolyPrimitive* StaticSphere;
static PolyPrimitive* StaticCylinder;

SCM build_cube()
{
	PolyPrimitive *BoxPrim = new PolyPrimitive(PolyPrimitive::QUADS);
    MakeCube(BoxPrim);
    BoxPrim->Finalise();
    return gh_double2scm(Fluxus->GetRenderer()->AddPrimitive(BoxPrim));
}

SCM build_plane()
{
	PolyPrimitive *PlanePrim = new PolyPrimitive(PolyPrimitive::QUADS);
    MakePlane(PlanePrim);
    PlanePrim->Finalise();    	
    return gh_double2scm(Fluxus->GetRenderer()->AddPrimitive(PlanePrim));
}


SCM build_cylinder(SCM s_hsegments, SCM s_rsegments)
{
	SCM_ASSERT(SCM_NUMBERP(s_hsegments), s_hsegments, SCM_ARG1, "h segments");
	SCM_ASSERT(SCM_NUMBERP(s_rsegments), s_rsegments, SCM_ARG2, "r segments");
	double hsegments;
	hsegments=gh_scm2double(s_hsegments);
    double rsegments;
	rsegments=gh_scm2double(s_rsegments);	

	PolyPrimitive *CylPrim = new PolyPrimitive(PolyPrimitive::TRILIST);
    MakeCylinder(CylPrim, 1, 1, (int)hsegments, (int)rsegments);
    CylPrim->Finalise();    	

    return gh_double2scm(Fluxus->GetRenderer()->AddPrimitive(CylPrim));
}

SCM build_line(SCM start, SCM swidth, SCM end, SCM ewidth)
{
	SCM_ASSERT(SCM_VECTORP(start), start, SCM_ARG1, "start");
	SCM_ASSERT(SCM_NUMBERP(swidth), swidth, SCM_ARG2, "start width");
	SCM_ASSERT(SCM_VECTORP(end), end, SCM_ARG3, "end");
	SCM_ASSERT(SCM_NUMBERP(ewidth), ewidth, SCM_ARG4, "end width");
	float s[3],e[3];
	gh_scm2floats(start,s);
	gh_scm2floats(end,e);
	
	LinePrimitive *LinePrim = new LinePrimitive;
	LinePrim->SetStart(dVertex(dVector(s[0],s[1],s[2]),dVector(0,1,0)),gh_scm2double(swidth));
	LinePrim->SetEnd(dVertex(dVector(e[0],e[1],e[2]),dVector(0,1,0)),gh_scm2double(ewidth));
	
	return gh_double2scm(Fluxus->GetRenderer()->AddPrimitive(LinePrim));
}

SCM draw_cube()
{    	
    Fluxus->GetRenderer()->RenderPrimitive(StaticCube);
    return SCM_UNSPECIFIED;
}

SCM draw_plane()
{    	
    Fluxus->GetRenderer()->RenderPrimitive(StaticPlane);
    return SCM_UNSPECIFIED;
}

SCM draw_sphere()
{    	
    Fluxus->GetRenderer()->RenderPrimitive(StaticSphere);
    return SCM_UNSPECIFIED;
}

SCM draw_cylinder()
{    	
    Fluxus->GetRenderer()->RenderPrimitive(StaticCylinder);
    return SCM_UNSPECIFIED;
}

SCM build_sphere(SCM s_hsegments, SCM s_rsegments)
{
	SCM_ASSERT(SCM_NUMBERP(s_hsegments), s_hsegments, SCM_ARG1, "h segments");
	SCM_ASSERT(SCM_NUMBERP(s_rsegments), s_rsegments, SCM_ARG2, "r segments");
	double hsegments;
	hsegments=gh_scm2double(s_hsegments);
    double rsegments;
	rsegments=gh_scm2double(s_rsegments);	
	
	PolyPrimitive *SphPrim = new PolyPrimitive(PolyPrimitive::TRILIST);
    MakeSphere(SphPrim, 1, (int)hsegments, (int)rsegments);
    SphPrim->Finalise();    	
    return gh_double2scm(Fluxus->GetRenderer()->AddPrimitive(SphPrim));
}

SCM key_pressed(SCM s_key)
{
	SCM_ASSERT(SCM_STRINGP(s_key), s_key, SCM_ARG1, "key");
	char *key=0;
	size_t size=0;
	key=gh_scm2newstr(s_key,&size);	
    double pressed = Fluxus->KeyPressed(key[0]);
    free(key);
    return gh_bool2scm((int)pressed);
}

SCM show_axis(SCM s_id)
{
	SCM_ASSERT(SCM_NUMBERP(s_id), s_id, SCM_ARG1, "texture id");	
    Fluxus->GetRenderer()->ShowAxis(gh_scm2int(s_id));
    return SCM_UNSPECIFIED;
}

SCM make_light(SCM cam)
{
	SCM_ASSERT(SCM_NUMBERP(cam), cam, SCM_ARG1, "camera locked");
	Light *l=new Light;
	l->SetCameraLock((bool)gh_scm2double(cam));
	return gh_double2scm(Fluxus->GetRenderer()->AddLight(l));
}

SCM clear_lights()
{
	Fluxus->GetRenderer()->ClearLights();
	return SCM_UNSPECIFIED;
}

SCM light_ambient(SCM id, SCM v)
{
	SCM_ASSERT(SCM_NUMBERP(id), id, SCM_ARG1, "light id");	
	SCM_ASSERT(SCM_VECTORP(v), v, SCM_ARG2, "ambient");
	float vec[3];
	gh_scm2floats(v,vec);
	Fluxus->GetRenderer()->GetLight((int)gh_scm2double(id))->SetAmbient(dColour(vec[0],vec[1],vec[2]));
	return SCM_UNSPECIFIED;
}

SCM light_diffuse(SCM id, SCM v)
{
	SCM_ASSERT(SCM_NUMBERP(id), id, SCM_ARG1, "light id");	
	SCM_ASSERT(SCM_VECTORP(v), v, SCM_ARG2, "diffuse");
	float vec[3];
	gh_scm2floats(v,vec);
	Fluxus->GetRenderer()->GetLight((int)gh_scm2double(id))->SetDiffuse(dColour(vec[0],vec[1],vec[2]));
	return SCM_UNSPECIFIED;
}

SCM light_specular(SCM id, SCM v)
{
	SCM_ASSERT(SCM_NUMBERP(id), id, SCM_ARG1, "light id");	
	SCM_ASSERT(SCM_VECTORP(v), v, SCM_ARG2, "specular");
	float vec[3];
	gh_scm2floats(v,vec);
	Fluxus->GetRenderer()->GetLight((int)gh_scm2double(id))->SetSpecular(dColour(vec[0],vec[1],vec[2]));
	return SCM_UNSPECIFIED;
}

SCM light_position(SCM id, SCM v)
{
	SCM_ASSERT(SCM_NUMBERP(id), id, SCM_ARG1, "light id");	
	SCM_ASSERT(SCM_VECTORP(v), v, SCM_ARG2, "position");
	float vec[3];
	gh_scm2floats(v,vec);
	Fluxus->GetRenderer()->GetLight((int)gh_scm2double(id))->SetPosition(dVector(vec[0],vec[1],vec[2]));
	return SCM_UNSPECIFIED;
}

SCM lock_camera(SCM s_ob)
{
	SCM_ASSERT(SCM_NUMBERP(s_ob), s_ob, SCM_ARG1, "primitive");
	int ob=0;
	ob=(int)gh_scm2double(s_ob);	
    Fluxus->GetRenderer()->LockCamera(ob);
    return SCM_UNSPECIFIED;
}

SCM destroy(SCM s_name)
{
	SCM_ASSERT(SCM_NUMBERP(s_name), s_name, SCM_ARG1, "name");
	int name=0;
	name=(int)gh_scm2double(s_name);	
	
	Primitive *p=Fluxus->GetRenderer()->GetPrimitive(name);
	if (p)
	{
    	if (p->IsPhysicalHint())
    	{
    		Fluxus->GetPhysics()->Free(name);
    	}
    	Fluxus->GetRenderer()->RemovePrimitive(name);
    }

    return SCM_UNSPECIFIED;
}

SCM clear()
{
	Fluxus->GetRenderer()->Clear();
	Fluxus->GetPhysics()->Clear();
	Fluxus->ClearLifeforms();
	if (engine_callback_str) free(engine_callback_str);
	engine_callback_str=NULL;
	return SCM_UNSPECIFIED;
}

SCM grab(SCM s_id)
{
	SCM_ASSERT(SCM_NUMBERP(s_id), s_id, SCM_ARG1, "id");
	Fluxus->GetRenderer()->Grab((int)gh_scm2double(s_id));
	return SCM_UNSPECIFIED;
}

SCM ungrab()
{
	Fluxus->GetRenderer()->UnGrab();
	return SCM_UNSPECIFIED;
}

SCM apply(SCM s_id)
{
	SCM_ASSERT(SCM_NUMBERP(s_id), s_id, SCM_ARG1, "id");
	Fluxus->GetRenderer()->GetPrimitive((int)gh_scm2double(s_id))->ApplyTransform();
	return SCM_UNSPECIFIED;
}

SCM opacity(SCM s_opac)
{
    SCM_ASSERT(SCM_NUMBERP(s_opac), s_opac, SCM_ARG1, "opacity");
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Opacity=gh_scm2double(s_opac);
    else Fluxus->GetRenderer()->GetState()->Opacity=gh_scm2double(s_opac);
    return SCM_UNSPECIFIED;
}

SCM shinyness(SCM s_opac)
{
    SCM_ASSERT(SCM_NUMBERP(s_opac), s_opac, SCM_ARG1, "shinyness");
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Shinyness=gh_scm2double(s_opac);
    else Fluxus->GetRenderer()->GetState()->Shinyness=gh_scm2double(s_opac);
    return SCM_UNSPECIFIED;
}

SCM colour(SCM s_vec)
{
    SCM_ASSERT(SCM_VECTORP(s_vec), s_vec, SCM_ARG1, "colour");
	float col[3];
	gh_scm2floats(s_vec,col);
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Colour=dColour(col[0],col[1],col[2]);
    else Fluxus->GetRenderer()->GetState()->Colour=dColour(col[0],col[1],col[2]);
    return SCM_UNSPECIFIED;
}

SCM specular(SCM s_vec)
{
    SCM_ASSERT(SCM_VECTORP(s_vec), s_vec, SCM_ARG1, "colour");
	float col[3];
	gh_scm2floats(s_vec,col);
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Specular=dColour(col[0],col[1],col[2]);
    else Fluxus->GetRenderer()->GetState()->Specular=dColour(col[0],col[1],col[2]);
    return SCM_UNSPECIFIED;
}

SCM ambient(SCM s_vec)
{
    SCM_ASSERT(SCM_VECTORP(s_vec), s_vec, SCM_ARG1, "colour");
	float col[3];
	gh_scm2floats(s_vec,col);
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Ambient=dColour(col[0],col[1],col[2]);
    else Fluxus->GetRenderer()->GetState()->Ambient=dColour(col[0],col[1],col[2]);
    return SCM_UNSPECIFIED;
}

SCM emissive(SCM s_vec)
{
    SCM_ASSERT(SCM_VECTORP(s_vec), s_vec, SCM_ARG1, "colour");
	float col[3];
	gh_scm2floats(s_vec,col);
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Emissive=dColour(col[0],col[1],col[2]);
    else Fluxus->GetRenderer()->GetState()->Emissive=dColour(col[0],col[1],col[2]);
    return SCM_UNSPECIFIED;
}

SCM identity()
{
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Transform.init();
    else Fluxus->GetRenderer()->GetState()->Transform.init();
    return SCM_UNSPECIFIED;
}

SCM translate(SCM s_vec)
{
    SCM_ASSERT(SCM_VECTORP(s_vec), s_vec, SCM_ARG1, "colour");
	float trans[3];
	gh_scm2floats(s_vec,trans);
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Transform.translate(trans[0],trans[1],trans[2]);
    else Fluxus->GetRenderer()->GetState()->Transform.translate(trans[0],trans[1],trans[2]);
    return SCM_UNSPECIFIED;
}

SCM rotate(SCM s_vec)
{
    SCM_ASSERT(SCM_VECTORP(s_vec), s_vec, SCM_ARG1, "colour");
	float rot[0];
	gh_scm2floats(s_vec,rot);
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed)
    {
    	Grabbed->GetState()->Transform.rotxyz(rot[0],rot[1],rot[2]);
    }
    else
    {
    	Fluxus->GetRenderer()->GetState()->Transform.rotxyz(rot[0],rot[1],rot[2]);
    }
    return SCM_UNSPECIFIED;
}

SCM scale(SCM s_vec)
{
    SCM_ASSERT(SCM_VECTORP(s_vec), s_vec, SCM_ARG1, "colour");
	float scale[3];
	gh_scm2floats(s_vec,scale);
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Transform.scale(scale[0],scale[1],scale[2]);
    else Fluxus->GetRenderer()->GetState()->Transform.scale(scale[0],scale[1],scale[2]);
    return SCM_UNSPECIFIED;
}

SCM parent(SCM s_p)
{
    SCM_ASSERT(SCM_NUMBERP(s_p), s_p, SCM_ARG1, "parent");
    Fluxus->GetRenderer()->GetState()->Parent=(int)gh_scm2double(s_p);
    return SCM_UNSPECIFIED;
}

SCM line_width(SCM s_p)
{
    SCM_ASSERT(SCM_NUMBERP(s_p), s_p, SCM_ARG1, "width");
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->LineWidth=gh_scm2double(s_p);
    else Fluxus->GetRenderer()->GetState()->LineWidth=gh_scm2double(s_p);
    return SCM_UNSPECIFIED;
}

SCM hint_solid()
{
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Hints|=HINT_SOLID;
    else Fluxus->GetRenderer()->GetState()->Hints|=HINT_SOLID;
    return SCM_UNSPECIFIED;
}

SCM hint_wire()
{
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Hints|=HINT_WIRE;
    else Fluxus->GetRenderer()->GetState()->Hints|=HINT_WIRE;
    return SCM_UNSPECIFIED;
}

SCM hint_normal()
{
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Hints|=HINT_NORMAL;
    else Fluxus->GetRenderer()->GetState()->Hints|=HINT_NORMAL;
    return SCM_UNSPECIFIED;
}

SCM hint_points()
{
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Hints|=HINT_POINTS;
    else Fluxus->GetRenderer()->GetState()->Hints|=HINT_POINTS;
    return SCM_UNSPECIFIED;
}

SCM hint_anti_alias()
{
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Hints|=HINT_AALIAS;
    else Fluxus->GetRenderer()->GetState()->Hints|=HINT_AALIAS;
    return SCM_UNSPECIFIED;
}

SCM hint_none()
{
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Hints=0;
    else Fluxus->GetRenderer()->GetState()->Hints=0;
    return SCM_UNSPECIFIED;
}

SCM blur(SCM s_blur)
{
    SCM_ASSERT(SCM_NUMBERP(s_blur), s_blur, SCM_ARG1, "blur amount");
	double blur;
	blur=gh_scm2double(s_blur);	
	if (!blur) Fluxus->GetRenderer()->SetMotionBlur(false);
    else Fluxus->GetRenderer()->SetMotionBlur(true, (float)blur);
    return SCM_UNSPECIFIED;
}

SCM push()
{
    Fluxus->GetRenderer()->PushState();
    return SCM_UNSPECIFIED;
}

SCM pop()
{
    Fluxus->GetRenderer()->PopState();
    return SCM_UNSPECIFIED;
}

SCM collisions(SCM s)
{
	SCM_ASSERT(SCM_NUMBERP(s), s, SCM_ARG1, "set");
	Fluxus->GetPhysics()->SetCollisions(gh_scm2double(s));
	return SCM_UNSPECIFIED;
}

SCM ground_plane(SCM s_ori, SCM s_off)
{
    SCM_ASSERT(SCM_VECTORP(s_ori), s_ori, SCM_ARG1, "orientation");
    SCM_ASSERT(SCM_NUMBERP(s_off), s_off, SCM_ARG2, "offset");
    float ori[3];
	gh_scm2floats(s_ori,ori);
	float off = (float) gh_scm2double(s_off);
	Fluxus->GetPhysics()->GroundPlane(dVector(ori[0],ori[1],ori[2]),off);
	 return SCM_UNSPECIFIED;
}

SCM active_box(SCM s_name)
{
    SCM_ASSERT(SCM_NUMBERP(s_name), s_name, SCM_ARG1, "object name");
	int name=0;
	name=(int)gh_scm2double(s_name);	
	Fluxus->GetPhysics()->MakeActive(name,1.0f,Physics::BOX);
    return SCM_UNSPECIFIED;
}

SCM active_cylinder(SCM s_name)
{
    SCM_ASSERT(SCM_NUMBERP(s_name), s_name, SCM_ARG1, "object name");
	int name=0;
	name=(int)gh_scm2double(s_name);	
	Fluxus->GetPhysics()->MakeActive(name,1.0f,Physics::CYLINDER);
    return SCM_UNSPECIFIED;
}

SCM active_sphere(SCM s_name)
{
    SCM_ASSERT(SCM_NUMBERP(s_name), s_name, SCM_ARG1, "object name");
	int name=0;
	name=(int)gh_scm2double(s_name);	
	Fluxus->GetPhysics()->MakeActive(name,1.0f,Physics::SPHERE);
    return SCM_UNSPECIFIED;
}

SCM passive_box(SCM s_name)
{
    SCM_ASSERT(SCM_STRINGP(s_name), s_name, SCM_ARG1, "object name");
	int name=0;
	name=(int)gh_scm2double(s_name);	
	Fluxus->GetPhysics()->MakePassive(name,1.0f,Physics::BOX);
    return SCM_UNSPECIFIED;
}

SCM passive_cylinder(SCM s_name)
{
    SCM_ASSERT(SCM_STRINGP(s_name), s_name, SCM_ARG1, "object name");
	int name=0;
	name=(int)gh_scm2double(s_name);	
	Fluxus->GetPhysics()->MakePassive(name,1.0f,Physics::CYLINDER);
    return SCM_UNSPECIFIED;
}

SCM passive_sphere(SCM s_name)
{
    SCM_ASSERT(SCM_STRINGP(s_name), s_name, SCM_ARG1, "object name");
	int name=0;
	name=(int)gh_scm2double(s_name);	
	Fluxus->GetPhysics()->MakePassive(name,1.0f,Physics::SPHERE);
   return SCM_UNSPECIFIED;
}

SCM build_hinge2joint(SCM s_ob1, SCM s_ob2, SCM s_anchor, SCM s_hinge1, SCM s_hinge2)
{
    SCM_ASSERT(SCM_NUMBERP(s_ob1),    s_ob1,    SCM_ARG1, "object 1");
    SCM_ASSERT(SCM_NUMBERP(s_ob2),    s_ob2,    SCM_ARG2, "object 2");
    SCM_ASSERT(SCM_VECTORP(s_anchor), s_anchor, SCM_ARG3, "anchor");
    SCM_ASSERT(SCM_VECTORP(s_hinge1), s_hinge1, SCM_ARG4, "hinge 1");
    SCM_ASSERT(SCM_VECTORP(s_hinge2), s_hinge2, SCM_ARG5, "hinge 2");

    int name1=0;
	name1=(int)gh_scm2double(s_ob1);
	int name2=0;
	name2=(int)gh_scm2double(s_ob2);
	
	float anchor[3];
	gh_scm2floats(s_anchor,anchor);
	
	dVector Hinge[2];
	float temp[3];
	gh_scm2floats(s_hinge1,temp);
	Hinge[0].x=temp[0];
	Hinge[0].y=temp[1];
	Hinge[0].z=temp[2];
	
	gh_scm2floats(s_hinge2,temp);
	Hinge[1].x=temp[0];
	Hinge[1].y=temp[1];
	Hinge[1].z=temp[2];
	
	return gh_double2scm(Fluxus->GetPhysics()->CreateJointHinge2(name1, name2, dVector(anchor[0],anchor[1],anchor[2]), Hinge));
}

SCM build_balljoint(SCM s_ob1, SCM s_ob2, SCM s_anchor)
{
    SCM_ASSERT(SCM_NUMBERP(s_ob1),    s_ob1,    SCM_ARG1, "object 1");
    SCM_ASSERT(SCM_NUMBERP(s_ob2),    s_ob2,    SCM_ARG2, "object 2");
    SCM_ASSERT(SCM_VECTORP(s_anchor), s_anchor, SCM_ARG3, "anchor");
    int name1=0;
	name1=(int)gh_scm2double(s_ob1);
	int name2=0;
	name2=(int)gh_scm2double(s_ob2);	
	float anchor[3];
	gh_scm2floats(s_anchor,anchor);
	return gh_double2scm(Fluxus->GetPhysics()->CreateJointBall(name1, name2, dVector(anchor[0],anchor[1],anchor[2])));
}

SCM joint_vel2(SCM s_joint, SCM s_value)
{
    SCM_ASSERT(SCM_NUMBERP(s_joint), s_joint, SCM_ARG1, "joint");
    SCM_ASSERT(SCM_NUMBERP(s_value), s_value, SCM_ARG2, "value");
    int joint=0;
	joint=(int)gh_scm2double(s_joint);
    double v = gh_scm2double(s_value);
	Fluxus->GetPhysics()->SetJointParam(joint,Physics::Vel2,v);
	return SCM_UNSPECIFIED;
}

SCM joint_fmax2(SCM s_joint, SCM s_value)
{
    SCM_ASSERT(SCM_NUMBERP(s_joint), s_joint, SCM_ARG1, "joint");
    SCM_ASSERT(SCM_NUMBERP(s_value), s_value, SCM_ARG2, "value");
    int joint=0;
	joint=(int)gh_scm2double(s_joint);
    double v = gh_scm2double(s_value);
	Fluxus->GetPhysics()->SetJointParam(joint,Physics::FMax2,v);
	return SCM_UNSPECIFIED;
}

SCM joint_fmax(SCM s_joint, SCM s_value)
{
    SCM_ASSERT(SCM_NUMBERP(s_joint), s_joint, SCM_ARG1, "joint");
    SCM_ASSERT(SCM_NUMBERP(s_value), s_value, SCM_ARG2, "value");
    int joint=0;
	joint=(int)gh_scm2double(s_joint);
    double v = gh_scm2double(s_value);
	Fluxus->GetPhysics()->SetJointParam(joint,Physics::FMax,v);
	return SCM_UNSPECIFIED;
}

SCM joint_histop(SCM s_joint, SCM s_value)
{
    SCM_ASSERT(SCM_NUMBERP(s_joint), s_joint, SCM_ARG1, "joint");
    SCM_ASSERT(SCM_NUMBERP(s_value), s_value, SCM_ARG2, "value");
    int joint=0;
	joint=(int)gh_scm2double(s_joint);
    double v = gh_scm2double(s_value);
	Fluxus->GetPhysics()->SetJointParam(joint,Physics::HiStop,v);
	return SCM_UNSPECIFIED;
}

SCM joint_lostop(SCM s_joint, SCM s_value)
{
    SCM_ASSERT(SCM_NUMBERP(s_joint), s_joint, SCM_ARG1, "joint");
    SCM_ASSERT(SCM_NUMBERP(s_value), s_value, SCM_ARG2, "value");
    int joint=0;
	joint=(int)gh_scm2double(s_joint);
    double v = gh_scm2double(s_value);
	Fluxus->GetPhysics()->SetJointParam(joint,Physics::LoStop,v);
	return SCM_UNSPECIFIED;
}

SCM joint_vel(SCM s_joint, SCM s_value)
{
    SCM_ASSERT(SCM_NUMBERP(s_joint), s_joint, SCM_ARG1, "joint");
    SCM_ASSERT(SCM_NUMBERP(s_value), s_value, SCM_ARG2, "value");
    int joint=0;
	joint=(int)gh_scm2double(s_joint);
    double v = gh_scm2double(s_value);
	Fluxus->GetPhysics()->SetJointParam(joint,Physics::Vel,v);
	return SCM_UNSPECIFIED;
}

SCM joint_fudge(SCM s_joint, SCM s_value)
{
    SCM_ASSERT(SCM_NUMBERP(s_joint), s_joint, SCM_ARG1, "joint");
    SCM_ASSERT(SCM_NUMBERP(s_value), s_value, SCM_ARG2, "value");
    int joint=0;
	joint=(int)gh_scm2double(s_joint);
    double v = gh_scm2double(s_value);
	Fluxus->GetPhysics()->SetJointParam(joint,Physics::FudgeFactor,v);
	return SCM_UNSPECIFIED;
}

SCM set_max_physical(SCM s_value)
{
    SCM_ASSERT(SCM_NUMBERP(s_value), s_value, SCM_ARG2, "value");
    double v = gh_scm2double(s_value);
    Fluxus->GetPhysics()->SetMaxObjectCount((int)v);
	return SCM_UNSPECIFIED;
}

SCM kick(SCM s_obj, SCM s_vec)
{
    SCM_ASSERT(SCM_NUMBERP(s_obj), s_obj, SCM_ARG1, "object");
    SCM_ASSERT(SCM_VECTORP(s_vec), s_vec, SCM_ARG2, "vector");
    int obj=0;
	obj=(int)gh_scm2double(s_obj);
    float vec[3];
    gh_scm2floats(s_vec,vec);
	Fluxus->GetPhysics()->Kick(obj,dVector(vec[0],vec[1],vec[2]));
	return SCM_UNSPECIFIED;
}

SCM twist(SCM s_obj, SCM s_vec)
{
    SCM_ASSERT(SCM_NUMBERP(s_obj), s_obj, SCM_ARG1, "object");
    SCM_ASSERT(SCM_VECTORP(s_vec), s_vec, SCM_ARG2, "vector");
    int obj=0;
	obj=(int)gh_scm2double(s_obj);
    float vec[3];
    gh_scm2floats(s_vec,vec);
	Fluxus->GetPhysics()->Twist(obj,dVector(vec[0],vec[1],vec[2]));
	return SCM_UNSPECIFIED;
}

SCM srandom()
{
	return gh_double2scm(RandFloat());
}

SCM get_harmonic(SCM s_harm)
{
	SCM_ASSERT(SCM_NUMBERP(s_harm), s_harm, SCM_ARG1, "harmonic");	
    return gh_double2scm(Audio->GetHarmonic(gh_scm2int(s_harm)));
}

SCM load_texture(SCM s_name)
{
	SCM_ASSERT(SCM_STRINGP(s_name), s_name, SCM_ARG1, "name");
	char *name=0;
	size_t size=0;
	name=gh_scm2newstr(s_name,&size);	
	
	int id=Fluxus->GetRenderer()->LoadTexture(name);

    free(name);
    return gh_int2scm(id);
}

SCM texture(SCM s_id)
{
	SCM_ASSERT(SCM_NUMBERP(s_id), s_id, SCM_ARG1, "texture id");	
    Fluxus->GetRenderer()->GetState()->Texture=gh_scm2int(s_id);
    return SCM_UNSPECIFIED;
}

SCM engine_callback(SCM s_func)
{
	SCM_ASSERT(SCM_STRINGP(s_func), s_func, SCM_ARG1, "callback");
	size_t size=0;
	engine_callback_str=gh_scm2newstr(s_func,&size);
    return SCM_UNSPECIFIED;
}

SCM ortho()
{
	Fluxus->ortho();
	Fluxus->GetRenderer()->SetOrtho(true);
    return SCM_UNSPECIFIED;
}

SCM persp()
{
	Fluxus->GetRenderer()->SetOrtho(false);
    return SCM_UNSPECIFIED;
}

SCM frame()
{
	return gh_double2scm(frame_count);
}

SCM reset_camera()
{
	Fluxus->ResetCamera();
	return SCM_UNSPECIFIED;
}

SCM print_scene_graph()
{
	Fluxus->GetRenderer()->PrintSceneGraph();
	return SCM_UNSPECIFIED;
}

SCM make_lifeforms(SCM s_name)
{
	SCM_ASSERT(SCM_STRINGP(s_name), s_name, SCM_ARG1, "name");
	size_t size=0;
	char *name=gh_scm2newstr(s_name,&size);SCM_ASSERT(SCM_STRINGP(s_name), s_name, SCM_ARG1, "name");
	Fluxus->AddLifeforms(name);
	free(name);
    return SCM_UNSPECIFIED;
}

SCM add_lifeform(SCM s_name, SCM s_obj)
{
	SCM_ASSERT(SCM_STRINGP(s_name), s_name, SCM_ARG1, "name");
	SCM_ASSERT(SCM_NUMBERP(s_obj), s_obj, SCM_ARG2, "object");	
	size_t size=0;
	char *name=gh_scm2newstr(s_name,&size);
	int o = (int)gh_scm2double(s_obj);
	Lifeforms *life=Fluxus->GetLifeforms(name);
	if (life) life->AddLifeform(o,Fluxus->GetRenderer()->GetPrimitive(o)->GetState()->Transform);
	free(name);
    return SCM_UNSPECIFIED;
}

SCM lifeform_avoidance(SCM s_name, SCM s_obj)
{
	SCM_ASSERT(SCM_STRINGP(s_name), s_name, SCM_ARG1, "name");
	SCM_ASSERT(SCM_NUMBERP(s_obj), s_obj, SCM_ARG2, "value");	
	size_t size=0;
	char *name=gh_scm2newstr(s_name,&size);
	Fluxus->GetLifeforms(name)->SetAvoidance(gh_scm2double(s_obj));
	free(name);
    return SCM_UNSPECIFIED;
}

SCM lifeform_flockcentering(SCM s_name, SCM s_obj)
{
	SCM_ASSERT(SCM_STRINGP(s_name), s_name, SCM_ARG1, "name");
	SCM_ASSERT(SCM_NUMBERP(s_obj), s_obj, SCM_ARG2, "value");	
	size_t size=0;
	char *name=gh_scm2newstr(s_name,&size);
	Fluxus->GetLifeforms(name)->SetFlockCentering(gh_scm2double(s_obj));
	free(name);
    return SCM_UNSPECIFIED;
}

SCM lifeform_scenecentering(SCM s_name, SCM s_obj)
{
	SCM_ASSERT(SCM_STRINGP(s_name), s_name, SCM_ARG1, "name");
	SCM_ASSERT(SCM_NUMBERP(s_obj), s_obj, SCM_ARG2, "value");	
	size_t size=0;
	char *name=gh_scm2newstr(s_name,&size);
	Fluxus->GetLifeforms(name)->SetSceneCentering(gh_scm2double(s_obj));
	free(name);
    return SCM_UNSPECIFIED;
}	

SCM lifeform_inertia(SCM s_name, SCM s_obj)
{
	SCM_ASSERT(SCM_STRINGP(s_name), s_name, SCM_ARG1, "name");
	SCM_ASSERT(SCM_NUMBERP(s_obj), s_obj, SCM_ARG2, "value");	
	size_t size=0;
	char *name=gh_scm2newstr(s_name,&size);
	Fluxus->GetLifeforms(name)->SetInertia(gh_scm2double(s_obj));
	free(name);
    return SCM_UNSPECIFIED;
}
	
SCM lifeform_scenecentre(SCM s_name, SCM s_obj)
{
	SCM_ASSERT(SCM_STRINGP(s_name), s_name, SCM_ARG1, "name");
	SCM_ASSERT(SCM_VECTORP(s_obj), s_obj, SCM_ARG2, "value");	
	size_t size=0;
	char *name=gh_scm2newstr(s_name,&size);
	float centre[3];
	gh_scm2floats(s_obj,centre);
	Fluxus->GetLifeforms(name)->SetSceneCentre(dVector(centre[0],centre[1],centre[2]));
	free(name);
    return SCM_UNSPECIFIED;
}

SCM lifeform_maxspeed(SCM s_name, SCM s_obj)
{
	SCM_ASSERT(SCM_STRINGP(s_name), s_name, SCM_ARG1, "name");
	SCM_ASSERT(SCM_NUMBERP(s_obj), s_obj, SCM_ARG2, "value");	
	size_t size=0;
	char *name=gh_scm2newstr(s_name,&size);
	Fluxus->GetLifeforms(name)->SetMaxSpeed(gh_scm2double(s_obj));
	free(name);
    return SCM_UNSPECIFIED;
}
	
SCM save_frame(SCM s_name)
{
	SCM_ASSERT(SCM_STRINGP(s_name), s_name, SCM_ARG1, "name");	
    size_t size=0;
	char *name=gh_scm2newstr(s_name,&size);
	ofstream out(name,ios::binary);
	out<<*Fluxus->GetRenderer();
    return SCM_UNSPECIFIED;
}

SCM start_flx(SCM s_name)
{
	SCM_ASSERT(SCM_STRINGP(s_name), s_name, SCM_ARG1, "name");	
    size_t size=0;
	char *name=gh_scm2newstr(s_name,&size);
	if (flx_out==NULL) flx_out = new ofstream(name,ios::binary);
    return SCM_UNSPECIFIED;
}

SCM end_flx()
{   	
	if (flx_out!=NULL)
	{	
		delete flx_out;
		flx_out=NULL;
	}
    return SCM_UNSPECIFIED;
}

SCM load(SCM s_name)
{
	SCM_ASSERT(SCM_STRINGP(s_name), s_name, SCM_ARG1, "name");	
    size_t size=0;
	char *name=gh_scm2newstr(s_name,&size);
	Fluxus->LoadScript(name);
    return SCM_UNSPECIFIED;
}

SCM source(SCM s_name)
{
	SCM_ASSERT(SCM_STRINGP(s_name), s_name, SCM_ARG1, "name");	
    size_t size=0;
	char *name=gh_scm2newstr(s_name,&size);
	Fluxus->LoadScript(name);
	Fluxus->RunScript();
    return SCM_UNSPECIFIED;
}

SCM fluxface(SCM s_name, SCM x, SCM y)
{
	SCM_ASSERT(SCM_STRINGP(s_name), s_name, SCM_ARG1, "name");	
    size_t size=0;
	char *name=gh_scm2newstr(s_name,&size);
	Fluxus->GetFluxFace()->New(name,(int)gh_scm2double(x),(int)gh_scm2double(y));
    return SCM_UNSPECIFIED;
}

SCM clear_fluxface()
{
	Fluxus->GetFluxFace()->Clear();
    return SCM_UNSPECIFIED;
}

SCM gain(SCM s_gain)
{
	SCM_ASSERT(SCM_NUMBERP(s_gain), s_gain, SCM_ARG1, "gain");	
	Audio->SetGain(gh_scm2double(s_gain));
    return SCM_UNSPECIFIED;
}

SCM backfacecull(SCM s)
{
	SCM_ASSERT(SCM_NUMBERP(s), s, SCM_ARG1, "set");
	Fluxus->GetRenderer()->SetBackFaceCull(gh_scm2double(s));
	return SCM_UNSPECIFIED;
}

SCM desiredfps(SCM s)
{
	SCM_ASSERT(SCM_NUMBERP(s), s, SCM_ARG1, "set");
	Fluxus->GetRenderer()->SetDesiredFPS(gh_scm2double(s));
	return SCM_UNSPECIFIED;
}

SCM clear_colour(SCM s_vec)
{
    SCM_ASSERT(SCM_VECTORP(s_vec), s_vec, SCM_ARG1, "colour");
	float col[3];
	gh_scm2floats(s_vec,col);
    Fluxus->GetRenderer()->SetBGColour(dColour(col[0],col[1],col[2]));
    return SCM_UNSPECIFIED;
}

SCM clear_frame(SCM s_gain)
{
	SCM_ASSERT(SCM_NUMBERP(s_gain), s_gain, SCM_ARG1, "value");	
	Fluxus->GetRenderer()->SetClearFrame(gh_scm2double(s_gain));
    return SCM_UNSPECIFIED;
}

SCM turtle_prim(SCM type)
{
	SCM_ASSERT(SCM_NUMBERP(type), type, SCM_ARG1, "type");
	turtle.Prim((int)gh_scm2double(type));
	return SCM_UNSPECIFIED;
}

SCM turtle_vert()
{
	turtle.Vert();
	return SCM_UNSPECIFIED;
}

SCM turtle_build()
{
	return gh_double2scm(turtle.Build(Fluxus->GetRenderer()));
}

SCM turtle_move(SCM dist)
{
	SCM_ASSERT(SCM_NUMBERP(dist), dist, SCM_ARG1, "dist");
	turtle.Move(gh_scm2double(dist));
	return SCM_UNSPECIFIED;
}

SCM turtle_turn(SCM s_vec)
{
    SCM_ASSERT(SCM_VECTORP(s_vec), s_vec, SCM_ARG1, "colour");
	float rot[3];
	gh_scm2floats(s_vec,rot);
	turtle.Turn(dVector(rot[0],rot[1],rot[2]));
	return SCM_UNSPECIFIED;	
}

SCM turtle_reset()
{
	turtle.Reset();
	return SCM_UNSPECIFIED;	
}

SCM start_framedump(SCM s_name)
{
	SCM_ASSERT(SCM_STRINGP(s_name), s_name, SCM_ARG1, "filename");	
    size_t size=0;
	char *name=gh_scm2newstr(s_name,&size);
	Fluxus->StartDumpFrames(name);
    return SCM_UNSPECIFIED;
}

SCM end_framedump()
{
	Fluxus->EndDumpFrames();
    return SCM_UNSPECIFIED;
}

SCM process(SCM s_wavname)
{
	SCM_ASSERT(SCM_STRINGP(s_wavname), s_wavname, SCM_ARG1, "wav filename");	
    size_t size=0;
	char *wavname=gh_scm2newstr(s_wavname,&size);
	Audio->Process(wavname);
    return SCM_UNSPECIFIED;
}

void register_procs(void)
{
	// primitives
	gh_new_procedure("build_cube",      build_cube,  0,0,0);
    gh_new_procedure("build_plane",     build_plane, 0,0,0);
    gh_new_procedure0_2("build_cylinder", build_cylinder);
    gh_new_procedure0_2("build_sphere", build_sphere);
	gh_new_procedure4_0("build_line",   build_line);
    gh_new_procedure("draw_cube",       draw_cube,     0,0,0);
    gh_new_procedure("draw_plane",      draw_plane,    0,0,0);
    gh_new_procedure("draw_sphere",     draw_sphere,   0,0,0);
    gh_new_procedure("draw_cylinder",   draw_cylinder, 0,0,0);
	gh_new_procedure0_1("destroy",      destroy);

	// renderstate operations
	gh_new_procedure("push",            push,        0,0,0);
	gh_new_procedure("pop",             pop,         0,0,0);
	gh_new_procedure0_1("grab",         grab);
    gh_new_procedure("ungrab",          ungrab      ,0,0,0);
    gh_new_procedure("print_scene_graph",print_scene_graph,0,0,0);
	gh_new_procedure0_1("apply",        apply);
	gh_new_procedure("identity",        identity, 0,0,0);
    gh_new_procedure0_1("translate",    translate);
    gh_new_procedure0_1("scale",        scale);
    gh_new_procedure0_1("rotate",       rotate);
    gh_new_procedure0_1("colour",       colour);
    gh_new_procedure0_1("opacity",      opacity);
    gh_new_procedure0_1("specular",     specular);
    gh_new_procedure0_1("ambient",      ambient);
    gh_new_procedure0_1("emissive",     emissive);
	gh_new_procedure0_1("shinyness",    shinyness);
	gh_new_procedure0_1("texture",      texture);
    gh_new_procedure("hint_solid",      hint_solid,       0,0,0);
    gh_new_procedure("hint_wire",       hint_wire,        0,0,0);
    gh_new_procedure("hint_normal",     hint_normal,      0,0,0);
    gh_new_procedure("hint_points",     hint_points,      0,0,0);
    gh_new_procedure("hint_anti_alias", hint_anti_alias,  0,0,0);
    gh_new_procedure("hint_none",       hint_none,  0,0,0);
	gh_new_procedure0_1("line_width",   line_width);
    gh_new_procedure0_1("parent",       parent);
	
	// global state operations
	gh_new_procedure("clear",           clear,       0,0,0);
	gh_new_procedure("ortho",       	ortho,   	 0,0,0);
	gh_new_procedure("persp",       	persp,   	 0,0,0);
    gh_new_procedure("reset_camera",    reset_camera,0,0,0);
	gh_new_procedure0_1("lock_camera",  lock_camera);
	gh_new_procedure0_1("clear_colour",    clear_colour);	
	gh_new_procedure0_1("clear_frame",     clear_frame);
	gh_new_procedure0_1("blur",         blur);
    gh_new_procedure0_1("show_axis",    show_axis);
	gh_new_procedure0_1("backfacecull",    backfacecull);
	gh_new_procedure0_1("load_texture", load_texture);

	// lights
    gh_new_procedure0_1("make_light",         make_light);
    gh_new_procedure("clear_lights",       clear_lights,       0,0,0);
	gh_new_procedure0_2("light_ambient",   light_ambient);
	gh_new_procedure0_2("light_diffuse",   light_diffuse);
	gh_new_procedure0_2("light_specular",  light_specular);
	gh_new_procedure0_2("light_position",  light_position);
	
	// interpreter + misc
	gh_new_procedure0_1("load",   load);
	gh_new_procedure0_1("source", source);
	gh_new_procedure0_1("key_pressed",  key_pressed);
     gh_new_procedure("frame",       	frame,  	 0,0,0);
   gh_new_procedure0_1("engine_callback", engine_callback);
    gh_new_procedure("flxrnd",          srandom,     0,0,0);
	gh_new_procedure0_1("desiredfps",          desiredfps);
	gh_new_procedure0_1("start_framedump",     start_framedump);
	gh_new_procedure("end_framedump",          end_framedump, 0,0,0);
	
	// audio
	gh_new_procedure0_1("gain",   gain);	
	gh_new_procedure0_1("get_harmonic", get_harmonic);
	gh_new_procedure0_1("gh",           get_harmonic);
	gh_new_procedure0_1("process",   	process);
	
	// turtle
	gh_new_procedure("turtle_vert",            turtle_vert,       0,0,0);
	gh_new_procedure("turtle_build",           turtle_build,       0,0,0);
	gh_new_procedure("turtle_reset",           turtle_reset,       0,0,0);
	gh_new_procedure0_1("turtle_move",         turtle_move);
	gh_new_procedure0_1("turtle_turn",         turtle_turn);
	gh_new_procedure0_1("turtle_prim",         turtle_prim);
	
	// fluxface
	gh_new_procedure("clear_fluxface",   clear_fluxface, 0,0,0);
	gh_new_procedure3_0("fluxface",   fluxface);
	
	// lifeforms	
    gh_new_procedure0_1("make_lifeforms", make_lifeforms);
    gh_new_procedure0_2("add_lifeform", add_lifeform);
    gh_new_procedure0_2("lifeform_avoidance", lifeform_avoidance);
    gh_new_procedure0_2("lifeform_flockcentering", lifeform_flockcentering);
    gh_new_procedure0_2("lifeform_scenecentering", lifeform_scenecentering);
    gh_new_procedure0_2("lifeform_inertia", lifeform_inertia);
    gh_new_procedure0_2("lifeform_scenecentre", lifeform_scenecentre);
    gh_new_procedure0_2("lifeform_maxspeed", lifeform_maxspeed);
	
	// physics
	gh_new_procedure0_1("collisions", collisions);
	gh_new_procedure0_1("set_max_physical", set_max_physical);
    gh_new_procedure0_1("active_box",     active_box);
    gh_new_procedure0_1("active_sphere",  active_sphere);
    gh_new_procedure0_1("active_cylinder",active_cylinder);
    gh_new_procedure0_1("passive_box",     passive_box);
    gh_new_procedure0_1("passive_sphere",  passive_sphere);
    gh_new_procedure0_1("passive_cylinder",passive_cylinder);
    gh_new_procedure0_2("ground_plane", ground_plane);
    gh_new_procedure5_0("build_hinge2joint", build_hinge2joint);
    gh_new_procedure3_0("build_balljoint", build_balljoint);
    gh_new_procedure0_2("joint_vel2",   joint_vel2);
    gh_new_procedure0_2("joint_fmax2",  joint_fmax2);
    gh_new_procedure0_2("joint_fmax",   joint_fmax);
    gh_new_procedure0_2("joint_histop", joint_histop);
    gh_new_procedure0_2("joint_lostop", joint_lostop);
    gh_new_procedure0_2("joint_vel",    joint_vel);
    gh_new_procedure0_2("joint_fudge",  joint_fudge);
    gh_new_procedure0_2("kick",         kick);
    gh_new_procedure0_2("twist",        twist);
	
	// flx (deprecated)
	gh_new_procedure0_1("save_frame",   save_frame);
	gh_new_procedure0_1("start_flx",   start_flx);
	gh_new_procedure("end_flx",   end_flx, 0,0,0);
}

SCM Handler (void *data, SCM tag, SCM throw_args)
{
	char *str=NULL;
	size_t size=0;
	str=gh_scm2newstr(gh_car(gh_cdr(throw_args)),&size);	
	
	Fluxus->Dump(string("Error in: \"")+string((char*)data)+string("\"\n"));
	Fluxus->Dump(str);
	Fluxus->Dump("\n");
	
	free(str);
	//scm_init_guile();
	return SCM_UNDEFINED;
}

void EngineCallback()
{
    if (engine_callback_str)
    {
        gh_eval_str_with_catch(engine_callback_str, (scm_t_catch_handler)Handler);
    }
}

char *Script;

void inner_main(int argc, char **argv)
{
	register_procs();
    string fragment;

    Fluxus->GetRenderer()->SetEngineCallback(EngineCallback);

	string Init = string(getenv("HOME"))+"/"+INIT_FILE;
    gh_eval_file_with_catch(Init.c_str(),(scm_t_catch_handler)Handler);
	
	if (argc>1)
	{
	    gh_eval_file_with_catch(argv[1],(scm_t_catch_handler)Handler);
	}
	
	while (Fl::check())
    {
    	Fluxus->GetFFTWindow()->SetData(Audio->GetFFT(),Audio->GetAudioBuffer());
    	Fluxus->GetFFTWindow()->redraw();
    	
    	fragment = Fluxus->GetScriptFragment();
    	if (fragment!="")
    	{
    		gh_eval_str_with_catch(fragment.c_str(), (scm_t_catch_handler)Handler);
    	}
    	
    	fragment = Fluxus->GetFluxFace()->GetScriptFragment();
    	if (fragment!="")
    	{
    		gh_eval_str_with_catch(fragment.c_str(), (scm_t_catch_handler)Handler);
    	}
    		
    	Fluxus->redraw();
    	if (flx_out!=NULL) (*flx_out)<<*Fluxus->GetRenderer();
    	frame_count++;
    }
}

int main(int argc, char *argv[])
{
	InitDada();
	
	srand(time(NULL));
	Fl::visual(FL_DOUBLE|FL_RGB);
	Fl::visible_focus(false);
	
	//if (argc>1)
	//{
	//	if (argc==4) RenderFlx(argv[1],(int)atof(argv[2]),(int)atof(argv[3]));
	//	else RenderFlx(argv[1],640,480);
	//}
	//else
	{
		StaticCube = new PolyPrimitive(PolyPrimitive::QUADS);
    	MakeCube(StaticCube);
    	StaticCube->Finalise();

		StaticPlane = new PolyPrimitive(PolyPrimitive::QUADS);
    	MakePlane(StaticCube);
    	StaticPlane->Finalise();

		StaticSphere = new PolyPrimitive(PolyPrimitive::TRILIST);
    	MakeSphere(StaticSphere,1,5,10);
    	StaticSphere->Finalise();

		StaticCylinder = new PolyPrimitive(PolyPrimitive::TRILIST);
    	MakeCylinder(StaticCylinder,1,1,5,10);
    	StaticCylinder->Finalise();

		Fluxus = new GUI(640,480,"fluxus",AUDIO_BUFFER_SIZE);
    	Fluxus->show();
	
		Audio = new AudioCollector(AUDIO_BUFFER_SIZE);
		
    	gh_enter(argc, argv, inner_main);
    }

	return 0;
}

////////////////////////////////////////////////////////////////////////////////////

class FlxWindow : public Fl_Gl_Window
{
public:	
	FlxWindow(int w, int h, char *n, Renderer *r) :
		Fl_Gl_Window(w,h,n), m_Renderer(r) {}
	~FlxWindow() {}
	
	virtual void draw() { Fl_Gl_Window::draw(); m_Renderer->Render(); }
private:
	Renderer *m_Renderer;
};

void RenderFlx(const string &Filename, int w, int h)
{
	Renderer FlxRenderer;
	FlxRenderer.SetResolution(w,h);
	FlxWindow Win(w,h,"flx",&FlxRenderer);
	Win.show();
	ifstream s(Filename.c_str(),ios::binary);
	while (!s.eof())
	{
		s>>FlxRenderer;
		Win.redraw();
		Fl::check();
	}
}
