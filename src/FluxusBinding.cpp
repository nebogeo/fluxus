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
#include "FluxusBinding.h"

FluxusMain     *FluxusBinding::Fluxus=NULL;
AudioCollector *FluxusBinding::Audio=NULL;
TurtleBuilder   FluxusBinding::turtle;
string          FluxusBinding::CallbackString;
PolyPrimitive*  FluxusBinding::StaticCube=NULL;
PolyPrimitive*  FluxusBinding::StaticPlane=NULL;
PolyPrimitive*  FluxusBinding::StaticSphere=NULL;
PolyPrimitive*  FluxusBinding::StaticCylinder=NULL;
set<int>        FluxusBinding::m_KeySet;
int				FluxusBinding::GrabbedID=-1;

// little helper function for making normal vectors (the guile api is annoying)
SCM gh_floats2scm(float *d, unsigned int len)
{
	double *t=(double*)malloc(sizeof(double)*len);
	for (unsigned int i=0; i<len; i++) t[i]=d[i];
	SCM ret=gh_doubles2scm(t,len);
	free(t);
	return ret;
}

FluxusBinding::FluxusBinding(int w, int h) 
{
	StaticCube = new PolyPrimitive(PolyPrimitive::QUADS);
    MakeCube(StaticCube);

	StaticPlane = new PolyPrimitive(PolyPrimitive::QUADS);
    MakePlane(StaticCube);

	StaticSphere = new PolyPrimitive(PolyPrimitive::TRILIST);
    MakeSphere(StaticSphere,1,5,10);

	StaticCylinder = new PolyPrimitive(PolyPrimitive::TRILIST);
    MakeCylinder(StaticCylinder,1,1,5,10);
	
	Fluxus = new FluxusMain(w,h);
}

FluxusBinding::~FluxusBinding()
{
	if (Audio!=NULL) delete Audio;
	delete Fluxus;
}

SCM FluxusBinding::build_polygons(SCM s_size, SCM s_type)
{
	SCM_ASSERT(SCM_NUMBERP(s_size), s_size, SCM_ARG1, "build-polygons");
	SCM_ASSERT(SCM_NUMBERP(s_type), s_type, SCM_ARG2, "build-polygons");
	
	PolyPrimitive *Prim = new PolyPrimitive((PolyPrimitive::Type)(int)gh_scm2double(s_type));
	Prim->Resize((int)gh_scm2double(s_size));
    return gh_double2scm(Fluxus->GetRenderer()->AddPrimitive(Prim));
}

SCM FluxusBinding::build_nurbs(SCM s_size)
{
	SCM_ASSERT(SCM_NUMBERP(s_size), s_size, SCM_ARG1, "build-nurbs");
	
	NURBSPrimitive *Prim = new NURBSPrimitive();
	Prim->Resize((int)gh_scm2double(s_size));
    return gh_double2scm(Fluxus->GetRenderer()->AddPrimitive(Prim));
}

SCM FluxusBinding::build_cube()
{
	PolyPrimitive *BoxPrim = new PolyPrimitive(PolyPrimitive::QUADS);
    MakeCube(BoxPrim);
    return gh_double2scm(Fluxus->GetRenderer()->AddPrimitive(BoxPrim));
}

SCM FluxusBinding::build_sphere(SCM s_hsegments, SCM s_rsegments)
{
	SCM_ASSERT(SCM_NUMBERP(s_hsegments), s_hsegments, SCM_ARG1, "build_sphere");
	SCM_ASSERT(SCM_NUMBERP(s_rsegments), s_rsegments, SCM_ARG2, "build_sphere");
	double hsegments;
	hsegments=gh_scm2double(s_hsegments);
    double rsegments;
	rsegments=gh_scm2double(s_rsegments);	
	
	PolyPrimitive *SphPrim = new PolyPrimitive(PolyPrimitive::TRILIST);
    MakeSphere(SphPrim, 1, (int)hsegments, (int)rsegments);
    return gh_double2scm(Fluxus->GetRenderer()->AddPrimitive(SphPrim));
}

SCM FluxusBinding::build_plane()
{
	PolyPrimitive *PlanePrim = new PolyPrimitive(PolyPrimitive::QUADS);
    MakePlane(PlanePrim);
    return gh_double2scm(Fluxus->GetRenderer()->AddPrimitive(PlanePrim));
}

SCM FluxusBinding::build_plane(SCM s_xsegments, SCM s_ysegments)
{
	SCM_ASSERT(SCM_NUMBERP(s_xsegments), s_xsegments, SCM_ARG1, "build-plane");
	SCM_ASSERT(SCM_NUMBERP(s_ysegments), s_ysegments, SCM_ARG2, "build-plane");

	PolyPrimitive *PlanePrim = new PolyPrimitive(PolyPrimitive::QUADS);
    MakePlane(PlanePrim,(int)gh_scm2double(s_xsegments),(int)gh_scm2double(s_ysegments));
    return gh_double2scm(Fluxus->GetRenderer()->AddPrimitive(PlanePrim));
}

SCM FluxusBinding::build_cylinder(SCM s_hsegments, SCM s_rsegments)
{
	SCM_ASSERT(SCM_NUMBERP(s_hsegments), s_hsegments, SCM_ARG1, "build_cylinder");
	SCM_ASSERT(SCM_NUMBERP(s_rsegments), s_rsegments, SCM_ARG2, "build_cylinder");
	double hsegments;
	hsegments=gh_scm2double(s_hsegments);
    double rsegments;
	rsegments=gh_scm2double(s_rsegments);	

	PolyPrimitive *CylPrim = new PolyPrimitive(PolyPrimitive::TRILIST);
    MakeCylinder(CylPrim, 1, 1, (int)hsegments, (int)rsegments);

    return gh_double2scm(Fluxus->GetRenderer()->AddPrimitive(CylPrim));
}

SCM FluxusBinding::build_line(SCM start, SCM swidth, SCM end, SCM ewidth)
{
	SCM_ASSERT(SCM_VECTORP(start), start, SCM_ARG1, "build_line");
	SCM_ASSERT(SCM_VECTOR_LENGTH(start)==3, start, SCM_ARG1, "build_line");
	SCM_ASSERT(SCM_NUMBERP(swidth), swidth, SCM_ARG2, "build_line");
	SCM_ASSERT(SCM_VECTORP(end), end, SCM_ARG3, "build_line");
	SCM_ASSERT(SCM_VECTOR_LENGTH(end)==3, end, SCM_ARG3, "build_line");
	SCM_ASSERT(SCM_NUMBERP(ewidth), ewidth, SCM_ARG4, "build_line");
	float s[3],e[3];
	gh_scm2floats(start,s);
	gh_scm2floats(end,e);
	
	LinePrimitive *LinePrim = new LinePrimitive;
	LinePrim->SetStart(dVertex(dVector(s[0],s[1],s[2]),dVector(0,1,0)),gh_scm2double(swidth));
	LinePrim->SetEnd(dVertex(dVector(e[0],e[1],e[2]),dVector(0,1,0)),gh_scm2double(ewidth));
	
	return gh_double2scm(Fluxus->GetRenderer()->AddPrimitive(LinePrim));
}

SCM FluxusBinding::build_text(SCM s_text)
{
	SCM_ASSERT(SCM_STRINGP(s_text), s_text, SCM_ARG1, "build_text");
	
	size_t size=0;
	char *text=gh_scm2newstr(s_text,&size);
	
	TextPrimitive *TextPrim = new TextPrimitive(15/256.0f,25/256.0f,17,40);
	TextPrim->SetText(text,20,-20);
	
	return gh_double2scm(Fluxus->GetRenderer()->AddPrimitive(TextPrim));
}
	
SCM FluxusBinding::build_nurbs_sphere(SCM s_hsegments, SCM s_rsegments)
{
	SCM_ASSERT(SCM_NUMBERP(s_hsegments), s_hsegments, SCM_ARG1, "build_nurbs_sphere");
	SCM_ASSERT(SCM_NUMBERP(s_rsegments), s_rsegments, SCM_ARG2, "build_nurbs_sphere");
	double hsegments;
	hsegments=gh_scm2double(s_hsegments);
    double rsegments;
	rsegments=gh_scm2double(s_rsegments);	
	
	NURBSPrimitive *SphPrim = new NURBSPrimitive;
    MakeNURBSSphere(SphPrim, 1, (int)hsegments, (int)rsegments);
    return gh_double2scm(Fluxus->GetRenderer()->AddPrimitive(SphPrim));
}

SCM FluxusBinding::build_nurbs_plane(SCM s_usegments, SCM s_vsegments)
{
	SCM_ASSERT(SCM_NUMBERP(s_usegments), s_usegments, SCM_ARG1, "build_nurbs_sphere");
	SCM_ASSERT(SCM_NUMBERP(s_vsegments), s_vsegments, SCM_ARG2, "build_nurbs_sphere");
	double usegments;
	usegments=gh_scm2double(s_usegments);
    double vsegments;
	vsegments=gh_scm2double(s_vsegments);	
	
	NURBSPrimitive *Prim = new NURBSPrimitive;
    MakeNURBSPlane(Prim, (int)usegments, (int)vsegments);
    return gh_double2scm(Fluxus->GetRenderer()->AddPrimitive(Prim));
}

SCM FluxusBinding::build_particles(SCM s_count)
{
	SCM_ASSERT(SCM_NUMBERP(s_count), s_count, SCM_ARG1, "build_particles");
	ParticlePrimitive *Prim = new ParticlePrimitive;
	int count=(int)gh_scm2double(s_count);
	for (int i=0; i<count; i++)
	{
		Prim->AddParticle(dVector(0,0,0),dColour(0,0,0),dVector(0.01,0.01,0.01));
	}
	
    return gh_double2scm(Fluxus->GetRenderer()->AddPrimitive(Prim));
}

SCM FluxusBinding::draw_instance(SCM s_ob)
{    	
	SCM_ASSERT(SCM_NUMBERP(s_ob), s_ob, SCM_ARG1, "draw_instance");
	int ob=0;
	ob=(int)gh_scm2double(s_ob);	
    Fluxus->GetRenderer()->RenderPrimitive(Fluxus->GetRenderer()->GetPrimitive(ob));
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::draw_cube()
{    	
    Fluxus->GetRenderer()->RenderPrimitive(StaticCube);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::draw_plane()
{    	
    Fluxus->GetRenderer()->RenderPrimitive(StaticPlane);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::draw_sphere()
{    	
    Fluxus->GetRenderer()->RenderPrimitive(StaticSphere);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::draw_cylinder()
{    	
    Fluxus->GetRenderer()->RenderPrimitive(StaticCylinder);
    return SCM_UNSPECIFIED;
}


SCM FluxusBinding::key_pressed(SCM s_key)
{
	SCM_ASSERT(SCM_STRINGP(s_key), s_key, SCM_ARG1, "key_pressed");
	char *key=0;
	size_t size=0;
	key=gh_scm2newstr(s_key,&size);
	double pressed;
	if (m_KeySet.find(key[0])!=m_KeySet.end()) pressed=1;
	else pressed=0;
    free(key);
    return gh_bool2scm((int)pressed);
}

SCM FluxusBinding::show_axis(SCM s_id)
{
	SCM_ASSERT(SCM_NUMBERP(s_id), s_id, SCM_ARG1, "show_axis");	
    Fluxus->GetRenderer()->ShowAxis(gh_scm2double(s_id));
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::show_fps(SCM s_id)
{
	SCM_ASSERT(SCM_NUMBERP(s_id), s_id, SCM_ARG1, "show_fps");	
    Fluxus->GetRenderer()->SetFPSDisplay(gh_scm2double(s_id));
    return SCM_UNSPECIFIED;
}
SCM FluxusBinding::make_light(SCM cam)
{
	SCM_ASSERT(SCM_NUMBERP(cam), cam, SCM_ARG1, "make_light");
	Light *l=new Light;
	l->SetCameraLock((bool)gh_scm2double(cam));
	return gh_double2scm(Fluxus->GetRenderer()->AddLight(l));
}

SCM FluxusBinding::clear_lights()
{
	Fluxus->GetRenderer()->ClearLights();
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::light_ambient(SCM id, SCM v)
{
	SCM_ASSERT(SCM_NUMBERP(id), id, SCM_ARG1, "light_ambient");	
	SCM_ASSERT(SCM_VECTORP(v), v, SCM_ARG2, "light_ambient");
	SCM_ASSERT(SCM_VECTOR_LENGTH(v)==3, v, SCM_ARG2, "light_ambient");
	float vec[3];
	gh_scm2floats(v,vec);
	Fluxus->GetRenderer()->GetLight((int)gh_scm2double(id))->SetAmbient(dColour(vec[0],vec[1],vec[2]));
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::light_diffuse(SCM id, SCM v)
{
	SCM_ASSERT(SCM_NUMBERP(id), id, SCM_ARG1, "light_diffuse");	
	SCM_ASSERT(SCM_VECTORP(v), v, SCM_ARG2, "light_diffuse");
	SCM_ASSERT(SCM_VECTOR_LENGTH(v)==3, v, SCM_ARG2, "light_diffuse");
	float vec[3];
	gh_scm2floats(v,vec);
	Fluxus->GetRenderer()->GetLight((int)gh_scm2double(id))->SetDiffuse(dColour(vec[0],vec[1],vec[2]));
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::light_specular(SCM id, SCM v)
{
	SCM_ASSERT(SCM_NUMBERP(id), id, SCM_ARG1, "light_specular");	
	SCM_ASSERT(SCM_VECTORP(v), v, SCM_ARG2, "light_specular");
	SCM_ASSERT(SCM_VECTOR_LENGTH(v)==3, v, SCM_ARG2, "light_specular");
	float vec[3];
	gh_scm2floats(v,vec);
	Fluxus->GetRenderer()->GetLight((int)gh_scm2double(id))->SetSpecular(dColour(vec[0],vec[1],vec[2]));
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::light_position(SCM id, SCM v)
{
	SCM_ASSERT(SCM_NUMBERP(id), id, SCM_ARG1, "light_position");	
	SCM_ASSERT(SCM_VECTORP(v), v, SCM_ARG2, "light_position");
	SCM_ASSERT(SCM_VECTOR_LENGTH(v)==3, v, SCM_ARG2, "light_position");	
	float vec[3];
	gh_scm2floats(v,vec);
	Fluxus->GetRenderer()->GetLight((int)gh_scm2double(id))->SetPosition(dVector(vec[0],vec[1],vec[2]));
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::lock_camera(SCM s_ob)
{
	SCM_ASSERT(SCM_NUMBERP(s_ob), s_ob, SCM_ARG1, "lock_camera");
	int ob=0;
	ob=(int)gh_scm2double(s_ob);	
    Fluxus->GetRenderer()->LockCamera(ob);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::destroy(SCM s_name)
{
	SCM_ASSERT(SCM_NUMBERP(s_name), s_name, SCM_ARG1, "destroy");
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

SCM FluxusBinding::clear()
{
	Fluxus->GetRenderer()->Clear();
	Fluxus->GetPhysics()->Clear();
	CallbackString="";
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::grab(SCM s_id)
{
	SCM_ASSERT(SCM_NUMBERP(s_id), s_id, SCM_ARG1, "id");
	GrabbedID=(int)gh_scm2double(s_id);
	Fluxus->GetRenderer()->Grab(GrabbedID);
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::ungrab()
{
	Fluxus->GetRenderer()->UnGrab();
	GrabbedID=-1;
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::apply(SCM s_id)
{
	SCM_ASSERT(SCM_NUMBERP(s_id), s_id, SCM_ARG1, "apply");
	Fluxus->GetRenderer()->GetPrimitive((int)gh_scm2double(s_id))->ApplyTransform();
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::opacity(SCM s_opac)
{
    SCM_ASSERT(SCM_NUMBERP(s_opac), s_opac, SCM_ARG1, "opacity");
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Opacity=gh_scm2double(s_opac);
    else Fluxus->GetRenderer()->GetState()->Opacity=gh_scm2double(s_opac);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::shinyness(SCM s_opac)
{
    SCM_ASSERT(SCM_NUMBERP(s_opac), s_opac, SCM_ARG1, "shinyness");
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Shinyness=gh_scm2double(s_opac);
    else Fluxus->GetRenderer()->GetState()->Shinyness=gh_scm2double(s_opac);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::colour(SCM s_vec)
{
    SCM_ASSERT(SCM_VECTORP(s_vec), s_vec, SCM_ARG1, "colour");
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_vec)==3, s_vec, SCM_ARG1, "colour");
	float col[3];
	gh_scm2floats(s_vec,col);
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Colour=dColour(col[0],col[1],col[2]);
    else Fluxus->GetRenderer()->GetState()->Colour=dColour(col[0],col[1],col[2]);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::specular(SCM s_vec)
{
    SCM_ASSERT(SCM_VECTORP(s_vec), s_vec, SCM_ARG1, "specular");
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_vec)==3, s_vec, SCM_ARG1, "specular");
	float col[3];
	gh_scm2floats(s_vec,col);
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Specular=dColour(col[0],col[1],col[2]);
    else Fluxus->GetRenderer()->GetState()->Specular=dColour(col[0],col[1],col[2]);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::ambient(SCM s_vec)
{
    SCM_ASSERT(SCM_VECTORP(s_vec), s_vec, SCM_ARG1, "ambient");
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_vec)==3, s_vec, SCM_ARG1, "ambient");
	float col[3];
	gh_scm2floats(s_vec,col);
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Ambient=dColour(col[0],col[1],col[2]);
    else Fluxus->GetRenderer()->GetState()->Ambient=dColour(col[0],col[1],col[2]);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::emissive(SCM s_vec)
{
    SCM_ASSERT(SCM_VECTORP(s_vec), s_vec, SCM_ARG1, "emissive");
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_vec)==3, s_vec, SCM_ARG1, "emissive");
	float col[3];
	gh_scm2floats(s_vec,col);
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Emissive=dColour(col[0],col[1],col[2]);
    else Fluxus->GetRenderer()->GetState()->Emissive=dColour(col[0],col[1],col[2]);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::flux_identity()
{
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Transform.init();
    else Fluxus->GetRenderer()->GetState()->Transform.init();
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::translate(SCM s_vec)
{
    SCM_ASSERT(SCM_VECTORP(s_vec), s_vec, SCM_ARG1, "translate");
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_vec)==3, s_vec, SCM_ARG1, "translate");
	float trans[3];
	gh_scm2floats(s_vec,trans);
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Transform.translate(trans[0],trans[1],trans[2]);
    else Fluxus->GetRenderer()->GetState()->Transform.translate(trans[0],trans[1],trans[2]);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::concat(SCM s_m)
{
    SCM_ASSERT(SCM_VECTORP(s_m), s_m, SCM_ARG1, "concat");
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_m)==16, s_m, SCM_ARG1, "concat");
	dMatrix m;
	gh_scm2floats(s_m,m.arr());
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Transform*=m;
    else Fluxus->GetRenderer()->GetState()->Transform*=m;
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::rotate(SCM s_vec)
{
    SCM_ASSERT(SCM_VECTORP(s_vec), s_vec, SCM_ARG1, "rotate");
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_vec)==3, s_vec, SCM_ARG1, "rotate");
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

SCM FluxusBinding::scale(SCM s_vec)
{
    SCM_ASSERT(SCM_VECTORP(s_vec), s_vec, SCM_ARG1, "scale");
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_vec)==3, s_vec, SCM_ARG1, "scale");
	float scale[3];
	gh_scm2floats(s_vec,scale);
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Transform.scale(scale[0],scale[1],scale[2]);
    else Fluxus->GetRenderer()->GetState()->Transform.scale(scale[0],scale[1],scale[2]);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::parent(SCM s_p)
{
    SCM_ASSERT(SCM_NUMBERP(s_p), s_p, SCM_ARG1, "parent");
    Fluxus->GetRenderer()->GetState()->Parent=(int)gh_scm2double(s_p);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::line_width(SCM s_p)
{
    SCM_ASSERT(SCM_NUMBERP(s_p), s_p, SCM_ARG1, "line_width");
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->LineWidth=gh_scm2double(s_p);
    else Fluxus->GetRenderer()->GetState()->LineWidth=gh_scm2double(s_p);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::point_width(SCM s_p)
{
    SCM_ASSERT(SCM_NUMBERP(s_p), s_p, SCM_ARG1, "point_width");
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->PointWidth=gh_scm2double(s_p);
    else Fluxus->GetRenderer()->GetState()->PointWidth=gh_scm2double(s_p);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::blend_mode(SCM s_s, SCM s_d)
{
    SCM_ASSERT(SCM_STRINGP(s_s), s_s, SCM_ARG1, "blend_mode");
    SCM_ASSERT(SCM_STRINGP(s_d), s_d, SCM_ARG2, "blend_mode");
	
	size_t size=0;
	char *s=gh_scm2newstr(s_s,&size);	
	char *d=gh_scm2newstr(s_s,&size);	
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->SetBlendMode(s,d);
    else Fluxus->GetRenderer()->GetState()->SetBlendMode(s,d);
	free(s);
	free(d);
	
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::hint_solid()
{
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Hints|=HINT_SOLID;
    else Fluxus->GetRenderer()->GetState()->Hints|=HINT_SOLID;
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::hint_wire()
{
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Hints|=HINT_WIRE;
    else Fluxus->GetRenderer()->GetState()->Hints|=HINT_WIRE;
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::hint_normal()
{
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Hints|=HINT_NORMAL;
    else Fluxus->GetRenderer()->GetState()->Hints|=HINT_NORMAL;
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::hint_points()
{
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Hints|=HINT_POINTS;
    else Fluxus->GetRenderer()->GetState()->Hints|=HINT_POINTS;
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::hint_anti_alias()
{
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Hints|=HINT_AALIAS;
    else Fluxus->GetRenderer()->GetState()->Hints|=HINT_AALIAS;
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::hint_unlit()
{
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Hints|=HINT_UNLIT;
    else Fluxus->GetRenderer()->GetState()->Hints|=HINT_UNLIT;
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::hint_none()
{
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Hints=0;
    else Fluxus->GetRenderer()->GetState()->Hints=0;
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::blur(SCM s_blur)
{
    SCM_ASSERT(SCM_NUMBERP(s_blur), s_blur, SCM_ARG1, "blur");
	double blur;
	blur=gh_scm2double(s_blur);	
	if (!blur) Fluxus->GetRenderer()->SetMotionBlur(false);
    else Fluxus->GetRenderer()->SetMotionBlur(true, (float)blur);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::fog(SCM s_col, SCM s_d, SCM s_s, SCM s_e)
{
    SCM_ASSERT(SCM_VECTORP(s_col), s_col, SCM_ARG1, "fog");
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_col)==3, s_col, SCM_ARG1, "fog");
    SCM_ASSERT(SCM_NUMBERP(s_d), s_d, SCM_ARG2, "fog");
    SCM_ASSERT(SCM_NUMBERP(s_s), s_s, SCM_ARG3, "fog");
    SCM_ASSERT(SCM_NUMBERP(s_e), s_e, SCM_ARG4, "fog");
	dColour c;
	gh_scm2floats(s_col,c.arr());
	Fluxus->GetRenderer()->SetFog(c,gh_scm2double(s_d),gh_scm2double(s_s),gh_scm2double(s_e));
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::feedback(SCM s_fb)
{
    SCM_ASSERT(SCM_NUMBERP(s_fb), s_fb, SCM_ARG1, "feedback");
	double fb;
	fb=gh_scm2double(s_fb);	
	Fluxus->GetRenderer()->SetFeedBack(fb);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::feedback_transform(SCM s_a)
{
	SCM_ASSERT(SCM_VECTORP(s_a),  s_a,  SCM_ARG1, "feedback_transform");
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_a)==16,  s_a,  SCM_ARG1, "feedback_transform");
	dMatrix m;
	gh_scm2floats(s_a,m.arr());
	Fluxus->GetRenderer()->SetFeedBackMat(m);
	return SCM_UNSPECIFIED;	
}

SCM FluxusBinding::push()
{
	if (GrabbedID!=-1)
	{
		cerr<<"error: can't (push) while an object is (grab)bed"<<endl;
		return SCM_UNSPECIFIED;
	}
	
    Fluxus->GetRenderer()->PushState();
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::pop()
{
	if (GrabbedID!=-1)
	{
		cerr<<"error: can't (pop) while an object is (grab)bed"<<endl;
		return SCM_UNSPECIFIED;
	}

    Fluxus->GetRenderer()->PopState();
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::collisions(SCM s)
{
	SCM_ASSERT(SCM_NUMBERP(s), s, SCM_ARG1, "collisions");
	Fluxus->GetPhysics()->SetCollisions(gh_scm2double(s));
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::ground_plane(SCM s_ori, SCM s_off)
{
    SCM_ASSERT(SCM_VECTORP(s_ori), s_ori, SCM_ARG1, "ground_plane");
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_ori)==3, s_ori, SCM_ARG1, "ground_plane");
    SCM_ASSERT(SCM_NUMBERP(s_off), s_off, SCM_ARG2, "ground_plane");
    float ori[3];
	gh_scm2floats(s_ori,ori);
	float off = (float) gh_scm2double(s_off);
	Fluxus->GetPhysics()->GroundPlane(dVector(ori[0],ori[1],ori[2]),off);
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::active_box(SCM s_name)
{
    SCM_ASSERT(SCM_NUMBERP(s_name), s_name, SCM_ARG1, "active_box");
	int name=0;
	name=(int)gh_scm2double(s_name);	
	Fluxus->GetPhysics()->MakeActive(name,1.0f,Physics::BOX);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::active_cylinder(SCM s_name)
{
    SCM_ASSERT(SCM_NUMBERP(s_name), s_name, SCM_ARG1, "active_cylinder");
	int name=0;
	name=(int)gh_scm2double(s_name);	
	Fluxus->GetPhysics()->MakeActive(name,1.0f,Physics::CYLINDER);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::active_sphere(SCM s_name)
{
    SCM_ASSERT(SCM_NUMBERP(s_name), s_name, SCM_ARG1, "active_sphere");
	int name=0;
	name=(int)gh_scm2double(s_name);	
	Fluxus->GetPhysics()->MakeActive(name,1.0f,Physics::SPHERE);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::passive_box(SCM s_name)
{
    SCM_ASSERT(SCM_NUMBERP(s_name), s_name, SCM_ARG1, "passive_box");
	int name=0;
	name=(int)gh_scm2double(s_name);	
	Fluxus->GetPhysics()->MakePassive(name,1.0f,Physics::BOX);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::passive_cylinder(SCM s_name)
{
    SCM_ASSERT(SCM_NUMBERP(s_name), s_name, SCM_ARG1, "passive_cylinder");
	int name=0;
	name=(int)gh_scm2double(s_name);	
	Fluxus->GetPhysics()->MakePassive(name,1.0f,Physics::CYLINDER);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::passive_sphere(SCM s_name)
{
    SCM_ASSERT(SCM_NUMBERP(s_name), s_name, SCM_ARG1, "passive_sphere");
	int name=0;
	name=(int)gh_scm2double(s_name);	
	Fluxus->GetPhysics()->MakePassive(name,1.0f,Physics::SPHERE);
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::surface_params(SCM s_slip1, SCM s_slip2, SCM s_softerp, SCM s_softcfm)
{
	SCM_ASSERT(SCM_NUMBERP(s_slip1),    s_slip1,    SCM_ARG1, "surface_params");
	SCM_ASSERT(SCM_NUMBERP(s_slip2),    s_slip2,    SCM_ARG2, "surface_params");
	SCM_ASSERT(SCM_NUMBERP(s_softerp),  s_softerp,  SCM_ARG3, "surface_params");
	SCM_ASSERT(SCM_NUMBERP(s_softcfm),  s_softcfm,  SCM_ARG4, "surface_params");
	Fluxus->GetPhysics()->SetGlobalSurfaceParams((float)gh_scm2double(s_slip1),(float)gh_scm2double(s_slip2),
		(float)gh_scm2double(s_softerp),(float)gh_scm2double(s_softcfm));
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::build_balljoint(SCM s_ob1, SCM s_ob2, SCM s_anchor)
{
    SCM_ASSERT(SCM_NUMBERP(s_ob1),    s_ob1,    SCM_ARG1, "build_balljoint");
    SCM_ASSERT(SCM_NUMBERP(s_ob2),    s_ob2,    SCM_ARG2, "build_balljoint");
    SCM_ASSERT(SCM_VECTORP(s_anchor), s_anchor, SCM_ARG3, "build_balljoint");
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_anchor)==3, s_anchor, SCM_ARG3, "build_balljoint");
    int name1=0;
	name1=(int)gh_scm2double(s_ob1);
	int name2=0;
	name2=(int)gh_scm2double(s_ob2);	
	float anchor[3];
	gh_scm2floats(s_anchor,anchor);
	return gh_double2scm(Fluxus->GetPhysics()->CreateJointBall(name1, name2, dVector(anchor[0],anchor[1],anchor[2])));
}

SCM FluxusBinding::build_hingejoint(SCM s_ob1, SCM s_ob2, SCM s_anchor, SCM s_hinge)
{
    SCM_ASSERT(SCM_NUMBERP(s_ob1),    s_ob1,    SCM_ARG1, "build_hingejoint");
    SCM_ASSERT(SCM_NUMBERP(s_ob2),    s_ob2,    SCM_ARG2, "build_hingejoint");
    SCM_ASSERT(SCM_VECTORP(s_anchor), s_anchor, SCM_ARG3, "build_hingejoint");
 	SCM_ASSERT(SCM_VECTOR_LENGTH(s_anchor)==3, s_anchor, SCM_ARG3, "build_hingejoint");
    SCM_ASSERT(SCM_VECTORP(s_hinge),  s_hinge,  SCM_ARG4, "build_hingejoint");
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_hinge)==3, s_hinge, SCM_ARG4, "build_hingejoint");

    int name1=0;
	name1=(int)gh_scm2double(s_ob1);
	int name2=0;
	name2=(int)gh_scm2double(s_ob2);
	
	float anchor[3];
	gh_scm2floats(s_anchor,anchor);
	
	dVector Hinge;
	float temp[3];
	gh_scm2floats(s_hinge,temp);
	Hinge.x=temp[0];
	Hinge.y=temp[1];
	Hinge.z=temp[2];
	
	return gh_double2scm(Fluxus->GetPhysics()->CreateJointHinge(name1, name2, dVector(anchor[0],anchor[1],anchor[2]), Hinge));
}

SCM FluxusBinding::build_sliderjoint(SCM s_ob1, SCM s_ob2, SCM s_hinge)
{
    SCM_ASSERT(SCM_NUMBERP(s_ob1),    s_ob1,    SCM_ARG1, "build_sliderjoint");
    SCM_ASSERT(SCM_NUMBERP(s_ob2),    s_ob2,    SCM_ARG2, "build_sliderjoint");
    SCM_ASSERT(SCM_VECTORP(s_hinge),  s_hinge,  SCM_ARG3, "build_sliderjoint");
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_hinge)==3, s_hinge, SCM_ARG3, "build_sliderjoint");

    int name1=0;
	name1=(int)gh_scm2double(s_ob1);
	int name2=0;
	name2=(int)gh_scm2double(s_ob2);
		
	dVector Hinge;
	float temp[3];
	gh_scm2floats(s_hinge,temp);
	Hinge.x=temp[0];
	Hinge.y=temp[1];
	Hinge.z=temp[2];
	
	return gh_double2scm(Fluxus->GetPhysics()->CreateJointSlider(name1, name2, Hinge));
}

SCM FluxusBinding::build_hinge2joint(SCM s_ob1, SCM s_ob2, SCM s_anchor, SCM s_hinge1, SCM s_hinge2)
{
    SCM_ASSERT(SCM_NUMBERP(s_ob1),    s_ob1,    SCM_ARG1, "build_hinge2joint");
    SCM_ASSERT(SCM_NUMBERP(s_ob2),    s_ob2,    SCM_ARG2, "build_hinge2joint");
    SCM_ASSERT(SCM_VECTORP(s_anchor), s_anchor, SCM_ARG3, "build_hinge2joint");
    SCM_ASSERT(SCM_VECTOR_LENGTH(s_anchor)==3, s_anchor, SCM_ARG3, "build_hinge2joint");
	SCM_ASSERT(SCM_VECTORP(s_hinge1), s_hinge1, SCM_ARG4, "build_hinge2joint");
    SCM_ASSERT(SCM_VECTOR_LENGTH(s_hinge1)==3, s_hinge1, SCM_ARG4, "build_hinge2joint");
	SCM_ASSERT(SCM_VECTORP(s_hinge2), s_hinge2, SCM_ARG5, "build_hinge2joint");
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_hinge2)==3, s_hinge2, SCM_ARG5, "build_hinge2joint");

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

SCM FluxusBinding::build_amotorjoint(SCM s_ob1, SCM s_ob2, SCM s_axis)
{
    SCM_ASSERT(SCM_NUMBERP(s_ob1),    s_ob1,    SCM_ARG1, "build_amotorjoint");
    SCM_ASSERT(SCM_NUMBERP(s_ob2),    s_ob2,    SCM_ARG2, "build_amotorjoint");
    SCM_ASSERT(SCM_VECTORP(s_axis),   s_axis,   SCM_ARG3, "build_amotorjoint");
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_axis)==3, s_axis, SCM_ARG3, "build_amotorjoint");
    int name1=0;
	name1=(int)gh_scm2double(s_ob1);
	int name2=0;
	name2=(int)gh_scm2double(s_ob2);	
	float axis[3];
	gh_scm2floats(s_axis,axis);
	return gh_double2scm(Fluxus->GetPhysics()->CreateJointAMotor(name1, name2, dVector(axis[0],axis[1],axis[2])));
}

SCM FluxusBinding::joint_param(SCM s_joint, SCM s_param, SCM s_value)
{
    SCM_ASSERT(SCM_NUMBERP(s_joint), s_joint, SCM_ARG1, "joint_param");
    SCM_ASSERT(SCM_STRINGP(s_param), s_param, SCM_ARG1, "joint_param");
    SCM_ASSERT(SCM_NUMBERP(s_value), s_value, SCM_ARG2, "joint_param");
    int joint=0;
	joint=(int)gh_scm2double(s_joint);
	char *param=0;
	size_t size=0;
	param=gh_scm2newstr(s_param,&size);	
    double v = gh_scm2double(s_value);
	Fluxus->GetPhysics()->SetJointParam(joint,param,v);
	free(param);
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::joint_angle(SCM s_joint, SCM s_vel, SCM s_angle)
{
    SCM_ASSERT(SCM_NUMBERP(s_joint), s_joint, SCM_ARG1, "joint_angle");
    SCM_ASSERT(SCM_NUMBERP(s_vel),   s_vel,   SCM_ARG1, "joint_angle");
    SCM_ASSERT(SCM_NUMBERP(s_angle), s_angle, SCM_ARG2, "joint_angle");
	Fluxus->GetPhysics()->SetJointAngle((int)gh_scm2double(s_joint),gh_scm2double(s_vel),gh_scm2double(s_angle));
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::set_max_physical(SCM s_value)
{
    SCM_ASSERT(SCM_NUMBERP(s_value), s_value, SCM_ARG2, "set_max_physical");
    double v = gh_scm2double(s_value);
    Fluxus->GetPhysics()->SetMaxObjectCount((int)v);
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::set_mass(SCM s_obj, SCM s_mass)
{
    SCM_ASSERT(SCM_NUMBERP(s_obj), s_obj, SCM_ARG1, "set_mass");
    SCM_ASSERT(SCM_NUMBERP(s_mass), s_mass, SCM_ARG2, "set_mass");
    int obj=0;
	obj=(int)gh_scm2double(s_obj);
    float mass=gh_scm2double(s_mass);
	Fluxus->GetPhysics()->SetMass(obj,mass);
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::gravity(SCM s_vec)
{
	SCM_ASSERT(SCM_VECTORP(s_vec), s_vec, SCM_ARG1, "gravity");
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_vec)==3, s_vec, SCM_ARG1, "gravity");
	float vec[3];
	gh_scm2floats(s_vec,vec);
	Fluxus->GetPhysics()->SetGravity(dVector(vec[0],vec[1],vec[2]));
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::kick(SCM s_obj, SCM s_vec)
{
    SCM_ASSERT(SCM_NUMBERP(s_obj), s_obj, SCM_ARG1, "kick");
    SCM_ASSERT(SCM_VECTORP(s_vec), s_vec, SCM_ARG2, "kick");
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_vec)==3, s_vec, SCM_ARG2, "kick");
    int obj=0;
	obj=(int)gh_scm2double(s_obj);
    float vec[3];
    gh_scm2floats(s_vec,vec);
	Fluxus->GetPhysics()->Kick(obj,dVector(vec[0],vec[1],vec[2]));
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::twist(SCM s_obj, SCM s_vec)
{
    SCM_ASSERT(SCM_NUMBERP(s_obj), s_obj, SCM_ARG1, "twist");
    SCM_ASSERT(SCM_VECTORP(s_vec), s_vec, SCM_ARG2, "twist");
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_vec)==3, s_vec, SCM_ARG2, "twist");
    int obj=0;
	obj=(int)gh_scm2double(s_obj);
    float vec[3];
    gh_scm2floats(s_vec,vec);
	Fluxus->GetPhysics()->Twist(obj,dVector(vec[0],vec[1],vec[2]));
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::srandom()
{
	return gh_double2scm(RandFloat());
}

SCM FluxusBinding::has_collided(SCM s_id)
{
	SCM_ASSERT(SCM_NUMBERP(s_id), s_id, SCM_ARG1, "has-collided");
	return gh_bool2scm(Fluxus->GetPhysics()->HasCollided((int)gh_scm2double(s_id)));
}

SCM FluxusBinding::start_audio(SCM s_dev, SCM s_bs, SCM s_sr)
{
	SCM_ASSERT(SCM_NUMBERP(s_dev), s_dev, SCM_ARG1, "start-audio");
	SCM_ASSERT(SCM_NUMBERP(s_bs), s_bs, SCM_ARG2, "start-audio");
	SCM_ASSERT(SCM_NUMBERP(s_sr), s_sr, SCM_ARG3, "start-audio");
	if (Audio==NULL)
	{
		Audio = new AudioCollector((int)gh_scm2double(s_dev),(unsigned int)gh_scm2double(s_bs),(int)gh_scm2double(s_sr));
		Fluxus->SetAudio(Audio);
	}
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::get_harmonic(SCM s_harm)
{
	SCM_ASSERT(SCM_NUMBERP(s_harm), s_harm, SCM_ARG1, "get_harmonic");	
	if (Audio!=NULL)
	{	
    	return gh_double2scm(Audio->GetHarmonic((int)gh_scm2double(s_harm)));
	}
	return gh_double2scm(0);
}

SCM FluxusBinding::load_texture(SCM s_name)
{
	SCM_ASSERT(SCM_STRINGP(s_name), s_name, SCM_ARG1, "load-texture");
	char *name=0;
	size_t size=0;
	name=gh_scm2newstr(s_name,&size);	
	
	int id=Fluxus->GetRenderer()->LoadTexture(name);

    free(name);
    return gh_int2scm(id);
}

SCM FluxusBinding::force_load_texture(SCM s_name)
{
	SCM_ASSERT(SCM_STRINGP(s_name), s_name, SCM_ARG1, "force-load-texture");
	char *name=0;
	size_t size=0;
	name=gh_scm2newstr(s_name,&size);	
	
	int id=Fluxus->GetRenderer()->LoadTexture(name,true);

    free(name);
    return gh_int2scm(id);
}

SCM FluxusBinding::texture(SCM s_id)
{
	SCM_ASSERT(SCM_NUMBERP(s_id), s_id, SCM_ARG1, "texture");	
    Fluxus->GetRenderer()->GetState()->Texture=(int)gh_scm2double(s_id);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::engine_callback(SCM s_func)
{
	SCM_ASSERT(SCM_STRINGP(s_func), s_func, SCM_ARG1, "engine_callback");
	size_t size=0;
	char *temp=gh_scm2newstr(s_func,&size);
	CallbackString=temp;
	free(temp);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::ortho()
{
	//Fluxus->ortho();
	Fluxus->GetRenderer()->SetOrtho(true);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::persp()
{
	Fluxus->GetRenderer()->SetOrtho(false);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::time()
{
	return gh_double2scm(Fluxus->GetRenderer()->GetTime());
}

SCM FluxusBinding::delta()
{
	return gh_double2scm(Fluxus->GetRenderer()->GetDelta());
}

SCM FluxusBinding::reset_camera()
{
	Fluxus->ResetCamera();
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::print_scene_graph()
{
	Fluxus->GetRenderer()->PrintSceneGraph();
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::load(SCM s_name)
{
	SCM_ASSERT(SCM_STRINGP(s_name), s_name, SCM_ARG1, "load");	
    size_t size=0;
	char *name=gh_scm2newstr(s_name,&size);
	Fluxus->LoadScript(name);
	free(name);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::save_name(SCM s_name)
{
	SCM_ASSERT(SCM_STRINGP(s_name), s_name, SCM_ARG1, "save_name");	
    size_t size=0;
	char *name=gh_scm2newstr(s_name,&size);
	Fluxus->SetSaveName(name);
	free(name);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::source(SCM s_name)
{
	SCM_ASSERT(SCM_STRINGP(s_name), s_name, SCM_ARG1, "source");	
    size_t size=0;
	char *name=gh_scm2newstr(s_name,&size);
	Fluxus->LoadScript(name);
	Fluxus->RunScript();
	free(name);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::gain(SCM s_gain)
{
	SCM_ASSERT(SCM_NUMBERP(s_gain), s_gain, SCM_ARG1, "gain");	
	if (Audio!=NULL)
	{	
		Audio->SetGain(gh_scm2double(s_gain));
	}
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::smoothing_bias(SCM s_gain)
{
	SCM_ASSERT(SCM_NUMBERP(s_gain), s_gain, SCM_ARG1, "smoothing-bias");	
	if (Audio!=NULL)
	{	
		Audio->SetSmoothingBias(gh_scm2double(s_gain));
	}
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::backfacecull(SCM s)
{
	SCM_ASSERT(SCM_NUMBERP(s), s, SCM_ARG1, "backfacecull");
	Fluxus->GetRenderer()->SetBackFaceCull(gh_scm2double(s));
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::desiredfps(SCM s)
{
	SCM_ASSERT(SCM_NUMBERP(s), s, SCM_ARG1, "desiredfps");
	Fluxus->GetRenderer()->SetDesiredFPS(gh_scm2double(s));
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::clear_colour(SCM s_vec)
{
    SCM_ASSERT(SCM_VECTORP(s_vec), s_vec, SCM_ARG1, "clear_colour");
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_vec)==3, s_vec, SCM_ARG1, "clear_colour");
	float col[3];
	gh_scm2floats(s_vec,col);
    Fluxus->GetRenderer()->SetBGColour(dColour(col[0],col[1],col[2]));
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::clear_frame(SCM s_gain)
{
	SCM_ASSERT(SCM_NUMBERP(s_gain), s_gain, SCM_ARG1, "clear_frame");	
	Fluxus->GetRenderer()->SetClearFrame(gh_scm2double(s_gain));
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::turtle_prim(SCM type)
{
	SCM_ASSERT(SCM_NUMBERP(type), type, SCM_ARG1, "turtle_prim");
	turtle.Prim((int)gh_scm2double(type));
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::turtle_vert()
{
	turtle.Vert();
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::turtle_build()
{
	return gh_double2scm(turtle.Build(Fluxus->GetRenderer()));
}

SCM FluxusBinding::turtle_move(SCM dist)
{
	SCM_ASSERT(SCM_NUMBERP(dist), dist, SCM_ARG1, "turtle_move");
	turtle.Move(gh_scm2double(dist));
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::turtle_turn(SCM s_vec)
{
    SCM_ASSERT(SCM_VECTORP(s_vec), s_vec, SCM_ARG1, "turtle_turn");
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_vec)==3, s_vec, SCM_ARG1, "turtle_turn");
	float rot[3];
	gh_scm2floats(s_vec,rot);
	turtle.Turn(dVector(rot[0],rot[1],rot[2]));
	return SCM_UNSPECIFIED;	
}

SCM FluxusBinding::turtle_reset()
{
	turtle.Reset();
	return SCM_UNSPECIFIED;	
}

SCM FluxusBinding::start_framedump(SCM s_name)
{
	SCM_ASSERT(SCM_STRINGP(s_name), s_name, SCM_ARG1, "start_framedump");	
    size_t size=0;
	char *name=gh_scm2newstr(s_name,&size);
	Fluxus->StartDumpFrames(name);
	free(name);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::end_framedump()
{
	Fluxus->EndDumpFrames();
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::process(SCM s_wavname)
{
	SCM_ASSERT(SCM_STRINGP(s_wavname), s_wavname, SCM_ARG1, "process");	
    size_t size=0;
	char *wavname=gh_scm2newstr(s_wavname,&size);
	if (Audio!=NULL)
	{	
		Audio->Process(wavname);
	}
	free(wavname);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::osc_source(SCM s_port)
{
	SCM_ASSERT(SCM_STRINGP(s_port), s_port, SCM_ARG1, "osc_source");	
    size_t size=0;
	char *port=gh_scm2newstr(s_port,&size);
	Fluxus->StartOSC(port);
	free(port);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::osc_msg(SCM s_token)
{
	SCM_ASSERT(SCM_STRINGP(s_token), s_token, SCM_ARG1, "msg_osc");
	size_t size=0;	
 	char *name=gh_scm2newstr(s_token,&size);
	return gh_bool2scm(Fluxus->MsgOSC(name));	
}

SCM FluxusBinding::osc(SCM s_index)
{
    SCM_ASSERT(SCM_NUMBERP(s_index), s_index, SCM_ARG2, "osc");
 
	unsigned int index=(unsigned int)gh_scm2double(s_index);
	char type = Fluxus->TypeFromOSC(index);
	SCM ret;
	if (type=='n') ret=gh_double2scm(Fluxus->NumberFromOSC(index));
	else if (type=='s') 
	{
		string value=Fluxus->StringFromOSC(index);
		ret=gh_str2scm(value.c_str(),value.size());	
	}
	else ret=SCM_UNSPECIFIED;
	return ret;
}

SCM FluxusBinding::osc_destination(SCM s_port)
{
	SCM_ASSERT(SCM_STRINGP(s_port), s_port, SCM_ARG1, "osc_source");	
    size_t size=0;
	char *port=gh_scm2newstr(s_port,&size);
	Fluxus->StartOSCClient(port);
	free(port);
    return SCM_UNSPECIFIED;
}


SCM FluxusBinding::osc_peek()
{
	string value=Fluxus->GetLastMsg();
	return gh_str2scm(value.c_str(),value.size());	
}

SCM FluxusBinding::osc_send(SCM s_msg, SCM s_types, SCM s_arglist)
{
	SCM_ASSERT(SCM_STRINGP(s_msg), s_msg, SCM_ARG1, "osc-send");	
	SCM_ASSERT(SCM_STRINGP(s_types), s_types, SCM_ARG2, "osc-send");	
	// todo: fix this...
	//SCM_ASSERT(SCM_LISTP(s_arglist), s_arglist, SCM_ARG2, "osc-send");
    size_t size=0, typesize=0;
	char *msg=gh_scm2newstr(s_msg,&size);
	char *types=gh_scm2newstr(s_types,&typesize);
	
	// vectors seem easier to handle than lists with this api
	SCM argvec = gh_list_to_vector(s_arglist);
	
	vector<OSCData*> oscargs;
	for (unsigned int n=0; n<gh_vector_length(argvec); n++)
	{
		SCM arg=gh_vector_ref(argvec, gh_int2scm(n));

		if (gh_number_p(arg) || gh_exact_p(arg) || gh_inexact_p(arg))
		{
			if (n<typesize)
			{
				if (types[n]=='f') oscargs.push_back(new OSCFloat(gh_scm2double(arg)));
				else if (types[n]=='i') oscargs.push_back(new OSCInt((int)gh_scm2double(arg)));
			}
		}
		else if (gh_string_p(arg))
		{
			char *argstring=gh_scm2newstr(arg,&size);
			oscargs.push_back(new OSCString(argstring));
			free(argstring);
		}
		else
		{
			cerr<<"osc-send has found an argument type it can't send, numbers and strings only"<<endl;
		}
	}

	Fluxus->SendOSC(msg,oscargs);
	free(msg);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::pdata_get(SCM s_t, SCM s_i)
{
	SCM_ASSERT(SCM_STRINGP(s_t), s_t, SCM_ARG1, "pdata-get");
    SCM_ASSERT(SCM_NUMBERP(s_i), s_i, SCM_ARG2, "pdata-get");
	
	Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();    
	if (Grabbed) 
	{
		size_t ssize=0;
		char *name=gh_scm2newstr(s_t,&ssize);
		unsigned int index=(int)gh_scm2double(s_i);
		unsigned int size;
		char type;
		SCM ret=gh_floats2scm(dVector().arr(),3);
		
		if (Grabbed->GetDataInfo(name,type,size))
		{
			if (index<size)
			{
				if (type=='v')	
				{
					ret=gh_floats2scm(Grabbed->GetData<dVector>(name,index).arr(),3); 
				}
				else if (type=='c')	
				{
					ret=gh_floats2scm(Grabbed->GetData<dColour>(name,index).arr(),3); 
				}
				else if (type=='m')	
				{
					ret=gh_floats2scm(Grabbed->GetData<dMatrix>(name,index).arr(),16); 
				}
			}
		}
		free(name); 
		return ret;
	}
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::pdata_set(SCM s_t, SCM s_i, SCM s_v)
{
    SCM_ASSERT(SCM_STRINGP(s_t), s_t, SCM_ARG1, "pdata-set");
    SCM_ASSERT(SCM_NUMBERP(s_i), s_i, SCM_ARG2, "pdata-set");
	SCM_ASSERT(SCM_VECTORP(s_v), s_v, SCM_ARG3, "pdata-set");	
	//SCM_ASSERT(SCM_VECTOR_LENGTH(s_v)==3, s_v, SCM_ARG3, "pdata-set");
	
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();    
	if (Grabbed) 
	{
		size_t ssize=0;
		char *name=gh_scm2newstr(s_t,&ssize);
		unsigned int index=(int)gh_scm2double(s_i);
		unsigned int size;
		char type;
		
		if (Grabbed->GetDataInfo(name,type,size))
		{
			if (index<size)
			{
				if (type=='v')	
				{
					dVector v;
					gh_scm2floats(s_v,v.arr());
					Grabbed->SetData<dVector>(name,index,v);
				}
				else if (type=='c')	
				{
					dColour c;
					gh_scm2floats(s_v,c.arr());
					Grabbed->SetData<dColour>(name,index,c);
				}
				else if (type=='m')	
				{
					dMatrix m;
					gh_scm2floats(s_v,m.arr());
					Grabbed->SetData<dMatrix>(name,index,m);
				}
			}
		}
		free(name); 
	}
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::pdata_add(SCM s_name, SCM s_type)
{
    SCM_ASSERT(SCM_STRINGP(s_name), s_name, SCM_ARG1, "pdata-add");
    SCM_ASSERT(SCM_STRINGP(s_type), s_type, SCM_ARG2, "pdata-add");
	
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();    
	if (Grabbed) 
	{
		size_t ssize=0;
		char *names=gh_scm2newstr(s_name,&ssize);
		char *types=gh_scm2newstr(s_type,&ssize);
		char type=0;
		unsigned int size=0;
		
		PData *ptr=NULL;
		Grabbed->GetDataInfo("p", type, size);
		
		switch (types[0])
		{
			case 'v': ptr = new TypedPData<dVector>; ((TypedPData<dVector>*)ptr)->m_Data.resize(size); break;
			case 'c': ptr = new TypedPData<dColour>; ((TypedPData<dColour>*)ptr)->m_Data.resize(size); break;
			case 'f': ptr = new TypedPData<float>; ((TypedPData<float>*)ptr)->m_Data.resize(size); break;
			case 'm': ptr = new TypedPData<dMatrix>; ((TypedPData<dMatrix>*)ptr)->m_Data.resize(size); break;
			default : cerr<<"pdata-new: unknown type "<<types[0]<<endl; break;
		}
		
		if (ptr)
		{
			Grabbed->AddData(names,ptr);
		}
		
		free(names);
		free(types);
	}
	
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::pdata_op(SCM s_op, SCM s_pd, SCM s_oper)
{
    SCM_ASSERT(SCM_STRINGP(s_op), s_op, SCM_ARG1, "pdata-op");
    SCM_ASSERT(SCM_STRINGP(s_pd), s_pd, SCM_ARG2, "pdata-op");
	PData *ret=NULL;
	
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();    
	if (Grabbed) 
	{
		size_t ssize=0;
		char *op=gh_scm2newstr(s_op,&ssize);
		char *pd=gh_scm2newstr(s_pd,&ssize);
		
		// find out what the inputs are, and call the corresponding function
		if (gh_string_p(s_oper))
		{
			char *operand=gh_scm2newstr(s_oper,&ssize);
			
			PData* pd2 = Grabbed->GetDataRaw(operand);
			
			TypedPData<dVector> *data = dynamic_cast<TypedPData<dVector>*>(pd2);	
			if (data) ret = Grabbed->DataOp(op, pd, data);
			else
			{
				TypedPData<dColour> *data = dynamic_cast<TypedPData<dColour>*>(pd2);
				if (data) ret = Grabbed->DataOp(op, pd, data);
				else 
				{
					TypedPData<float> *data = dynamic_cast<TypedPData<float>*>(pd2);
					if (data) ret = Grabbed->DataOp(op, pd, data);
					else 
					{
						TypedPData<dMatrix> *data = dynamic_cast<TypedPData<dMatrix>*>(pd2);
						if (data) ret = Grabbed->DataOp(op, pd, data);
					}
				}
			}
			free(operand);
		}
		else if (gh_number_p(s_oper))
		{
			ret = Grabbed->DataOp(op, pd, (float)gh_scm2double(s_oper));
		}
		else if (gh_vector_p(s_oper))
		{
			switch (gh_vector_length(s_oper))
			{
				case 3:
				{
					dVector v;
					gh_scm2floats(s_oper,v.arr());
					ret = Grabbed->DataOp(op, pd, v);
				}
				break;
				case 4:
				{
					dColour v;
					gh_scm2floats(s_oper,v.arr());
					ret = Grabbed->DataOp(op, pd, v);
				}
				break;
				case 16:
				{
					dMatrix v;
					gh_scm2floats(s_oper,v.arr());
					ret = Grabbed->DataOp(op, pd, v);
				}
				break;	
			}
		}
		
		free(op);
		free(pd);
	}
		
	// convert the return data
	if (ret!=NULL)
	{
		TypedPData<dVector> *data = dynamic_cast<TypedPData<dVector>*>(ret);	
		if (data) 
		{
			dVector r = data->m_Data[0];
			delete ret;
			return gh_floats2scm(r.arr(),3);
		}
		else
		{
			TypedPData<dColour> *data = dynamic_cast<TypedPData<dColour>*>(ret);
			if (data) 
			{
				dColour r = data->m_Data[0];
				delete ret;
				return gh_floats2scm(r.arr(),4);
			}
			else 
			{
				TypedPData<float> *data = dynamic_cast<TypedPData<float>*>(ret);
				if (data) 
				{		
					float r = data->m_Data[0];
					delete ret;
					return gh_double2scm(r);
				}
				else 
				{
					TypedPData<dMatrix> *data = dynamic_cast<TypedPData<dMatrix>*>(ret);
					if (data) 
					{
						dMatrix r = data->m_Data[0];
						delete ret;
						return gh_floats2scm(r.arr(),16);
					}
				}
			}
		}
	}
	
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::pdata_copy(SCM s_s, SCM s_d)
{
    SCM_ASSERT(SCM_STRINGP(s_s), s_s, SCM_ARG1, "pdata-copy");
    SCM_ASSERT(SCM_STRINGP(s_d), s_d, SCM_ARG2, "pdata-copy");
	
	Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();    
	if (Grabbed) 
	{
		size_t ssize=0;
		char *source=gh_scm2newstr(s_s,&ssize);
		char *dest=gh_scm2newstr(s_d,&ssize);
		Grabbed->CopyData(source,dest);
		free(source);
		free(dest);
	}
	
	return SCM_UNSPECIFIED;
}
/*
SCM FluxusBinding::pdata_op(SCM s_op, SCM s_pd, SCM s_other)
{
    SCM_ASSERT(SCM_STRINGP(s_op), s_op, SCM_ARG1, "pdata-op");
    SCM_ASSERT(SCM_STRINGP(s_pd), s_pd, SCM_ARG2, "pdata-op");
	
	Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();    
	if (Grabbed) 
	{
		size_t ssize=0;
		char *op=gh_scm2newstr(s_op,&ssize);
		string opstr(op);
		
		
		free(op);
	}
	
	return SCM_UNSPECIFIED;
}*/

SCM FluxusBinding::pdata_size()
{
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();    
	if (Grabbed) 
	{
		char type;
		unsigned int size;
		// all prims have a p - this might have to be changed if we 
		// ever support data of size other than vertex - but it will 
		// only have to be changed here...
		Grabbed->GetDataInfo("p", type, size);
		return gh_double2scm(size);
	}
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::finalise()
{
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::recalc_normals()
{
	Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();    
	if (Grabbed) Grabbed->RecalculateNormals();
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::hide(SCM s_b)
{
    SCM_ASSERT(SCM_NUMBERP(s_b), s_b, SCM_ARG1, "hide");
	Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();    
	if (Grabbed) Grabbed->Hide(gh_scm2double(s_b));
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::selectable(SCM s_b)
{
    SCM_ASSERT(SCM_NUMBERP(s_b), s_b, SCM_ARG1, "selectable");
	Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();    
	if (Grabbed) Grabbed->Selectable(gh_scm2double(s_b));
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::get_transform()
{
	return gh_floats2scm(Fluxus->GetRenderer()->GetGlobalTransform(GrabbedID).arr(),16);
}

SCM FluxusBinding::get_camera_transform()
{
	return gh_floats2scm(Fluxus->GetRenderer()->GetCamera()->inverse().arr(),16);
}

///////////////////////////////////////////////////////////////////////////////////////

SCM FluxusBinding::vmul(SCM s_a, SCM s_b)
{
	SCM_ASSERT(SCM_VECTORP(s_a),  s_a,  SCM_ARG1, "vmul");
	SCM_ASSERT(SCM_NUMBERP(s_b),  s_b,  SCM_ARG2, "vmul");	
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_a)==3,  s_a,  SCM_ARG1, "vmul");
	dVector a;
	gh_scm2floats(s_a,a.arr());
	return gh_floats2scm((a*gh_scm2double(s_b)).arr(),3);
}

SCM FluxusBinding::vadd(SCM s_a, SCM s_b)
{
	SCM_ASSERT(SCM_VECTORP(s_a),  s_a,  SCM_ARG1, "vadd");
	SCM_ASSERT(SCM_VECTORP(s_b),  s_b,  SCM_ARG2, "vadd");
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_a)==3,  s_a,  SCM_ARG1, "vadd");
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_b)==3,  s_b,  SCM_ARG2, "vadd");
	dVector a;
	gh_scm2floats(s_a,a.arr());
	dVector b;
	gh_scm2floats(s_b,b.arr());
	return gh_floats2scm((a+b).arr(),3);
}

SCM FluxusBinding::vsub(SCM s_a, SCM s_b)
{
	SCM_ASSERT(SCM_VECTORP(s_a),  s_a,  SCM_ARG1, "vsub");
	SCM_ASSERT(SCM_VECTORP(s_b),  s_b,  SCM_ARG2, "vsub");
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_a)==3,  s_a,  SCM_ARG1, "vsub");
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_b)==3,  s_b,  SCM_ARG2, "vsub");	
	dVector a;
	gh_scm2floats(s_a,a.arr());
	dVector b;
	gh_scm2floats(s_b,b.arr());
	return gh_floats2scm((a-b).arr(),3);
}

SCM FluxusBinding::vdiv(SCM s_a, SCM s_b)
{
	SCM_ASSERT(SCM_VECTORP(s_a),  s_a,  SCM_ARG1, "vdiv");
	SCM_ASSERT(SCM_NUMBERP(s_b),  s_b,  SCM_ARG2, "vdiv");
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_a)==3,  s_a,  SCM_ARG1, "vdiv");
	dVector a;
	gh_scm2floats(s_a,a.arr());
	return gh_floats2scm((a/gh_scm2double(s_b)).arr(),3);
}

SCM FluxusBinding::vtransform(SCM s_v, SCM s_m)
{
	SCM_ASSERT(SCM_VECTORP(s_v),  s_v,  SCM_ARG1, "vtransform");
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_v)==3,  s_v,  SCM_ARG1, "vtransform");
	SCM_ASSERT(SCM_VECTORP(s_m),  s_m,  SCM_ARG2, "vtransform");	
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_m)==16,  s_m,  SCM_ARG2, "vtransform");
	dVector v;
	gh_scm2floats(s_v,v.arr());
	dMatrix m;
	gh_scm2floats(s_m,m.arr());
	return gh_floats2scm((m.transform(v)).arr(),3);
}

SCM FluxusBinding::vtransform_rot(SCM s_v, SCM s_m)
{
	SCM_ASSERT(SCM_VECTORP(s_v),  s_v,  SCM_ARG1, "vtransform-rot");
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_v)==3,  s_v,  SCM_ARG1, "vtransform-rot");
	SCM_ASSERT(SCM_VECTORP(s_m),  s_m,  SCM_ARG2, "vtransform-rot");	
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_m)==16,  s_m,  SCM_ARG2, "vtransform-rot");
	dVector v;
	gh_scm2floats(s_v,v.arr());
	dMatrix m;
	gh_scm2floats(s_m,m.arr());
	return gh_floats2scm((m.transform_no_trans(v)).arr(),3);
}

SCM FluxusBinding::vnormalise(SCM s_v)
{
	SCM_ASSERT(SCM_VECTORP(s_v),  s_v,  SCM_ARG1, "vnormalise");
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_v)==3,  s_v,  SCM_ARG1, "vnormalise");
	dVector v;
	gh_scm2floats(s_v,v.arr());
	v.normalise();
	return gh_floats2scm(v.arr(),3);
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::vdot(SCM s_a, SCM s_b)
{
	SCM_ASSERT(SCM_VECTORP(s_a),  s_a,  SCM_ARG1, "vdot");
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_a)==3,  s_a,  SCM_ARG1, "vdot");
	SCM_ASSERT(SCM_VECTORP(s_b),  s_b,  SCM_ARG2, "vdot");
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_b)==3,  s_a,  SCM_ARG2, "vdot");
	dVector a;
	gh_scm2floats(s_a,a.arr());
	dVector b;
	gh_scm2floats(s_b,b.arr());
	return gh_double2scm(a.dot(b));
}

SCM FluxusBinding::vmag(SCM s_a)
{
	SCM_ASSERT(SCM_VECTORP(s_a),  s_a,  SCM_ARG1, "vmag");
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_a)==3,  s_a,  SCM_ARG1, "vmag");
	dVector a;
	gh_scm2floats(s_a,a.arr());
	return gh_double2scm(a.mag());
}

SCM FluxusBinding::vdist(SCM s_a, SCM s_b)
{
	SCM_ASSERT(SCM_VECTORP(s_a),  s_a,  SCM_ARG1, "vdist");
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_a)==3,  s_a,  SCM_ARG1, "vdist");
	SCM_ASSERT(SCM_VECTORP(s_b),  s_b,  SCM_ARG2, "vdist");
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_b)==3,  s_a,  SCM_ARG2, "vdist");
	dVector a;
	gh_scm2floats(s_a,a.arr());
	dVector b;
	gh_scm2floats(s_b,b.arr());
	return gh_double2scm(a.dist(b));
}

SCM FluxusBinding::vcross(SCM s_a, SCM s_b)
{
	SCM_ASSERT(SCM_VECTORP(s_a),  s_a,  SCM_ARG1, "vcross");
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_a)==3,  s_a,  SCM_ARG1, "vcross");
	SCM_ASSERT(SCM_VECTORP(s_b),  s_b,  SCM_ARG2, "vcross");
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_b)==3,  s_b,  SCM_ARG2, "vcross");	
	dVector a;
	gh_scm2floats(s_a,a.arr());
	dVector b;
	gh_scm2floats(s_b,b.arr());
	return gh_floats2scm((a.cross(b)).arr(),3);
}

SCM FluxusBinding::mmul(SCM s_a, SCM s_b)
{
	SCM_ASSERT(SCM_VECTORP(s_a),  s_a,  SCM_ARG1, "mmul");
	SCM_ASSERT(SCM_VECTORP(s_b),  s_b,  SCM_ARG2, "mmul");
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_b)==16,  s_b,  SCM_ARG2, "mmul");
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_a)==16,  s_a,  SCM_ARG1, "mmul");		
	dMatrix a;
	gh_scm2floats(s_a,a.arr());
	dMatrix b;
	gh_scm2floats(s_b,b.arr());
	return gh_floats2scm((a*b).arr(),16);
}

SCM FluxusBinding::madd(SCM s_a, SCM s_b)
{
	SCM_ASSERT(SCM_VECTORP(s_a),  s_a,  SCM_ARG1, "madd");
	SCM_ASSERT(SCM_VECTORP(s_b),  s_b,  SCM_ARG2, "madd");	
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_b)==16,  s_b,  SCM_ARG2, "madd");
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_a)==16,  s_a,  SCM_ARG1, "madd");		
	dMatrix a;
	gh_scm2floats(s_a,a.arr());
	dMatrix b;
	gh_scm2floats(s_b,b.arr());
	return gh_floats2scm((a+b).arr(),16);
}

SCM FluxusBinding::msub(SCM s_a, SCM s_b)
{
	SCM_ASSERT(SCM_VECTORP(s_a),  s_a,  SCM_ARG1, "msub");
	SCM_ASSERT(SCM_VECTORP(s_b),  s_b,  SCM_ARG2, "msub");	
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_b)==16,  s_b,  SCM_ARG2, "msub");
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_a)==16,  s_a,  SCM_ARG1, "msub");		
	dMatrix a;
	gh_scm2floats(s_a,a.arr());
	dMatrix b;
	gh_scm2floats(s_b,b.arr());
	return gh_floats2scm((a-b).arr(),16);
}

SCM FluxusBinding::mdiv(SCM s_a, SCM s_b)
{
	SCM_ASSERT(SCM_VECTORP(s_a),  s_a,  SCM_ARG1, "mdiv");
	SCM_ASSERT(SCM_VECTORP(s_b),  s_b,  SCM_ARG2, "mdiv");	
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_b)==16,  s_b,  SCM_ARG2, "mdiv");
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_a)==16,  s_a,  SCM_ARG1, "mdiv");
	dMatrix a;
	gh_scm2floats(s_a,a.arr());
	dMatrix b;
	gh_scm2floats(s_b,b.arr());
	return gh_floats2scm((a/b).arr(),16);
}

SCM FluxusBinding::mident()
{
	dMatrix m;
	return gh_floats2scm(m.arr(),16);	
}

SCM FluxusBinding::mtranslate(SCM s_v)
{
	SCM_ASSERT(SCM_VECTORP(s_v),  s_v,  SCM_ARG1, "mtranslate");
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_v)==3,  s_v,  SCM_ARG1, "mtranslate");
	dVector a;
	gh_scm2floats(s_v,a.arr());
	dMatrix m;
	m.translate(a.x,a.y,a.z);
	return gh_floats2scm(m.arr(),16);	
}

SCM FluxusBinding::mrotate(SCM s_v)
{
	SCM_ASSERT(SCM_VECTORP(s_v),  s_v,  SCM_ARG1, "mrotate");
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_v)==3,  s_v,  SCM_ARG1, "mrotate");
	dVector a;
	gh_scm2floats(s_v,a.arr());
	dMatrix m;
	m.rotxyz(a.x,a.y,a.z);
	return gh_floats2scm(m.arr(),16);	
}

SCM FluxusBinding::mscale(SCM s_v)
{
	SCM_ASSERT(SCM_VECTORP(s_v),  s_v,  SCM_ARG1, "mscale");
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_v)==3,  s_v,  SCM_ARG1, "mscale");
	dVector a;
	gh_scm2floats(s_v,a.arr());
	dMatrix m;
	m.scale(a.x,a.y,a.z);
	return gh_floats2scm(m.arr(),16);	
}

SCM FluxusBinding::mtranspose(SCM s_a)
{
	SCM_ASSERT(SCM_VECTORP(s_a),  s_a,  SCM_ARG1, "mtranspose");
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_a)==16,  s_a,  SCM_ARG1, "mtranspose");
	dMatrix m;
	gh_scm2floats(s_a,m.arr());
	m.transpose();
	return gh_floats2scm(m.arr(),16);	
}

SCM FluxusBinding::minverse(SCM s_a)
{
	SCM_ASSERT(SCM_VECTORP(s_a),  s_a,  SCM_ARG1, "minverse");
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_a)==16,  s_a,  SCM_ARG1, "minverse");
	dMatrix m;
	gh_scm2floats(s_a,m.arr());
	m=m.inverse();
	return gh_floats2scm(m.arr(),16);	
}

SCM FluxusBinding::maim(SCM s_a, SCM s_b)
{
	SCM_ASSERT(SCM_VECTORP(s_a),  s_a,  SCM_ARG1, "maim");
	SCM_ASSERT(SCM_VECTORP(s_b),  s_b,  SCM_ARG2, "maim");	
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_a)==3,  s_a,  SCM_ARG1, "maim");
	SCM_ASSERT(SCM_VECTOR_LENGTH(s_b)==3,  s_b,  SCM_ARG2, "maim");
	dVector a;
	gh_scm2floats(s_a,a.arr());
	dVector b;
	gh_scm2floats(s_b,b.arr());
	dMatrix m;
	m.aim(a,b);
	return gh_floats2scm(m.arr(),16);
}

SCM FluxusBinding::mouse_over()
{
	return gh_double2scm(Fluxus->GetRenderer()->Select((int)Fluxus->GetMouseX(),(int)Fluxus->GetMouseY(),5));
}

SCM FluxusBinding::mouse_button(SCM s_b)
{
	SCM_ASSERT(SCM_NUMBERP(s_b),  s_b,  SCM_ARG1, "mouse-button");
	int but=(int)gh_scm2double(s_b);		
	return gh_bool2scm(Fluxus->GetMouseButton()==but);
}

SCM FluxusBinding::load_recorded_code(SCM s_name)
{
	SCM_ASSERT(SCM_STRINGP(s_name), s_name, SCM_ARG1, "load-recorded-code");	
    size_t size=0;
	char *name=gh_scm2newstr(s_name,&size);
	Fluxus->LoadRecordedCode(name);
	free(name);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::save_recorded_code(SCM s_name)
{
	SCM_ASSERT(SCM_STRINGP(s_name), s_name, SCM_ARG1, "save-recorded-code");	
    size_t size=0;
	char *name=gh_scm2newstr(s_name,&size);
	Fluxus->SaveRecordedCode(name);
	free(name);
    return SCM_UNSPECIFIED;
}

void FluxusBinding::RegisterProcs()
{
	// primitives
	gh_new_procedure0_2("build-polygons",  build_polygons);
	gh_new_procedure0_0("build-cube",      build_cube);
    gh_new_procedure0_2("build-sphere", build_sphere);
    gh_new_procedure0_0("build-plane",     build_plane);
    gh_new_procedure0_2("build-seg-plane",     build_plane);
    gh_new_procedure0_2("build-cylinder", build_cylinder);
	gh_new_procedure4_0("build-line",   build_line);
	gh_new_procedure0_1("build-text",   build_text);
	gh_new_procedure0_1("build-nurbs",  build_nurbs);
	gh_new_procedure0_2("build-nurbs-sphere", build_nurbs_sphere);
	gh_new_procedure0_2("build-nurbs-plane", build_nurbs_plane);
	gh_new_procedure0_1("build-particles", build_particles);
	gh_new_procedure0_1("draw-instance", draw_instance);
    gh_new_procedure0_0("draw-cube",       draw_cube);
    gh_new_procedure0_0("draw-plane",      draw_plane);
    gh_new_procedure0_0("draw-sphere",     draw_sphere);
    gh_new_procedure0_0("draw-cylinder",   draw_cylinder);
	gh_new_procedure0_1("destroy",      destroy);
	gh_new_procedure0_0("get-transform", get_transform);
	gh_new_procedure0_0("get-camera-transform", get_camera_transform);

	// renderstate operations
	gh_new_procedure0_0("push",            push);
	gh_new_procedure0_0("pop",             pop);
	gh_new_procedure0_1("grab",         grab);
    gh_new_procedure0_0("ungrab",          ungrab);
    gh_new_procedure0_0("print-scene-graph",print_scene_graph);
	gh_new_procedure0_1("apply",        apply);
	gh_new_procedure0_0("identity",        flux_identity);
	gh_new_procedure0_1("concat",          concat);
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
    gh_new_procedure0_0("hint-solid",      hint_solid);
    gh_new_procedure0_0("hint-wire",       hint_wire);
    gh_new_procedure0_0("hint-normal",     hint_normal);
    gh_new_procedure0_0("hint-points",     hint_points);
    gh_new_procedure0_0("hint-anti-alias", hint_anti_alias);
    gh_new_procedure0_0("hint-none",       hint_none);
    gh_new_procedure0_0("hint-unlit",      hint_unlit);
	gh_new_procedure0_1("line-width",   line_width);
	gh_new_procedure0_1("point-width",  point_width);
	gh_new_procedure0_2("blend-mode",   blend_mode);
    gh_new_procedure0_1("parent",       parent);
	gh_new_procedure0_1("hide",         hide);
	gh_new_procedure0_1("selectable",   selectable);
	
	// global state operations
	gh_new_procedure0_0("clear",           clear);
	gh_new_procedure0_0("ortho",       	ortho);
	gh_new_procedure0_0("persp",       	persp);
    gh_new_procedure0_0("reset-camera",    reset_camera);
	gh_new_procedure0_1("lock-camera",  lock_camera);
	gh_new_procedure0_1("clear-colour",    clear_colour);	
	gh_new_procedure0_1("clear-frame",     clear_frame);
	gh_new_procedure0_1("blur",         blur);
	gh_new_procedure0_1("feedback",     feedback);
	gh_new_procedure0_1("feedback-transform", feedback_transform);
    gh_new_procedure0_1("show-axis",    show_axis);
    gh_new_procedure0_1("show-fps",     show_fps);
	gh_new_procedure0_1("backfacecull",    backfacecull);
	gh_new_procedure0_1("load-texture", load_texture);
	gh_new_procedure0_1("force-load-texture", force_load_texture);
	gh_new_procedure4_0("fog", fog);

	// lights
    gh_new_procedure0_1("make-light",         make_light);
    gh_new_procedure0_0("clear-lights",       clear_lights);
	gh_new_procedure0_2("light-ambient",   light_ambient);
	gh_new_procedure0_2("light-diffuse",   light_diffuse);
	gh_new_procedure0_2("light-specular",  light_specular);
	gh_new_procedure0_2("light-position",  light_position);
	
	// interpreter + misc
	gh_new_procedure0_1("load", load);
	gh_new_procedure0_1("save-name", save_name);
	gh_new_procedure0_1("source", source);
	gh_new_procedure0_1("key-pressed", key_pressed);
	gh_new_procedure0_0("mouse-over", mouse_over);
	gh_new_procedure0_1("mouse-button", mouse_button);
    gh_new_procedure0_0("time", time);
    gh_new_procedure0_0("delta", delta);
    gh_new_procedure0_1("every-frame", engine_callback);
    gh_new_procedure0_0("flxrnd", srandom);
	gh_new_procedure0_1("desiredfps", desiredfps);
	gh_new_procedure0_1("start-framedump", start_framedump);
	gh_new_procedure0_0("end-framedump", end_framedump);
	gh_new_procedure0_1("load-code", load_recorded_code);
	gh_new_procedure0_1("save-code", save_recorded_code);
	
	// audio
	gh_new_procedure3_0("start-audio",  start_audio);	
	gh_new_procedure0_1("smoothing-bias", smoothing_bias);	
	gh_new_procedure0_1("gain",         gain);	
	gh_new_procedure0_1("get-harmonic", get_harmonic);
	gh_new_procedure0_1("gh",           get_harmonic);
	gh_new_procedure0_1("process",   	process);
	
	// turtle
	gh_new_procedure0_0("turtle-vert",            turtle_vert);
	gh_new_procedure0_0("turtle-build",           turtle_build);
	gh_new_procedure0_0("turtle-reset",           turtle_reset);
	gh_new_procedure0_1("turtle-move",         turtle_move);
	gh_new_procedure0_1("turtle-turn",         turtle_turn);
	gh_new_procedure0_1("turtle-prim",         turtle_prim);
	
	// physics
	gh_new_procedure0_1("collisions", collisions);
	gh_new_procedure0_1("gravity", gravity);
	gh_new_procedure0_1("set-max-physical", set_max_physical);
    gh_new_procedure0_1("active-box",     active_box);
    gh_new_procedure0_1("active-sphere",  active_sphere);
    gh_new_procedure0_1("active-cylinder",active_cylinder);
    gh_new_procedure0_1("passive-box",     passive_box);
    gh_new_procedure0_1("passive-sphere",  passive_sphere);
    gh_new_procedure0_1("passive-cylinder",passive_cylinder);
    gh_new_procedure0_2("ground-plane", ground_plane);
    gh_new_procedure4_0("build-hingejoint", build_hingejoint);
    gh_new_procedure3_0("build-balljoint", build_balljoint);
    gh_new_procedure3_0("build-sliderjoint", build_sliderjoint);
    gh_new_procedure5_0("build-hinge2joint", build_hinge2joint);
    gh_new_procedure3_0("build-amotorjoint", build_amotorjoint);
    gh_new_procedure4_0("surface-params", surface_params);
    gh_new_procedure3_0("joint-param",  joint_param);
    gh_new_procedure3_0("joint-angle",  joint_angle);
    gh_new_procedure0_2("set-mass",     set_mass);
    gh_new_procedure0_2("kick",         kick);
    gh_new_procedure0_2("twist",        twist);
	gh_new_procedure0_1("has-collided", has_collided);
	
	gh_new_procedure0_1("osc-source",   osc_source);
	gh_new_procedure0_1("osc",          osc);
	gh_new_procedure0_0("osc-peek",        osc_peek);
	gh_new_procedure0_1("osc-msg",      osc_msg);
	gh_new_procedure0_1("osc-destination",    osc_destination);
	gh_new_procedure3_0("osc-send",     osc_send);
	
	// advanced prim editing
	gh_new_procedure3_0("pdata-set", pdata_set);
	gh_new_procedure0_2("pdata-get", pdata_get);
	gh_new_procedure0_0("pdata-size", pdata_size);
	gh_new_procedure0_2("pdata-add", pdata_add);
	gh_new_procedure0_2("pdata-copy", pdata_copy);
	gh_new_procedure3_0("pdata-op", pdata_op);	
	gh_new_procedure0_0("finalise", finalise);
	gh_new_procedure0_0("recalc-normals", recalc_normals);
	
	// maths
	gh_new_procedure0_2("vmul", vmul);
	gh_new_procedure0_2("vadd", vadd);
	gh_new_procedure0_2("vsub", vsub);
	gh_new_procedure0_2("vdiv", vdiv);
	gh_new_procedure0_2("vtransform", vtransform);	
	gh_new_procedure0_2("vtransform-rot", vtransform_rot);	
	gh_new_procedure0_1("vnormalise", vnormalise);	
	gh_new_procedure0_2("vdot", vdot);	
	gh_new_procedure0_1("vmag", vmag);	
	gh_new_procedure0_2("vdist", vdist);	
	gh_new_procedure0_2("vcross", vcross);	
	gh_new_procedure0_2("mmul", mmul);
	gh_new_procedure0_2("madd", madd);
	gh_new_procedure0_2("msub", msub);
	gh_new_procedure0_2("mdiv", mdiv);
	gh_new_procedure0_0("mident",  mident);
	gh_new_procedure0_1("mtranslate", mtranslate);	
	gh_new_procedure0_1("mrotate", mrotate);	
	gh_new_procedure0_1("mscale", mscale);
	gh_new_procedure0_1("mtranspose", mtranspose);
	gh_new_procedure0_1("minverse", minverse);
	gh_new_procedure0_2("maim", maim);
}
