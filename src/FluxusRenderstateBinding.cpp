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

#include "FluxusRenderstateBinding.h"
#include <fstream>
#include <deque>
#include <libguile.h>
#include "FluxusRenderstateBinding.h"
#include "SearchPaths.h"
#include "Repl.h"
#include <LinePrimitive.h>
#include <ParticlePrimitive.h>
#include <PixelPrimitive.h>
#include <LocatorPrimitive.h>

void FluxusRenderstateBinding::RegisterProcs()
{
	SchemePrim::Init();
	
	// renderstate operations
	scm_c_define_gsubr("push",0,0,0,(CALLBACK_CAST) 		   push);
	scm_c_define_gsubr("pop",0,0,0,(CALLBACK_CAST)  		   pop);
	scm_c_define_gsubr("grab",1,0,0,(CALLBACK_CAST) 		grab);
    scm_c_define_gsubr("ungrab",0,0,0,(CALLBACK_CAST)		   ungrab);
    scm_c_define_gsubr("print-scene-graph",0,0,0,(CALLBACK_CAST)print_scene_graph);
	scm_c_define_gsubr("apply-transform",1,0,0,(CALLBACK_CAST)  	  apply);
	scm_c_define_gsubr("identity",0,0,0,(CALLBACK_CAST) 	   flux_identity);
	scm_c_define_gsubr("concat",1,0,0,(CALLBACK_CAST)		   concat);
    scm_c_define_gsubr("translate",1,0,0,(CALLBACK_CAST)	translate);
    scm_c_define_gsubr("rotate", 1,0,0, (CALLBACK_CAST)rotate);
    scm_c_define_gsubr("scale",1,0,0,(CALLBACK_CAST)		scale);
    scm_c_define_gsubr("colour",1,0,0,(CALLBACK_CAST)		colour);
    scm_c_define_gsubr("wire-colour",1,0,0,(CALLBACK_CAST)  wire_colour);
    scm_c_define_gsubr("opacity",1,0,0,(CALLBACK_CAST)  	opacity);
    scm_c_define_gsubr("specular",1,0,0,(CALLBACK_CAST) 	specular);
    scm_c_define_gsubr("ambient",1,0,0,(CALLBACK_CAST)     ambient);
    scm_c_define_gsubr("emissive",1,0,0,(CALLBACK_CAST) 	emissive);
	scm_c_define_gsubr("shinyness",1,0,0,(CALLBACK_CAST)	shinyness);
	scm_c_define_gsubr("texture",1,0,0,(CALLBACK_CAST)  	texture);
	scm_c_define_gsubr("multitexture",2,0,0,(CALLBACK_CAST) multitexture);
    scm_c_define_gsubr("hint-solid",0,0,0,(CALLBACK_CAST)	   hint_solid);
    scm_c_define_gsubr("hint-wire",0,0,0,(CALLBACK_CAST)	   hint_wire);
    scm_c_define_gsubr("hint-normal",0,0,0,(CALLBACK_CAST)     hint_normal);
    scm_c_define_gsubr("hint-points",0,0,0,(CALLBACK_CAST)     hint_points);
    scm_c_define_gsubr("hint-anti-alias",0,0,0,(CALLBACK_CAST) hint_anti_alias);
    scm_c_define_gsubr("hint-none",0,0,0,(CALLBACK_CAST)	   hint_none);
    scm_c_define_gsubr("hint-unlit",0,0,0,(CALLBACK_CAST)	   hint_unlit);
    scm_c_define_gsubr("hint-vertcols",0,0,0,(CALLBACK_CAST)   hint_vertcols);
    scm_c_define_gsubr("hint-box",0,0,0,(CALLBACK_CAST) 		hint_box);
    scm_c_define_gsubr("hint-multitex",0,0,0,(CALLBACK_CAST)   hint_multitex);
    scm_c_define_gsubr("hint-origin",0,0,0,(CALLBACK_CAST)    hint_origin);
    scm_c_define_gsubr("hint-cast-shadow",0,0,0,(CALLBACK_CAST) hint_cast_shadow);
    scm_c_define_gsubr("hint-ignore-depth",0,0,0,(CALLBACK_CAST) hint_ignore_depth);
	scm_c_define_gsubr("line-width",1,0,0,(CALLBACK_CAST)	line_width);
	scm_c_define_gsubr("point-width",1,0,0,(CALLBACK_CAST)  point_width);
	scm_c_define_gsubr("blend-mode",2,0,0,(CALLBACK_CAST)	blend_mode);
    scm_c_define_gsubr("parent", 1 ,0,0,(CALLBACK_CAST) 	parent);
	scm_c_define_gsubr("hide",1,0,0,(CALLBACK_CAST) 		hide);
	scm_c_define_gsubr("selectable",1,0,0,(CALLBACK_CAST)	selectable);
	scm_c_define_gsubr("shader",2,0,0,(CALLBACK_CAST)	shader);
	scm_c_define_gsubr("shader-set!",1,0,0,(CALLBACK_CAST)	shader_set);
}

SCM FluxusRenderstateBinding::push()
{
	if (GrabbedID!=-1)
	{
		cerr<<"error: can't (push) while an object is (grab)bed"<<endl;
		return SCM_UNSPECIFIED;
	}
	
    Fluxus->GetRenderer()->PushState();
    return SCM_UNSPECIFIED;
}

SCM FluxusRenderstateBinding::pop()
{
	if (GrabbedID!=-1)
	{
		cerr<<"error: can't (pop) while an object is (grab)bed"<<endl;
		return SCM_UNSPECIFIED;
	}

    Fluxus->GetRenderer()->PopState();
    return SCM_UNSPECIFIED;
}

SCM FluxusRenderstateBinding::grab(SCM s_id)
{
	int id = Smob2Prim(s_id);
	GrabbedIDStack.push_front(id);
	GrabbedID=id;
	Fluxus->GetRenderer()->Grab(GrabbedID);
	return SCM_UNSPECIFIED;
}

SCM FluxusRenderstateBinding::ungrab()
{
	Fluxus->GetRenderer()->UnGrab();
	GrabbedID=-1;
	
	if (!GrabbedIDStack.empty())
	{
		GrabbedIDStack.pop_front();
		if (!GrabbedIDStack.empty())
		{
			GrabbedID=*GrabbedIDStack.begin();
			Fluxus->GetRenderer()->Grab(GrabbedID);
		}
	}
		
	return SCM_UNSPECIFIED;
}

SCM FluxusRenderstateBinding::apply(SCM s_id)
{
	Fluxus->GetRenderer()->GetPrimitive(Smob2Prim(s_id))->ApplyTransform();
	return SCM_UNSPECIFIED;
}

SCM FluxusRenderstateBinding::opacity(SCM s_opac)
{
    SCM_ASSERT(scm_is_number(s_opac), s_opac, SCM_ARG1, "opacity");
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Opacity=scm_to_double(s_opac);
    else Fluxus->GetRenderer()->GetState()->Opacity=scm_to_double(s_opac);
    return SCM_UNSPECIFIED;
}

SCM FluxusRenderstateBinding::shinyness(SCM s_opac)
{
    SCM_ASSERT(scm_is_number(s_opac), s_opac, SCM_ARG1, "shinyness");
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Shinyness=scm_to_double(s_opac);
    else Fluxus->GetRenderer()->GetState()->Shinyness=scm_to_double(s_opac);
    return SCM_UNSPECIFIED;
}

SCM FluxusRenderstateBinding::colour(SCM s_vec)
{
    SCM_ASSERT(scm_is_generalized_vector(s_vec), s_vec, SCM_ARG1, "colour");
	SCM_ASSERT(scm_c_generalized_vector_length(s_vec)==3, s_vec, SCM_ARG1, "colour");
	float col[3];
	flx_floats_from_scm(s_vec,col);
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Colour=dColour(col[0],col[1],col[2]);
    else Fluxus->GetRenderer()->GetState()->Colour=dColour(col[0],col[1],col[2]);
    return SCM_UNSPECIFIED;
}

SCM FluxusRenderstateBinding::wire_colour(SCM s_vec)
{
    SCM_ASSERT(scm_is_generalized_vector(s_vec), s_vec, SCM_ARG1, "wire-colour");
	SCM_ASSERT(scm_c_generalized_vector_length(s_vec)==3, s_vec, SCM_ARG1, "wire-colour");
	float col[3];
	flx_floats_from_scm(s_vec,col);
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->WireColour=dColour(col[0],col[1],col[2]);
    else Fluxus->GetRenderer()->GetState()->WireColour=dColour(col[0],col[1],col[2]);
    return SCM_UNSPECIFIED;
}

SCM FluxusRenderstateBinding::specular(SCM s_vec)
{
    SCM_ASSERT(scm_is_generalized_vector(s_vec), s_vec, SCM_ARG1, "specular");
	SCM_ASSERT(scm_c_generalized_vector_length(s_vec)==3, s_vec, SCM_ARG1, "specular");
	float col[3];
	flx_floats_from_scm(s_vec,col);
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Specular=dColour(col[0],col[1],col[2]);
    else Fluxus->GetRenderer()->GetState()->Specular=dColour(col[0],col[1],col[2]);
    return SCM_UNSPECIFIED;
}

SCM FluxusRenderstateBinding::ambient(SCM s_vec)
{
    SCM_ASSERT(scm_is_generalized_vector(s_vec), s_vec, SCM_ARG1, "ambient");
	SCM_ASSERT(scm_c_generalized_vector_length(s_vec)==3, s_vec, SCM_ARG1, "ambient");
	float col[3];
	flx_floats_from_scm(s_vec,col);
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Ambient=dColour(col[0],col[1],col[2]);
    else Fluxus->GetRenderer()->GetState()->Ambient=dColour(col[0],col[1],col[2]);
    return SCM_UNSPECIFIED;
}

SCM FluxusRenderstateBinding::emissive(SCM s_vec)
{
    SCM_ASSERT(scm_is_generalized_vector(s_vec), s_vec, SCM_ARG1, "emissive");
	SCM_ASSERT(scm_c_generalized_vector_length(s_vec)==3, s_vec, SCM_ARG1, "emissive");
	float col[3];
	flx_floats_from_scm(s_vec,col);
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Emissive=dColour(col[0],col[1],col[2]);
    else Fluxus->GetRenderer()->GetState()->Emissive=dColour(col[0],col[1],col[2]);
    return SCM_UNSPECIFIED;
}

SCM FluxusRenderstateBinding::flux_identity()
{
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Transform.init();
    else Fluxus->GetRenderer()->GetState()->Transform.init();
    return SCM_UNSPECIFIED;
}

SCM FluxusRenderstateBinding::translate(SCM s_vec)
{
    SCM_ASSERT(scm_is_generalized_vector(s_vec), s_vec, SCM_ARG1, "translate");
	SCM_ASSERT(scm_c_generalized_vector_length(s_vec)==3, s_vec, SCM_ARG1, "translate");
	float trans[3];
	flx_floats_from_scm(s_vec,trans);
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Transform.translate(trans[0],trans[1],trans[2]);
    else Fluxus->GetRenderer()->GetState()->Transform.translate(trans[0],trans[1],trans[2]);
    return SCM_UNSPECIFIED;
}

SCM FluxusRenderstateBinding::concat(SCM s_m)
{
    SCM_ASSERT(scm_is_generalized_vector(s_m), s_m, SCM_ARG1, "concat");
	SCM_ASSERT(scm_c_generalized_vector_length(s_m)==16, s_m, SCM_ARG1, "concat");
	dMatrix m;
	flx_floats_from_scm(s_m,m.arr());
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Transform*=m;
    else Fluxus->GetRenderer()->GetState()->Transform*=m;
    return SCM_UNSPECIFIED;
}

SCM FluxusRenderstateBinding::rotate(SCM s_vec)
{
    SCM_ASSERT(scm_is_generalized_vector(s_vec), s_vec, SCM_ARG1, "rotate");
	
	if (scm_c_generalized_vector_length(s_vec)==3)
	{
		// euler angles
		float rot[3];
		flx_floats_from_scm(s_vec,rot);
	    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
	    if (Grabbed)
	    {
	    	Grabbed->GetState()->Transform.rotxyz(rot[0],rot[1],rot[2]);
	    }
	    else
	    {
	    	Fluxus->GetRenderer()->GetState()->Transform.rotxyz(rot[0],rot[1],rot[2]);
	    }
	}
	else if (scm_c_generalized_vector_length(s_vec)==4)
	{
		// quaternion
		dQuat a;
		flx_floats_from_scm(s_vec,a.arr());
		dMatrix m=a.toMatrix();
	 	Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
	    if (Grabbed)
	    {
	    	Grabbed->GetState()->Transform*=m;
	    }
	    else
	    {
	    	Fluxus->GetRenderer()->GetState()->Transform*=m;
	    }
	}
	else
	{
		cerr<<"rotate - wrong number of elements in vector"<<endl;
	}
    return SCM_UNSPECIFIED;
}

SCM FluxusRenderstateBinding::scale(SCM s_vec)
{
    SCM_ASSERT(scm_is_generalized_vector(s_vec), s_vec, SCM_ARG1, "scale");
	SCM_ASSERT(scm_c_generalized_vector_length(s_vec)==3, s_vec, SCM_ARG1, "scale");
	float scale[3];
	flx_floats_from_scm(s_vec,scale);
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Transform.scale(scale[0],scale[1],scale[2]);
    else Fluxus->GetRenderer()->GetState()->Transform.scale(scale[0],scale[1],scale[2]);
    return SCM_UNSPECIFIED;
}

SCM FluxusRenderstateBinding::parent(SCM s_p)
{
    Fluxus->GetRenderer()->GetState()->Parent=Smob2Prim(s_p);
	scm_remember_upto_here_1(s_p);
    return SCM_UNSPECIFIED;
}

SCM FluxusRenderstateBinding::line_width(SCM s_p)
{
    SCM_ASSERT(scm_is_number(s_p), s_p, SCM_ARG1, "line_width");
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->LineWidth=scm_to_double(s_p);
    else Fluxus->GetRenderer()->GetState()->LineWidth=scm_to_double(s_p);
    return SCM_UNSPECIFIED;
}

SCM FluxusRenderstateBinding::point_width(SCM s_p)
{
    SCM_ASSERT(scm_is_number(s_p), s_p, SCM_ARG1, "point_width");
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->PointWidth=scm_to_double(s_p);
    else Fluxus->GetRenderer()->GetState()->PointWidth=scm_to_double(s_p);
    return SCM_UNSPECIFIED;
}

SCM FluxusRenderstateBinding::blend_mode(SCM s_s, SCM s_d)
{
    SCM_ASSERT(scm_is_string(s_s), s_s, SCM_ARG1, "blend_mode");
    SCM_ASSERT(scm_is_string(s_d), s_d, SCM_ARG2, "blend_mode");
	
	char *s=scm_to_locale_string(s_s);	
	char *d=scm_to_locale_string(s_d);	
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->SetBlendMode(s,d);
    else Fluxus->GetRenderer()->GetState()->SetBlendMode(s,d);
	free(s);
	free(d);
	
    return SCM_UNSPECIFIED;
}

SCM FluxusRenderstateBinding::hint_solid()
{
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Hints|=HINT_SOLID;
    else Fluxus->GetRenderer()->GetState()->Hints|=HINT_SOLID;
    return SCM_UNSPECIFIED;
}

SCM FluxusRenderstateBinding::hint_wire()
{
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Hints|=HINT_WIRE;
    else Fluxus->GetRenderer()->GetState()->Hints|=HINT_WIRE;
    return SCM_UNSPECIFIED;
}

SCM FluxusRenderstateBinding::hint_normal()
{
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Hints|=HINT_NORMAL;
    else Fluxus->GetRenderer()->GetState()->Hints|=HINT_NORMAL;
    return SCM_UNSPECIFIED;
}

SCM FluxusRenderstateBinding::hint_points()
{
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Hints|=HINT_POINTS;
    else Fluxus->GetRenderer()->GetState()->Hints|=HINT_POINTS;
    return SCM_UNSPECIFIED;
}

SCM FluxusRenderstateBinding::hint_anti_alias()
{
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Hints|=HINT_AALIAS;
    else Fluxus->GetRenderer()->GetState()->Hints|=HINT_AALIAS;
    return SCM_UNSPECIFIED;
}

SCM FluxusRenderstateBinding::hint_unlit()
{
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Hints|=HINT_UNLIT;
    else Fluxus->GetRenderer()->GetState()->Hints|=HINT_UNLIT;
    return SCM_UNSPECIFIED;
}

SCM FluxusRenderstateBinding::hint_vertcols()
{
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Hints|=HINT_VERTCOLS;
    else Fluxus->GetRenderer()->GetState()->Hints|=HINT_VERTCOLS;
    return SCM_UNSPECIFIED;
}

SCM FluxusRenderstateBinding::hint_box()
{
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Hints|=HINT_BOUND;
    else Fluxus->GetRenderer()->GetState()->Hints|=HINT_BOUND;
    return SCM_UNSPECIFIED;
}

SCM FluxusRenderstateBinding::hint_multitex()
{
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Hints|=HINT_MULTITEX;
    else Fluxus->GetRenderer()->GetState()->Hints|=HINT_MULTITEX;
    return SCM_UNSPECIFIED;
}

SCM FluxusRenderstateBinding::hint_none()
{
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Hints=0;
    else Fluxus->GetRenderer()->GetState()->Hints=0;
    return SCM_UNSPECIFIED;
}

SCM FluxusRenderstateBinding::hint_origin()
{
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Hints|=HINT_ORIGIN;
    else Fluxus->GetRenderer()->GetState()->Hints|=HINT_ORIGIN;
    return SCM_UNSPECIFIED;
}

SCM FluxusRenderstateBinding::hint_cast_shadow()
{
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Hints|=HINT_CAST_SHADOW;
    else Fluxus->GetRenderer()->GetState()->Hints|=HINT_CAST_SHADOW;
    return SCM_UNSPECIFIED;
}

SCM FluxusRenderstateBinding::hint_ignore_depth()
{
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Hints|=HINT_IGNORE_DEPTH;
    else Fluxus->GetRenderer()->GetState()->Hints|=HINT_IGNORE_DEPTH;	
    return SCM_UNSPECIFIED;
}

SCM FluxusRenderstateBinding::texture(SCM s_id)
{
	SCM_ASSERT(scm_is_number(s_id), s_id, SCM_ARG1, "texture");	
	Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
	if (Grabbed) Grabbed->GetState()->Textures[0]=(int)scm_to_int(s_id);
    else Fluxus->GetRenderer()->GetState()->Textures[0]=(int)scm_to_int(s_id);
    return SCM_UNSPECIFIED;
}

SCM FluxusRenderstateBinding::multitexture(SCM s_t, SCM s_id)
{
	SCM_ASSERT(scm_is_number(s_t), s_t, SCM_ARG1, "multitexture");	
	SCM_ASSERT(scm_is_number(s_id), s_id, SCM_ARG2, "multitexture");	
	Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
	if (Grabbed) Grabbed->GetState()->Textures[scm_to_int(s_t)]=scm_to_int(s_id);
    else Fluxus->GetRenderer()->GetState()->Textures[scm_to_int(s_t)]=scm_to_int(s_id);
    return SCM_UNSPECIFIED;
}

SCM FluxusRenderstateBinding::print_scene_graph()
{
	Fluxus->GetRenderer()->PrintSceneGraph();
	return SCM_UNSPECIFIED;
}

SCM FluxusRenderstateBinding::hide(SCM s_b)
{
    SCM_ASSERT(scm_is_number(s_b), s_b, SCM_ARG1, "hide");
	Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();    
	if (Grabbed) Grabbed->Hide(scm_to_double(s_b));
	return SCM_UNSPECIFIED;
}

SCM FluxusRenderstateBinding::selectable(SCM s_b)
{
    SCM_ASSERT(scm_is_number(s_b), s_b, SCM_ARG1, "selectable");
	Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();    
	if (Grabbed) Grabbed->Selectable(scm_to_double(s_b));
	return SCM_UNSPECIFIED;
}

SCM FluxusRenderstateBinding::shader(SCM s_vert, SCM s_frag)
{
	SCM_ASSERT(scm_is_string(s_vert), s_vert, SCM_ARG1, "shader");
	SCM_ASSERT(scm_is_string(s_frag), s_frag, SCM_ARG2, "shader");
	
	char *vert=scm_to_locale_string(s_vert);
	char *frag=scm_to_locale_string(s_frag);
	
 	GLSLShader *shader = new GLSLShader(vert,frag);
	
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) 
	{
		// remove the old one
		if (Grabbed->GetState()->Shader)
		{
			delete Grabbed->GetState()->Shader;
		}
		
		Grabbed->GetState()->Shader=shader;
	}
    else 
	{
		// remove the old one
		if (Fluxus->GetRenderer()->GetState()->Shader)
		{
			delete Fluxus->GetRenderer()->GetState()->Shader;
		}

		Fluxus->GetRenderer()->GetState()->Shader=shader;
	}
	
	free(vert);
	free(frag);

	return SCM_UNSPECIFIED;
}

SCM FluxusRenderstateBinding::shader_set(SCM s_params)
{	
 	GLSLShader *shader;
	
	Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) shader=Grabbed->GetState()->Shader;
    else shader=Fluxus->GetRenderer()->GetState()->Shader;

	if (shader)
	{
		// vectors seem easier to handle than lists with this api
		SCM paramvec = scm_vector(s_params);

		// apply to set parameters
		shader->Apply();

		for (unsigned int n=0; n<scm_c_generalized_vector_length(paramvec); n+=2)
		{
			SCM arg=scm_vector_ref(paramvec, scm_from_int(n));

			if (scm_is_string(arg))
			{
				// get the parameter name
				char *param = scm_to_locale_string(arg);

				// get the value
				SCM arg=scm_vector_ref(paramvec, scm_from_int(n+1));

				if (scm_is_number(arg))
				{
					if (scm_is_integer(arg)) shader->SetInt(param,scm_to_int(arg));
					else shader->SetFloat(param,(float)scm_to_double(arg));
				}
				else if (scm_is_vector(arg))
				{
					if (scm_c_generalized_vector_length(arg) == 3)
					{
						dVector vec;
						flx_floats_from_scm(arg,vec.arr());
						shader->SetVector(param,vec);
					}
					else if (scm_c_generalized_vector_length(arg) == 4)
					{
						dColour vec;
						flx_floats_from_scm(arg,vec.arr());
						shader->SetColour(param,vec);
					}
					else
					{	
						cerr<<"shader has found an argument vector of a strange size"<<endl;
					}
				}
				else
				{
					cerr<<"shader has found an argument type it can't send, numbers and vectors only"<<endl;
				}

				free(param);
			}
			else
			{
				cerr<<"shader has found a mal-formed parameter list"<<endl;
			}
		}
		GLSLShader::Unapply();
	}   
	return SCM_UNSPECIFIED;
}
