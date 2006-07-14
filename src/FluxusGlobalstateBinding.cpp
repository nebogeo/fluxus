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

#include "FluxusPrimitiveBinding.h"
#include <fstream>
#include <deque>
#include <libguile.h>
#include "FluxusGlobalstateBinding.h"
#include "SearchPaths.h"
#include "Repl.h"
#include <LinePrimitive.h>
#include <ParticlePrimitive.h>
#include <PixelPrimitive.h>
#include <LocatorPrimitive.h>

void FluxusGlobalstateBinding::RegisterProcs()
{		
	scm_c_define_gsubr("clear",0,0,0,(CALLBACK_CAST)		   clear);
	scm_c_define_gsubr("ortho",0,0,0,(CALLBACK_CAST)		 ortho);
	scm_c_define_gsubr("persp",0,0,0,(CALLBACK_CAST)		 persp);
	scm_c_define_gsubr("frustum",4,0,0,(CALLBACK_CAST)  	frustum);
	scm_c_define_gsubr("clip",2,0,0,(CALLBACK_CAST) 		clip);
    scm_c_define_gsubr("reset-camera",0,0,0,(CALLBACK_CAST)    reset_camera);
	scm_c_define_gsubr("lock-camera",1,0,0,(CALLBACK_CAST)  lock_camera);
	scm_c_define_gsubr("camera-lag",1,0,0,(CALLBACK_CAST)  camera_lag);
	scm_c_define_gsubr("clear-colour",1,0,0,(CALLBACK_CAST)    clear_colour);	 
	scm_c_define_gsubr("clear-frame",1,0,0,(CALLBACK_CAST)     clear_frame);
	scm_c_define_gsubr("blur",1 ,0,0,(CALLBACK_CAST)		blur);
	scm_c_define_gsubr("feedback",1,0,0,(CALLBACK_CAST) 	feedback);
	scm_c_define_gsubr("feedback-transform",1,0,0,(CALLBACK_CAST) feedback_transform);
    scm_c_define_gsubr("show-axis",1,0,0,(CALLBACK_CAST)	show_axis);
    scm_c_define_gsubr("show-fps", 1,0,0,(CALLBACK_CAST)	show_fps);
	scm_c_define_gsubr("backfacecull",1,0,0,(CALLBACK_CAST)    backfacecull);
	scm_c_define_gsubr("load-texture",1,0,0,(CALLBACK_CAST) load_texture);
	scm_c_define_gsubr("force-load-texture",1,0,0,(CALLBACK_CAST) force_load_texture);
	scm_c_define_gsubr("fog",4,0,0,(CALLBACK_CAST) fog);
	scm_c_define_gsubr("get-transform",0,0,0,(CALLBACK_CAST) get_transform);
	scm_c_define_gsubr("get-camera-transform",0,0,0,(CALLBACK_CAST)get_camera_transform);
	scm_c_define_gsubr("set-camera-transform",1,0,0,(CALLBACK_CAST)set_camera_transform);
	scm_c_define_gsubr("get-projection-transform",0,0,0,(CALLBACK_CAST) get_projection_transform);
	scm_c_define_gsubr("get-screen-size",0,0,0,(CALLBACK_CAST) get_screen_size);
	scm_c_define_gsubr("set-screen-size",1,0,0,(CALLBACK_CAST) set_screen_size);
}

SCM FluxusGlobalstateBinding::clear()
{
	Fluxus->GetRenderer()->Clear();
	Fluxus->GetPhysics()->Clear();
	Fluxus->GetRenderer()->ClearLights();

	if (FrameHook)
		scm_reset_hook_x(FrameHook);
	else {
		fprintf(stderr, "FrameHook == NULL during (clear)!\n");
	}
	
	GrabbedIDStack.clear();
	GrabbedID=-1;
	Fluxus->GetRenderer()->UnGrab();
	
	return SCM_UNSPECIFIED;
}

SCM FluxusGlobalstateBinding::reset_camera()
{
	Fluxus->ResetCamera();
	return SCM_UNSPECIFIED;
}

SCM FluxusGlobalstateBinding::blur(SCM s_blur)
{
    SCM_ASSERT(scm_is_number(s_blur), s_blur, SCM_ARG1, "blur");
	double blur;
	blur=scm_to_double(s_blur);	
	if (!blur) Fluxus->GetRenderer()->SetMotionBlur(false);
    else Fluxus->GetRenderer()->SetMotionBlur(true, (float)blur);
    return SCM_UNSPECIFIED;
}

SCM FluxusGlobalstateBinding::fog(SCM s_col, SCM s_d, SCM s_s, SCM s_e)
{
    SCM_ASSERT(scm_is_generalized_vector(s_col), s_col, SCM_ARG1, "fog");
	SCM_ASSERT(scm_c_generalized_vector_length(s_col)==3, s_col, SCM_ARG1, "fog");
    SCM_ASSERT(scm_is_number(s_d), s_d, SCM_ARG2, "fog");
    SCM_ASSERT(scm_is_number(s_s), s_s, SCM_ARG3, "fog");
    SCM_ASSERT(scm_is_number(s_e), s_e, SCM_ARG4, "fog");
	dColour c;
	flx_floats_from_scm(s_col,c.arr());
	Fluxus->GetRenderer()->SetFog(c,scm_to_double(s_d),scm_to_double(s_s),scm_to_double(s_e));
    return SCM_UNSPECIFIED;
}

SCM FluxusGlobalstateBinding::feedback(SCM s_fb)
{
    SCM_ASSERT(scm_is_number(s_fb), s_fb, SCM_ARG1, "feedback");
	double fb;
	fb=scm_to_double(s_fb);	
	Fluxus->GetRenderer()->SetFeedBack(fb);
    return SCM_UNSPECIFIED;
}

SCM FluxusGlobalstateBinding::feedback_transform(SCM s_a)
{
	SCM_ASSERT(scm_is_generalized_vector(s_a),  s_a,  SCM_ARG1, "feedback_transform");
	SCM_ASSERT(scm_c_generalized_vector_length(s_a)==16,  s_a,  SCM_ARG1, "feedback_transform");
	dMatrix m;
	flx_floats_from_scm(s_a,m.arr());
	Fluxus->GetRenderer()->SetFeedBackMat(m);
	return SCM_UNSPECIFIED;	
}

SCM FluxusGlobalstateBinding::show_axis(SCM s_id)
{
	SCM_ASSERT(scm_is_number(s_id), s_id, SCM_ARG1, "show_axis");	
    Fluxus->GetRenderer()->ShowAxis(scm_to_double(s_id));
    Fluxus->ShowLocators(scm_to_double(s_id));
    return SCM_UNSPECIFIED;
}

SCM FluxusGlobalstateBinding::show_fps(SCM s_id)
{
	SCM_ASSERT(scm_is_number(s_id), s_id, SCM_ARG1, "show_fps");	
    Fluxus->GetRenderer()->SetFPSDisplay(scm_to_double(s_id));
    return SCM_UNSPECIFIED;
}

SCM FluxusGlobalstateBinding::lock_camera(SCM s_ob)
{
	int ob=Smob2Prim(s_ob);	
    Fluxus->GetRenderer()->LockCamera(ob);
	scm_remember_upto_here_1(s_ob);
    return SCM_UNSPECIFIED;
}

SCM FluxusGlobalstateBinding::camera_lag(SCM s_amount)
{
	SCM_ASSERT(scm_is_number(s_amount), s_amount, SCM_ARG1, "camera-lag");	
    Fluxus->GetRenderer()->SetCameraLag(scm_to_double(s_amount));
    return SCM_UNSPECIFIED;
}

SCM FluxusGlobalstateBinding::load_texture(SCM s_name)
{
	SCM_ASSERT(scm_is_string(s_name), s_name, SCM_ARG1, "load-texture");
	char *name=scm_to_locale_string(s_name);	
	int id=Fluxus->GetRenderer()->LoadTexture(name);
    free(name);
    return scm_from_int(id);
}

SCM FluxusGlobalstateBinding::force_load_texture(SCM s_name)
{
	SCM_ASSERT(scm_is_string(s_name), s_name, SCM_ARG1, "force-load-texture");
	char *name=scm_to_locale_string(s_name);	
	
	int id=Fluxus->GetRenderer()->LoadTexture(name,true);

    free(name);
    return scm_from_int(id);
}

SCM FluxusGlobalstateBinding::frustum(SCM s_u, SCM s_d, SCM s_l, SCM s_r)
{
	SCM_ASSERT(scm_is_number(s_u), s_u, SCM_ARG1, "frustum");
	SCM_ASSERT(scm_is_number(s_d), s_d, SCM_ARG2, "frustum");
	SCM_ASSERT(scm_is_number(s_l), s_l, SCM_ARG3, "frustum");
	SCM_ASSERT(scm_is_number(s_r), s_r, SCM_ARG4, "frustum");
	
	Fluxus->GetRenderer()->SetFrustum(scm_to_double(s_u),scm_to_double(s_d),
									  scm_to_double(s_l),scm_to_double(s_r));
    return SCM_UNSPECIFIED;
}

SCM FluxusGlobalstateBinding::clip(SCM s_f, SCM s_b)
{
	SCM_ASSERT(scm_is_number(s_f), s_f, SCM_ARG1, "clip");
	SCM_ASSERT(scm_is_number(s_b), s_b, SCM_ARG2, "clip");
	
	Fluxus->GetRenderer()->SetClip(scm_to_double(s_f),scm_to_double(s_b));
    return SCM_UNSPECIFIED;
}

SCM FluxusGlobalstateBinding::ortho()
{
	//Fluxus->ortho();
	Fluxus->GetRenderer()->SetOrtho(true);
    return SCM_UNSPECIFIED;
}

SCM FluxusGlobalstateBinding::persp()
{
	Fluxus->GetRenderer()->SetOrtho(false);
    return SCM_UNSPECIFIED;
}

SCM FluxusGlobalstateBinding::backfacecull(SCM s)
{
	SCM_ASSERT(scm_is_number(s), s, SCM_ARG1, "backfacecull");
	Fluxus->GetRenderer()->SetBackFaceCull(scm_to_double(s));
	return SCM_UNSPECIFIED;
}

SCM FluxusGlobalstateBinding::clear_colour(SCM s_vec)
{
    SCM_ASSERT(scm_is_generalized_vector(s_vec), s_vec, SCM_ARG1, "clear_colour");
	SCM_ASSERT(scm_c_generalized_vector_length(s_vec)==3, s_vec, SCM_ARG1, "clear_colour");
	float col[3];
	flx_floats_from_scm(s_vec,col);
    Fluxus->GetRenderer()->SetBGColour(dColour(col[0],col[1],col[2]));
    return SCM_UNSPECIFIED;
}

SCM FluxusGlobalstateBinding::clear_frame(SCM s_gain)
{
	SCM_ASSERT(scm_is_number(s_gain), s_gain, SCM_ARG1, "clear_frame");	
	Fluxus->GetRenderer()->SetClearFrame(scm_to_double(s_gain));
    return SCM_UNSPECIFIED;
}

SCM FluxusGlobalstateBinding::get_transform()
{
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) 
	{
		return flx_floats_to_scm(Fluxus->GetRenderer()->GetGlobalTransform(GrabbedID).arr(),16);
	}
	return flx_floats_to_scm(Fluxus->GetRenderer()->GetState()->Transform.arr(),16);
}

SCM FluxusGlobalstateBinding::get_camera_transform()
{
	return flx_floats_to_scm(Fluxus->GetRenderer()->GetCamera()->inverse().arr(),16);
}

SCM FluxusGlobalstateBinding::set_camera_transform(SCM s_m)
{
	SCM_ASSERT(scm_is_generalized_vector(s_m),  s_m,  SCM_ARG1, "set-camera-transform");
	SCM_ASSERT(scm_c_generalized_vector_length(s_m)==16,  s_m,  SCM_ARG1, "set-camera-transform");
	dMatrix m;
	flx_floats_from_scm(s_m,m.arr());
	(*Fluxus->GetRenderer()->GetCamera())=m.inverse();
	Fluxus->SetInteractiveCamera(false);
	return SCM_UNSPECIFIED;
}

SCM FluxusGlobalstateBinding::get_projection_transform()
{
	return flx_floats_to_scm(Fluxus->GetRenderer()->GetProjection().arr(),16);
}

SCM FluxusGlobalstateBinding::get_screen_size()
{
	float res[2];
	int x=0,y=0;
	Fluxus->GetRenderer()->GetResolution(x,y);
	res[0]=x; res[1]=y;
	return flx_floats_to_scm(res,2);
}

SCM FluxusGlobalstateBinding::set_screen_size(SCM s_size)
{
	SCM_ASSERT(scm_is_generalized_vector(s_size), s_size, SCM_ARG1, "set-screen-size");	
	SCM_ASSERT(scm_c_generalized_vector_length(s_size)==2, s_size, SCM_ARG3, "set-screen-size");
	float v[2];
	flx_floats_from_scm(s_size,v);
	// hmmm, seems a bit wrong, but hey...
	glutReshapeWindow((int)v[0],(int)v[1]);
	return SCM_UNSPECIFIED;
}
