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
#include "SchemeHelper.h"
#include "Engine.h"
#include "GlobalStateFunctions.h"
#include "Renderer.h"
 
using namespace GlobalStateFunctions;
using namespace SchemeHelper;
using namespace fluxus;

// StartSectionDoc-en
// GlobalState
// Global state is really anything that controls the renderer globally, so it affects all primitives
// or controls the renderer directly - ie camera control or full screen effects like blurring.  
// Example:
// EndSectionDoc 

// StartFunctionDoc-en
// clear-engine 
// Returns: void
// Description:
// Clears the renderer, and physics system. This command should not be called directly, use clear 
// instead, as this clears a few other things, and calls clear-engine itself.
// Example:
// (clear-engine) ; woo hoo!
// EndFunctionDoc

Scheme_Object *clear_engine(int argc, Scheme_Object **argv)
{
	Engine::Get()->Renderer()->Clear();
	Engine::Get()->Physics()->Clear();
	Engine::Get()->Renderer()->ClearLights();
	Engine::Get()->ClearGrabStack();
	Engine::Get()->Renderer()->UnGrab();
	return scheme_void;
}

// StartFunctionDoc-en
// blur amount-number
// Returns: void
// Description:
// Sets the full screen blur setting. Less is more, but if you set it too low it will make the
// on screen editing impossible to read, so save your script first :)
// Example:
// (blur 0.1) ; for nice trails
// EndFunctionDoc

Scheme_Object *blur(int argc, Scheme_Object **argv)
{
	ArgCheck("blur", "f", argc, argv);
	float blur=FloatFromScheme(argv[0]);	
	if (!blur) Engine::Get()->Renderer()->SetMotionBlur(false);
    else Engine::Get()->Renderer()->SetMotionBlur(true, blur);
    return scheme_void;
}

// StartFunctionDoc-en
// fog fogcolour-vector amount-number begin-number end-number
// Returns: void
// Description:
// Sets the fogging parameters to give a visual depth cue (aerial perspective in painter's jargon). 
// This can obscure the on screen editing, so keep the amount small.
// Example:
// (clear-colour (vector 0 0 1))   ; looks nice if the background matches
// (fog (vector 0 0 1) 0.01 1 100) ; blue fog
// EndFunctionDoc

Scheme_Object *fog(int argc, Scheme_Object **argv)
{
	ArgCheck("fog", "vfff", argc, argv);
	Engine::Get()->Renderer()->SetFog(ColourFromScheme(argv[0]),
		FloatFromScheme(argv[1]),
		FloatFromScheme(argv[2]),
		FloatFromScheme(argv[3]));
    return scheme_void;
}

// StartFunctionDoc-en
// feedback amount-number
// Returns: void
// Description:
// Full screen feedback for Jeff Minter style crazyness (renders the last frame in the background, 
// including the previous feedback background...). This allocates large amounts of texture
// space and seems to be unstable, so it's probably better not to use it. If you do, use with 
// feedback-transform, but don't say I didn't warn you.
// Example:
// (feedback 0.1) ; set the feedback amount
// (build-cube)
// (define (animate)
//     (feedback-transform (mrotate (vector 1 1 (* 45 (sin (time))))))) ; change the transform
// (every-frame (animate))
// EndFunctionDoc

Scheme_Object *feedback(int argc, Scheme_Object **argv)
{
 	ArgCheck("feedback", "f", argc, argv);
	Engine::Get()->Renderer()->SetFeedBack(FloatFromScheme(argv[0]));
    return scheme_void;
}

// StartFunctionDoc-en
// feedback-transform matrix-vector
// Returns: void
// Description:
// Sets the transform for the feedback plane. See feedback for more details, probably shouldn't be  
// used.
// Example:
// (feedback 0.1) ; set the feedback amount
// (build-cube)
// (define (animate)
//     (feedback-transform (mrotate (vector 1 1 (* 45 (sin (time))))))) ; change the transform
// (every-frame (animate))
// EndFunctionDoc

Scheme_Object *feedback_transform(int argc, Scheme_Object **argv)
{
 	ArgCheck("feedback-transform", "m", argc, argv);
	Engine::Get()->Renderer()->SetFeedBackMat(MatrixFromScheme(argv[0]));
	return scheme_void;	
}

// StartFunctionDoc-en
// show-axis show-number
// Returns: void
// Description:
// Shows the worldspace origin axis. 
// used.
// Example:
// (show-axis 1)
// EndFunctionDoc

Scheme_Object *show_axis(int argc, Scheme_Object **argv)
{
 	ArgCheck("show-axis", "i", argc, argv);
    Engine::Get()->Renderer()->ShowAxis(IntFromScheme(argv[0]));
    //Fluxus->ShowLocators(IntFromScheme(argv[0]));
    return scheme_void;
}

// StartFunctionDoc-en
// show-fps show-number
// Returns: void
// Description:
// Shows an fps count in the lower left of the screen.
// used.
// Example:
// (show-fps 1)
// EndFunctionDoc

Scheme_Object *show_fps(int argc, Scheme_Object **argv)
{
 	ArgCheck("show-fps", "i", argc, argv);
    Engine::Get()->Renderer()->SetFPSDisplay(IntFromScheme(argv[0]));
    return scheme_void;
}

// StartFunctionDoc-en
// lock-camera primitiveid-number
// Returns: void
// Description:
// Locks the camera transform onto the specified primitive's transform. It's like parenting the camera
// to the object. This is the easiest way to procedurally drive the camera. Use an id number of 0 to 
// unlock the camera.
// Example:
// (clear)
// (define obj (build-cube)) ; make a cube for the camera to lock to
// 
// (push) ; make a background cube so we can tell what's happening
// (hint-wire)  
// (hint-unlit) 
// (colour (vector 0 0.4 0))
// (scale (vector -50 -50 -50))
// (build-cube)
// (pop)
// 
// (lock-camera obj) ; lock the camera to our first cube
// 
// (define (animate)
//     (grab obj)
//     (rotate (vector 1 0 0)) ; rotate the cube
//     (ungrab))
// 
// (every-frame (animate))
// EndFunctionDoc

Scheme_Object *lock_camera(int argc, Scheme_Object **argv)
{
 	ArgCheck("lock-camera", "i", argc, argv);
    Engine::Get()->Renderer()->LockCamera(IntFromScheme(argv[0]));
    return scheme_void;
}

// StartFunctionDoc-en
// camera-lag amount-number
// Returns: void
// Description:
// The camera locking has an inbuilt lagging which means it will smoothly blend the movement relative
// to the primitive it's locked to.
// Example:
// (clear)
// (define obj (build-cube)) ; make a cube for the camera to lock to
// 
// (push) ; make a background cube so we can tell what's happening
// (hint-wire)
// (hint-unlit)
// (colour (vector 0 0.4 0))
// (scale (vector -50 -50 -50))
// (build-cube)
// (pop)
// 
// (lock-camera obj) ; lock the camera to our first cube
// (camera-lag 0.1)  ; set the lag amount, this will smooth out the cube jittery movement
// 
// (define (animate)
//     (grab obj)
//     (identity)
//     (translate (vector (modulo (round (inexact->exact (time))) 6) 0 0)) ; make a jittery movement
//     (ungrab))
// 
// (every-frame (animate))
// EndFunctionDoc

Scheme_Object *camera_lag(int argc, Scheme_Object **argv)
{
 	ArgCheck("camera-lag", "f", argc, argv);
    Engine::Get()->Renderer()->SetCameraLag(FloatFromScheme(argv[0]));
    return scheme_void;
}

// StartFunctionDoc-en
// load-texture pngfilename-string
// Returns: textureid-number
// Description:
// Loads a texture from disk, converts it to a texture, and returns the id number. The texture loading
// is memory cached, so repeatedly calling this will not cause it to load again. Use force-load-texture
// if you are changing the texture while running the script. The png may be RGB or RGBA to use alpha 
// transparency.
// Example:
// (texture (load-texture "mytexture.png"))
// (build-cube) ; the cube will be texture mapped with the image
// EndFunctionDoc

Scheme_Object *load_texture(int argc, Scheme_Object **argv)
{
 	ArgCheck("load-texture", "s", argc, argv);		
    return scheme_make_integer_value(Engine::Get()->Renderer()->LoadTexture(StringFromScheme(argv[0])));
}

// StartFunctionDoc-en
// load-texture pngfilename-string
// Returns: textureid-number
// Description:
// Uncached loading of textures from disk, converts it to a texture, and returns the id number. 
// Useful if you are changing the texture while running the script, otherwise use load-texture, which
// will be much faster. The png may be RGB or RGBA to use alpha transparency.
// Example:
// (texture (force-load-texture "mytexture.png"))
// (build-cube) ; the cube will be texture mapped with the image
// EndFunctionDoc

Scheme_Object *force_load_texture(int argc, Scheme_Object **argv)
{
 	ArgCheck("force-load-texture", "s", argc, argv);
    return scheme_make_integer_value(Engine::Get()->Renderer()->LoadTexture(StringFromScheme(argv[0]),true));
}

// StartFunctionDoc-en
// frustum top-number bottom-number left-number right-number
// Returns: void
// Description:
// Sets the camera frustum, and thus the aspect ratio of the frame. 
// Example:
// (frustum -1 1 -0.75 0.75) ; default settings
// EndFunctionDoc

Scheme_Object *frustum(int argc, Scheme_Object **argv)
{
 	ArgCheck("frustum", "ffff", argc, argv);
	Engine::Get()->Renderer()->SetFrustum(FloatFromScheme(argv[0]),
												FloatFromScheme(argv[1]),
									 		 	FloatFromScheme(argv[2]),
												FloatFromScheme(argv[3]));
    return scheme_void;
}

// StartFunctionDoc-en
// clip front-number back-number
// Returns: void
// Description:
// Sets the front & back clipping planes for the camera frustum, and thus the viewing angle. 
// Change the front clipping distance to alter the perspective from telephoto to fisheye.
// Example:
// (clip 1 10000) ; default settings
// EndFunctionDoc

Scheme_Object *clip(int argc, Scheme_Object **argv)
{
	ArgCheck("clip", "ff", argc, argv);
	Engine::Get()->Renderer()->SetClip(FloatFromScheme(argv[0]),
											 FloatFromScheme(argv[1]));
    return scheme_void;
}

// StartFunctionDoc-en
// ortho
// Returns: void
// Description:
// Sets orthographic projection - i.e. no perspective.
// Example:
// (ortho) 
// EndFunctionDoc

Scheme_Object *ortho(int argc, Scheme_Object **argv)
{
	Engine::Get()->Renderer()->SetOrtho(true);
    return scheme_void;
}

// StartFunctionDoc-en
// persp
// Returns: void
// Description:
// Sets perspective projection (the default) after ortho has been set.
// Example:
// (persp) 
// EndFunctionDoc

Scheme_Object *persp(int argc, Scheme_Object **argv)
{
	Engine::Get()->Renderer()->SetOrtho(false);
    return scheme_void;
}

// StartFunctionDoc-en
// set-ortho-zoom amount-number
// Returns: void
// Description:
// Sets the zoom level for the orthographic projection. 
// Example:
// (set-ortho-zoom 2) 
// EndFunctionDoc

Scheme_Object *set_ortho_zoom(int argc, Scheme_Object **argv)
{
	ArgCheck("set-ortho-zoom", "f", argc, argv);
	Engine::Get()->Renderer()->SetOrthoZoom(FloatFromScheme(argv[0]));
    return scheme_void;
}

// StartFunctionDoc-en
// backfacecull setting-number
// Returns: void
// Description:
// Turns backface culling on or off. Backface culling speeds up rendering by removing faces not 
// orientated towards the camera. Defaults to on, but this is not always desired, eg for double 
// sided polygons.
// Example:
// (backfacecull 0) 
// EndFunctionDoc

Scheme_Object *backfacecull(int argc, Scheme_Object **argv)
{
	ArgCheck("backfacecull", "i", argc, argv);
	Engine::Get()->Renderer()->SetBackFaceCull(IntFromScheme(argv[0]));
	return scheme_void;
}

// StartFunctionDoc-en
// clear-colour colour-vector
// Returns: void
// Description:
// Sets the colour we clear the renderer with, this forms the background colour for the scene.
// Example:
// (clear-colour (vector 1 0 0)) ; RED!!! 
// EndFunctionDoc

Scheme_Object *clear_colour(int argc, Scheme_Object **argv)
{
 	ArgCheck("clear-colour", "v", argc, argv);
    Engine::Get()->Renderer()->SetBGColour(ColourFromScheme(argv[0]));
    return scheme_void;
}

// StartFunctionDoc-en
// clear-frame setting-number
// Returns: void
// Description:
// Sets the frame and zbuffer clearing on or off. 
// Example:
// (clear-frame 0) 
// EndFunctionDoc

Scheme_Object *clear_frame(int argc, Scheme_Object **argv)
{
 	ArgCheck("clear-frame", "i", argc, argv);
	Engine::Get()->Renderer()->SetClearFrame(IntFromScheme(argv[0]));
    return scheme_void;
}

// StartFunctionDoc-en
// get-camera-transform 
// Returns: matrix-vector
// Description:
// Gets the current camera transform matrix.
// Example:
// (get-camera-transform) 
// EndFunctionDoc

Scheme_Object *get_camera_transform(int argc, Scheme_Object **argv)
{
	return FloatsToScheme(Engine::Get()->Renderer()->GetCamera()->inverse().arr(),16);
}

// StartFunctionDoc-en
// set-camera
// Returns: void
// Description:
// Sets the camera transform matrix. This is the low level interface used by set-camera-transform, 
// which you should generally use instead.
// Example:
// (set-camera) 
// EndFunctionDoc

Scheme_Object *set_camera(int argc, Scheme_Object **argv)
{
	ArgCheck("set-camera", "m", argc, argv);
	dMatrix m;
	FloatsFromScheme(argv[0],m.arr(),16);
	(*Engine::Get()->Renderer()->GetCamera())=m;
	return scheme_void;
}

// StartFunctionDoc-en
// get-projection-transfrom
// Returns: projection-matrix
// Description:
// Gets the current projection matrix.
// Example:
// (get-projection-transfrom) 
// EndFunctionDoc

Scheme_Object *get_projection_transform(int argc, Scheme_Object **argv)
{
	return FloatsToScheme(Engine::Get()->Renderer()->GetProjection().arr(),16);
}

// StartFunctionDoc-en
// get-screen-size
// Returns: size-vector
// Description:
// Returns a vector containing the current width and height of the window.
// Example:
// (get-screen-size) 
// EndFunctionDoc

Scheme_Object *get_screen_size(int argc, Scheme_Object **argv)
{
	float res[2];
	int x=0,y=0;
	Engine::Get()->Renderer()->GetResolution(x,y);
	res[0]=x; res[1]=y;
	return FloatsToScheme(res,2);
}

// StartFunctionDoc-en
// set-screen-size size-vector
// Returns: void
// Description:
// Sets the window width and height.
// Example:
// (set-screen-size (vector 10 10)) ; small window time :) 
// EndFunctionDoc

Scheme_Object *set_screen_size(int argc, Scheme_Object **argv)
{
	if (!SCHEME_VECTORP(argv[0])) scheme_wrong_type("set-screen-size", "vector", 0, argc, argv);
	if (SCHEME_VEC_SIZE(argv[0])!=2) scheme_wrong_type("set-screen-size", "vector size 2", 0, argc, argv);
	float v[2];
	FloatsFromScheme(argv[0],v,2);
	// hmmm, seems a bit wrong, but hey...
	glutReshapeWindow((int)v[0],(int)v[1]);
	return scheme_void;
}

// StartFunctionDoc-en
// select screenxpos-number screenypos-number pixelssize-number
// Returns: primitiveid-number
// Description:
// Looks in the region specified and returns the id of the closest primitive to the camera rendered 
// there, or 0 if none exist.  
// Example:
// (display (select 10 10 2))(newline)
// EndFunctionDoc

Scheme_Object *select(int argc, Scheme_Object **argv)
{
	ArgCheck("select", "iii", argc, argv);
	return scheme_make_integer_value(Engine::Get()->Renderer()->Select(IntFromScheme(argv[0]), 
																	   IntFromScheme(argv[1]),
																	   IntFromScheme(argv[2])));
}

// StartFunctionDoc-en
// desiredfps fps-number
// Returns: void
// Description:
// Throttles the renderer so as to not take 100% cpu. This gives an upper limit on the fps rate, which
// doesn't quite match the given number, but I'm working on it...  
// Example:
// (desiredfps 100000) ; makes fluxus render as fast as it can, and take 100% cpu.
// EndFunctionDoc 

Scheme_Object *desiredfps(int argc, Scheme_Object **argv)
{
	ArgCheck("desiredfps", "f", argc, argv);
	Engine::Get()->Renderer()->SetDesiredFPS(scheme_real_to_double(argv[0]));
	return scheme_void;
}

void GlobalStateFunctions::AddGlobals(Scheme_Env *env)
{	
	scheme_add_global("clear-engine", scheme_make_prim_w_arity(clear_engine, "clear-engine", 0, 0), env);
	scheme_add_global("blur", scheme_make_prim_w_arity(blur, "blur", 1, 1), env);
	scheme_add_global("fog", scheme_make_prim_w_arity(fog, "fog", 4, 4), env);
	scheme_add_global("feedback", scheme_make_prim_w_arity(feedback, "feedback", 1, 1), env);
	scheme_add_global("feedback-transform", scheme_make_prim_w_arity(feedback_transform, "feedback-transform", 1, 1), env);
	scheme_add_global("show-axis", scheme_make_prim_w_arity(show_axis, "show-axis", 1, 1), env);
	scheme_add_global("show-fps", scheme_make_prim_w_arity(show_fps, "show-fps", 1, 1), env);
	scheme_add_global("lock-camera", scheme_make_prim_w_arity(lock_camera, "lock-camera", 1, 1), env);
	scheme_add_global("camera-lag", scheme_make_prim_w_arity(camera_lag, "camera-lag", 1, 1), env);
	scheme_add_global("load-texture", scheme_make_prim_w_arity(load_texture, "load-texture", 1, 1), env);
	scheme_add_global("force-load-texture", scheme_make_prim_w_arity(force_load_texture, "force-load-texture", 1, 1), env);
	scheme_add_global("frustum", scheme_make_prim_w_arity(frustum, "frustum", 0, 0), env);
	scheme_add_global("clip", scheme_make_prim_w_arity(clip, "clip", 2, 2), env);
	scheme_add_global("ortho", scheme_make_prim_w_arity(ortho, "ortho", 0, 0), env);
	scheme_add_global("persp", scheme_make_prim_w_arity(persp, "persp", 0, 0), env);
	scheme_add_global("set-ortho-zoom", scheme_make_prim_w_arity(set_ortho_zoom, "set-ortho-zoom", 1, 1), env);
	scheme_add_global("backfacecull", scheme_make_prim_w_arity(backfacecull, "backfacecull", 1, 1), env);
	scheme_add_global("clear-colour", scheme_make_prim_w_arity(clear_colour, "clear-colour", 1, 1), env);
	scheme_add_global("clear-frame", scheme_make_prim_w_arity(clear_frame, "clear-frame", 1, 1), env);
	//scheme_add_global("get-camera-transform", scheme_make_prim_w_arity(get_camera_transform, "get-camera-transform", 0, 0), env);
	scheme_add_global("set-camera", scheme_make_prim_w_arity(set_camera, "set-camera", 1, 1), env);
	scheme_add_global("get-projection-transform", scheme_make_prim_w_arity(get_projection_transform, "get-projection-transform", 0, 0), env);
	scheme_add_global("get-screen-size", scheme_make_prim_w_arity(get_screen_size, "get-screen-size", 0, 0), env);
	scheme_add_global("set-screen-size", scheme_make_prim_w_arity(set_screen_size, "set-screen-size", 1, 1), env);
	scheme_add_global("select", scheme_make_prim_w_arity(select, "select", 3, 3), env);
	scheme_add_global("desiredfps", scheme_make_prim_w_arity(desiredfps, "desiredfps", 1, 1), env);
}
