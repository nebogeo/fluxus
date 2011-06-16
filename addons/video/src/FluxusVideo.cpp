// Copyright (C) 2009 Gabor Papp
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

#include <escheme.h>
#include <iostream>
#include <string>
#include <map>

#include "Video.h"

using namespace std;

// StartSectionDoc-en
// video
// The video module provides functions to load in a movie file via Quicktime
// in OSX or GStreamer in Linux, and offers various controls to play or
// control the properties of the movie.
// The module also provides access to live cameras.
// Example:
// (clear)
// (video-clear-cache)
// (define vt (video-load "/path/to/movie"))
// (video-play vt)
// (let ([p (build-plane)]
//      [tcoords (video-tcoords vt)])
//    (with-primitive p
//        (texture vt)
//        (pdata-index-map!
//            (lambda (i t)
//                (list-ref tcoords (remainder i 4)))
//            "t")))
// (every-frame (video-update vt))
// EndSectionDoc

static map<int, Video *> Videos; // texture handle cache
static map<string, Video *> VideoFilenames; // video filename cache

static map<int, Camera *> Cameras;
static map<unsigned, Camera *> CameraDevices; // device id cache

// StartFunctionDoc-en
// video-clear-cache
// Returns: void
// Description:
// The video loading is memory cached, so repeatedly calling (video-load)
// will not cause the file to be loaded again. (video-clear-cache) clears
// the video cache, meaning the videos will be reloaded from disk.
// Example:
// (video-clear-cache)
// EndFunctionDoc

Scheme_Object *video_clear_cache(int argc, Scheme_Object **argv)
{
	for (map<int, Video *>::iterator i = Videos.begin(); i != Videos.end(); ++i)
	{
		delete i->second;
	}
	Videos.clear();
	VideoFilenames.clear();

	return scheme_void;
}

static Video *find_video(string func, Scheme_Object *sid)
{
	unsigned id = (unsigned)scheme_real_to_double(sid);
	map<int, Video *>::iterator i = Videos.find(id);
	if (i != Videos.end())
		return i->second;
	else
	{
		cerr << func << ": video " << id << " not found." << endl;
		return NULL;
	}
}

// StartFunctionDoc-en
// video-load filename-string
// Returns: videoid-number
// Description:
// Loads a movie file. The video loading is memory cached, so repeatedly
// calling this will not cause the file to be loaded again. The cache can
// be cleared with (video-clear-cache). Returns a video-id that can be
// used directly in (texture) calls, and in other video functions.
// Example:
// (define movie (video-load "/path/to/movie"))
// EndFunctionDoc

Scheme_Object *video_load(int argc, Scheme_Object **argv)
{
	char *filename = NULL;
	MZ_GC_DECL_REG(2);
	MZ_GC_VAR_IN_REG(0, argv);
	MZ_GC_VAR_IN_REG(1, filename);
	MZ_GC_REG();

	if (!SCHEME_CHAR_STRINGP(argv[0]))
		scheme_wrong_type("video-load", "string", 0, argc, argv);

	filename = scheme_utf8_encode_to_buffer(SCHEME_CHAR_STR_VAL(argv[0]),
					SCHEME_CHAR_STRLEN_VAL(argv[0]), NULL, 0);

	Video *v;

	map<string, Video *>::iterator i = VideoFilenames.find(filename);
	if (i != VideoFilenames.end())
		v = i->second;
	else
	{
		v = new Video(filename);
		Videos[v->get_texture_id()] = v;
		VideoFilenames[filename] = v;
	}

	MZ_GC_UNREG();
	return scheme_make_integer_value(v->get_texture_id());
}

static Scheme_Object *scheme_vector(float v0, float v1, float v2)
{
	Scheme_Object *ret = NULL;
	Scheme_Object *tmp = NULL;
	MZ_GC_DECL_REG(2);
	MZ_GC_VAR_IN_REG(0, ret);
	MZ_GC_VAR_IN_REG(1, tmp);
	MZ_GC_REG();
	ret = scheme_make_vector(3, scheme_void);
	tmp = scheme_make_double(v0);
	SCHEME_VEC_ELS(ret)[0] = tmp;
	tmp = scheme_make_double(v1);
	SCHEME_VEC_ELS(ret)[1] = tmp;
	tmp = scheme_make_double(v2);
	SCHEME_VEC_ELS(ret)[2] = tmp;

	MZ_GC_UNREG();
	return ret;
}

// StartFunctionDoc-en
// video-tcoords videoid-number
// Returns: list-of-texture-coordinates
// Description:
// Returns the texture coordinates of the video texture. This is necessary,
// because video images are usually rectangular non-power-of-two textures, while
// fluxus uses GL_TEXTURE_2D power-of-two textures.
// Example:
// (define vt (video-load "/path/to/movie"))
// (video-play vt)
// (let ([p (build-plane)]
//      [tcoords (video-tcoords vt)])
//    (with-primitive p
//        (texture vt)
//        (pdata-index-map!
//            (lambda (i t)
//                (list-ref tcoords (remainder i 4)))
//            "t")))
// (every-frame (video-update vt))
// EndFunctionDoc

Scheme_Object *video_tcoords(int argc, Scheme_Object **argv)
{
	Scheme_Object *ret = NULL;
	Scheme_Object **coord_list = NULL;
	MZ_GC_DECL_REG(2);
	MZ_GC_VAR_IN_REG(0, argv);
	MZ_GC_VAR_IN_REG(1, coord_list);
	MZ_GC_REG();
	if (!SCHEME_NUMBERP(argv[0]))
		scheme_wrong_type("video-tcoords", "number", 0, argc, argv);

	Video *v = find_video("video-tcoords", argv[0]);
	if (v != NULL)
	{
		coord_list = (Scheme_Object **)scheme_malloc(4 *
				sizeof(Scheme_Object *));

		float *coords  = v->get_tcoords();

		coord_list[0] = scheme_vector(coords[0], coords[4], coords[2]);
		coord_list[1] = scheme_vector(coords[3], coords[4], coords[5]);
		coord_list[2] = scheme_vector(coords[3], coords[1], coords[5]);
		coord_list[3] = scheme_vector(coords[0], coords[1], coords[2]);

		ret = scheme_build_list(4, coord_list);
	}
	else
	{
		ret = scheme_void;
	}

	MZ_GC_UNREG();
	return ret;
}

// StartFunctionDoc-en
// video-update videoid-number
// Returns: void
// Description:
// Updates the movie player, so that the movie can play.
// Example:
// (define vt (video-load "/path/to/movie"))
// (video-play vt)
// (let ([p (build-plane)]
//      [tcoords (video-tcoords vt)])
//    (with-primitive p
//        (texture vt)
//        (pdata-index-map!
//            (lambda (i t)
//                (list-ref tcoords (remainder i 4)))
//            "t")))
// (every-frame (video-update vt))
// EndFunctionDoc

Scheme_Object *video_update(int argc, Scheme_Object **argv)
{
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, argv);
	MZ_GC_REG();
	if (!SCHEME_NUMBERP(argv[0]))
		scheme_wrong_type("video-update", "number", 0, argc, argv);

	Video *v = find_video("video-update", argv[0]);
	if (v != NULL)
		v->update();

	MZ_GC_UNREG();
    return scheme_void;
}

// StartFunctionDoc-en
// video-play videoid-number
// Returns: void
// Description:
// Plays the movie.
// Example:
// (define vt (video-load "/path/to/movie"))
// (video-play vt)
// EndFunctionDoc

Scheme_Object *video_play(int argc, Scheme_Object **argv)
{
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, argv);
	MZ_GC_REG();
	if (!SCHEME_NUMBERP(argv[0]))
		scheme_wrong_type("video-play", "number", 0, argc, argv);

	Video *v = find_video("video-play", argv[0]);
	if (v != NULL)
		v->play();

	MZ_GC_UNREG();
    return scheme_void;
}

// StartFunctionDoc-en
// video-stop videoid-number
// Returns: void
// Description:
// Stops the movie.
// Example:
// (define vt (video-load "/path/to/movie"))
// (video-play vt)
// EndFunctionDoc

Scheme_Object *video_stop(int argc, Scheme_Object **argv)
{
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, argv);
	MZ_GC_REG();
	if (!SCHEME_NUMBERP(argv[0]))
		scheme_wrong_type("video-stop", "number", 0, argc, argv);

	Video *v = find_video("video-stop", argv[0]);
	if (v != NULL)
		v->stop();

	MZ_GC_UNREG();
    return scheme_void;
}

// StartFunctionDoc-en
// video-seek videoid-number position-number
// Returns: void
// Description:
// Sets the position of the playhead to a given percentage through the
// movie. Position is between 0 and 1.
// Example:
// (define vt (video-load "/path/to/movie"))
// (video-play vt)
// (video-seek vt 0.5)
// EndFunctionDoc

Scheme_Object *video_seek(int argc, Scheme_Object **argv)
{
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, argv);
	MZ_GC_REG();
	if (!SCHEME_NUMBERP(argv[0]))
		scheme_wrong_type("video-seek", "number", 0, argc, argv);
	if (!SCHEME_NUMBERP(argv[1]))
		scheme_wrong_type("video-seek", "number", 1, argc, argv);

	Video *v = find_video("video-seek", argv[0]);
	float pos = scheme_real_to_double(argv[1]);
	if (v != NULL)
		v->seek(pos);

	MZ_GC_UNREG();
    return scheme_void;
}
// StartFunctionDoc-en
// video-width videoid-number
// Returns: width-number
// Description:
// Returns the width of the video zero if the video id is invalid.
// Example:
// (define vt (video-load "/path/to/movie"))
// (video-width vt)
// EndFunctionDoc

Scheme_Object *video_width(int argc, Scheme_Object **argv)
{
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, argv);
	MZ_GC_REG();
	if (!SCHEME_NUMBERP(argv[0]))
		scheme_wrong_type("video-width", "number", 0, argc, argv);

	Video *v = find_video("video-width", argv[0]);
	int w = 0;
	if (v != NULL)
		w = v->get_width();

	MZ_GC_UNREG();
    return scheme_make_integer_value(w);
}

// StartFunctionDoc-en
// video-height videoid-number
// Returns: height-number
// Description:
// Returns the height of the video or zero if the video id is invalid.
// Example:
// (define vt (video-load "/path/to/movie"))
// (video-height vt)
// EndFunctionDoc

Scheme_Object *video_height(int argc, Scheme_Object **argv)
{
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, argv);
	MZ_GC_REG();
	if (!SCHEME_NUMBERP(argv[0]))
		scheme_wrong_type("video-heigth", "number", 0, argc, argv);

	Video *v = find_video("video-height", argv[0]);
	int h = 0;
	if (v != NULL)
		h = v->get_height();

	MZ_GC_UNREG();
    return scheme_make_integer_value(h);
}

// StartFunctionDoc-en
// video-imgptr videoid-number
// Returns: cpointer:imgptr
// Description:
// Returns a tagged cpointer to the video pixel buffer to be passed to
// other modules. The data is stored as RGB in an array of width*height*3
// size.
// Example:
// (define vt (video-load "/path/to/movie"))
// (video-imgptr vt)
// EndFunctionDoc

Scheme_Object *video_imgptr(int argc, Scheme_Object **argv)
{
	Scheme_Object *ret = NULL;
	MZ_GC_DECL_REG(2);
	MZ_GC_VAR_IN_REG(0, argv);
	MZ_GC_VAR_IN_REG(1, ret);
	MZ_GC_REG();

	if (!SCHEME_NUMBERP(argv[0]))
		scheme_wrong_type("video-imgptr", "number", 0, argc, argv);

	Video *v = find_video("video-imgptr", argv[0]);
	if (v == NULL)
	{
		MZ_GC_UNREG();
		return scheme_void;
	}

	ret = scheme_make_cptr(v->get_pixels(), scheme_make_utf8_string("imgptr"));

	MZ_GC_UNREG();
    return ret;
}

static Camera *find_camera(string func, Scheme_Object *sid, int noerr = 0)
{
	unsigned id = (unsigned)scheme_real_to_double(sid);
	map<int, Camera *>::iterator i = Cameras.find(id);
	if (i != Cameras.end())
		return i->second;
	else
	{
		if (!noerr)
			cerr << func << ": camera " << id << " not found." << endl;
		return NULL;
	}
}

// StartFunctionDoc-en
// camera-list-devices
// Returns: void
// Description:
// Prints the available camera devices to the console.
// Example:
// (camera-list-devices)
// EndFunctionDoc

Scheme_Object *camera_list_devices(int argc, Scheme_Object **argv)
{
	ofVideoGrabber camera;
	camera.listDevices();

	return scheme_void;
}

// StartFunctionDoc-en
// camera-clear-cache
// Returns: void
// Description:
// The initialized cameras are stored in cache, so they are not getting
// reinited every time the script is recompiled. The function clears the
// camera cache.
// Example:
// (camera-clear-cache)
// EndFunctionDoc

Scheme_Object *camera_clear_cache(int argc, Scheme_Object **argv)
{
	for (map<int, Camera *>::iterator i = Cameras.begin(); i != Cameras.end(); ++i)
	{
		delete i->second;
	}
	Cameras.clear();
	CameraDevices.clear();

	return scheme_void;
}

// StartFunctionDoc-en
// camera-init device-id width height
// Returns: cameraid-number
// Description:
// Initializes the camera with the given device-id and resolution. Returns a
// camera-id that can be used directly in (texture) calls, and in other camera
// functions to reference the device. The camera-id is different from the
// device-id.
// Example:
// (define cam (camera-init 0 320 240))
// (printf "camera resolution: ~ax~a~n" (camera-width cam) (camera-height cam))
// (texture cam)
// (build-cube)
// EndFunctionDoc

Scheme_Object *camera_init(int argc, Scheme_Object **argv)
{
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, argv);
	MZ_GC_REG();
	for (int i = 0; i < 3; i++)
	{
		if (!SCHEME_NUMBERP(argv[i]))
			scheme_wrong_type("camera-init", "number", i, argc, argv);
	}
	unsigned id = (unsigned)scheme_real_to_double(argv[0]);
	int w = (int)scheme_real_to_double(argv[1]);
	int h = (int)scheme_real_to_double(argv[2]);

	Camera *g;

	map<unsigned, Camera *>::iterator i = CameraDevices.find(id);
	if (i != CameraDevices.end())
		g = i->second;
	else
	{
		g = new Camera(id, w, h);
		Cameras[g->get_texture_id()] = g;
		CameraDevices[id] = g;
	}

	MZ_GC_UNREG();
	return scheme_make_integer_value(g->get_texture_id());
}

// StartFunctionDoc-en
// camera-update cameraid-number
// Returns: void
// Description:
// This function should be called regularly to get new data from the camera.
// Example:
// (define cam (camera-init 0 320 240))
// (texture cam)
// (build-cube)
// (every-frame (camera-update cam))
// EndFunctionDoc

Scheme_Object *camera_update(int argc, Scheme_Object **argv)
{
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, argv);
	MZ_GC_REG();
	if (!SCHEME_NUMBERP(argv[0]))
		scheme_wrong_type("camera-update", "number", 0, argc, argv);

	Camera *g = find_camera("camera-update", argv[0]);
	if (g != NULL)
		g->update();

	MZ_GC_UNREG();
    return scheme_void;
}

// StartFunctionDoc-en
// camera-tcoords cameraid-number
// Returns: list-of-texture-coordinates
// Description:
// Returns the texture coordinates of the camera image. This is necessary,
// because camera images are rectangular non-power-of-two textures, while
// fluxus uses GL_TEXTURE_2D power-of-two textures.
//
// Example:
// (define cam (camera-init 0 320 240))
// (let ([p (build-cube)]
//       [tcoords (camera-tcoords cam)])
//    (with-primitive p
//        (texture cam)
//        (pdata-index-map!
//            (lambda (i t)
//                (list-ref tcoords (remainder i 4)))
//            "t")))
// (every-frame (camera-update cam))
// EndFunctionDoc

Scheme_Object *camera_tcoords(int argc, Scheme_Object **argv)
{
	Scheme_Object *ret = NULL;
	Scheme_Object **coord_list = NULL;
	MZ_GC_DECL_REG(2);
	MZ_GC_VAR_IN_REG(0, argv);
	MZ_GC_VAR_IN_REG(1, coord_list);
	MZ_GC_REG();
	if (!SCHEME_NUMBERP(argv[0]))
		scheme_wrong_type("camera-tcoords", "number", 0, argc, argv);

	Camera *g = find_camera("camera-tcoords", argv[0]);
	if (g != NULL)
	{
		coord_list = (Scheme_Object **)scheme_malloc(4 *
				sizeof(Scheme_Object *));

		float *coords  = g->get_tcoords();

		coord_list[0] = scheme_vector(coords[0], coords[4], coords[2]);
		coord_list[1] = scheme_vector(coords[3], coords[4], coords[5]);
		coord_list[2] = scheme_vector(coords[3], coords[1], coords[5]);
		coord_list[3] = scheme_vector(coords[0], coords[1], coords[2]);

		ret = scheme_build_list(4, coord_list);
	}
	else
	{
		ret = scheme_void;
	}

	MZ_GC_UNREG();
	return ret;
}

// StartFunctionDoc-en
// camera-width cameraid-number
// Returns: width-number
// Description:
// Returns the width of the camera image or zero if the camera id is invalid.
// Example:
// (define cam (camera-init 0 320 240))
// (camera-width cam)
// EndFunctionDoc

Scheme_Object *camera_width(int argc, Scheme_Object **argv)
{
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, argv);
	MZ_GC_REG();
	if (!SCHEME_NUMBERP(argv[0]))
		scheme_wrong_type("camera-width", "number", 0, argc, argv);

	Camera *g = find_camera("camera-width", argv[0]);
	int w = 0;
	if (g != NULL)
		w = g->get_width();

	MZ_GC_UNREG();
    return scheme_make_integer_value(w);
}

// StartFunctionDoc-en
// camera-height cameraid-number
// Returns: height-number
// Description:
// Returns the height of the camera image or zero if the camera id is invalid.
// Example:
// (define cam (camera-init 0 320 240))
// (camera-height cam)
// EndFunctionDoc

Scheme_Object *camera_height(int argc, Scheme_Object **argv)
{
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, argv);
	MZ_GC_REG();
	if (!SCHEME_NUMBERP(argv[0]))
		scheme_wrong_type("camera-heigth", "number", 0, argc, argv);

	Camera *g = find_camera("camera-height", argv[0]);
	int h = 0;
	if (g != NULL)
		h = g->get_height();

	MZ_GC_UNREG();
    return scheme_make_integer_value(h);
}

// StartFunctionDoc-en
// camera-imgptr cameraid-number
// Returns: cpointer:imgptr
// Description:
// Returns a tagged cpointer to the camera image pixel buffer to be passed to
// other modules. The data is stored as RGB in an array of width*height*3
// size.
// Example:
// (define cam (camera-init 0 320 240))
// (camera-imgptr cam)
// EndFunctionDoc

Scheme_Object *camera_imgptr(int argc, Scheme_Object **argv)
{
	Scheme_Object *ret = NULL;
	MZ_GC_DECL_REG(2);
	MZ_GC_VAR_IN_REG(0, argv);
	MZ_GC_VAR_IN_REG(1, ret);
	MZ_GC_REG();

	if (!SCHEME_NUMBERP(argv[0]))
		scheme_wrong_type("camera-imgptr", "number", 0, argc, argv);

	Camera *g = find_camera("camera-imgptr", argv[0]);
	if (g == NULL)
	{
		MZ_GC_UNREG();
		return scheme_void;
	}

	ret = scheme_make_cptr(g->get_pixels(), scheme_make_utf8_string("imgptr"));

	MZ_GC_UNREG();
    return ret;
}

Scheme_Object *scheme_reload(Scheme_Env *env)
{
	Scheme_Env *menv = NULL;
	MZ_GC_DECL_REG(2);
	MZ_GC_VAR_IN_REG(0, env);
	MZ_GC_VAR_IN_REG(1, menv);
	MZ_GC_REG();

	// add all the modules from this extension
	menv = scheme_primitive_module(scheme_intern_symbol("fluxus-video"), env);

	scheme_add_global("video-clear-cache", scheme_make_prim_w_arity(video_clear_cache, "video-clear-cache", 0, 0), menv);
	scheme_add_global("video-load", scheme_make_prim_w_arity(video_load, "video-load", 1, 1), menv);
	scheme_add_global("video-update", scheme_make_prim_w_arity(video_update, "video-update", 1, 1), menv);
	scheme_add_global("video-tcoords", scheme_make_prim_w_arity(video_tcoords, "video-tcoords", 1, 1), menv);
	scheme_add_global("video-play", scheme_make_prim_w_arity(video_play, "video-play", 1, 1), menv);
	scheme_add_global("video-stop", scheme_make_prim_w_arity(video_stop, "video-stop", 1, 1), menv);
	scheme_add_global("video-seek", scheme_make_prim_w_arity(video_seek, "video-seek", 2, 2), menv);
	scheme_add_global("video-width", scheme_make_prim_w_arity(video_width, "video-width", 1, 1), menv);
	scheme_add_global("video-height", scheme_make_prim_w_arity(video_height, "video-height", 1, 1), menv);
	scheme_add_global("video-imgptr", scheme_make_prim_w_arity(video_imgptr, "video-imgptr", 1, 1), menv);

	scheme_add_global("camera-clear-cache", scheme_make_prim_w_arity(camera_clear_cache, "camera-clear-cache", 0, 0), menv);
	scheme_add_global("camera-list-devices", scheme_make_prim_w_arity(camera_list_devices, "camera_list_devices", 0, 0), menv);
	scheme_add_global("camera-init", scheme_make_prim_w_arity(camera_init, "camera-init", 3, 3), menv);
	scheme_add_global("camera-update", scheme_make_prim_w_arity(camera_update, "camera-update", 1, 1), menv);
	scheme_add_global("camera-tcoords", scheme_make_prim_w_arity(camera_tcoords, "camera-tcoords", 1, 1), menv);
	scheme_add_global("camera-width", scheme_make_prim_w_arity(camera_width, "camera-width", 1, 1), menv);
	scheme_add_global("camera-height", scheme_make_prim_w_arity(camera_height, "camera-height", 1, 1), menv);
	scheme_add_global("camera-imgptr", scheme_make_prim_w_arity(camera_imgptr, "camera-imgptr", 1, 1), menv);

	scheme_finish_primitive_module(menv);
	MZ_GC_UNREG();

	return scheme_void;
}

Scheme_Object *scheme_initialize(Scheme_Env *env)
{
	return scheme_reload(env);
}

Scheme_Object *scheme_module_name()
{
	return scheme_intern_symbol("fluxus-video");
}

