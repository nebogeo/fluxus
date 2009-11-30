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

#include "ARTracker.h"
#include "SchemeHelper.h"

using namespace std;
using namespace SchemeHelper;

static ARTracker *tracker = NULL;

// StartSectionDoc-en
// artkp
// TODO
// Fluxus ARToolKitPlus module.
// Example:
// EndSectionDoc

// StartFunctionDoc-en
// ar-init width-number height-number filename-string
// Returns: void
// Description:
// Initializes the AR single marker tracker with the camera resolution and
// camera parameter file.
// Example:
// (define cam (camera-init 0 320 240))
// (ar-init (camera-width cam) (camera-height cam) "data/camera-para.dat")
// EndFunctionDoc

Scheme_Object *ar_init(int argc, Scheme_Object **argv)
{
	DECL_ARGV();

	ArgCheck("ar-init", "iis", argc, argv);
	int width = IntFromScheme(argv[0]);
	int height = IntFromScheme(argv[1]);
	string filename = StringFromScheme(argv[2]);
	if (tracker == NULL)
	{
		tracker = new ARTracker();
	}
	tracker->init(filename, width, height);
	MZ_GC_UNREG();
	return scheme_void;
}

// StartFunctionDoc-en
// ar-set-threshold threshold-number
// Returns: void
// Description:
// Sets the threshold value that is used for black/white conversion.
// Example:
// (ar-set-threshold 150)
// EndFunctionDoc

Scheme_Object *ar_set_threshold(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("ar-set-threshold", "i", argc, argv);

	if (tracker != NULL)
	{
		tracker->set_threshold(IntFromScheme(argv[0]));
	}
	else
	{
		cerr << "ar-set-threshold: tracker is not initialized." << endl;
	}

	MZ_GC_UNREG();
	return scheme_void;
}

// StartFunctionDoc-en
// ar-get-threshold
// Returns: threshold-number
// Description:
// Returns the current threshold value.
// Example:
// (ar-get-threshold)
// EndFunctionDoc

Scheme_Object *ar_get_threshold(int argc, Scheme_Object **argv)
{
	Scheme_Object *ret = NULL;
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, ret);
	MZ_GC_REG();

	ret = scheme_void;

	if (tracker != NULL)
	{
		ret = scheme_make_integer_value(tracker->get_threshold());
	}
	else
	{
		cerr << "ar-get-threshold: tracker is not initialized." << endl;
	}

	MZ_GC_UNREG();
	return ret;
}

// StartFunctionDoc-en
// ar-auto-threshold activate-bool
// Returns: void
// Description:
// Enables or disables automatic threshold calculation.
// Example:
// (ar-auto-threshold #t)
// EndFunctionDoc

Scheme_Object *ar_auto_threshold(int argc, Scheme_Object **argv)
{
    DECL_ARGV();
    ArgCheck("ar-auto-threshold", "b", argc, argv);
    if (tracker != NULL)
    {
		tracker->activate_auto_threshold(BoolFromScheme(argv[0]));
    }
	else
	{
		cerr << "ar-auto-threshold: tracker is not initialized." << endl;
	}

    MZ_GC_UNREG();
    return scheme_void;
}

// StartFunctionDoc-en
// ar-set-pattern-width width-number
// Returns: void
// Description:
// Example:
// (ar-set-pattern-width 40)
// EndFunctionDoc

Scheme_Object *ar_set_pattern_width(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("ar-set-pattern-width", "f", argc, argv);

	if (tracker != NULL)
	{
		tracker->set_pattern_width(FloatFromScheme(argv[0]));
	}
	else
	{
		cerr << "ar-set-pattern-width: tracker is not initialized." << endl;
	}

	MZ_GC_UNREG();
	return scheme_void;
}

// StartFunctionDoc-en
// ar-activate-vignetting-compensation activate-bool
// Returns: void
// Description:
// Activates the compensation of brightness falloff in the corners of the
// camera image.
// Example:
// (ar-activate-vignetting-compensation #t)
// EndFunctionDoc

Scheme_Object *ar_activate_vignetting_compensation(int argc, Scheme_Object **argv)
{
    DECL_ARGV();
    ArgCheck("ar-activate-vignetting-compensation", "b", argc, argv);
    if (tracker != NULL)
    {
		tracker->activate_vignetting_compensation(BoolFromScheme(argv[0]));
    }
	else
	{
		cerr << "ar-activate-vignetting-compensation: tracker is not initialized." << endl;
	}

    MZ_GC_UNREG();
    return scheme_void;
}

// StartFunctionDoc-en
// ar-detect buffer-imgptr
// Returns: number
// Description:
// Detects the markers in the image. Returns the number of detected markers.
// Example:
// TODO
// EndFunctionDoc

Scheme_Object *ar_detect(int argc, Scheme_Object **argv)
{
	DECL_ARGV();

	ArgCheck("ar-detect", "I", argc, argv);

	int marker_count = 0;

	if (tracker != NULL)
	{
		marker_count = tracker->detect(static_cast<const unsigned char *>SCHEME_CPTR_VAL(argv[0]));
	}
	else
	{
		cerr << "ar-detect: tracker is not initialized." << endl;
	}

	MZ_GC_UNREG();
	return scheme_make_integer_value(marker_count);
}

// StartFunctionDoc-en
// ar-get-projection-matrix
// Returns: projection-matrix-vector
// Description:
// Returns the projection matrix acquired from the camera parameter file.
// Example:
// (set-projection-transform (ar-get-projection-matrix))
// (set-camera-transform (mident))
// EndFunctionDoc

Scheme_Object *ar_get_projection_matrix(int argc, Scheme_Object **argv)
{
	const ARFloat *matrix;

	if (tracker != NULL)
	{
		matrix = tracker->get_projection_matrix();
	}
	else
	{
		cerr << "ar-get-projection-matrix: tracker is not initialized." << endl;
		return scheme_void;
	}

	if (sizeof(ARFloat) == sizeof(float))
	{
		return FloatsToScheme((float *)matrix, 16);
	}
	else
	{
		return DoublesToScheme((double *)matrix, 16);
	}
}

// StartFunctionDoc-en
// ar-get-modelview-matrix index-number
// Returns: modelview-matrix-vector
// Description:
// Returns the modelview matrix of the marker with the given index or #f if no
// marker is detected. The number of detected markers are returned by (ar-detect).
// The index can go from 0 to the number of detected markers - 1.
// Example:
// (let ([marker-count (ar-detect (camera-imgptr cam))])
//   (for ([i (in-range marker-count)])
//		(let ([m (ar-get-modelview-matrix i)]
//			  [id (ar-get-marker-id)])
//		   (printf "marker index:~a id:~a matrix:~a~n" i id m))))
// EndFunctionDoc

Scheme_Object *ar_get_modelview_matrix(int argc, Scheme_Object **argv)
{
	DECL_ARGV();

	const ARFloat *matrix;

	ArgCheck("ar-get-modelview-matrix", "i", argc, argv);

	if (tracker != NULL)
	{
		matrix = tracker->get_modelview_matrix(IntFromScheme(argv[0]));
	}
	else
	{
		cerr << "ar-get-modelview-matrix: tracker is not initialized." << endl;
		MZ_GC_UNREG();
		return scheme_void;
	}

	MZ_GC_UNREG();
	if (matrix == NULL)
	{
		return scheme_false;
	}
	else if (sizeof(ARFloat) == sizeof(float))
	{
		return FloatsToScheme((float *)matrix, 16);
	}
	else
	{
		return DoublesToScheme((double *)matrix, 16);
	}
}

// StartFunctionDoc-en
// ar-get-id index-number
// Returns: marker-id-number
// Description:
// Returns the ID of the marker with the given index or #f if the id could not be
// determined. The number of detected markers are returned by (ar-detect).
// The index can go from 0 to the number of detected markers - 1.
// Example:
// (let ([marker-count (ar-detect (camera-imgptr cam))])
//   (for ([i (in-range marker-count)])
//		(let ([m (ar-get-modelview-matrix i)]
//			  [id (ar-get-id i)])
//		   (printf "marker index:~a id:~a matrix:~a~n" i id m))))
// EndFunctionDoc

Scheme_Object *ar_get_id(int argc, Scheme_Object **argv)
{
	DECL_ARGV();

	int id = -1;

	ArgCheck("ar-get-id", "i", argc, argv);

	if (tracker != NULL)
	{
		id = tracker->get_id(IntFromScheme(argv[0]));
	}
	else
	{
		cerr << "ar-get-id: tracker is not initialized." << endl;
		MZ_GC_UNREG();
		return scheme_void;
	}

	MZ_GC_UNREG();
	if (id < 0)
	{
		return scheme_false;
	}
	else
	{
		return scheme_make_integer_value(id);
	}
}

// StartFunctionDoc-en
// ar-get-confidence index-number
// Returns: confidence-number
// Description:
// Returns the confidence value of the marker with the given index.
// The number of detected markers are returned by (ar-detect).
// The index can go from 0 to the number of detected markers - 1.
// Example:
// (let ([marker-count (ar-detect (camera-imgptr cam))])
//   (for ([i (in-range marker-count)])
//		(let ([conf (ar-get-confidence i)])
//		   (printf "marker index:~a confidence:~a matrix:~a~n" i id m))))
// EndFunctionDoc

Scheme_Object *ar_get_confidence(int argc, Scheme_Object **argv)
{
	DECL_ARGV();

	float cf;

	ArgCheck("ar-get-confidence", "i", argc, argv);

	if (tracker != NULL)
	{
		cf = tracker->get_confidence(IntFromScheme(argv[0]));
	}
	else
	{
		cerr << "ar-get-confidence: tracker is not initialized." << endl;
		MZ_GC_UNREG();
		return scheme_void;
	}

	MZ_GC_UNREG();
	return scheme_make_double(cf);
}

Scheme_Object *scheme_reload(Scheme_Env *env)
{
	Scheme_Env *menv = NULL;
	MZ_GC_DECL_REG(2);
	MZ_GC_VAR_IN_REG(0, env);
	MZ_GC_VAR_IN_REG(1, menv);
	MZ_GC_REG();

	// add all the modules from this extension
	menv = scheme_primitive_module(scheme_intern_symbol("fluxus-artkp"), env);

	scheme_add_global("ar-init",
			scheme_make_prim_w_arity(ar_init, "ar-init", 3, 3), menv);
	scheme_add_global("ar-set-threshold",
			scheme_make_prim_w_arity(ar_set_threshold, "ar-set-threshold", 1, 1), menv);
	scheme_add_global("ar-get-threshold",
			scheme_make_prim_w_arity(ar_get_threshold, "ar-get-threshold", 0, 0), menv);
	scheme_add_global("ar-auto-threshold",
			scheme_make_prim_w_arity(ar_auto_threshold, "ar-auto-threshold", 1, 1), menv);
	scheme_add_global("ar-detect",
			scheme_make_prim_w_arity(ar_detect, "ar-detect", 1, 1), menv);
	scheme_add_global("ar-set-pattern-width",
			scheme_make_prim_w_arity(ar_set_pattern_width, "ar-set-pattern-width", 1, 1), menv);
	scheme_add_global("ar-activate-vignetting-compensation",
			scheme_make_prim_w_arity(ar_activate_vignetting_compensation, "ar-activate-vignetting-compensation", 1, 1), menv);
	scheme_add_global("ar-get-projection-matrix",
			scheme_make_prim_w_arity(ar_get_projection_matrix, "ar-get-projection-matrix", 0, 0), menv);
	scheme_add_global("ar-get-modelview-matrix",
			scheme_make_prim_w_arity(ar_get_modelview_matrix, "ar-get-modelview-matrix", 1, 1), menv);
	scheme_add_global("ar-get-id",
			scheme_make_prim_w_arity(ar_get_id, "ar-get-id", 1, 1), menv);
	scheme_add_global("ar-get-confidence",
			scheme_make_prim_w_arity(ar_get_confidence, "ar-get-confidence", 1, 1), menv);

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
	return scheme_intern_symbol("fluxus-artkp");
}

