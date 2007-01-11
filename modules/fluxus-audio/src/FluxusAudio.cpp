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

#include <escheme.h>
#include "AudioCollector.h"

using namespace std;

AudioCollector *Audio = NULL;

Scheme_Object *start_audio(int argc, Scheme_Object **argv)
{
	if (!SCHEME_CHAR_STRINGP(argv[0])) scheme_wrong_type("start-audio", "string", 0, argc, argv);
	if (!SCHEME_NUMBERP(argv[1])) scheme_wrong_type("start-audio", "number", 1, argc, argv);
	if (!SCHEME_NUMBERP(argv[2])) scheme_wrong_type("start-audio", "number", 2, argc, argv);
	
	if (Audio==NULL)
	{
		char *name = scheme_utf8_encode_to_buffer(SCHEME_CHAR_STR_VAL(argv[0]),SCHEME_CHAR_STRLEN_VAL(argv[0]),NULL,0);
		Audio = new AudioCollector(name,(unsigned int)scheme_real_to_double(argv[1]),(int)scheme_real_to_double(argv[2]));
	}
	return scheme_void;
}

Scheme_Object *get_harmonic(int argc, Scheme_Object **argv)
{
	if (!SCHEME_NUMBERP(argv[0])) scheme_wrong_type("gh", "number", 0, argc, argv);
	if (Audio!=NULL)
	{	
    	return scheme_make_double(Audio->GetHarmonic((int)scheme_real_to_double(argv[0])));
	}
	return scheme_make_double(0);
}

Scheme_Object *gain(int argc, Scheme_Object **argv)
{
	if (!SCHEME_NUMBERP(argv[0])) scheme_wrong_type("gain", "number", 0, argc, argv);
	if (Audio!=NULL)
	{	
		Audio->SetGain(scheme_real_to_double(argv[0]));
	}
    return scheme_void;
}


Scheme_Object *process(int argc, Scheme_Object **argv)
{
	if (!SCHEME_CHAR_STRINGP(argv[0])) scheme_wrong_type("process", "string", 0, argc, argv);
	char *wavname=scheme_utf8_encode_to_buffer(SCHEME_CHAR_STR_VAL(argv[0]),SCHEME_CHAR_STRLEN_VAL(argv[0]),NULL,0);
	if (Audio!=NULL)
	{	
		Audio->Process(wavname);
	}
    return scheme_void;
}

Scheme_Object *smoothing_bias(int argc, Scheme_Object **argv)
{
	if (!SCHEME_NUMBERP(argv[0])) scheme_wrong_type("smoothing-bias", "number", 0, argc, argv);
	if (Audio!=NULL)
	{	
		Audio->SetSmoothingBias(scheme_real_to_double(argv[0]));
	}
    return scheme_void;
}

Scheme_Object *update_audio(int argc, Scheme_Object **argv)
{
	if (Audio!=NULL)
	{	
		Audio->GetFFT();
	}
    return scheme_void;
}

/////////////////////

Scheme_Object *scheme_reload(Scheme_Env *env)
{
	// add all the modules from this extension
	Scheme_Env *menv=scheme_primitive_module(scheme_intern_symbol("fluxus-audio"), env);

	scheme_add_global("start-audio", scheme_make_prim_w_arity(start_audio, "start-audio", 3, 3), menv);
	scheme_add_global("gh", scheme_make_prim_w_arity(get_harmonic, "gh", 1, 1), menv);
	scheme_add_global("gain", scheme_make_prim_w_arity(gain, "gain", 1, 1), menv);
	scheme_add_global("process", scheme_make_prim_w_arity(process, "process", 1, 1), menv);
	scheme_add_global("smoothing-bias", scheme_make_prim_w_arity(smoothing_bias, "smoothing-bias", 1, 1), menv);
	scheme_add_global("update-audio", scheme_make_prim_w_arity(update_audio, "update-audio", 0, 0), menv);

	scheme_finish_primitive_module(menv);	
	
	return scheme_void;
}

Scheme_Object *scheme_initialize(Scheme_Env *env)
{
	return scheme_reload(env);
}

Scheme_Object *scheme_module_name()
{
	return scheme_intern_symbol("fluxus-audio");
}
