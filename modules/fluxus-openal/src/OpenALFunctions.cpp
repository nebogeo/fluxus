// Copyright (C) 2009 Dave Griffiths
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
#include <map>
#include <assert.h>
#include "FluxAudio.h"

using namespace std;
using namespace fluxus;

FluxAudio *Audio = NULL;

// StartSectionDoc-en
// openal
// OpenAL is a cross platform audio library designed for use in conjunction
// with 3D applications.
// Example:
// EndSectionDoc

// StartSectionDoc-pt
// openal
// Openal é uma biblioteca de audio multi-plataforma feita para ser
// usada junto com aplicativos 3D.
// EndSectionDoc

void FloatsFromScheme(Scheme_Object *src, float *dst, unsigned int size)
{
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, src);
	MZ_GC_REG();
	assert(size<=(unsigned int)SCHEME_VEC_SIZE(src));
	for (unsigned int n=0; n<size; n++)
	{
		dst[n]=scheme_real_to_double(SCHEME_VEC_ELS(src)[n]);
	}
	MZ_GC_UNREG();
}

// StartFunctionDoc-en
// oa-start
// Returns: void
// Description:
// Starts up the openal audio system
// Example:
// (oa-start)
// (define mysample (oa-load-sample (fullpath "sample.wav")))
// (oa-play mysample (vector 0 0 0) 1 0.2)
// EndFunctionDoc

// StartFunctionDoc-pt
// oa-start
// Retorna: void
// Descrição:
// Inicia o sistema de áudio openal
// Exemplo:
// (oa-start)
// (define mysample (oa-load-sample (fullpath "sample.wav")))
// (oa-play mysample (vector 0 0 0) 1 0.2)
// EndFunctionDoc

Scheme_Object *start(int argc, Scheme_Object **argv)
{
	if (Audio==NULL) Audio = new FluxAudio;
	return scheme_void;
}

// StartFunctionDoc-en
// oa-load-sample 
// Returns: sampleid-number
// Description:
// Loads a sample and returns the id number, so it can be played.
// Example:
// (oa-start)
// (define mysample (oa-load-sample "sample.wav"))
// (oa-play mysample (vector 0 0 0) 1 0.2)
// EndFunctionDoc

// StartFunctionDoc-pt
// oa-load-sample
// Retorna: número-sampleid
// Descrição:
// Carrega uma amostra e retorna o número id, de forma que possa ser
// tocada.
// Exemplo:
// (oa-start)
// (define mysample (oa-load-sample "sample.wav"))
// (oa-play mysample (vector 0 0 0) 1 0.2)
// EndFunctionDoc

Scheme_Object *load_sample(int argc, Scheme_Object **argv)
{
	char *filename=NULL;
	MZ_GC_DECL_REG(2); 
	MZ_GC_VAR_IN_REG(0, argv); 
	MZ_GC_VAR_IN_REG(1, filename); 
	MZ_GC_REG();	

	if (!SCHEME_CHAR_STRINGP(argv[0])) scheme_wrong_type("oa-load-sample", "string", 0, argc, argv);
	int id=0;

	if (Audio!=NULL)
	{
		filename=scheme_utf8_encode_to_buffer(SCHEME_CHAR_STR_VAL(argv[0]),SCHEME_CHAR_STRLEN_VAL(argv[0]),NULL,0);
		id=Audio->Load(filename);
	}
	
	MZ_GC_UNREG(); 
	return scheme_make_integer_value(id);
}

// StartFunctionDoc-en
// oa-update
// Returns: void
// Description:
// Keeps the openal code running, is called by the every-frame code.
// Example:
// (oa-update)
// EndFunctionDoc

// StartFunctionDoc-pt
// oa-update
// Retorna: void
// Descrição:
// Mantém o código openal rodando, é chamado pelo código every-frame.
// Exemplo:
// (oa-update)
// EndFunctionDoc

Scheme_Object *update(int argc, Scheme_Object **argv)
{
	if (Audio!=NULL)
	{
		Audio->Update();
	}
	return scheme_void;
}

// StartFunctionDoc-en
// oa-play sample-id [position] [pitch] [gain] [looping]
// Returns: source-id-number
// Description:
// Plays a sample.
// Example:
// (oa-start)
// (define mysample (oa-load-sample (fullpath "sample.wav")))
// (oa-play mysample (vector 0 0 0) 1 0.2)
// EndFunctionDoc

// StartFunctionDoc-pt
// oa-play sampleid posição pitch gain
// Retorna: source-id-number
// Descrição:
// Toca uma amostra.
// Exemplo:
// (oa-start)
// (define mysample (oa-load-sample (fullpath "sample.wav")))
// (oa-play mysample (vector 0 0 0) 1 0.2)
// EndFunctionDoc

Scheme_Object *play(int argc, Scheme_Object **argv)
{
	Scheme_Object *ret = scheme_void;

	MZ_GC_DECL_REG(2);
	MZ_GC_VAR_IN_REG(0, argv);
	MZ_GC_VAR_IN_REG(1, ret);
	MZ_GC_REG();

	dVector pos(0, 0, 0);
	float pitch = 1.0;
	float gain = 1.0;
	bool looping = false;

	if (!SCHEME_NUMBERP(argv[0])) scheme_wrong_type("oa-play", "number", 0, argc, argv);

	if (argc > 1)
	{
		if (SCHEME_VECTORP(argv[1]) && (SCHEME_VEC_SIZE(argv[1]) == 3))
			FloatsFromScheme(argv[1], pos.arr(), 3);
		else
			scheme_wrong_type("oa-play", "vector size 3", 1, argc, argv);
	}

	if (argc > 2)
	{
		if (!SCHEME_NUMBERP(argv[2]))
			scheme_wrong_type("oa-play", "number", 2, argc, argv);
		else
			pitch = scheme_real_to_double(argv[2]);
	}

	if (argc > 3)
	{
		if (!SCHEME_NUMBERP(argv[3]))
			scheme_wrong_type("oa-play", "number", 3, argc, argv);
		else
			gain = scheme_real_to_double(argv[3]);
	}

	if (argc > 4)
	{
		if (!SCHEME_BOOLP(argv[4]))
			scheme_wrong_type("oa-play", "bool", 4, argc, argv);
		else
			looping = SCHEME_TRUEP(argv[4]);
	}

	if (Audio != NULL)
	{
		unsigned int id=(unsigned int)scheme_real_to_double(argv[0]);
		int source_id = Audio->Play(id, pos, pitch, gain, looping);
		if (source_id >= 0)
			ret = scheme_make_integer_value(source_id);
	}

	MZ_GC_UNREG();
	return ret;
}

// StartFunctionDoc-en
// oa-stop
// Returns: void
// Description:
// Stops all playing samples.
// Example:
// (oa-start)
// (define s (oa-load-sample (fullpath "sample.wav")))
// (oa-play s (vector 0 0 0) 1 1 #t)
// ; schedule a task 5 seconds from now
// (spawn-timed-task (+ (time-now) 5)
//     (lambda () (oa-stop)))
// EndFunctionDoc

Scheme_Object *stop(int argc, Scheme_Object **argv)
{
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, argv);
	MZ_GC_REG();

	if (Audio != NULL)
	{
		Audio->Stop();
	}

	MZ_GC_UNREG();
	return scheme_void;
}

// StartFunctionDoc-en
// oa-set-head-pos pos dir
// Returns: void
// Description:
// Sets the head position, or listener position - which changes the panning and
// perhaps filtering of the sounds to relate to world space position.
// Example:
// (oa-start)
// (define mysample (oa-load-sample (fullpath "sample.wav")))
// (oa-set-head-pos (vector -1 0 0) (vector 0 0 1))
// (oa-play mysample (vector 0 0 0) 1 0.2)
// EndFunctionDoc

// StartFunctionDoc-pt
// oa-set-head-pos pos dir
// Retorna: void
// Descrição:
// Ajusta a posição da cabeça, ou posição do ouvinte - que muda o
// panning e talvez a filtragem dos soms em relação a posição espacial
// no mundo.
// Exemplo:
// (oa-start)
// (define mysample (oa-load-sample (fullpath "sample.wav")))
// (oa-set-head-pos (vector -1 0 0) (vector 0 0 1))
// (oa-play mysample (vector 0 0 0) 1 0.2)
// EndFunctionDoc

Scheme_Object *set_head_pos(int argc, Scheme_Object **argv)
{
	MZ_GC_DECL_REG(2); 
	MZ_GC_VAR_IN_REG(0, argv); 
	MZ_GC_REG();	
	if (!SCHEME_VECTORP(argv[0])) scheme_wrong_type("oa-set-head-pos", "vector", 0, argc, argv);
	if (!SCHEME_VECTORP(argv[1])) scheme_wrong_type("oa-set-head-pos", "vector", 1, argc, argv);

	if (Audio!=NULL)
	{
		dVector pos;
		FloatsFromScheme(argv[0],pos.arr(),3);
		dVector dir;
		FloatsFromScheme(argv[1],dir.arr(),3);
	
		Audio->SetHeadPos(pos,dir);
	}
	
	MZ_GC_UNREG(); 
	return scheme_void;
}

// StartFunctionDoc-en
// oa-set-poly polyphony-count
// Returns: void
// Description:
// Sets the number of samples that can be played at once. Defaults to 256.
// Example:
// (oa-set-poly 256)
// EndFunctionDoc

// StartFunctionDoc-pt
// oa-set-poly contagem-polifônica
// Retorna: void
// Descrição:
// Ajusta o número de amostras que podem ser tocadas de uma
// vez. Padrão é 256.
// Exemplo:
// (oa-set-poly 256)
// EndFunctionDoc

Scheme_Object *set_poly(int argc, Scheme_Object **argv)
{
	MZ_GC_DECL_REG(1); 
	MZ_GC_VAR_IN_REG(0, argv); 
	MZ_GC_REG();	
	if (!SCHEME_NUMBERP(argv[0])) scheme_wrong_type("al-poly", "number", 0, argc, argv);

	if (Audio!=NULL)
	{
		Audio->SetPoly((unsigned int)scheme_real_to_double(argv[0]));
	}
	
	MZ_GC_UNREG(); 
	return scheme_void;
}

// StartFunctionDoc-en
// oa-set-cull-dist distance
// Returns: void
// Description:
// Sets distance at which sounds will be stopped playing
// Example:
// (oa-set-cull-dist 1000)
// EndFunctionDoc

// StartFunctionDoc-pt
// oa-set-cull-dist distancia
// Retorna: void
// Descrição:
// Ajusta a distancia à qual o som vai deixar de tocar.
// Exemplo:
// (oa-set-cull-dist 1000)
// EndFunctionDoc

Scheme_Object *set_cull_dist(int argc, Scheme_Object **argv)
{
	MZ_GC_DECL_REG(1); 
	MZ_GC_VAR_IN_REG(0, argv); 
	MZ_GC_REG();	
	if (!SCHEME_NUMBERP(argv[0])) scheme_wrong_type("oa-set-cull-dist", "number", 0, argc, argv);

	if (Audio!=NULL)
	{
		Audio->SetCullDist(scheme_real_to_double(argv[0]));
	}
	
	MZ_GC_UNREG(); 
	return scheme_void;
}

// StartFunctionDoc-en
// oa-set-acoustics attenuation-scale max-distance ref-distance rolloff
// Returns: void
// Description:
// Sets some global acoustic parameters
// Example:
// (oa-set-acoustics 1 1 1 1)
// EndFunctionDoc

// StartFunctionDoc-pt
// oa-set-acoustics escala-atenuação distancia-max distancia-ref rolloff
// Retorna: void
// Descrição:
// Ajusta alguns parametros globais de acústica.
// Exemplo:
// (oa-set-acoustics 1 1 1 1)
// EndFunctionDoc

Scheme_Object *set_acoustics(int argc, Scheme_Object **argv)
{
	MZ_GC_DECL_REG(4); 
	MZ_GC_VAR_IN_REG(0, argv); 
	MZ_GC_REG();	
	if (!SCHEME_NUMBERP(argv[0])) scheme_wrong_type("oa-set-acoustics", "number", 0, argc, argv);
	if (!SCHEME_NUMBERP(argv[1])) scheme_wrong_type("oa-set-acoustics", "number", 1, argc, argv);
	if (!SCHEME_NUMBERP(argv[2])) scheme_wrong_type("oa-set-acoustics", "number", 2, argc, argv);
	if (!SCHEME_NUMBERP(argv[3])) scheme_wrong_type("oa-set-acoustics", "number", 3, argc, argv);

	if (Audio!=NULL)
	{
		FluxAudio::AcousticDesc desc;
		desc.AttenScale=scheme_real_to_double(argv[0]);
		desc.MaxDistance=scheme_real_to_double(argv[1]);
		desc.RefDistance=scheme_real_to_double(argv[2]);
		desc.Rolloff=scheme_real_to_double(argv[3]);

		Audio->SetAcoustics(desc);
	}
	
	MZ_GC_UNREG(); 
	return scheme_void;
}

// StartFunctionDoc-en
// oa-set-pitch source-id-number pitch-number
// Returns: void
// Description:
// Pitch shifts the playing source with the desired value, where 1.0 equals identity.
// Each reduction by 50 percent equals a pitch shift of -12 semitones (one octave reduction).
// Each doubling equals a pitch shift of 12 semitones (one octave increase). Zero is not a legal value.
// Example:
// (define s (oa-load-sample (fullpath "sample.wav")))
// (define id (oa-play s (vector 0 0 0) 1 1 #t))
// (every-frame
//    (oa-set-pitch id (+ 1.1 (sin (time)))))
// EndFunctionDoc

Scheme_Object *set_pitch(int argc, Scheme_Object **argv)
{
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, argv);
	MZ_GC_REG();
	if (!SCHEME_INTP(argv[0])) scheme_wrong_type("oa-set-pitch", "int", 0, argc, argv);
	if (!SCHEME_NUMBERP(argv[1])) scheme_wrong_type("oa-set-pitch", "number", 1, argc, argv);

	if (Audio!=NULL)
	{
		Audio->SetPitch(SCHEME_INT_VAL(argv[0]), scheme_real_to_double(argv[1]));
	}

	MZ_GC_UNREG();
	return scheme_void;
}

/////////////////////

Scheme_Object *scheme_reload(Scheme_Env *env)
{
	// add all the modules from this extension
	Scheme_Env *menv=scheme_primitive_module(scheme_intern_symbol("fluxus-openal"), env);

	scheme_add_global("oa-start", scheme_make_prim_w_arity(start, "oa-start", 0, 0), menv);
	scheme_add_global("oa-load-sample", scheme_make_prim_w_arity(load_sample, "oa-load-sample", 1, 1), menv);
	scheme_add_global("oa-update", scheme_make_prim_w_arity(update, "oa-update", 0, 0), menv);
	scheme_add_global("oa-play", scheme_make_prim_w_arity(play, "oa-play", 1, 5), menv);
	scheme_add_global("oa-stop", scheme_make_prim_w_arity(stop, "oa-stop", 0, 0), menv);
	scheme_add_global("oa-set-head-pos", scheme_make_prim_w_arity(set_head_pos, "oa-set-head-pos", 2, 2), menv);
	scheme_add_global("oa-set-poly", scheme_make_prim_w_arity(set_poly, "oa-set-poly", 1, 1), menv);
	scheme_add_global("oa-set-cull-dist", scheme_make_prim_w_arity(set_cull_dist, "oa-set-cull-dist", 1, 1), menv);
	scheme_add_global("oa-set-acoustics", scheme_make_prim_w_arity(set_acoustics, "oa-set-acoustics", 4, 4), menv);
	scheme_add_global("oa-set-pitch", scheme_make_prim_w_arity(set_pitch, "oa-set-pitch", 2, 2), menv);

	scheme_finish_primitive_module(menv);

	return scheme_void;
}

Scheme_Object *scheme_initialize(Scheme_Env *env)
{
	return scheme_reload(env);
}

Scheme_Object *scheme_module_name()
{
	return scheme_intern_symbol("fluxus-openal");
}
