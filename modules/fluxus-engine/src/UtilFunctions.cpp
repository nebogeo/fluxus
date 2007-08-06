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
#include "UtilFunctions.h"
#include "Renderer.h"
#include "Utils.h"
#include "SearchPaths.h"

using namespace UtilFunctions;
using namespace SchemeHelper;
using namespace Fluxus;

// StartSectionDoc-en
// UtilFunctions
// Handy functions to make your life easier...
// Example:
// time
// delta
// flxrnd
// flxseed
// searchpaths
// fullpath
// framedump
// EndSectionDoc 

// StartSectionDoc-pt
// UtilFunctions
// FunÅÁÅıes prÅ·ticas para fazer a sua vida mais fÅ·cil.
// Exemplo:
// time
// delta
// flxrnd
// flxseed
// searchpaths
// fullpath
// framedump
// EndSectionDoc

// StartFunctionDoc-en
// time
// Returns: time-number
// Description:
// Returns the number of seconds (+ fraction) since midnight January 1st 1970. This is the
// simpest animation source for your scripts.
// Example:
// (define (animate)
//     (rotate (vector (sin (time)) 0 0))
//     (draw-cube))
// (every-frame (animate))    
// EndFunctionDoc

// StartFunctionDoc-pt
// time
// Retorna: numero do tempo(transcorrido, passado)
// Descri√ß√£o:
// Retorna o numero de segundos (+ fracao) desde meia noite do dia 1 de janeiro de 1970.
// Essa e a forma mais simples de animacao para seus scripts
// Exemplo:
// (define (animate)
//     (rotate (vector (sin (time)) 0 0))
//     (draw-cube))
// (every-frame (animate))    
// EndFunctionDoc

Scheme_Object *time(int argc, Scheme_Object **argv)
{
	return scheme_make_double(Engine::Get()->Renderer()->GetTime());
}

// StartFunctionDoc-en
// delta
// Returns: time-number
// Description:
// Time in seconds since the last frame. Used to make animation frame rate independant.
// Example:
// (define (animate)
//     (rotate ((vector * (delta) 10) 0 0) 
//     (draw-cube))
// (every-frame (animate))    
// EndFunctionDoc

// StartFunctionDoc-pt
// delta
// Retorna: numero do tempo(transcorrido, passado)
// Descri√ß√£o:
// Tempo em segundos desde o ultimo frame. Usado pra fazer animacoes com taxa de frames independendente(constante).
// Exemplo:
// (define (animate)
//     (rotate (* (delta) 10) 0 0) 
//     (draw-cube))
// (every-frame (animate))    
// EndFunctionDoc

Scheme_Object *delta(int argc, Scheme_Object **argv)
{
	return scheme_make_double(Engine::Get()->Renderer()->GetDelta());
}

// StartFunctionDoc-en
// flxrnd
// Returns: random-number
// Description:
// Returns a random number between 0 and 1.
// Example:
// (define (animate)
//     (colour (flxrnd) (flxrnd) (flxrnd)) 
//     (draw-cube))
// (every-frame (animate))
// EndFunctionDoc

// StartFunctionDoc-pt
// flxrnd
// Retorna: nÅ˙mero randÅÙmico
// Descri√ß√£o:
// Retorna um nÅ˙mero randÅÙmico entre 0 e 1.
// Exemplo:
// (define (animate)
//     (colour (flxrnd) (flxrnd) (flxrnd)) 
//     (draw-cube))
// (every-frame (animate))
// EndFunctionDoc

Scheme_Object *flxrnd(int argc, Scheme_Object **argv)
{
	return scheme_make_double(RandFloat());
}

// StartFunctionDoc-en
// flxseed seed-number
// Returns: void
// Description:
// Seeds the random number generator so we can get the same sequence.
// Example:
// (define (animate)
//     (colour (flxrnd) (flxrnd) (flxrnd)) 
//     (draw-cube))
// (flxseed 10) 
// (every-frame (animate)) ; the same sequence of colours will be generated
// EndFunctionDoc

// StartFunctionDoc-pt
// flxseed nÅ˙mero da semente
// Retorna: void
// Descri√ß√£o:
// Alimenta o gerador de nÅ˙mero randÅÙmico de forma que nÅÛs consigamos a mesma sequÅÍncia.
// Exemplo:
// (define (animate)
//     (colour (flxrnd) (flxrnd) (flxrnd)) 
//     (draw-cube))
// (flxseed 10) 
// (every-frame (animate)) ; a mesma sequÅÍncia de cores serÅ· gerada
// EndFunctionDoc

Scheme_Object *flxseed(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
  	ArgCheck("flxseed", "i", argc, argv);
	srand(IntFromScheme(argv[0]));
	MZ_GC_UNREG(); 
	return scheme_void;
}

// StartFunctionDoc-en
// searchpaths paths-list
// Returns: void
// Description:
// Sets a list of search path strings to use for looking for fluxus related files, such as
// textures, shaders etc. Paths will be searched in order each time.
// Example:
// (searchpaths (list "/path/to/my/textures" "/path/to/my/other/textures"))
// EndFunctionDoc

// StartFunctionDoc-pt
// searchpaths lista de diretÅÛrios
// Retorna: void
// Descri√ß√£o:
// Arranja um lista de diretÅÛrios de busca para procurar por arquivos relacionado ao fluxus,
// como texturas, shaders etc. Os diretÅÛrios serÅ„o procurados em ordem a cada vez.
// Exemplo:
// (searchpaths (list "/path/to/my/textures" "/path/to/my/other/textures"))
// EndFunctionDoc

Scheme_Object *searchpaths(int argc, Scheme_Object **argv)
{
	Scheme_Object *vec=NULL;
	MZ_GC_DECL_REG(2);
	MZ_GC_VAR_IN_REG(0, argv);
	MZ_GC_VAR_IN_REG(1, vec);
	MZ_GC_REG();	
	ArgCheck("searchpaths", "l", argc, argv);
 	
	// vectors seem easier to handle than lists with this api
	vec = scheme_list_to_vector(argv[0]);
	size_t size=0;
		
	for (int n=0; n<SCHEME_VEC_SIZE(vec); n++)
	{
		if (SCHEME_CHAR_STRINGP(SCHEME_VEC_ELS(vec)[n]))
		{
			SearchPaths::Get()->AddPath(StringFromScheme(SCHEME_VEC_ELS(vec)[n]));
		}
	}
	MZ_GC_UNREG(); 
    return scheme_void;
}

// StartFunctionDoc-en
// fullpath filename-string
// Returns: fullpath-string
// Description:
// Searches the search paths for the specified file and returns the first location
// it finds.
// Example:
// (fullpath "myfile")
// EndFunctionDoc

// StartFunctionDoc-pt
// fullpath string do nome do arquivo
// Retorna: fullpath-string
// Descri√ß√£o:
// Busca os diretÅÛrios de procura para o arquivo especificado e retorna a primeira 
// localizaÅÁÅ„o que encontra.
// Exemplo:
// (fullpath "myfile")
// EndFunctionDoc

Scheme_Object *fullpath(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("fullpath", "s", argc, argv);
	string fullpath = SearchPaths::Get()->GetFullPath(StringFromScheme(argv[0]));
	MZ_GC_UNREG(); 
	return scheme_make_utf8_string(fullpath.c_str());
}

// StartFunctionDoc-en
// framedump filename
// Returns: void
// Description:
// Saves out the current OpenGL front buffer to disk. Reads the filename extension to 
// decide on the format used for saving, "tif", "jpg" or "ppm" are supported. This is the 
// low level form of the frame dumping, use start-framedump and end-framedump instead.
// Example:
// (framedump "picture.jpg")
// EndFunctionDoc

// StartFunctionDoc-pt
// framedump nome-do-arquivo
// Retorna: void
// Descri√ß√£o:
// Salva o buffer OpenGL da frente no disco. LÅÍ a extensÅ„o do arquivo pra decidir qual
// qual formato serÅ· usado pra salvar, "tif", "jpg" ou "ppm" sÅ„o suportados. Esta ÅÈ a 
// forma de frame dumping de baixo nivel, use start-framedump e end-framedump ao invÅÈs.
// Exemplo:
// (framedump "picture.jpg")
// EndFunctionDoc


Scheme_Object *framedump(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("framedump", "s", argc, argv);
	
	int w=0,h=0;
	Engine::Get()->Renderer()->GetResolution(w,h);
	
	string filename=StringFromScheme(argv[0]);
	if (strlen(filename.c_str())>3)
	{
		if (!strcmp(filename.c_str()+strlen(filename.c_str())-3,"tif"))
		{
			WriteTiff(filename.c_str(), "made in fluxus", 0, 0, w, h, 1);
		}
		else if (!strcmp(filename.c_str()+strlen(filename.c_str())-3,"jpg"))
		{
			WriteJPG(filename.c_str(), "made in fluxus", 0, 0, w, h, 80);
		}
		else if (!strcmp(filename.c_str()+strlen(filename.c_str())-3,"ppm"))
		{
			WritePPM(filename.c_str(), "made in fluxus", 0, 0, w, h, 1);
		}
		else
		{
			Trace::Stream<<"framedump: Unknown image extension "<<filename.c_str()+strlen(filename.c_str())-3<<endl;
		}
	}
	
	MZ_GC_UNREG(); 
	return scheme_void;
}

void UtilFunctions::AddGlobals(Scheme_Env *env)
{	
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, env);
	MZ_GC_REG();
	// renderstate operations
	scheme_add_global("flxtime",scheme_make_prim_w_arity(time,"flxtime",0,0), env);
	scheme_add_global("delta",scheme_make_prim_w_arity(delta,"delta",0,0), env);
	scheme_add_global("flxrnd",scheme_make_prim_w_arity(flxrnd,"flxrnd",0,0), env);
	scheme_add_global("flxseed",scheme_make_prim_w_arity(flxseed,"flxseed",1,1), env);	
	scheme_add_global("searchpaths",scheme_make_prim_w_arity(searchpaths,"searchpaths",1,1), env);	
	scheme_add_global("fullpath",scheme_make_prim_w_arity(fullpath,"fullpath",1,1), env);	
	scheme_add_global("framedump",scheme_make_prim_w_arity(framedump,"framedump",1,1), env);	
 	MZ_GC_UNREG(); 
}
