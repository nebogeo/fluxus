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

#include <assert.h>
#include "SchemeHelper.h"
#include "Engine.h"
#include "FFGLFunctions.h"
#include "FFGLManager.h"

using namespace FFGLFunctions;
using namespace SchemeHelper;
using namespace Fluxus;

// StartSectionDoc-en
// ffgl
// NOTE: section document is in modules/scheme/ffgl.ss
// FreeFrame is a cross platform real-time video effects plugin system.
// Fluxus supports FreeFrame 1.5 also known as FreeFrameGL or FFGL. FF CPU
// software rendering plugins are not supported at the moment.
// For more information visit http://www.freeframe.org
// Example:
// (clear)
//
// ; pixelprimitive with 2 textures and an active renderer
// (define p (build-pixels 256 256 #t 2))
//
// ; load the FFGLTile plugin from the FreeFrame SDK
// (define plugin (ffgl-load "FFGLTile" 256 256))
//
// (with-ffgl plugin
//   (for ([i (ffgl-get-info)]) ; print plugin information
//        (printf "~a~n" i))
//   (printf "~a~n" (ffgl-get-parameters)) ; parameter names as strings
//   (ffgl-process p ; pixel primitive
//                (pixels->texture p 1) ; output texture
//                (pixels->texture p 0))) ; input texture
//
// (with-primitive p
//    ; the renderer of the pixelprimitive renders to texture 0
//    (pixels-render-to (pixels->texture p 0))
//    ; the pixel primitive is displayed using texture 1
//    (pixels-display (pixels->texture p 1)))
// (define (anim)
//    ; set plugin parameters as keywords arguments
//    (with-ffgl plugin
//        (ffgl-set-parameter! #:tilex (/ (mouse-x) (vx (get-screen-size)))
//                             #:tiley (/ (mouse-y) (vy (get-screen-size)))))
//    ; render to the input pixelprimitive
//    (with-pixels-renderer p
//        (with-state
//            (clear-colour #(0 1 0))
//            (scale 5)
//            (rotate (vector (* 50 (time)) -17 (* -35 (time))))
//            (draw-cube))))
//
// (every-frame (anim))
// EndSectionDoc

// StartSectionDoc-pt
// ffgl
// Freeframe é um sistema de plugin para efeitos de vídeo em
// tempo-real multi-plataforma. Fluxus suporta FreeFrame 1.5 também
// conhecido como FreeFrameGL ou FFGL. Plugins FF CPU de renderização
// em software não são suportadas no momento.
// Para mais informação visite http://www.freeframe.org
// Exemplo:
// (clear)
//
// (define p (build-pixels 256 256 #t)) ; input pixelprimitive
//
// (translate (vector 1.1 0 0))
// ; output pixelprimitive - rendering is not active
// ; otherwise it would overwrite the plugin output
// (define op (build-pixels 256 256))
//
// ; load the FFGLTile plugin from the FreeFrame SDK
// (define plugin (ffgl-load "FFGLTile" 256 256))
//
// (with-ffgl plugin
//   (for ([i (ffgl-get-info)]) ; print plugin information
//        (printf "~a~n" i))
//   (printf "~a~n" (ffgl-get-parameters)) ; parameter names as strings
//   (ffgl-process op p)) ; set destination and source pixelprimitives
//
// (define (anim)
//    ; set plugin parameters as keywords arguments
//    (with-ffgl plugin
//        (ffgl-set-parameter! #:tilex (/ (mouse-x) (vx (get-screen-size)))
//                             #:tiley (/ (mouse-y) (vy (get-screen-size)))))
//    ; render to the input pixelprimitive
//    (with-pixels-renderer p
//        (with-state
//            (clear-colour #(0 1 0))
//            (scale 5)
//            (rotate (vector (* 50 (time)) -17 (* -35 (time))))
//            (draw-cube))))
//
// (every-frame (anim))
// EndSectionDoc

// StartSectionDoc-fr
// ffgl
// FreeFrame est un systême de plugin multi-plateformes à effets vidéo temps réel.
// Fluxus supporte les FreeFrame 1.5 aussi appellé FreeFrameGL ou FFGL. FF CPU
// Fluxus supports FreeFrame 1.5 also known as FreeFrameGL or FFGL. Le logiciel
// FF CPU rendering plugins n'est pas supporté pour le moment.
// Pour plus d'informations, visiter http://www.freeframe.org
// Exemple:
// (clear)
//
// (define p (build-pixels 256 256 #t)) ; input pixelprimitive
//
// (translate (vector 1.1 0 0))
// ; output pixelprimitive - rendering is not active
// ; otherwise it would overwrite the plugin output
// (define op (build-pixels 256 256))
//
// ; load the FFGLTile plugin from the FreeFrame SDK
// (define plugin (ffgl-load "FFGLTile" 256 256))
//
// (with-ffgl plugin
//   (for ([i (ffgl-get-info)]) ; print plugin information
//        (printf "~a~n" i))
//   (printf "~a~n" (ffgl-get-parameters)) ; parameter names as strings
//   (ffgl-process op p)) ; set destination and source pixelprimitives
//
// (define (anim)
//    ; set plugin parameters as keywords arguments
//    (with-ffgl plugin
//        (ffgl-set-parameter! #:tilex (/ (mouse-x) (vx (get-screen-size)))
//                             #:tiley (/ (mouse-y) (vy (get-screen-size)))))
//    ; render to the input pixelprimitive
//    (with-pixels-renderer p
//        (with-state
//            (clear-colour #(0 1 0))
//            (scale 5)
//            (rotate (vector (* 50 (time)) -17 (* -35 (time))))
//            (draw-cube))))
//
// (every-frame (anim))
// EndSectionDoc

// StartFunctionDoc-en
// ffgl-load filename-string width-number height-number
// Returns: plugininstance-number
// Description:
// Loads an FFGL plugin and returns a plugin instance. Plugin width and height
// have to be the same as the resolution of the pixel primitive you are about to
// process with the plugin.
// Example:
// (clear)
// ; load the FFGLTile plugin from the FreeFrame SDK
// (define plugin (ffgl-load "FFGLTile" 256 256))
// EndFunctionDoc

// StartFunctionDoc-pt
// ffgl-load string-nome-do-arquivo tamanho-comprimento tamanho-altura
// Retorna: número-da-instancia-do-plugin
// Descrição:
// Carrega um plugin FFGL e retorna uma sua instância. Altura e
// comprimento do plugin tem de ser da mesma resolução da primitiva
// piexel que você está prestes a usar.
// Exemplo:
// (clear)
// ; load the FFGLTile plugin from the FreeFrame SDK
// (define plugin (ffgl-load "FFGLTile" 256 256))
// EndFunctionDoc

// StartFunctionDoc-fr
// ffgl-load nom_de_fichier-string largeur-nombre hauteur-nombre
// Retour: plugininstance-nombre
// Description:
// Charge un plugin FFGL et retourne une instance du plugin. La largeur et hauteur
// doivent être la même que la résolution de la pixel primitive que vous
// transformez avec le plugin.
// Exemple:
// (clear)
// ; load the FFGLTile plugin from the FreeFrame SDK
// (define plugin (ffgl-load "FFGLTile" 256 256))
// EndFunctionDoc

Scheme_Object *ffgl_load(int argc, Scheme_Object **argv)
{
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, argv);
	MZ_GC_REG();
	ArgCheck("ffgl-load", "sii", argc, argv);
	unsigned ret = FFGLManager::Get()->Load(StringFromScheme(argv[0]),
			IntFromScheme(argv[1]), IntFromScheme(argv[2]));
	MZ_GC_UNREG();
	if (ret == 0)
		return scheme_void;
	else
		return scheme_make_integer_value(ret);
}

Scheme_Object *ffgl_push(int argc, Scheme_Object **argv)
{
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, argv);
	MZ_GC_REG();
	ArgCheck("ffgl-push", "i", argc, argv);
	FFGLManager::Get()->Push(IntFromScheme(argv[0]));
	MZ_GC_UNREG();
	return scheme_void;
}

Scheme_Object *ffgl_pop(int argc, Scheme_Object **argv)
{
	FFGLManager::Get()->Pop();
	return scheme_void;
}

// StartFunctionDoc-en
// ffgl-get-info
// Returns: (list of plugin-version-number plugin-id-string plugin-name-string
//                   plugin-type-symbol plugint-description-string plugin-about-string)
// Description:
// Returns plugin information.
// Example:
// (clear)
// (define plugin (ffgl-load "FFGLTile" 256 256))
//
// (with-ffgl plugin
//   (for ([i (ffgl-get-info)]) ; print plugin information
//        (printf "~a~n" i)))
// EndFunctionDoc

// StartFunctionDoc-pt
// ffgl-get-info
// Retorna: (lista de numero-versao-plugin string-id-plugin string-nome-plugin
//                    tipo-simbolo-plugin string-descriçao plugin string-sobre-plugin)
// Descrição:
// Retorna informação sobre o plugin.
// Exemplo:
// (clear)
// (define plugin (ffgl-load "FFGLTile" 256 256))
//
// (with-ffgl plugin
//   (for ([i (ffgl-get-info)]) ; print plugin information
//        (printf "~a~n" i)))
// EndFunctionDoc

// StartFunctionDoc-fr
// ffgl-get-info
// Retour: (liste de plugin-version-nombre plugin-id-string plugin-nom-string
//                   plugin-type-symbol plugin-description-string plugin-about-string)
// Description:
// Retourne les informations du plugin.
// Exemple:
// (clear)
// (define plugin (ffgl-load "FFGLTile" 256 256))
//
// (with-ffgl plugin
//   (for ([i (ffgl-get-info)]) ; print plugin information
//        (printf "~a~n" i)))
// EndFunctionDoc

Scheme_Object *ffgl_get_info(int argc, Scheme_Object **argv)
{
	Scheme_Object *info[6];
	Scheme_Object *ret = NULL;
	MZ_GC_DECL_REG(4);
	MZ_GC_ARRAY_VAR_IN_REG(0, info, 6);
	MZ_GC_VAR_IN_REG(3, ret);
	MZ_GC_REG();

	FFGLPluginInstance *pi = FFGLManager::Get()->Current();
	if (pi == NULL)
	{
		Trace::Stream << "ffgl-get-info can only be called while an FFGL plugin is grabbed" << endl;
		MZ_GC_UNREG();
		return scheme_void;
	}
	FFGLPlugin *p = pi->plugin;

	info[0] = scheme_make_double(p->PluginVersion);
	info[1] = scheme_make_utf8_string(p->PluginID);
	info[2] = scheme_make_utf8_string(p->PluginName);
	info[3] = (p->PluginType == FFGLPlugin::FFGL_EFFECT) ?
		scheme_intern_symbol("effect") :
		scheme_intern_symbol("source");
	info[4] = scheme_make_utf8_string(p->PluginDescription.c_str());
	info[5] = scheme_make_utf8_string(p->PluginAbout.c_str());

	ret = scheme_build_list(6, info);
	MZ_GC_UNREG();
	return ret;
}

// StartFunctionDoc-en
// ffgl-get-parameters
// Returns: parameter-string-list
// Description:
// Returns the list of parameters.
// Example:
// (clear)
// (define plugin (ffgl-load "FFGLTile" 256 256))
//
// (with-ffgl plugin
//   (printf "~a~n" (ffgl-get-parameters)))
// EndFunctionDoc

// StartFunctionDoc-pt
// ffgl-get-parameters
// Retorna: lista-string-parametros
// Descrição:
// Retorna a lista com os parametros
// Exemplo:
// (clear)
// (define plugin (ffgl-load "FFGLTile" 256 256))
//
// (with-ffgl plugin
//   (printf "~a~n" (ffgl-get-parameters)))
// EndFunctionDoc

// StartFunctionDoc-fr
// ffgl-get-parameters
// Retour: paramètres-string-liste
// Description:
// Retourne la liste des paramètres du plugin.
// Exemple:
// (clear)
// (define plugin (ffgl-load "FFGLTile" 256 256))
//
// (with-ffgl plugin
//   (printf "~a~n" (ffgl-get-parameters)))
// EndFunctionDoc

Scheme_Object *ffgl_get_parameters(int argc, Scheme_Object **argv)
{
	FFGLPluginInstance *pi = FFGLManager::Get()->Current();
	if (pi == NULL)
	{
		Trace::Stream << "ffgl-get-parameters can only be called while an FFGL plugin is grabbed" << endl;
		return scheme_void;
	}
	FFGLPlugin *p = pi->plugin;

	map<string, FFGLParameter> parameters = p->GetParameters();

	int n = parameters.size();
	map<string, FFGLParameter>::iterator i = parameters.begin();

	Scheme_Object *params[n];
	Scheme_Object *ret = NULL;
	MZ_GC_DECL_REG(4);
	MZ_GC_ARRAY_VAR_IN_REG(0, params, n);
	MZ_GC_VAR_IN_REG(3, ret);
	MZ_GC_REG();
	for (int j = 0; j < n; j++, ++i)
	{
		params[j] = scheme_make_utf8_string(i->first.c_str());
	}

	ret = scheme_build_list(n, params);
	MZ_GC_UNREG();
	return ret;
}

// StartFunctionDoc-en
// ffgl-get-parameter-default parameter-name-symbol
// Returns: default-parameter-value
// Description:
// Returns the default parameter value for the given parameter.
// Example:
// (clear)
// (define plugin (ffgl-load "FFGLTile" 256 256))
//
// (with-ffgl plugin
//   (printf "tilex default: ~a~n" (ffgl-get-parameter-default 'tilex)))
// EndFunctionDoc

// StartFunctionDoc-pt
// ffgl-get-parameter-default simbolo-nome-parametro
// Retorna: valor-padrao-parametro
// Descrição:
// Retorna o valor padrão do parametro dado.
// Exemplo:
// (clear)
// (define plugin (ffgl-load "FFGLTile" 256 256))
//
// (with-ffgl plugin
//   (printf "tilex default: ~a~n" (ffgl-get-parameter-default 'tilex)))
// EndFunctionDoc

// StartFunctionDoc-fr
// ffgl-get-parameter-default paramètre-nom-symbol
// Retour: défaut-paramètre-valeur
// Description:
// Retourne la valeur par défaut du paramètre donné.
// Exemple:
// (clear)
// (define plugin (ffgl-load "FFGLTile" 256 256))
//
// (with-ffgl plugin
//   (printf "tilex default: ~a~n" (ffgl-get-parameter-default 'tilex)))
// EndFunctionDoc

Scheme_Object *ffgl_get_parameter_default(int argc, Scheme_Object **argv)
{
	FFGLPluginInstance *pi = FFGLManager::Get()->Current();
	if (pi == NULL)
	{
		Trace::Stream << "ffgl-get-parameter-default can only be called while an FFGL plugin is grabbed" << endl;
		return scheme_void;
	}
	FFGLPlugin *p = pi->plugin;

	Scheme_Object *ret = NULL;
	MZ_GC_DECL_REG(2);
	MZ_GC_VAR_IN_REG(0, argv);
	MZ_GC_VAR_IN_REG(1, ret);
	MZ_GC_REG();

	ArgCheck("ffgl-get-parameter-default", "S", argc, argv);
	string pname = SCHEME_SYM_VAL(argv[0]);
	float f;
	const char *str;
	if (!(p->GetDefaultValue(pname, &f, &str)))
	{
		Trace::Stream << "ffgl-get-parameter-default: cannot find parameter " << pname << endl;
		MZ_GC_UNREG();
		return scheme_void;
	}
	if (str == NULL)
	{
		ret = scheme_make_double(f);
	}
	else
	{
		ret = scheme_make_utf8_string(str);
	}

	MZ_GC_UNREG();
	return ret;
}

// StartFunctionDoc-en
// ffgl-get-parameter parameter-name-symbol
// Returns: parameter-value
// Description:
// Returns the current value of the given parameter.
// Example:
// (define plugin (ffgl-load "FFGLTile" 256 256))
//
// (with-ffgl plugin
//   (printf "tilex default: ~a~n" (ffgl-get-parameter 'tilex)))
// EndFunctionDoc

// StartFunctionDoc-pt
// ffgl-get-parameter simbolo-nome-parametro
// Retorna: valor-parametro
// Descrição:
// Retorna o valor atual do parametro dado.
// Exemplo:
// (define plugin (ffgl-load "FFGLTile" 256 256))
//
// (with-ffgl plugin
//   (printf "tilex default: ~a~n" (ffgl-get-parameter 'tilex)))
// EndFunctionDoc

// StartFunctionDoc-fr
// ffgl-get-parameter paramètre-nom-symbol
// Retour: paramètre-valeur
// Description:
// Retourne la valeur actuelle du paramètre donné.
// Exemple:
// (define plugin (ffgl-load "FFGLTile" 256 256))
//
// (with-ffgl plugin
//   (printf "tilex default: ~a~n" (ffgl-get-parameter 'tilex)))
// EndFunctionDoc

Scheme_Object *ffgl_get_parameter(int argc, Scheme_Object **argv)
{
	FFGLPluginInstance *pi = FFGLManager::Get()->Current();
	if (pi == NULL)
	{
		Trace::Stream << "ffgl-get-parameter can only be called while an FFGL plugin is grabbed" << endl;
		return scheme_void;
	}
	FFGLPlugin *p = pi->plugin;

	Scheme_Object *ret = NULL;
	MZ_GC_DECL_REG(2);
	MZ_GC_VAR_IN_REG(0, argv);
	MZ_GC_VAR_IN_REG(1, ret);
	MZ_GC_REG();

	ArgCheck("ffgl-get-parameter", "S", argc, argv);
	string pname = SCHEME_SYM_VAL(argv[0]);
	float f;
	const char *str;
	if (!(p->GetParameter(pi, pname, &f, &str)))
	{
		Trace::Stream << "ffgl-get-parameter: cannot find parameter " << pname << endl;
		MZ_GC_UNREG();
		return scheme_void;
	}
	if (str == NULL)
	{
		ret = scheme_make_double(f);
	}
	else
	{
		ret = scheme_make_utf8_string(str);
	}

	MZ_GC_UNREG();
	return ret;
}

// StartFunctionDoc-en
// ffgl-activate boolean
// Returns: void
// Description:
// Activates, deactivates the plugin.
// Example:
// (clear)
// (define plugin (ffgl-load "FFGLTile" 256 256))
//
// (with-ffgl plugin
//   (ffgl-activate #t))
// EndFunctionDoc

// StartFunctionDoc-pt
// ffgl-activate booleano
// Retorna: void
// Descrição:
// Ativa, desativa o plugin.
// Exemplo:
// (clear)
// (define plugin (ffgl-load "FFGLTile" 256 256))
//
// (with-ffgl plugin
//   (ffgl-activate #t))
// EndFunctionDoc

// StartFunctionDoc-fr
// ffgl-activate booléen
// Retour: void
// Description:
// Active, desactive le plugin.
// Exemple:
// (clear)
// (define plugin (ffgl-load "FFGLTile" 256 256))
//
// (with-ffgl plugin
//   (ffgl-activate #t))
// EndFunctionDoc

Scheme_Object *ffgl_activate(int argc, Scheme_Object **argv)
{
	FFGLPluginInstance *pi = FFGLManager::Get()->Current();
	if (pi == NULL)
	{
		Trace::Stream << "ffgl-activate can only be called while an FFGL plugin is grabbed" << endl;
		return scheme_void;
	}

	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, argv);
	MZ_GC_REG();

	ArgCheck("ffgl-activate", "b", argc, argv);

	pi->Activate(BoolFromScheme(argv[0]));

	MZ_GC_UNREG();
	return scheme_void;
}

// StartFunctionDoc-en
// ffgl-active?
// Returns: boolean
// Description:
// Returns #t if the plugin is active, or #f otherwise.
// Example:
// (clear)
// (define plugin (ffgl-load "FFGLTile" 256 256))
//
// (with-ffgl plugin
//   (when (ffgl-active?)
//     (display "plugin is active")))
// EndFunctionDoc

// StartFunctionDoc-pt
// ffgl-active?
// Retorna: booleano
// Descrição:
// Retorna #t se o plugin está ativo, ou #f se não.
// Exemplo:
// (clear)
// (define plugin (ffgl-load "FFGLTile" 256 256))
//
// (with-ffgl plugin
//   (when (ffgl-active?)
//     (display "plugin is active")))
// EndFunctionDoc

// StartFunctionDoc-fr
// ffgl-active?
// Retour: booléen
// Description:
// Retourne #t si le plugin est actif, sinon #f.
// Exemple:
// (clear)
// (define plugin (ffgl-load "FFGLTile" 256 256))
//
// (with-ffgl plugin
//   (when (ffgl-active?)
//     (display "plugin is active")))
// EndFunctionDoc

Scheme_Object *ffgl_active(int argc, Scheme_Object **argv)
{
	FFGLPluginInstance *pi = FFGLManager::Get()->Current();
	if (pi == NULL)
	{
		Trace::Stream << "ffgl-active? can only be called while an FFGL plugin is grabbed" << endl;
		return scheme_void;
	}

	Scheme_Object *ret;
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, ret);
	MZ_GC_REG();

	ret = pi->Active() ? scheme_true : scheme_false;

	MZ_GC_UNREG();
	return ret;
}

Scheme_Object *ffgl_set_parameter_list(int argc, Scheme_Object **argv)
{
	FFGLPluginInstance *pi = FFGLManager::Get()->Current();
	if (pi == NULL)
	{
		Trace::Stream << "ffgl-set-parameter! can only be called while an FFGL plugin is grabbed" << endl;
		return scheme_void;
	}
	FFGLPlugin *p = pi->plugin;

	Scheme_Object *params = NULL;
	MZ_GC_DECL_REG(2);
	MZ_GC_VAR_IN_REG(0, argv);
	MZ_GC_VAR_IN_REG(1, params);
	MZ_GC_REG();

	ArgCheck("ffgl-set-parameter-list", "l", argc, argv);

	params = scheme_list_to_vector(argv[0]);

	for (int n = 0; n < SCHEME_VEC_SIZE(params); n += 2)
	{
		if (SCHEME_SYMBOLP(SCHEME_VEC_ELS(params)[n]) && SCHEME_VEC_SIZE(params) > (n + 1))
		{
			// get the parameter name
			string pname = SCHEME_SYM_VAL(SCHEME_VEC_ELS(params)[n]);
			if (SCHEME_NUMBERP(SCHEME_VEC_ELS(params)[n + 1]))
			{
				float f = FloatFromScheme(SCHEME_VEC_ELS(params)[n + 1]);
				if (!(p->SetParameter(pi, pname, f)))
				{
					Trace::Stream << "ffgl-set-parameter!: cannot set parameter " << pname << endl;
				}
			}
			else
				if (SCHEME_CHAR_STRINGP(SCHEME_VEC_ELS(params)[n + 1]))
				{
					string s = StringFromScheme(SCHEME_VEC_ELS(params)[n + 1]);
					if (!(p->SetParameter(pi, pname, s)))
					{
						Trace::Stream << "ffgl-set-parameter!: cannot set parameter " << pname << endl;
					}
				}
				else
				{
					Trace::Stream << "ffgl-set-parameter!: wrong parameter type " << pname << endl;
				}
		}
		else
		{
			Trace::Stream << "ffgl-set-parameter-list: wrong parameter list" << endl;
		}
	}

	MZ_GC_UNREG();
	return scheme_void;
}

// StartFunctionDoc-en
// ffgl-get-min-inputs
// Returns: number
// Description:
// Returns the minimum number of input pixel primitives the plugin requires.
// Example:
// (clear)
// (define plugin (ffgl-load "FFGLTile" 256 256))
//
// (with-ffgl plugin
//   (printf "~a~n" (ffgl-get-min-inputs)))
// EndFunctionDoc

// StartFunctionDoc-pt
// ffgl-get-min-inputs
// Retorna: número
// Descrição:
// Retorna o número mínimo de primitivas pixel que o plugin precisa.
// Exemplo:
// (clear)
// (define plugin (ffgl-load "FFGLTile" 256 256))
//
// (with-ffgl plugin
//   (printf "~a~n" (ffgl-get-min-inputs)))
// EndFunctionDoc

// StartFunctionDoc-fr
// ffgl-get-min-inputs
// Retour: nombre
// Description:
// Retourne le nombre minimum d'entrées de pixel primitives que le plugin exige.
// Exemple:
// (clear)
// (define plugin (ffgl-load "FFGLTile" 256 256))
//
// (with-ffgl plugin
//   (printf "~a~n" (ffgl-get-min-inputs)))
// EndFunctionDoc

Scheme_Object *ffgl_get_min_inputs(int argc, Scheme_Object **argv)
{
	FFGLPluginInstance *pi = FFGLManager::Get()->Current();
	if (pi == NULL)
	{
		Trace::Stream << "ffgl-get-min-inputs can only be called while an FFGL plugin is grabbed" << endl;
		return scheme_void;
	}
	FFGLPlugin *p = pi->plugin;

	Scheme_Object *ret;
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, ret);
	MZ_GC_REG();

	ret = scheme_make_integer(p->GetMinInputs());

	MZ_GC_UNREG();
	return ret;
}

// StartFunctionDoc-en
// ffgl-get-max-inputs
// Returns: number
// Description:
// Returns the maximum number of input pixel primitives the plugin accepts.
// Example:
// (clear)
// (define plugin (ffgl-load "FFGLTile" 256 256))
//
// (with-ffgl plugin
//   (printf "~a~n" (ffgl-get-max-inputs)))
// EndFunctionDoc

// StartFunctionDoc-pt
// ffgl-get-max-inputs
// Retorna: número
// Descrição:
// Retorna o número máximo de primitivas pixel que o plugin aceita.
// Exemplo:
// (clear)
// (define plugin (ffgl-load "FFGLTile" 256 256))
//
// (with-ffgl plugin
//   (printf "~a~n" (ffgl-get-max-inputs)))
// EndFunctionDoc

// StartFunctionDoc-fr
// ffgl-get-max-inputs
// Retour: nombre
// Description:
// Retourne le nombre maximum d'entrées de pixel primitives que le plugin exige.
// Exemple:
// (clear)
// (define plugin (ffgl-load "FFGLTile" 256 256))
//
// (with-ffgl plugin
//   (printf "~a~n" (ffgl-get-max-inputs)))
// EndFunctionDoc

Scheme_Object *ffgl_get_max_inputs(int argc, Scheme_Object **argv)
{
	FFGLPluginInstance *pi = FFGLManager::Get()->Current();
	if (pi == NULL)
	{
		Trace::Stream << "ffgl-get-max-inputs can only be called while an FFGL plugin is grabbed" << endl;
		return scheme_void;
	}
	FFGLPlugin *p = pi->plugin;

	Scheme_Object *ret;
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, ret);
	MZ_GC_REG();

	ret = scheme_make_integer(p->GetMaxInputs());

	MZ_GC_UNREG();
	return ret;
}

// StartFunctionDoc-en
// ffgl-set-time! time-number
// Returns: void
// Description:
// Sets the time in seconds.
// Example:
// (clear)
// (define plugin (ffgl-load "FFGLTime" 256 256))
//
// (with-ffgl plugin
//   (ffgl-set-time! (time)))
// EndFunctionDoc

// StartFunctionDoc-pt
// ffgl-set-time! número-tempo
// Retorna: void
// Descrição:
// Ajusta o tempo em segundos
// Exemplo:
// (clear)
// (define plugin (ffgl-load "FFGLTime" 256 256))
//
// (with-ffgl plugin
//   (ffgl-set-time! (time)))
// EndFunctionDoc

// StartFunctionDoc-fr
// ffgl-set-time! temps-nombre
// Retour: void
// Description:
// Définis le temps en secondes.
// Exemple:
// (clear)
// (define plugin (ffgl-load "FFGLTime" 256 256))
//
// (with-ffgl plugin
//   (ffgl-set-time! (time)))
// EndFunctionDoc

Scheme_Object *ffgl_set_time(int argc, Scheme_Object **argv)
{
	FFGLPluginInstance *pi = FFGLManager::Get()->Current();
	if (pi == NULL)
	{
		Trace::Stream << "ffgl-set-time! can only be called while an FFGL plugin is grabbed" << endl;
		return scheme_void;
	}
	FFGLPlugin *p = pi->plugin;

	DECL_ARGV();
	ArgCheck("ffgl-set-time!", "f", argc, argv);

	double time = DoubleFromScheme(argv[0]);
	if (!(p->SetTime(pi, time)))
	{
		Trace::Stream << "ffgl-set-time!: cannot set time" << endl;
	}

	MZ_GC_UNREG();
	return scheme_void;
}

// StartFunctionDoc-en
// ffgl-process output-pixelprimitiveid-number output-textureid-number input-textureid-number ...
// Returns: void
// Description:
// Sets output pixel primitive, output texture and input textures for the
// grabbed plugin. The resolution of the pixel primitive and the textures
// have to be same as the resolution the plugin initialised. This is automatic
// if the textures belong to the same pixel primitive.
// Example:
// (clear)
// (define p (build-pixels 256 256 #t 2))
//
// (define plugin (ffgl-load "FFGLTile" 256 256))
//
// (with-ffgl plugin
//    (ffgl-process p
//                  (pixels->texture p 1) ; output
//                  (pixels->texture p 0))) ; input
//
// (with-primitive p
//    (pixels-render-to (pixels->texture p 0))
//    (pixels-display (pixels->texture p 1)))
//
// (define (anim)
//    (with-pixels-renderer p
//        (with-state
//            (clear-colour #(0 1 0))
//            (scale 5)
//            (rotate (vector (* 50 (time)) -17 (* -35 (time))))
//            (draw-cube))))
//
// (every-frame (anim))
// EndFunctionDoc

// StartFunctionDoc-pt
// ffgl-process numero-saida-primitivapixelid número-entrada-primitivapixelid
// Retorna: void
// Descrição:
// Ajusta primitivas pixel de saída e entrada para a primitiva pega. A
// resolução das primitivas pixel tem de ser as mesmas do plugin inicializado.
// Exemplo:
// (clear)
//
// (define p (build-pixels 256 256 #t))
// (define op (build-pixels 256 256))
//
// (define plugin (ffgl-load "FFGLTile" 256 256))
//
// (with-ffgl plugin
//   (ffgl-process op p))
//
// (define (anim)
//    (with-pixels-renderer p
//        (with-state
//            (clear-colour #(0 1 0))
//            (scale 5)
//            (rotate (vector (* 50 (time)) -17 (* -35 (time))))
//            (draw-cube))))
//
// (every-frame (anim))
// EndFunctionDoc

// StartFunctionDoc-fr
// ffgl-process sortie-pixelprimitiveid-nombre entrée-pixelprimitiveid-nombre ...
// Retour: void
// Description:
// Définit la sortie et entrée pour le plugin saisi.
// La résolution des pixel primitives doit être à la même résolution
// que le plugin initialisé.
// Exemple:
// (clear)
//
// (define p (build-pixels 256 256 #t))
// (define op (build-pixels 256 256))
//
// (define plugin (ffgl-load "FFGLTile" 256 256))
//
// (with-ffgl plugin
//   (ffgl-process op p))
//
// (define (anim)
//    (with-pixels-renderer p
//        (with-state
//            (clear-colour #(0 1 0))
//            (scale 5)
//            (rotate (vector (* 50 (time)) -17 (* -35 (time))))
//            (draw-cube))))
//
// (every-frame (anim))
// EndFunctionDoc

Scheme_Object *ffgl_process(int argc, Scheme_Object **argv)
{
	FFGLPluginInstance *pi = FFGLManager::Get()->Current();
	if (pi == NULL)
	{
		Trace::Stream << "ffgl-process can only be called while an FFGL plugin is grabbed" << endl;
		return scheme_void;
	}

	Scheme_Object *ret = NULL;
	MZ_GC_DECL_REG(2);
	MZ_GC_VAR_IN_REG(0, argv);
	MZ_GC_VAR_IN_REG(1, ret);
	MZ_GC_REG();

	PixelPrimitive *pp = NULL;

	if (SCHEME_NUMBERP(argv[0]))
	{
		Primitive *prim = Engine::Get()->Renderer()->GetPrimitive(IntFromScheme(argv[0]));
		pp = dynamic_cast<PixelPrimitive *>(prim);
	}

	if (pp == NULL)
	{
		Trace::Stream << "ffgl-process can only be called on a pixelprimitive" << endl;
		MZ_GC_UNREG();
		return scheme_void;
	}

	vector<unsigned> textures;
	for (int i = 1; i < argc; i++)
	{
		if (SCHEME_NUMBERP(argv[i]))
		{
			textures.push_back(IntFromScheme(argv[i]));
		}
		else
		{
			textures.clear();
			Trace::Stream << "ffgl-process can only be called with texture ids" << endl;
			MZ_GC_UNREG();
			return scheme_void;
		}
	}

	pi->SetPixels(pp, textures);
	textures.clear();
	MZ_GC_UNREG();
	return scheme_void;
}

// StartFunctionDoc-en
// ffgl-clear-instances
// Returns: void
// Description:
// Clears FFGL plugin instances.
// Example:
// (ffgl-clear-instances)
// EndFunctionDoc

// StartFunctionDoc-pt
// ffgl-clear-instances
// Retorna: void
// Descrição:
// Limpa as instancias do plugin FFGL
// Exemplo:
// (ffgl-clear-instances)
// EndFunctionDoc

// StartFunctionDoc-fr
// ffgl-clear-instances
// Retour: void
// Description:
// Efface les instances des plugin FFGL
// Exemple:
// (ffgl-clear-instances)
// EndFunctionDoc

Scheme_Object *ffgl_clear_instances(int argc, Scheme_Object **argv)
{
	FFGLManager::Get()->ClearInstances();
	return scheme_void;
}

// StartFunctionDoc-en
// ffgl-clear-cache
// Returns: void
// Description:
// Clears FFGL plugin cache and instances.
// Example:
// (ffgl-clear-cache)
// EndFunctionDoc

// StartFunctionDoc-pt
// ffgl-clear-cache
// Retorna: void
// Descrição:
// Limpa o cache e instancias do plugin FFGL
// Exemplo:
// (ffgl-clear-cache)
// EndFunctionDoc

// StartFunctionDoc-fr
// ffgl-clear-cache
// Retour: void
// Description:
// Efface les instances et le cache des plugin FFGL
// Exemple:
// (ffgl-clear-cache)
// EndFunctionDoc

Scheme_Object *ffgl_clear_cache(int argc, Scheme_Object **argv)
{
	FFGLManager::Shutdown();
	return scheme_void;
}

void FFGLFunctions::AddGlobals(Scheme_Env *env)
{
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, env);
	MZ_GC_REG();
	scheme_add_global("ffgl-load", scheme_make_prim_w_arity(ffgl_load, "ffgl-load", 3, 3), env);
	scheme_add_global("ffgl-push", scheme_make_prim_w_arity(ffgl_push, "ffgl-push", 1, 1), env);
	scheme_add_global("ffgl-pop", scheme_make_prim_w_arity(ffgl_pop, "ffgl-pop", 0, 0), env);
	scheme_add_global("ffgl-get-info", scheme_make_prim_w_arity(ffgl_get_info, "ffgl-get-info", 0, 0), env);
	scheme_add_global("ffgl-get-parameters", scheme_make_prim_w_arity(ffgl_get_parameters, "ffgl-get-parameters", 0, 0), env);
	scheme_add_global("ffgl-get-parameter-default", scheme_make_prim_w_arity(ffgl_get_parameter_default, "ffgl-get-parameter-default", 1, 1), env);
	scheme_add_global("ffgl-get-parameter", scheme_make_prim_w_arity(ffgl_get_parameter, "ffgl-get-parameter", 1, 1), env);
	scheme_add_global("ffgl-set-parameter-list", scheme_make_prim_w_arity(ffgl_set_parameter_list, "ffgl-set-parameter-list", 1, 1), env);
	scheme_add_global("ffgl-activate", scheme_make_prim_w_arity(ffgl_activate, "ffgl-activate", 1, 1), env);
	scheme_add_global("ffgl-active?", scheme_make_prim_w_arity(ffgl_active, "ffgl-active?", 0, 0), env);
	scheme_add_global("ffgl-get-min-inputs", scheme_make_prim_w_arity(ffgl_get_min_inputs, "ffgl_get_min_inputs", 0, 0), env);
	scheme_add_global("ffgl-get-max-inputs", scheme_make_prim_w_arity(ffgl_get_max_inputs, "ffgl_get_max_inputs", 0, 0), env);
	scheme_add_global("ffgl-set-time!", scheme_make_prim_w_arity(ffgl_set_time, "ffgl_set_time!", 1, 1), env);
	scheme_add_global("ffgl-process", scheme_make_prim_w_arity(ffgl_process, "ffgl-process", 1, -1), env);
	scheme_add_global("ffgl-clear-instances", scheme_make_prim_w_arity(ffgl_clear_instances, "ffgl-clear-instances", 0, 0), env);
	scheme_add_global("ffgl-clear-cache", scheme_make_prim_w_arity(ffgl_clear_cache, "ffgl-clear-cache", 0, 0), env);
	MZ_GC_UNREG();
}

