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
// Example:
// EndSectionDoc

// StartFunctionDoc-en
// ffgl-load
// Returns: plugininstance-number
// Description:
// Example:
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

// StartFunctionDoc-en
// ffgl-push plugininstance-number
// Returns: void
// Description:
// Example:
// EndFunctionDoc
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

// StartFunctionDoc-en
// ffgl-pop
// Returns: void
// Description:
// Example:
// EndFunctionDoc
Scheme_Object *ffgl_pop(int argc, Scheme_Object **argv)
{
	FFGLManager::Get()->Pop();
	return scheme_void;
}

// StartFunctionDoc-en
// ffgl-get-info
// Returns: void
// Description:
// Example:
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
// Returns: parameter-list
// Description:
// Example:
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
// Example:
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
// Returns: default-parameter-value
// Description:
// Example:
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
// ffgl-set-parameter-list (list of parameter-name parameter-value)
// Returns: void
// Description:
// Example:
// EndFunctionDoc
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
// ffgl-process output-pixelprimitiveid-number input-pixelprimitiveid-number
// Returns: void
// Description:
// Example:
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

	vector<PixelPrimitive *> pixels;
	for (int i = 0; i < argc; i++)
	{
		if (SCHEME_NUMBERP(argv[i]))
		{
			Primitive *prim = Engine::Get()->Renderer()->GetPrimitive(IntFromScheme(argv[i]));
			PixelPrimitive *pp = dynamic_cast<PixelPrimitive *>(prim);
			if (pp == NULL)
			{
				pixels.clear();
				Trace::Stream << "ffgl-process can only be called on a pixelprimitive" << endl;
				MZ_GC_UNREG();
				return scheme_void;
			}
			pixels.push_back(pp);
		}
		else
		{
			pixels.clear();
			Trace::Stream << "ffgl-process can only be called on a pixelprimitive" << endl;
			MZ_GC_UNREG();
			return scheme_void;
		}
	}

	pi->SetPixels(pixels);
	pixels.clear();
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
	scheme_add_global("ffgl-process", scheme_make_prim_w_arity(ffgl_process, "ffgl-process", 2, 2), env);
	scheme_add_global("ffgl-clear-instances", scheme_make_prim_w_arity(ffgl_clear_instances, "ffgl-clear-instances", 0, 0), env);
	scheme_add_global("ffgl-clear-cache", scheme_make_prim_w_arity(ffgl_clear_cache, "ffgl-clear-cache", 0, 0), env);
	MZ_GC_UNREG();
}

