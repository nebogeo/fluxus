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

#include <fstream>
#include <deque>
#include <libguile.h>
#include "FluxusIOBinding.h"
#include "SearchPaths.h"
#include "Repl.h"
#include <LinePrimitive.h>
#include <ParticlePrimitive.h>
#include <PixelPrimitive.h>
#include <LocatorPrimitive.h>

void FluxusIOBinding::RegisterProcs()
{
	SchemePrim::Init();
	
	scm_c_define_gsubr("edit",1,0,0,(CALLBACK_CAST) edit);
	scm_c_define_gsubr("save-name",1,0,0,(CALLBACK_CAST) save_name);
	scm_c_define_gsubr("key-pressed",1,0,0,(CALLBACK_CAST) key_pressed);
	scm_c_define_gsubr("keys-down",0,0,0,(CALLBACK_CAST) keys_down);
	scm_c_define_gsubr("key-special-pressed",1,0,0,(CALLBACK_CAST) key_special_pressed);
	scm_c_define_gsubr("keys-special-down",0,0,0,(CALLBACK_CAST) keys_special_down);
	scm_c_define_gsubr("mouse-over",0,0,0,(CALLBACK_CAST) mouse_over);
	scm_c_define_gsubr("mouse-button",1,0,0,(CALLBACK_CAST) mouse_button);
	scm_c_define_gsubr("mouse-x",0,0,0,(CALLBACK_CAST) mouse_x);
 	scm_c_define_gsubr("mouse-y",0,0,0,(CALLBACK_CAST) mouse_y);
    scm_c_define_gsubr("time",0,0,0,(CALLBACK_CAST) time);
    scm_c_define_gsubr("delta",0,0,0,(CALLBACK_CAST) delta);
    scm_c_define_gsubr("flxrnd",0,0,0,(CALLBACK_CAST) srandom);
	scm_c_define_gsubr("desiredfps",1,0,0,(CALLBACK_CAST) desiredfps);
	scm_c_define_gsubr("start-framedump",2,0,0,(CALLBACK_CAST) start_framedump);
	scm_c_define_gsubr("end-framedump",0,0,0,(CALLBACK_CAST) end_framedump);
	scm_c_define_gsubr("load-code",1,0,0,(CALLBACK_CAST) load_recorded_code);
	scm_c_define_gsubr("save-code",1,0,0,(CALLBACK_CAST) save_recorded_code);
	scm_c_define_gsubr("searchpaths",1,0,0,(CALLBACK_CAST) searchpaths);
	scm_c_define_gsubr("full-path",1,0,0,(CALLBACK_CAST) full_path);
}

SCM FluxusIOBinding::edit(SCM s_name)
{
	SCM_ASSERT(scm_is_string(s_name), s_name, SCM_ARG1, "edit");
    SCM path = scm_sys_search_load_path(s_name);
    if (scm_is_string(path)) 
	{
        Fluxus->LoadScript(scm_to_locale_string(path));
    } 
	else 
	{
        Fluxus->LoadScript(scm_to_locale_string(s_name));
    }
        
    return SCM_UNSPECIFIED;
}


SCM FluxusIOBinding::key_pressed(SCM s_key)
{	
	SCM_ASSERT(scm_is_string(s_key), s_key, SCM_ARG1, "key_pressed");
	char *key=0;
	key=scm_to_locale_string(s_key);
	bool pressed;
	if (m_KeySet.find(key[0])!=m_KeySet.end()) pressed=true;
	else pressed=false;
    free(key);
    return scm_from_bool(pressed);
}

SCM FluxusIOBinding::keys_down()
{
	vector<float> keys;
	for (set<int>::iterator i=m_KeySet.begin(); i!=m_KeySet.end(); i++)
	{
		keys.push_back(*i);
	}
	return flx_floats_to_scm(&(*keys.begin()), keys.size());	
}

SCM FluxusIOBinding::key_special_pressed(SCM s_key)
{	
	SCM_ASSERT(scm_is_number(s_key), s_key, SCM_ARG1, "key-special-pressed");	
	bool pressed;
	if (m_SpecialKeySet.find(scm_to_int(s_key))!=m_SpecialKeySet.end()) pressed=true;
	else pressed=false;
    return scm_from_bool(pressed);
}

SCM FluxusIOBinding::keys_special_down()
{
	vector<float> keys;
	for (set<int>::iterator i=m_SpecialKeySet.begin(); i!=m_SpecialKeySet.end(); i++)
	{
		keys.push_back(*i);
	}
	return flx_floats_to_scm(&(*keys.begin()), keys.size());	
}

SCM FluxusIOBinding::time()
{
	timeval time;
	gettimeofday(&time,NULL);
	return scm_from_double(time.tv_sec+time.tv_usec*0.000001f);
}

SCM FluxusIOBinding::delta()
{
	return scm_from_double(Fluxus->GetRenderer()->GetDelta());
}

SCM FluxusIOBinding::save_name(SCM s_name)
{
	SCM_ASSERT(scm_is_string(s_name), s_name, SCM_ARG1, "save_name");	
	char *name=scm_to_locale_string(s_name);
	Fluxus->SetSaveName(name);
	free(name);
    return SCM_UNSPECIFIED;
}

SCM FluxusIOBinding::source(SCM s_name)
{
	SCM_ASSERT(scm_is_string(s_name), s_name, SCM_ARG1, "source");	
	char *name=scm_to_locale_string(s_name);
	Fluxus->SourceScript(name);
	free(name);
    return SCM_UNSPECIFIED;
}


SCM FluxusIOBinding::desiredfps(SCM s)
{
	SCM_ASSERT(scm_is_number(s), s, SCM_ARG1, "desiredfps");
	Fluxus->GetRenderer()->SetDesiredFPS(scm_to_double(s));
	return SCM_UNSPECIFIED;
}

SCM FluxusIOBinding::start_framedump(SCM s_name, SCM s_type)
{
	SCM_ASSERT(scm_is_string(s_name), s_name, SCM_ARG1, "start-framedump");	
	SCM_ASSERT(scm_is_string(s_type), s_type, SCM_ARG2, "start-framedump");	
	char *name=scm_to_locale_string(s_name);
	char *type=scm_to_locale_string(s_type);
	Fluxus->StartDumpFrames(name,type);
	free(name);
	free(type);
    return SCM_UNSPECIFIED;
}

SCM FluxusIOBinding::end_framedump()
{
	Fluxus->EndDumpFrames();
    return SCM_UNSPECIFIED;
}

SCM FluxusIOBinding::mouse_over()
{
	int id=Fluxus->GetRenderer()->Select((int)Fluxus->GetMouseX(),(int)Fluxus->GetMouseY(),5);
	if (id > 0)
	{
		return Prim2Smob(id);
	}
	else
	{
		return scm_from_bool(false);
	}
}

SCM FluxusIOBinding::mouse_button(SCM s_b)
{
	SCM_ASSERT(scm_is_number(s_b),  s_b,  SCM_ARG1, "mouse-button");
	int but=scm_to_int(s_b);		
	return scm_from_bool(Fluxus->GetMouseButton()==but);
}

SCM FluxusIOBinding::mouse_x()
{
	return scm_from_int(Fluxus->GetMouseX());
}

SCM FluxusIOBinding::mouse_y()
{
	return scm_from_int(Fluxus->GetMouseY());
}

SCM FluxusIOBinding::load_recorded_code(SCM s_name)
{
	SCM_ASSERT(scm_is_string(s_name), s_name, SCM_ARG1, "load-recorded-code");	
	char *name=scm_to_locale_string(s_name);
	Fluxus->LoadRecordedCode(name);
	free(name);
    return SCM_UNSPECIFIED;
}

SCM FluxusIOBinding::save_recorded_code(SCM s_name)
{
	SCM_ASSERT(scm_is_string(s_name), s_name, SCM_ARG1, "save-recorded-code");	
	char *name=scm_to_locale_string(s_name);
	Fluxus->SaveRecordedCode(name);
	free(name);
    return SCM_UNSPECIFIED;
}

SCM FluxusIOBinding::searchpaths(SCM s_list)
{
	// todo: fix this...
	//SCM_ASSERT(SCM_LISTP(s_list), s_list, SCM_ARG1, "searchpaths");
 	
	// vectors seem easier to handle than lists with this api
	SCM vec = scm_vector(s_list);
	size_t size=0;
	
	for (unsigned int n=0; n<scm_c_generalized_vector_length(vec); n++)
	{
		SCM arg=scm_vector_ref(vec, scm_from_int(n));

		if (scm_is_string(arg))
		{
			char *argstring=scm_to_locale_string(arg);
			SearchPaths::Get()->AddPath(argstring);
			free(argstring);
		}
	}

    return SCM_UNSPECIFIED;
}

SCM FluxusIOBinding::full_path(SCM s_filename)
{
	SCM_ASSERT(scm_is_string(s_filename), s_filename, SCM_ARG1, "full-path");	
	char *name=scm_to_locale_string(s_filename);
	string fullpath = SearchPaths::Get()->GetFullPath(name);
	free(name);
	return scm_from_locale_stringn(fullpath.c_str(),fullpath.length());
}

SCM FluxusIOBinding::srandom()
{
	return scm_from_double(RandFloat());
}
