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
#include "FluxusOSCBinding.h"
#include "SearchPaths.h"
#include "Repl.h"
#include <LinePrimitive.h>
#include <ParticlePrimitive.h>
#include <PixelPrimitive.h>
#include <LocatorPrimitive.h>

void FluxusOSCBinding::RegisterProcs()
{
	SchemePrim::Init();
	
	scm_c_define_gsubr("osc-source",1,0,0,(CALLBACK_CAST)	osc_source);
	scm_c_define_gsubr("osc",1,0,0,(CALLBACK_CAST)  		osc);
	scm_c_define_gsubr("osc-peek",0 ,0,0,(CALLBACK_CAST)	   osc_peek);
	scm_c_define_gsubr("osc-msg",1,0,0,(CALLBACK_CAST)  	osc_msg);
	scm_c_define_gsubr("osc-destination",1 ,0,0,(CALLBACK_CAST)   osc_destination);
	scm_c_define_gsubr("osc-send",3,0,0,(CALLBACK_CAST) 	osc_send);

}

SCM FluxusOSCBinding::osc_source(SCM s_port)
{
	SCM_ASSERT(scm_is_string(s_port), s_port, SCM_ARG1, "osc_source");	
	char *port=scm_to_locale_string(s_port);
	Fluxus->StartOSC(port);
	free(port);
    return SCM_UNSPECIFIED;
}

SCM FluxusOSCBinding::osc_msg(SCM s_token)
{
	SCM_ASSERT(scm_is_string(s_token), s_token, SCM_ARG1, "msg_osc");
 	char *name=scm_to_locale_string(s_token);
	bool ret=Fluxus->MsgOSC(name);
	free(name);
	return scm_from_bool(ret);	
}

SCM FluxusOSCBinding::osc(SCM s_index)
{
    SCM_ASSERT(scm_is_number(s_index), s_index, SCM_ARG2, "osc");
 
	unsigned int index=(unsigned int)scm_to_double(s_index);
	char type = Fluxus->TypeFromOSC(index);
	SCM ret;
	if (type=='f') ret=scm_from_double(Fluxus->NumberFromOSC(index));
	else if (type=='i') ret=scm_from_uint((int)Fluxus->NumberFromOSC(index));
	else if (type=='s') 
	{
		string value=Fluxus->StringFromOSC(index);
		ret=scm_from_locale_stringn(value.c_str(),value.size());	
	}
	else ret=SCM_UNSPECIFIED;
	return ret;
}

SCM FluxusOSCBinding::osc_destination(SCM s_port)
{
	SCM_ASSERT(scm_is_string(s_port), s_port, SCM_ARG1, "osc_source");	
	char *port=scm_to_locale_string(s_port);
	Fluxus->StartOSCClient(port);
	free(port);
    return SCM_UNSPECIFIED;
}


SCM FluxusOSCBinding::osc_peek()
{
	string value=Fluxus->GetLastMsg();
	return scm_from_locale_stringn(value.c_str(),value.size());	
}

SCM FluxusOSCBinding::osc_send(SCM s_msg, SCM s_types, SCM s_arglist)
{
	SCM_ASSERT(scm_is_string(s_msg), s_msg, SCM_ARG1, "osc-send");	
	SCM_ASSERT(scm_is_string(s_types), s_types, SCM_ARG2, "osc-send");	
	// todo: fix this...
	//SCM_ASSERT(SCM_LISTP(s_arglist), s_arglist, SCM_ARG2, "osc-send");
	char *msg=scm_to_locale_string(s_msg);
	char *types=scm_to_locale_string(s_types);
	
	// vectors seem easier to handle than lists with this api
	SCM argvec = scm_vector(s_arglist);
	
	vector<OSCData*> oscargs;
	for (unsigned int n=0; n<scm_c_generalized_vector_length(argvec); n++)
	{
		SCM arg=scm_vector_ref(argvec, scm_from_int(n));

		if (scm_is_number(arg))// ||  scm_is_true(scm_exact_p(arg)) || scm_is_true(scm_inexact_p(arg)))
		{
			if (n<strlen(types))
			{
				if (types[n]=='f') oscargs.push_back(new OSCFloat(scm_to_double(arg)));
				else if (types[n]=='i') oscargs.push_back(new OSCInt(scm_to_uint(arg)));
			}
		}
		else if (scm_is_string(arg))
		{
			char *argstring=scm_to_locale_string(arg);
			oscargs.push_back(new OSCString(argstring));
			free(argstring);
		}
		else
		{
			cerr<<"osc-send has found an argument type it can't send, numbers and strings only"<<endl;
			free(msg);
			free(types);
    		return SCM_UNSPECIFIED;
		}
	}

	Fluxus->SendOSC(msg,oscargs);
	free(msg);
	free(types);
    return SCM_UNSPECIFIED;
}
