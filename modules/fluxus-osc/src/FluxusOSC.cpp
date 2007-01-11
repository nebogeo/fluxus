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
#include <iostream>
#include "OSCServer.h"

using namespace std;
using namespace fluxus;

Server *OSCServer = NULL;
Client *OSCClient = NULL;

Scheme_Object *osc_source(int argc, Scheme_Object **argv)
{
	if (!SCHEME_CHAR_STRINGP(argv[0])) scheme_wrong_type("osc-source", "string", 0, argc, argv);
	char *port=scheme_utf8_encode_to_buffer(SCHEME_CHAR_STR_VAL(argv[0]),SCHEME_CHAR_STRLEN_VAL(argv[0]),NULL,0);

	if (!OSCServer)
	{
		OSCServer = new Server(port);
		OSCServer->Run();
	}
	else
	{
		OSCServer->SetPort(port);
	}

    return scheme_void;
}

Scheme_Object *osc_msg(int argc, Scheme_Object **argv)
{
	if (!SCHEME_CHAR_STRINGP(argv[0])) scheme_wrong_type("osc-msg", "string", 0, argc, argv);
	char *name=scheme_utf8_encode_to_buffer(SCHEME_CHAR_STR_VAL(argv[0]),SCHEME_CHAR_STRLEN_VAL(argv[0]),NULL,0);
	if (OSCServer->SetMsg(name))
	{
		return scheme_make_true();
	}
	return scheme_make_false();
}

Scheme_Object *osc(int argc, Scheme_Object **argv)
{
	if (!SCHEME_NUMBERP(argv[0])) scheme_wrong_type("osc", "number", 0, argc, argv);
 
	unsigned int index=(unsigned int)scheme_real_to_double(argv[0]);
	if (OSCServer!=NULL)
	{
		vector<OSCData*> args;
		OSCServer->GetArgs(args);
		char type = args[index]->Type();
		
		Scheme_Object *ret;
		if (type=='f') ret=scheme_make_double(static_cast<OSCFloat*>(args[index])->Value);
		else if (type=='i') ret=scheme_make_integer_value_from_unsigned(static_cast<OSCInt*>(args[index])->Value);
		else if (type=='s') 
		{
			string value=static_cast<OSCString*>(args[index])->Value;
			ret=scheme_make_utf8_string(value.c_str());	
		}
		else ret=scheme_void;
		
		return ret;
	}
	return scheme_void;
}

Scheme_Object *osc_destination(int argc, Scheme_Object **argv)
{
	if (!SCHEME_CHAR_STRINGP(argv[0])) scheme_wrong_type("osc-destination", "string", 0, argc, argv);
	char *port=scheme_utf8_encode_to_buffer(SCHEME_CHAR_STR_VAL(argv[0]),SCHEME_CHAR_STRLEN_VAL(argv[0]),NULL,0);
	if (!OSCClient)
	{
		OSCClient = new Client();
	}
	OSCClient->SetDestination(port);
    return scheme_void;
}

Scheme_Object *osc_peek(int argc, Scheme_Object **argv)
{
	return scheme_make_utf8_string(OSCServer->GetLastMsg().c_str());	
}

Scheme_Object *osc_send(int argc, Scheme_Object **argv)
{
	if (!SCHEME_CHAR_STRINGP(argv[0])) scheme_wrong_type("osc-send", "string", 0, argc, argv);
	if (!SCHEME_CHAR_STRINGP(argv[1])) scheme_wrong_type("osc-send", "string", 1, argc, argv);
	if (!SCHEME_LISTP(argv[2])) scheme_wrong_type("osc-send", "list", 2, argc, argv);

	char *msg=scheme_utf8_encode_to_buffer(SCHEME_CHAR_STR_VAL(argv[0]),SCHEME_CHAR_STRLEN_VAL(argv[0]),NULL,0);
	char *types=scheme_utf8_encode_to_buffer(SCHEME_CHAR_STR_VAL(argv[1]),SCHEME_CHAR_STRLEN_VAL(argv[1]),NULL,0);
	
	Scheme_Object *argvec = scheme_list_to_vector(argv[2]);
	Scheme_Object **argptr = SCHEME_VEC_ELS(argvec);
	
	vector<OSCData*> oscargs;
	for (unsigned int n=0; n<(unsigned int)SCHEME_VEC_SIZE(argvec); n++)
	{
		Scheme_Object *arg=argptr[n];

		if (SCHEME_NUMBERP(arg))// ||  scm_is_true(scm_exact_p(arg)) || scm_is_true(scm_inexact_p(arg)))
		{
			if (n<strlen(types))
			{
				if (types[n]=='f') oscargs.push_back(new OSCFloat(scheme_real_to_double(arg)));
				else if (types[n]=='i') 
				{
					unsigned long val=0;
					scheme_get_unsigned_int_val(arg,&val);
					oscargs.push_back(new OSCInt(val));
				}
			}
		}
		else if (SCHEME_CHAR_STRINGP(arg))
		{
			char *argstring=scheme_utf8_encode_to_buffer(SCHEME_CHAR_STR_VAL(arg),SCHEME_CHAR_STRLEN_VAL(arg),NULL,0);;
			oscargs.push_back(new OSCString(argstring));
		}
		else
		{
			cerr<<"osc-send has found an argument type it can't send, numbers and strings only"<<endl;
    		return scheme_void;
		}
	}
	OSCClient->Send(msg,oscargs);
    return scheme_void;
}

/////////////////////

Scheme_Object *scheme_reload(Scheme_Env *env)
{
	// add all the modules from this extension
	Scheme_Env *menv=scheme_primitive_module(scheme_intern_symbol("fluxus-osc"), env);

	scheme_add_global("osc-source", scheme_make_prim_w_arity(osc_source, "osc-source", 1, 1), menv);
	scheme_add_global("osc-msg", scheme_make_prim_w_arity(osc_msg, "osc-msg", 1, 1), menv);
	scheme_add_global("osc", scheme_make_prim_w_arity(osc, "osc", 1, 1), menv);
	scheme_add_global("osc-destination", scheme_make_prim_w_arity(osc_destination, "osc-destination", 1, 1), menv);
	scheme_add_global("osc-peek", scheme_make_prim_w_arity(osc_peek, "osc-peek", 0, 0), menv);
	scheme_add_global("osc-send", scheme_make_prim_w_arity(osc_send, "osc-send", 3, 3), menv);

	scheme_finish_primitive_module(menv);	
	
	return scheme_void;
}

Scheme_Object *scheme_initialize(Scheme_Env *env)
{
	return scheme_reload(env);
}

Scheme_Object *scheme_module_name()
{
	return scheme_intern_symbol("fluxus-osc");
}
