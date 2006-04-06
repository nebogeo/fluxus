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
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <sys/time.h>
#include <iostream>
#include <fstream>
#include <cstdlib>
#include <unistd.h>
#include <cstdio>
#include <libguile.h>
#include "FluxusBinding.h"

using namespace fluxus;

static const string INIT_FILE=".fluxus.scm";
FluxusBinding *binding;

// copied from the guile source handler_message from throw.cpp
// todo: replace with gl overlay error log
SCM ErrorHandler (void *handler_data, SCM tag, SCM args)
{
	char *prog_name = (char *) handler_data;
	SCM p = scm_current_error_port();

	if (scm_ilength (args) >= 3)
	{
		SCM stack   = scm_make_stack (SCM_BOOL_T, SCM_EOL);
		SCM subr    = SCM_CAR (args);
		SCM message = SCM_CADR (args);
		SCM parts   = SCM_CADDR (args);
		SCM rest    = SCM_CDDDR (args);

		if (SCM_BACKTRACE_P && SCM_NFALSEP (stack))
		{
			scm_puts ("Backtrace:\n", p);
			scm_display_backtrace (stack, p, SCM_UNDEFINED, SCM_UNDEFINED);
			scm_newline (p);
		}
		scm_i_display_error (stack, p, subr, message, parts, rest);
	}
	else
	{
		if (! prog_name)
		prog_name = "guile";

		scm_puts (prog_name, p);
		scm_puts (": ", p);

		scm_puts ("uncaught throw to ", p);
		scm_prin1 (tag, p, 0);
		scm_puts (": ", p);
		scm_prin1 (args, p, 1);
		scm_putc ('\n', p);
	}

        // binding->Fluxus->SwitchToRepl();

	return SCM_UNDEFINED;
}

SCM run_scm(void *data)
{
	scm_c_eval_string((char *)data);
	return SCM_UNDEFINED;
}

void DisplayCallback()
{
	binding->Fluxus->TickRecorder();
    
	string fragment = binding->Fluxus->GetScriptFragment();
    if (fragment!="")
    {
		scm_internal_catch(SCM_BOOL_T,run_scm,(void*)fragment.c_str(),ErrorHandler,NULL);
    }
	
	if (binding->Audio!=NULL) binding->Audio->GetFFT();
	binding->Fluxus->Render();	
	glutSwapBuffers();
}

void ReshapeCallback(int width, int height)
{
	binding->Fluxus->Reshape(width,height);
}

void KeyboardCallback(unsigned char key,int x, int y)
{
	binding->Fluxus->Handle(key, -1, -1, -1, x, y, glutGetModifiers());
		
	if (glutGetModifiers()&GLUT_ACTIVE_CTRL)
	{
		// pretty sure this is going to have to change...
		switch(key)
		{
			case 6: glutFullScreen(); break; // f	
			case 23: // w
			{
				glutReshapeWindow(640,480);
				glutPositionWindow(100,100);
			} 
			break;
			case 19: binding->Fluxus->SaveScript(); break; // s			
			case 8: binding->Fluxus->HideScript(); break; // h
			case 13: binding->Fluxus->HideCursor(); break; // m
#ifndef __APPLE__
			case 49: binding->Fluxus->SetCurrentEditor(0); break; // 1
			case 0: binding->Fluxus->SetCurrentEditor(1); break; // 2
			case 27: binding->Fluxus->SetCurrentEditor(2); break; // 3
			case 28: binding->Fluxus->SetCurrentEditor(3); break; // 4
			case 29: binding->Fluxus->SetCurrentEditor(4); break; // 5
			case 30: binding->Fluxus->SetCurrentEditor(5); break; // 6
			case 31: binding->Fluxus->SetCurrentEditor(6); break; // 7
			case 127: binding->Fluxus->SetCurrentEditor(7); break; // 8
			case 57: binding->Fluxus->SetCurrentEditor(8); break; // 9
			case 48: binding->Fluxus->SetCurrentEditor(9); break; // 0
#else
			case 49: binding->Fluxus->SetCurrentEditor(0); break; // 1
			case 50: binding->Fluxus->SetCurrentEditor(1); break; // 2
			case 51: binding->Fluxus->SetCurrentEditor(2); break; // 3
			case 52: binding->Fluxus->SetCurrentEditor(3); break; // 4
			case 53: binding->Fluxus->SetCurrentEditor(4); break; // 5
			case 54: binding->Fluxus->SetCurrentEditor(5); break; // 6
			case 55: binding->Fluxus->SetCurrentEditor(6); break; // 7
			case 56: binding->Fluxus->SetCurrentEditor(7); break; // 8
			case 57: binding->Fluxus->SetCurrentEditor(8); break; // 9
			case 48: binding->Fluxus->SetCurrentEditor(9); break; // 0
#endif
		}
	}
	
	binding->m_KeySet.insert(key);
}

void KeyboardUpCallback(unsigned char key,int x, int y)
{
	//binding->Fluxus->Handle(key, 0, 0, 1, x, y);
	binding->m_KeySet.erase(key);
}

void SpecialKeyboardCallback(int key,int x, int y)
{
	binding->Fluxus->Handle(0, -1, key, -1, x, y, glutGetModifiers());
}

void SpecialKeyboardUpCallback(int key,int x, int y)
{
	//binding->Fluxus->Handle( 0, 0, key, 1, x, y);
}

void MouseCallback(int button, int state, int x, int y)
{
	binding->Fluxus->Handle(0, button, -1, state, x, y, 0);
}

void MotionCallback(int x, int y)
{
	binding->Fluxus->Handle(0, -1, -1, -1, x, y, 0);
}

void IdleCallback()
{
	glutPostRedisplay();
}


SCM EngineCallbackThunk(void * dummy)
{
	scm_run_hook(binding->FrameHook,  SCM_EOL);
	return SCM_UNDEFINED;
}

void EngineCallback()
{
	if (binding->FrameHook)
		scm_internal_catch(SCM_BOOL_T, 
				   EngineCallbackThunk, (void*)NULL,
				   ErrorHandler, (void*)"fluxus");		
}

char *Script;



static void setup_repl_port() {
	SCM v = scm_c_make_vector(5, SCM_BOOL_F);
	scm_vector_set_x(v,scm_from_int(0),scm_c_eval_string("repl-princ"));
	scm_vector_set_x(v,scm_from_int(1),scm_c_eval_string("repl-print"));
	SCM p = scm_make_soft_port(v, scm_makfrom0str("w"));
	scm_set_current_output_port(p);
	scm_set_current_error_port(p);
}

SCM load_scm(void *data)
{
	scm_c_primitive_load((char *)data);
	return SCM_UNDEFINED;
}

void inner_main(void *context, int argc, char **argv)
{
	binding->RegisterProcs();

        // FIXME handle errors well
        SCM initFname = scm_sys_search_load_path(scm_makfrom0str("fluxus/init.scm"));
        if (scm_is_string(initFname))
                scm_primitive_load(initFname);
        else 
                cerr << "Couldn't find fluxus/init.scm script" << endl;
	setup_repl_port();

	
    string fragment;

    binding->FrameHook=scm_make_hook(scm_from_int(0));
    scm_gc_protect_object(binding->FrameHook);
    binding->Fluxus->GetRenderer()->SetEngineCallback(EngineCallback);

	string Init = string(getenv("HOME"))+"/"+INIT_FILE;
	scm_internal_catch(SCM_BOOL_T,load_scm,(void*)Init.c_str(),ErrorHandler,NULL);
	
	if (argc>1)
	{
		if (!strcmp(argv[1],"-x"))
		{
			binding->Fluxus->SourceScript(argv[2]); // just load and execute the script
			binding->Fluxus->Handle(0, 0, 0, 0, 0, 0, 0); // kick the camera into existance
			binding->Fluxus->HideScript(); // hide the cursor
		}
		else
		{
	    	binding->Fluxus->LoadScript(argv[1]);
		}
	}
	
	glutMainLoop();
}

int main(int argc, char *argv[])
{
#ifdef __APPLE__
        // make sure guile can boot on darwin
        std::string argv0(argv[0]);
        unsigned int lastpos = argv0.rfind('/');
        lastpos = argv0.rfind('/', lastpos-1); // skip "MacOS"
        if ( lastpos!=std::string::npos )
        {
                std::string guile_load_path = argv0.substr(0,lastpos) +
                        std::string("/Resources/guile_scripts");
                if (guile_load_path[0] != '/') {
                        // it isn't clever to have relative search paths
                        char * cwd = getwd(NULL);
                        if (cwd) {
                                guile_load_path = string(cwd) + '/' + guile_load_path;
                                free(cwd);
                        }
                }
                guile_load_path = guile_load_path + ":" +
                        guile_load_path + "/site";
                putenv( const_cast<char*>((std::string("GUILE_LOAD_PATH=") +
                                           guile_load_path).c_str() ) );
                cout << "Set GUILE_LOAD_PATH to " << guile_load_path << endl;

        }
#endif

        InitDada();
	srand(time(NULL));
	
	glutInitWindowSize(768,576) ;
  	glutInit(&argc,argv);
	glutInitDisplayMode(GLUT_DOUBLE|GLUT_RGBA|GLUT_DEPTH);
  	glutCreateWindow("fluxus");
	binding = new FluxusBinding(768,576);
	glutDisplayFunc(DisplayCallback);
	glutReshapeFunc(ReshapeCallback);
	glutKeyboardFunc(KeyboardCallback);
	glutSpecialFunc(SpecialKeyboardCallback); 
	glutMouseFunc(MouseCallback);
	glutMotionFunc(MotionCallback);
	glutIdleFunc(IdleCallback);
	glutKeyboardUpFunc(KeyboardUpCallback);
	glutSpecialUpFunc(SpecialKeyboardUpCallback);
		
	scm_boot_guile(argc, argv, inner_main, 0);
    
	return 0;
}


