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
#include <guile/gh.h>
#include "FluxusBinding.h"

using namespace fluxus;

static const string INIT_FILE=".fluxus.scm";
FluxusBinding *binding;

SCM ErrorHandler (void *data, SCM tag, SCM throw_args)
{
	char *str=NULL;
	size_t size=0;
	str=gh_scm2newstr(gh_car(gh_cdr(throw_args)),&size);	
	
	binding->Fluxus->Dump(string("Error in: \"")+string((char*)data)+string("\"\n"));
	binding->Fluxus->Dump(str);
	binding->Fluxus->Dump("\n");
	
	free(str);
	return SCM_UNDEFINED;
}

void DisplayCallback()
{
    string fragment = binding->Fluxus->GetScriptFragment();
    if (fragment!="")
    {
    	gh_eval_str_with_catch(fragment.c_str(), (scm_t_catch_handler)ErrorHandler);
    }
	
	binding->Fluxus->Render();
	binding->FrameCount++;
	
	glutSwapBuffers();
}

void ReshapeCallback(int width, int height)
{
	binding->Fluxus->Reshape(width,height);
}

void KeyboardCallback(unsigned char key,int x, int y)
{
	binding->Fluxus->Handle(key, -1, -1, -1, x, y);
	
	//cerr<<(int)key<<endl;
	
	if (glutGetModifiers()&GLUT_ACTIVE_CTRL)
	{
		if (key==6) // f
		{
			glutFullScreen();
		}
		else if (key==23) // w
		{
			glutReshapeWindow(640,480);
			glutPositionWindow(100,100);
		}
		else if (key==19) // s
		{
			binding->Fluxus->SaveScript();
		}
	}
}

void KeyboardUpCallback(unsigned char key,int x, int y)
{
	//binding->Fluxus->Handle(key, 0, 0, 1, x, y);
}

void SpecialKeyboardCallback(int key,int x, int y)
{
	binding->Fluxus->Handle(0, -1, key, -1, x, y);
}

void SpecialKeyboardUpCallback(int key,int x, int y)
{
	//binding->Fluxus->Handle( 0, 0, key, 1, x, y);
}

void MouseCallback(int button, int state, int x, int y)
{
	binding->Fluxus->Handle(0, button, -1, state, x, y);
}

void MotionCallback(int x, int y)
{
	binding->Fluxus->Handle(0, -1, -1, -1, x, y);
}

void IdleCallback()
{
	glutPostRedisplay();
}

void EngineCallback()
{
    if (binding->CallbackString!="")
    {
        gh_eval_str_with_catch(binding->CallbackString.c_str(), (scm_t_catch_handler)ErrorHandler);
    }
}

char *Script;

void inner_main(int argc, char **argv)
{
	binding->RegisterProcs();
    string fragment;

    binding->Fluxus->GetRenderer()->SetEngineCallback(EngineCallback);

	string Init = string(getenv("HOME"))+"/"+INIT_FILE;
    gh_eval_file_with_catch(Init.c_str(),(scm_t_catch_handler)ErrorHandler);
	
	if (argc>1)
	{
	    gh_eval_file_with_catch(argv[1],(scm_t_catch_handler)ErrorHandler);
	}
	
	glutMainLoop();

    //Fluxus->GetFFTWindow()->SetData(Audio->GetFFT(),Audio->GetAudioBuffer());
    //Fluxus->GetFFTWindow()->redraw();
}

int main(int argc, char *argv[])
{
	InitDada();
	srand(time(NULL));
		
	glutInitWindowSize(640,480) ;
  	glutInit(&argc,argv);
	glutInitDisplayMode(GLUT_DOUBLE|GLUT_RGBA|GLUT_DEPTH);
  	glutCreateWindow("fluxus");
	binding = new FluxusBinding(640,480);
	glutDisplayFunc(DisplayCallback);
	glutReshapeFunc(ReshapeCallback);
	glutKeyboardFunc(KeyboardCallback);
	glutSpecialFunc(SpecialKeyboardCallback); 
	glutMouseFunc(MouseCallback);
	glutMotionFunc(MotionCallback);
	glutIdleFunc(IdleCallback);
	glutKeyboardUpFunc(KeyboardUpCallback);
	glutSpecialUpFunc(SpecialKeyboardUpCallback);
		
    gh_enter(argc, argv, inner_main);
    
	return 0;
}


