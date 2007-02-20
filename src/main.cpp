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
#include <string>
#include <GL/glut.h>
#include "FluxusMain.h"
#include "Interpreter.h"

using namespace std;

static const string ENGINE_CALLBACK="(fluxus-frame-callback)";
static const string RESHAPE_CALLBACK="fluxus-reshape-callback";
static const string INPUT_CALLBACK="fluxus-input-callback";
static const string INPUT_RELEASE_CALLBACK="fluxus-input-release-callback";
static const string STARTUP_SCRIPT="(define fluxus-collects-location \"%s\") \
									(define fluxus-version \"%d.%d\") \
									(load (string-append (path->string (find-system-path 'home-dir)) \".fluxus/startup.scm\"))";

FluxusMain *app = NULL;
static Interpreter *interpreter = NULL; 

void DisplayCallback()
{    
	string fragment = app->GetScriptFragment();
    if (fragment!="")
    {
		interpreter->Interpret(fragment);
    }
	
	interpreter->Interpret(ENGINE_CALLBACK);
	app->Render();	
	glutSwapBuffers();
		
}

void ReshapeCallback(int width, int height)
{
	app->Reshape(width,height);
	char code[256];
	snprintf(code,256,"(%s %d %d)",RESHAPE_CALLBACK.c_str(),width,height);
	interpreter->Interpret(code);
}

void KeyboardCallback(unsigned char key,int x, int y)
{
	app->Handle(key, -1, -1, -1, x, y, glutGetModifiers());
	char code[256];
	snprintf(code,256,"(%s #\\%c %d %d %d %d %d %d)",INPUT_CALLBACK.c_str(),key,-1,-1,-1,x,y,glutGetModifiers());
	interpreter->Interpret(code);
}

void KeyboardUpCallback(unsigned char key,int x, int y)
{
	char code[256];
	snprintf(code,256,"(%s #\\%c %d %d %d %d %d %d)",INPUT_RELEASE_CALLBACK.c_str(),key,-1,-1,-1,x,y,0);
	interpreter->Interpret(code);
}

void SpecialKeyboardCallback(int key,int x, int y)
{
	app->Handle(0, -1, key, -1, x, y, glutGetModifiers());
	char code[256];
	snprintf(code,256,"(%s %d %d %d %d %d %d %d)",INPUT_CALLBACK.c_str(),0,-1,key,-1,x,y,glutGetModifiers());
	interpreter->Interpret(code);
}

void SpecialKeyboardUpCallback(int key,int x, int y)
{
	//app->Handle( 0, 0, key, 1, x, y);
	char code[256];
	snprintf(code,256,"(%s %d %d %d %d %d %d %d)",INPUT_RELEASE_CALLBACK.c_str(),0,-1,key,-1,x,y,glutGetModifiers());
	interpreter->Interpret(code);
}

void MouseCallback(int button, int state, int x, int y)
{
	app->Handle(0, button, -1, state, x, y, 0);
	char code[256];
	snprintf(code,256,"(%s %d %d %d %d %d %d %d)",INPUT_CALLBACK.c_str(),0,button,-1,state,x,y,0);
	interpreter->Interpret(code);
}

void MotionCallback(int x, int y)
{
	app->Handle(0, -1, -1, -1, x, y, 0);
	char code[256];
	snprintf(code,256,"(%s %d %d %d %d %d %d %d)",INPUT_CALLBACK.c_str(),0,-1,-1,-1,x,y,0);
	interpreter->Interpret(code);
}

void IdleCallback()
{
	glutPostRedisplay();
}

int main(int argc, char *argv[])
{
	// setup mzscheme 
	void *stack_start;
	stack_start = (void *)&stack_start;
	Scheme_Env *e = NULL;
	
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, e);
	
	#ifdef MZ_PRECISE_GC
	scheme_set_stack_base( &__gc_var_stack__, 1);
	#else
	scheme_set_stack_base( NULL, 1);
	#endif
	
 	MZ_GC_REG();
	e = scheme_basic_env();
	
	interpreter = new Interpreter(e);
	
	// load the startup script
	char startup[1024];
	// insert the version number
	snprintf(startup,1024,STARTUP_SCRIPT.c_str(),
		COLLECTS_LOCATION,FLUXUS_MAJOR_VERSION,FLUXUS_MINOR_VERSION);
	interpreter->Interpret(startup,NULL,true);
	srand(time(NULL));
		
	// init OpenGL
  	glutInit(&argc,argv);
	glutInitWindowSize(720,576);
	app = new FluxusMain(interpreter,720,576);
	glutInitDisplayMode(GLUT_DOUBLE|GLUT_RGBA|GLUT_DEPTH|GLUT_STENCIL);
	char windowtitle[256];
	snprintf(windowtitle,256,"fluxus scratchpad %d.%d",FLUXUS_MAJOR_VERSION,FLUXUS_MINOR_VERSION);
  	glutCreateWindow(windowtitle);
	glutDisplayFunc(DisplayCallback);
	glutReshapeFunc(ReshapeCallback);
	glutKeyboardFunc(KeyboardCallback);
	glutSpecialFunc(SpecialKeyboardCallback); 
	glutMouseFunc(MouseCallback);
	glutMotionFunc(MotionCallback);
	glutIdleFunc(IdleCallback);
	glutKeyboardUpFunc(KeyboardUpCallback);
	glutSpecialUpFunc(SpecialKeyboardUpCallback);
	
   	if (argc>1) 
	{
		if (argc>2)
		{
			if (!strcmp(argv[1],"-r"))
			{
			}
			else if (!strcmp(argv[1],"-p"))
			{
			}
		}
		else
		{
			app->LoadScript(argv[1]);
		}
	}
	
	glutMainLoop();
	
	#ifdef MZ_PRECISE_GC
	MZ_GC_UNREG();
	#endif
	
	return 0;
}


