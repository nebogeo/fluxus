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

#ifndef __APPLE__
#include "GL/glut.h"
#else
#include "GLUT/glut.h"
#endif

#include "FluxusMain.h"
#include "Interpreter.h"
#include "Recorder.h"

using namespace std;

static const string ENGINE_CALLBACK="(fluxus-frame-callback)";
static const string RESHAPE_CALLBACK="fluxus-reshape-callback";
static const string INPUT_CALLBACK="fluxus-input-callback";
static const string INPUT_RELEASE_CALLBACK="fluxus-input-release-callback";
static const string STARTUP_SCRIPT="(define fluxus-collects-location \"%s\") \
									(define fluxus-version \"%d.%d\") \
									(define fluxus-data-location \"%s\") \
									(load (string-append fluxus-collects-location \"/fluxus-\"\
										fluxus-version \"/scratchpad-boot.scm\"))";

FluxusMain *app = NULL;
static Interpreter *interpreter = NULL; 
EventRecorder *recorder = NULL;
int modifiers = 0;

void ReshapeCallback(int width, int height)
{
	app->Reshape(width,height);
	char code[256];
	snprintf(code,256,"(%s %d %d)",RESHAPE_CALLBACK.c_str(),width,height);
	interpreter->Interpret(code);
}

void KeyboardCallback(unsigned char key,int x, int y)
{
	int mod=modifiers;
	if (recorder->GetMode()!=EventRecorder::PLAYBACK) mod=glutGetModifiers();	
	app->Handle(key, -1, -1, -1, x, y, mod);
	char code[256];
	snprintf(code,256,"(%s #\\%c %d %d %d %d %d %d)",INPUT_CALLBACK.c_str(),key,-1,-1,-1,x,y,mod);
	interpreter->Interpret(code);
	recorder->Record(RecorderMessage("keydown",key,mod));
}

void KeyboardUpCallback(unsigned char key,int x, int y)
{
	char code[256];
	snprintf(code,256,"(%s #\\%c %d %d %d %d %d %d)",INPUT_RELEASE_CALLBACK.c_str(),key,-1,-1,-1,x,y,0);
	interpreter->Interpret(code);
	recorder->Record(RecorderMessage("keyup",key,0));
}

void SpecialKeyboardCallback(int key,int x, int y)
{
	int mod=modifiers;
	if (recorder->GetMode()!=EventRecorder::PLAYBACK) mod=glutGetModifiers();	
	app->Handle(0, -1, key, -1, x, y, mod);
	char code[256];
	snprintf(code,256,"(%s %d %d %d %d %d %d %d)",INPUT_CALLBACK.c_str(),0,-1,key,-1,x,y,mod);
	interpreter->Interpret(code);
	recorder->Record(RecorderMessage("specialkeydown",key,mod));
}

void SpecialKeyboardUpCallback(int key,int x, int y)
{
	//app->Handle( 0, 0, key, 1, x, y);
	char code[256];
	snprintf(code,256,"(%s %d %d %d %d %d %d %d)",INPUT_RELEASE_CALLBACK.c_str(),0,-1,key,-1,x,y,0);
	interpreter->Interpret(code);
	recorder->Record(RecorderMessage("specialkeyup",key,0));
}

void MouseCallback(int button, int state, int x, int y)
{
	app->Handle(0, button, -1, state, x, y, 0);
	char code[256];
	snprintf(code,256,"(%s %d %d %d %d %d %d %d)",INPUT_CALLBACK.c_str(),0,button,-1,state,x,y,0);
	interpreter->Interpret(code);
	recorder->Record(RecorderMessage("mouse",x,y,button,state));
}

void MotionCallback(int x, int y)
{
	app->Handle(0, -1, -1, -1, x, y, 0);
	char code[256];
	snprintf(code,256,"(%s %d %d %d %d %d %d %d)",INPUT_CALLBACK.c_str(),0,-1,-1,-1,x,y,0);
	interpreter->Interpret(code);
	recorder->Record(RecorderMessage("motion",x,y));
}

void IdleCallback()
{
	glutPostRedisplay();
}

void DoRecorder()
{
	vector<RecorderMessage> events;
	if (recorder->Get(events))
	{
		for (vector<RecorderMessage>::iterator i=events.begin(); i!=events.end(); i++)
		{
			if (i->Name=="keydown") 
			{
				modifiers=i->Mod;
				KeyboardCallback(i->Data,0,0);
			}
			else if (i->Name=="keyup") KeyboardUpCallback(i->Data,0,0);
			else if (i->Name=="specialkeydown") 
			{
				modifiers=i->Mod;
				SpecialKeyboardCallback(i->Data,0,0);
			}
			else if (i->Name=="specialkeyup") SpecialKeyboardUpCallback(i->Data,0,0);
			else if (i->Name=="mouse") MouseCallback(i->Button,i->State,i->Data,i->Mod);
			else if (i->Name=="motion") MotionCallback(i->Data,i->Mod);
		}
	}
	recorder->UpdateClock();
	recorder->Save();
}

void DisplayCallback()
{    
	string fragment = app->GetScriptFragment();
    if (fragment!="")
    {
		interpreter->Interpret(fragment);
    }
	
	if (!interpreter->Interpret(ENGINE_CALLBACK))
	{		
		// the callback has failed, so clear the screen so we can fix the error...
		glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);	
	}

	app->Render();	
	glutSwapBuffers();

	DoRecorder();		
}

void ExitHandler()
{
	delete app;
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
		COLLECTS_LOCATION,
		FLUXUS_MAJOR_VERSION,
		FLUXUS_MINOR_VERSION,
		DATA_LOCATION);
	interpreter->Interpret(startup,NULL,true);
	srand(time(NULL));
	
	#ifdef STEREODEFAULT

	#ifdef ACCUM_BUFFER
	unsigned int flags = GLUT_DOUBLE|GLUT_RGBA|GLUT_DEPTH|GLUT_STENCIL|GLUT_ACCUM|GLUT_STEREO;
  	#else
	unsigned int flags = GLUT_DOUBLE|GLUT_RGBA|GLUT_DEPTH|GLUT_STENCIL|GLUT_STEREO;
	#endif
 	
	#else
	
	#ifdef ACCUM_BUFFER
	unsigned int flags = GLUT_DOUBLE|GLUT_RGBA|GLUT_DEPTH|GLUT_STENCIL|GLUT_ACCUM;
  	#else
	unsigned int flags = GLUT_DOUBLE|GLUT_RGBA|GLUT_DEPTH|GLUT_STENCIL;
	#endif
	
	#endif
	
	// init OpenGL
  	glutInit(&argc,argv);
	glutInitWindowSize(720,576);
	app = new FluxusMain(interpreter,720,576);
	glutInitDisplayMode(flags);
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
	atexit(ExitHandler);
	
	recorder = new EventRecorder;
	
	int arg=1;
	while(arg<argc)
	{
		if (!strcmp(argv[arg],"-r"))
		{
			if (arg+1 < argc) 
			{
				recorder->SetFilename(argv[arg+1]);
				recorder->SetMode(EventRecorder::RECORD);
				arg++;
			}
		}
		else if (!strcmp(argv[arg],"-p"))
		{				
			if (arg+1 < argc) 
			{
				recorder->SetFilename(argv[arg+1]);
				recorder->Load();
				recorder->SetMode(EventRecorder::PLAYBACK);
				arg++;
			}
		}
		else if (!strcmp(argv[arg],"-d"))
		{				
			if (arg+1 < argc) 
			{
				recorder->SetDelta(atof(argv[arg+1]));
				arg++;
			}
		}
		else
		{
			app->SetCurrentEditor(0); // flip it out of the repl
			app->LoadScript(argv[arg]);
		}
		arg++;
	}
	
	glutMainLoop();
	
	#ifdef MZ_PRECISE_GC
	MZ_GC_UNREG();
	#endif
			
	return 0;
}


