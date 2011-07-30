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

#include <stdlib.h>
#include <limits.h>
#include <sys/time.h>
#include <iostream>
#include <string>
#include <unistd.h>
#include "Unicode.h"

#include "GL/glew.h"

#ifndef __APPLE__
#include "GL/glut.h"
#else
#include "GLUT/glut.h"
#endif

#include "FluxusMain.h"
#include "Interpreter.h"
#include "Recorder.h"

using namespace std;

static const wstring ENGINE_CALLBACK=L"(fluxus-frame-callback)";
static const wstring RESHAPE_CALLBACK=L"fluxus-reshape-callback";
static const wstring INPUT_CALLBACK=L"fluxus-input-callback";
static const wstring INPUT_RELEASE_CALLBACK=L"fluxus-input-release-callback";

FluxusMain *app = NULL;
EventRecorder *recorder = NULL;
int modifiers = 0;

void ReshapeCallback(int width, int height)
{
	app->Reshape(width,height);
	wchar_t code[256];
	#ifndef WIN32
	swprintf(code,256,L"(%S %d %d)",RESHAPE_CALLBACK.c_str(),width,height);
	#else
	swprintf(code,L"(%s %d %d)",RESHAPE_CALLBACK.c_str(),width,height);
	#endif
	Interpreter::Interpret(code);
}

void print_bin(unsigned char v)
{
    for (int i=0; i<8; i++)
    {
        if (v>>i&0x01) cerr<<"1";
        else cerr<<"0";
    }
    cerr<<endl;
}

void KeyboardCallback(unsigned char key,int x, int y)
{
	// multibyte characters seem broken on linux?
#ifndef __APPLE__
	if (key>0x80)
	{
		return;
	}
#endif

	int mod=modifiers;
	if (recorder->GetMode()!=EventRecorder::PLAYBACK) mod=glutGetModifiers();
	if ((recorder->GetMode() != EventRecorder::PLAYBACK) || ((x == -1) && (y == -1)))
		app->Handle(key, -1, -1, -1, x, y, mod);
	wchar_t code[256];
	if (key > 0 && key<0x80)
	{ // key is 0 on ctrl+2 and ignore extended ascii for the time being
		int imod = 0;
		if (mod & GLUT_ACTIVE_SHIFT)
			imod |= 1;
		if (mod & GLUT_ACTIVE_CTRL)
			imod |= 2;
		if (mod & GLUT_ACTIVE_ALT)
			imod |= 4;
#ifndef WIN32
		swprintf(code,256,L"(%S #\\%c %d %d %d %d %d %d)",INPUT_CALLBACK.c_str(),key,-1,-1,-1,x,y,imod);
#else
		swprintf(code,L"(%s #\\%c %d %d %d %d %d %d)",INPUT_CALLBACK.c_str(),key,-1,-1,-1,x,y,imod);
#endif
		Interpreter::Interpret(code);
	}
	recorder->Record(RecorderMessage("keydown",key,mod));
}

void KeyboardUpCallback(unsigned char key,int x, int y)
{
	wchar_t code[256];
	if (key > 0 && key<0x80) 
    { // key is 0 on ctrl+2
             #ifndef WIN32
		swprintf(code,256,L"(%S #\\%c %d %d %d %d %d %d)",INPUT_RELEASE_CALLBACK.c_str(),key,-1,-1,-1,x,y,0);
	     #else
		swprintf(code,L"(%s #\\%c %d %d %d %d %d %d)",INPUT_RELEASE_CALLBACK.c_str(),key,-1,-1,-1,x,y,0);
	     #endif
		Interpreter::Interpret(code);
	}
	recorder->Record(RecorderMessage("keyup",key,0));
}

void SpecialKeyboardCallback(int key,int x, int y)
{
	int mod=modifiers;
	if (recorder->GetMode()!=EventRecorder::PLAYBACK) mod=glutGetModifiers();
	if (key == GLUT_KEY_F12)
		recorder->PauseToggle();
	if ((recorder->GetMode() != EventRecorder::PLAYBACK) || ((x == -1) && (y == -1)))
		app->Handle(0, -1, key, -1, x, y, mod);
	wchar_t code[256];
	#ifndef WIN32
	swprintf(code,256,L"(%S %d %d %d %d %d %d %d)",INPUT_CALLBACK.c_str(),0,-1,key,-1,x,y,mod);
	#else
	swprintf(code,L"(%s %d %d %d %d %d %d %d)",INPUT_CALLBACK.c_str(),0,-1,key,-1,x,y,mod);
	#endif
	Interpreter::Interpret(code);
	recorder->Record(RecorderMessage("specialkeydown",key,mod));
}

void SpecialKeyboardUpCallback(int key,int x, int y)
{
	//app->Handle( 0, 0, key, 1, x, y);
	wchar_t code[256];
	#ifndef WIN32
	swprintf(code,256,L"(%S %d %d %d %d %d %d %d)",INPUT_RELEASE_CALLBACK.c_str(),0,-1,key,-1,x,y,0);
	#else
	swprintf(code,L"(%s %d %d %d %d %d %d %d)",INPUT_RELEASE_CALLBACK.c_str(),0,-1,key,-1,x,y,0);
	#endif
	Interpreter::Interpret(code);
	recorder->Record(RecorderMessage("specialkeyup",key,0));
}

void MouseCallback(int button, int state, int x, int y)
{
	app->Handle(0, button, -1, state, x, y, 0);
	wchar_t code[256];
	#ifndef WIN32
	swprintf(code,256,L"(%S %d %d %d %d %d %d %d)",INPUT_CALLBACK.c_str(),0,button,-1,state,x,y,0);
	#else
	swprintf(code,L"(%s %d %d %d %d %d %d %d)",INPUT_CALLBACK.c_str(),0,button,-1,state,x,y,0);
	#endif
	Interpreter::Interpret(code);
	recorder->Record(RecorderMessage("mouse",x,y,button,state));
}

void MotionCallback(int x, int y)
{
	app->Handle(0, -1, -1, -1, x, y, 0);
	wchar_t code[256];
	#ifndef WIN32
	swprintf(code,256,L"(%S %d %d %d %d %d %d %d)",INPUT_CALLBACK.c_str(),0,-1,-1,-1,x,y,0);
	#else
	swprintf(code,L"(%s %d %d %d %d %d %d %d)",INPUT_CALLBACK.c_str(),0,-1,-1,-1,x,y,0);
	#endif
	Interpreter::Interpret(code);
	recorder->Record(RecorderMessage("motion",x,y));
}

void PassiveMotionCallback(int x, int y)
{
	app->Handle(0, -1, -1, -1, x, y, 0);
	wchar_t code[256];
	#ifndef WIN32
	swprintf(code,256,L"(%S %d %d %d %d %d %d %d)",INPUT_CALLBACK.c_str(),0,-1,-1,-1,x,y,0);
	#else
	swprintf(code,L"(%s %d %d %d %d %d %d %d)",INPUT_CALLBACK.c_str(),0,-1,-1,-1,x,y,0);
	#endif
	Interpreter::Interpret(code);
	recorder->Record(RecorderMessage("passivemotion",x,y));
}

void IdleCallback()
{
	glutPostRedisplay();
#ifndef WIN32
	usleep(100);
#endif
}

void DoRecorder()
{
	list<RecorderMessage> events;
	if (recorder->Get(events))
	{
		for (list<RecorderMessage>::iterator i=events.begin(); i!=events.end(); i++)
		{
			if (i->Name=="keydown")
			{
				modifiers=i->Mod;
				KeyboardCallback(i->Data, -1, -1);
			}
			else if (i->Name=="keyup")
			{
				KeyboardUpCallback(i->Data, 0, 0);
			}
			else if (i->Name=="specialkeydown")
			{
				modifiers=i->Mod;
				SpecialKeyboardCallback(i->Data, -1, -1);
			}
			else if (i->Name=="specialkeyup")
			{
				SpecialKeyboardUpCallback(i->Data, 0, 0);
			}
			else if (i->Name=="mouse") MouseCallback(i->Button,i->State,i->Data,i->Mod);
			else if (i->Name=="motion") MotionCallback(i->Data,i->Mod);
			else if (i->Name=="passivemotion") PassiveMotionCallback(i->Data,i->Mod);
		}
	}
	recorder->UpdateClock();
	recorder->Save();
}

void DisplayCallback()
{
	wstring fragment = app->GetScriptFragment();
	if (fragment!=L"")
	{
		Interpreter::Interpret(fragment);
	}

	if (!Interpreter::Interpret(ENGINE_CALLBACK))
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

struct args
{
    int argc;
    char **argv;
};

int run(void *data)
{
    args *myargs = (args *)data;
    int argc = myargs->argc;
    char **argv = myargs->argv;
	// we create our own Scheme_Env in here, as we need
	// to be able to reset it with F6. Seems to be ok to ignore se...
	Interpreter::Register();
	Interpreter::Initialise();

	srand(time(NULL));

	unsigned int flags = GLUT_DOUBLE|GLUT_RGBA|GLUT_DEPTH|GLUT_STENCIL;
#ifdef MULTISAMPLE
	flags |= GLUT_MULTISAMPLE;
#endif
#ifdef ACCUM_BUFFER
	flags|=GLUT_ACCUM;
#endif
#ifdef STEREODEFAULT
	flags|=GLUT_STEREO;
#endif

	// init OpenGL
	glutInit(&argc,argv);
	glutInitWindowSize(DEFAULT_WIDTH,DEFAULT_HEIGHT);
	app = new FluxusMain(DEFAULT_WIDTH,DEFAULT_HEIGHT);
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
	glutPassiveMotionFunc(PassiveMotionCallback);
	atexit(ExitHandler);

	if(glewInit() != GLEW_OK)
	{
		cerr<< "ERROR Unable to check OpenGL extensions" << endl;
	}

	recorder = new EventRecorder;
	int arg=1;
	int currentEditor=0;
	bool exe=false;

	while(arg<argc)
	{
		if (!strcmp(argv[arg],"-v"))
		{
			cout<<"fluxus version: "<<FLUXUS_MAJOR_VERSION<<"."<<FLUXUS_MINOR_VERSION<<endl;
			exit(0);
		}
		else if (!strcmp(argv[arg],"--help") || !strcmp(argv[arg],"-h") || !strcmp(argv[arg],"-?"))
		{
			cout<<"fluxus [options] [filename1] [filename2] ..."<<endl;
			cout<<"options:"<<endl;
			cout<<"-h : help"<<endl;
			cout<<"-v : version info"<<endl;
			cout<<"-r filename : record keypresses"<<endl;
			cout<<"-p filename : playback keypresses"<<endl;
			cout<<"-d time : set delta time between frames for keypress playback"<<endl;
			cout<<"-lang language : sets the RACKET language to use (may not work)"<<endl;
			cout<<"-fs : startup in fullscreen mode"<<endl;
			cout<<"-hm : hide the mouse pointer on startup"<<endl;
			cout<<"-geom wxh : set window geometry, e.g. 640x480"<<endl;
			cout<<"-x : execute and hide script at startup"<<endl;
			exit(0);
		}
		else if (!strcmp(argv[arg],"-r"))
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
		else if (!strcmp(argv[arg],"-lang"))
		{
			if (arg+1 < argc)
			{
				Interpreter::SetLanguage(string_to_wstring(string(argv[arg+1])));
				arg++;
			}
		}
		else if (!strcmp(argv[arg],"-fs"))
		{
			glutFullScreen();
		}
		else if (!strcmp(argv[arg],"-hm"))
		{
			app->HideCursor();
		}
		else if (!strcmp(argv[arg],"-x"))
		{
			exe=true;
		}
		else if (!strcmp(argv[arg],"-geom"))
		{
			if (arg+1 < argc)
			{
				char *endptr;
				int width=strtol(argv[arg+1], &endptr, 10);
				arg++;
				if (*endptr != 'x')
				{
					arg++;
					continue;
				}

				int height=strtol(endptr+1, NULL, 10);

				if ((width > 0) && (height > 0))
				{
					glutReshapeWindow(width,height);
					app->m_OrigWidth=width;
					app->m_OrigHeight=height;
					app->Reshape(width,height);
				}

			}
		}
		else
		{
			if (currentEditor<fluxus::NUM_EDITORS-1)
			{
				app->SetCurrentEditor(currentEditor); // flip it out of the repl
				app->LoadScript(string_to_wstring(string(argv[arg])));
				if (exe && currentEditor==0)
				{
					app->Execute();
					app->HideScript();
				}
				currentEditor++;
			}
		}
		arg++;
	}

	glutMainLoop();

	return 0;
}

int main(int argc, char *argv[])
{
    args myargs;
    myargs.argc=argc;
    myargs.argv=argv;
	return scheme_main_stack_setup(1, run, (void *)&myargs);
}

