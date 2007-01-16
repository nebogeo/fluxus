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

#include "FluxusMain.h"
#ifndef __APPLE__
#include <GL/glut.h>
#else
#include <GLUT/glut.h>
#endif
#include "Utils.h"
#include <iostream>

using namespace std;
using namespace fluxus;

////////////////////////////////////////////////////////////////

FluxusMain::FluxusMain(Interpreter *interpreter, int x, int y) :
m_CurrentEditor(0),
m_Frame(-1),
m_Width(x),
m_Height(y),
m_HideScript(false),
m_ShowCursor(true),
m_ShowFileDialog(false)
{
	for(int i=0; i<9; i++) 
	{
		m_Editor[i] = new GLEditor();
	}
	Repl *repl = new Repl(interpreter);
	m_Editor[9] = repl;
	
	// register the repl with the interpreter so we can
	// use it to output error and output messages
	interpreter->SetRepl(repl);
}

FluxusMain::~FluxusMain() 
{
	for(int i=0; i<10; i++)
	{
		delete m_Editor[i];
	}
}

void FluxusMain::Handle(unsigned char key, int button, int special, int state, int x, int y, int mod) 
{	
	if (mod&GLUT_ACTIVE_CTRL)
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
			case 19: SaveScript(); break; // s			
			case 8: HideScript(); break; // h
			case 13: HideCursor(); break; // m
			case 12: 
				m_FileDialog.SetSaveAsMode(false);
				m_ShowFileDialog=!m_ShowFileDialog; 
			break; // l
			case 4: // d
				m_FileDialog.SetSaveAsMode(true);
				m_ShowFileDialog=!m_ShowFileDialog; 
			break; // l
#ifndef __APPLE__
			case 49: SetCurrentEditor(0); break; // 1
			case 0: SetCurrentEditor(1); break; // 2
			case 27: SetCurrentEditor(2); break; // 3
			case 28: SetCurrentEditor(3); break; // 4
			case 29: SetCurrentEditor(4); break; // 5
			case 30: SetCurrentEditor(5); break; // 6
			case 31: SetCurrentEditor(6); break; // 7
			case 127: SetCurrentEditor(7); break; // 8
			case 57: SetCurrentEditor(8); break; // 9
			case 48: SetCurrentEditor(9); break; // 0
#else
			case 49: SetCurrentEditor(0); break; // 1
			case 50: SetCurrentEditor(1); break; // 2
			case 51: SetCurrentEditor(2); break; // 3
			case 52: SetCurrentEditor(3); break; // 4
			case 53: SetCurrentEditor(4); break; // 5
			case 54: SetCurrentEditor(5); break; // 6
			case 55: SetCurrentEditor(6); break; // 7
			case 56: SetCurrentEditor(7); break; // 8
			case 57: SetCurrentEditor(8); break; // 9
			case 48: SetCurrentEditor(9); break; // 0
#endif
		}
	}
	
	if (key!=0 || special!=-1) 
	{
		if (special==GLUT_KEY_F9) 
		{
			m_Editor[m_CurrentEditor]->m_TextColourRed=rand()%1000/1000.0f;
			m_Editor[m_CurrentEditor]->m_TextColourBlue=rand()%1000/1000.0f;
			m_Editor[m_CurrentEditor]->m_TextColourGreen=rand()%1000/1000.0f;
		}	
		else if (special==GLUT_KEY_F10) m_Editor[m_CurrentEditor]->m_TextWidth--;
		else if (special==GLUT_KEY_F11) m_Editor[m_CurrentEditor]->m_TextWidth++;
		else if (special==GLUT_KEY_F5 && m_CurrentEditor<9) m_Script=m_Editor[m_CurrentEditor]->GetText();
	
		// the editors only take keyboard events
		if (m_ShowFileDialog) 
		{
			m_FileDialog.Handle(button,key,special,state,x,y,mod);
			if (m_FileDialog.GetOutput()!="")
			{
				if (m_FileDialog.GetSaveAsMode()) 
				{
					m_SaveName[m_CurrentEditor]=m_FileDialog.GetOutput();
					SaveScript();
				}
				else 
				{
					LoadScript(m_FileDialog.GetOutput());
				}
				m_FileDialog.Clear();
				m_ShowFileDialog=false;
			}
		}
		else if (!m_HideScript) 
		{
			m_Editor[m_CurrentEditor]->Handle(button,key,special,state,x,y,mod);
		}
	}
}

void FluxusMain::StartDumpFrames(const string &Filename, const string &Type)
{
	m_Frame=0;
	m_FrameName = Filename;
	m_FrameType = Type;
}

void FluxusMain::EndDumpFrames()
{
	m_Frame=-1;
}

void FluxusMain::Reshape(int width, int height)
{
	for(int n=0; n<NUM_EDITORS; n++)
	{
		m_Editor[n]->Reshape(width,height);
	}
	
	//m_Renderer.SetResolution(width,height);
	m_Width=width;
	m_Height=height;
}

void FluxusMain::Render()
{		
	if (m_ShowFileDialog) m_FileDialog.Render();
	else if (!m_HideScript) m_Editor[m_CurrentEditor]->Render();
	
	if (m_Frame!=-1)
	{
		char Fnum[7];
		snprintf(Fnum,7,"%06d",m_Frame);
		
		if (m_FrameType=="jpg")
		{
			string Filename=m_FrameName+"-"+string(Fnum)+".jpg";
			WriteJPG((char*)Filename.c_str(),"fluxus pixels",0,0,m_Width,m_Height,80,1);
			cerr<<Filename<<endl;
		}
		else if (m_FrameType=="tif")
		{
			string Filename=m_FrameName+"-"+string(Fnum)+".tif";
			WriteTiff((char*)Filename.c_str(),"fluxus pixels",0,0,m_Width,m_Height,1);
			cerr<<Filename<<endl;
		}
		else if (m_FrameType=="ppm")
		{
			string Filename=m_FrameName+"-"+string(Fnum)+".ppm";
			WritePPM((char*)Filename.c_str(),"fluxus pixels",0,0,m_Width,m_Height,1);
			cerr<<Filename<<endl;
		}
		
		m_Frame++;
	}
}

void FluxusMain::LoadScript(const string &Filename) 
{ 
	FILE *file=fopen(Filename.c_str(),"r");
	if (file)
	{
		fseek(file,0,SEEK_END);
		long size=ftell(file);
		fseek(file,0,SEEK_SET);		
		
		if (size==0) 
		{
			fclose(file);
			cerr<<"empty file: "<<Filename<<endl;
			return;
		}

		if (size<0) 
		{
			fclose(file);
			cerr<<"error loading file: "<<Filename<<" size: "<<size<<"??"<<endl;
			return;
		}
		
		char *buffer = new char[size+1];
		if (buffer)
		{
			if (size!=(long)fread(buffer,1,size,file))	
			{
				delete[] buffer;
				fclose(file);
				cerr<<"read error: "<<Filename<<endl;
				return;
			}			
			buffer[size]='\0';
			m_Editor[m_CurrentEditor]->SetText(buffer);	
		}
		else
		{
			cerr<<"couldn't allocate buffer for load"<<endl;
		}
		
		delete[] buffer;
		fclose(file);
	}
	else
	{
		cerr<<"couldn't load: "<<Filename<<endl;
	}
	
	m_SaveName[m_CurrentEditor]=Filename; // just a precaution
}

void FluxusMain::SaveScript() 
{ 
	if (m_SaveName[m_CurrentEditor]=="") 
	{	
		m_SaveName[m_CurrentEditor]="temp.scm";
	}
	
	FILE *file=fopen(m_SaveName[m_CurrentEditor].c_str(),"w");
	if (file)
	{	
		fwrite(m_Editor[m_CurrentEditor]->GetAllText().c_str(),1,m_Editor[m_CurrentEditor]->GetAllText().size(),file);	
		fclose(file);
	}
	
	cerr<<"Saved ["<<m_SaveName[m_CurrentEditor]<<"]"<<endl;
	
}

