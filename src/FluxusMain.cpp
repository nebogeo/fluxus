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

using namespace fluxus;

////////////////////////////////////////////////////////////////

FluxusMain::FluxusMain(int x, int y) :
m_Physics(&m_Renderer),
m_Audio(NULL),
m_CameraMode(SCENE),
m_CurrentEditor(0),
m_Init(true),
m_LastMouseX(0),
m_LastMouseY(0),
m_LastButton(0),
m_CurButton(0),
m_RotX(0),
m_RotY(0),
m_PosX(0),
m_PosY(0),
m_DisY(-10),
m_ShowLocators(false),
m_Frame(-1),
m_Width(x),
m_Height(y),
m_HideScript(false),
m_ShowCursor(true),
m_OSCServer(NULL),
m_OSCClient(NULL)
{
	m_Renderer.SetDesiredFPS(100000);
}

void FluxusMain::ResetCamera()
{
	m_RotX=m_RotY=m_PosX=m_PosY=0;
	m_DisY=-10;
}

void FluxusMain::TickRecorder()
{ 
	if (m_Recorder.GetMode()==EventRecorder::PLAYBACK)
	{
		vector<Event> events;
		m_Recorder.Get(events);
		for (vector<Event>::iterator i=events.begin(); i!=events.end(); i++)
		{
			HandleImpl(i->Key, i->Button, i->Special, i->State, i->X, i->Y, i->Mod);
		}
	}
	
	if (m_Audio && m_Audio->IsProcessing())
	{
		m_Recorder.IncClock(m_Audio->BufferTime());
	}
	else
	{
		m_Recorder.UpdateClock();
	}
}

void FluxusMain::Handle(unsigned char key, int button, int special, int state, int x, int y, int mod) 
{
	if (special==GLUT_KEY_F6) 
	{ 
		if (m_Recorder.GetMode()==EventRecorder::OFF)
		{
			m_Recorder.SetMode(EventRecorder::PLAYBACK); cerr<<"play"<<endl; 
		}
		else if (m_Recorder.GetMode()==EventRecorder::PLAYBACK)
		{
			m_Recorder.SetMode(EventRecorder::RECORD); cerr<<"record"<<endl; 
		}
	}
	else if (special==GLUT_KEY_F7) { m_Recorder.SetMode(EventRecorder::OFF);; cerr<<"off"<<endl; }
	else if (special==GLUT_KEY_F8) { m_Recorder.ResetClock(); cerr<<"reset clock"<<endl; }
	else 
	{
		if (m_Recorder.GetMode()==EventRecorder::RECORD)
		{
			Event event(key, button, special, state, x, y, mod);
			m_Recorder.Record(event);
		}
		
		HandleImpl(key, button, special, state, x, y, mod);
	}
}

void FluxusMain::HandleImpl(unsigned char key, int button, int special, int state, int x, int y, int mod) 
{
	//cerr<<"key:"<<key<<" button:"<<button<<" special:"<<special<<" state:"<<state<<" x:"<<x<<" y:"<<y<<endl;
	
	if (key!=0 || special!=-1) 
	{
		if (special==GLUT_KEY_F1) m_CameraMode=SCENE;
		else if (special==GLUT_KEY_F2) m_CameraMode=EDITOR;
		else if (special==GLUT_KEY_F3) ResetCamera();
		else if (special==GLUT_KEY_F4) m_Editor[m_CurrentEditor].Reset();
		else if (special==GLUT_KEY_F9) 
		{
			m_Editor[m_CurrentEditor].m_TextColourRed=rand()%1000/1000.0f;
			m_Editor[m_CurrentEditor].m_TextColourBlue=rand()%1000/1000.0f;
			m_Editor[m_CurrentEditor].m_TextColourGreen=rand()%1000/1000.0f;
		}	
		else if (special==GLUT_KEY_F10) m_Editor[m_CurrentEditor].m_TextWidth--;
		else if (special==GLUT_KEY_F11) m_Editor[m_CurrentEditor].m_TextWidth++;
		else if (special==GLUT_KEY_F5) m_Script=m_Editor[m_CurrentEditor].GetText();
	
		// the editor only takes keyboard events
		if (!m_HideScript) m_Editor[m_CurrentEditor].Handle(button,key,special,state,x,y,mod);
	}
	else
	{
		if (state==0) m_CurButton=button+1; // button on
		else if (state==1) m_CurButton=0; // button off			
	
		if (m_CameraMode==SCENE)
		{
			// mouse event then?
			if (state==0) 
			{
				m_LastButton=button;
				m_LastMouseX=x;
				m_LastMouseY=y;
			}
			else
			{
				switch (m_LastButton)
				{
					case 0:
					{
						m_RotY+=(x-m_LastMouseX)/4.0f;
						m_RotX+=-(y-m_LastMouseY)/4.0f;
					}
					break;
					case 1:
					{
						m_PosX+=(x-m_LastMouseX)/50.0f;
						m_PosY+=-(y-m_LastMouseY)/50.0f;
					}
					break;
					case 2:
					{
						m_DisY+=-(y-m_LastMouseY)/50.0f;
					}
					break;
				}
				m_LastMouseX=x;
				m_LastMouseY=y;
			}		
		}		
		else if (m_CameraMode==EDITOR)
		{
			// mouse event then?
			if (state==0) 
			{
				m_LastButton=button;
				m_LastMouseX=x;
				m_LastMouseY=y;
			}
			else
			{
				switch (m_LastButton)
				{
					case 0:
					{
						m_Editor[m_CurrentEditor].m_RotY+=(x-m_LastMouseX)/4.0f;
						m_Editor[m_CurrentEditor].m_RotX+=-(y-m_LastMouseY)/4.0f;
					}
					break;
					case 1:
					{
						m_Editor[m_CurrentEditor].m_PosX+=(x-m_LastMouseX);
						m_Editor[m_CurrentEditor].m_PosY+=-(y-m_LastMouseY);
					}
					break;
					case 2:
					{
						m_Editor[m_CurrentEditor].m_DisY+=-(y-m_LastMouseY);
					}
					break;
				}
				m_LastMouseX=x;
				m_LastMouseY=y;
			}		
		}
	}
		
	m_Renderer.GetCamera()->init();
	m_Renderer.GetCamera()->translate(m_PosX,m_PosY,m_DisY);
	m_Renderer.GetCamera()->roty(-m_RotY);
	m_Renderer.GetCamera()->rotx(-m_RotX);
}

bool FluxusMain::KeyPressed(char b)
{
	//return Fl::event_key(b);
	return false;
}

void FluxusMain::StartDumpFrames(const string &Filename)
{
	m_Frame=0;
	m_FrameName = Filename;
}

void FluxusMain::EndDumpFrames()
{
	m_Frame=-1;
}

void FluxusMain::Reshape(int width, int height)
{
	for(int n=0; n<NUM_EDITORS; n++)
	{
		m_Editor[n].Reshape(width,height);
	}
	
	m_Renderer.SetResolution(width,height);
	m_Width=width;
	m_Height=height;
	m_Init=true;
}

void FluxusMain::Render()
{		
	m_Physics.Tick();
	m_Renderer.Render();
 	if (m_ShowLocators) m_Physics.Render();
	if (!m_HideScript) m_Editor[m_CurrentEditor].Render();
	
	if (m_Frame!=-1)
	{
		char Fnum[7];
		snprintf(Fnum,7,"%06d",m_Frame);
		string Filename=m_FrameName+"-"+string(Fnum)+".jpg";
		
		cerr<<Filename<<endl;
		
		//string Filename=m_FrameName+"-"+string(Fnum)+".tif";
		WriteJPG((char*)Filename.c_str(),"fluxus pixels",0,0,m_Width,m_Height,80,1);
		//WriteTiff((char*)Filename.c_str(),"fluxus pixels",0,0,m_Width,m_Height,1);
		//  MAC RLE -----------------------------------------------------------^
		m_Frame++;
	}
}

void FluxusMain::LoadScript(const string &Filename) 
{ 
	string Fullpath = SearchPaths::Get()->GetFullPath(Filename);

	FILE *file=fopen(Fullpath.c_str(),"r");
	if (file)
	{
		fseek(file,0,SEEK_END);
		long size=ftell(file);
		fseek(file,0,SEEK_SET);		
		
		if (size==0) 
		{
			fclose(file);
			cerr<<"empty file: "<<Fullpath<<endl;
			return;
		}

		if (size<0) 
		{
			fclose(file);
			cerr<<"error loading file: "<<Fullpath<<" size: "<<size<<"??"<<endl;
			return;
		}
		
		char *buffer = new char[size+1];
		if (buffer)
		{
			if (size!=(long)fread(buffer,1,size,file))	
			{
				delete[] buffer;
				fclose(file);
				cerr<<"read error: "<<Fullpath<<endl;
				return;
			}			
			buffer[size]='\0';
			m_Editor[m_CurrentEditor].SetText(buffer);	
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
		cerr<<"couldn't load: "<<Fullpath<<endl;
	}
	
	m_SaveName[m_CurrentEditor]=Fullpath; // just a precaution
}

// bad - need to move the interpreter around
#include <guile/gh.h>

void FluxusMain::SourceScript(const string &Filename) 
{ 
	string Fullpath = SearchPaths::Get()->GetFullPath(Filename);
	gh_eval_file(Fullpath.c_str());
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
		fwrite(m_Editor[m_CurrentEditor].GetText().c_str(),1,m_Editor[m_CurrentEditor].GetAllText().size(),file);	
		fclose(file);
	}
	
	Dump("Saved ["+m_SaveName[m_CurrentEditor]+"]");
	
}

void FluxusMain::StartOSC(const string &port)
{
	if (!m_OSCServer)
	{
		m_OSCServer = new Server(port);
		m_OSCServer->Run();
	}
	else
	{
		cerr<<"osc server already running..."<<endl;
	}
}

char FluxusMain::TypeFromOSC(unsigned int index)
{
	if (!m_OSCServer) 
	{
		cerr<<"osc server not running..."<<endl;
		return '0';
	}
	
	vector<OSCData*> args;
	if (m_OSCServer->GetArgs(args))
	{
		if (index<args.size())
		{
			return args[index]->Type();
		}
	}
	return '0';
}

float FluxusMain::NumberFromOSC(unsigned int index) 
{ 
	if (!m_OSCServer) 
	{
		cerr<<"osc server not running..."<<endl;
		return '0';
	}
	
	vector<OSCData*> args;
	if (m_OSCServer->GetArgs(args))
	{
		if (index<args.size())
		{
			if (args[index]->Type()=='i')
			{
				return (float)static_cast<OSCInt*>(args[index])->Value;
			}
			else if (args[index]->Type()=='f')
			{
				return static_cast<OSCFloat*>(args[index])->Value;
			}
		}
	}
	return 0;
}

string FluxusMain::StringFromOSC(unsigned int index) 
{ 
	if (!m_OSCServer) 
	{
		cerr<<"osc server not running..."<<endl;
		return "";
	}
	
	vector<OSCData*> args;
	if (m_OSCServer->GetArgs(args))
	{
		if (index<args.size() && args[index]->Type()=='s')
		{
			return static_cast<OSCString*>(args[index])->Value;
		}
	}
	return "";
}

bool FluxusMain::MsgOSC(const string &token) 
{ 
	if (!m_OSCServer) 
	{
		cerr<<"osc server not running..."<<endl;
		return "";
	}
	
	return m_OSCServer->SetMsg(token);	
}

string FluxusMain::GetLastMsg() 
{ 
	if (!m_OSCServer) 
	{
		cerr<<"osc server not running..."<<endl;
		return "";
	}
	
	return m_OSCServer->GetLastMsg();
}

void FluxusMain::StartOSCClient(const string &port)
{
	if (!m_OSCClient)
	{
		m_OSCClient = new Client();
	}
	
	m_OSCClient->SetDestination(port.c_str());
}

void FluxusMain::SendOSC(const string &msg, const vector<OSCData*> &args)
{
	if (!m_OSCClient) 
	{
		cerr<<"osc client not running... (you need to set the osc destination)"<<endl;
		return;
	}
	
	m_OSCClient->Send(msg,args);
}

//////////////////////////////////////////////////////////////////////

/*void FFTWindow::draw()
{
	
	fl_color(FL_WHITE);
	fl_rectf(0,0,w(),h());

	if (m_Data)
	{
		fl_color(FL_BLACK);
		char t[3];
		int BarWidth = (int)(w()/16.0f);
		for (int n=0; n<16; n++)
		{
			int x=n*BarWidth, y=h()-(int)(m_Data[n]*(float)h());
			sprintf(t,"%d",n);
			fl_font(FL_COURIER|FL_BOLD,10);
			fl_draw(t, x, y);
			fl_rectf(x, y, BarWidth, (int)(m_Data[n]*(float)h()));
		}
	}
		
	if (m_Data2)
	{
		fl_color(FL_BLUE);
		int lstx=0, lsty=50, scale=h()/2;
		for (int n=0; n<m_Length; n++)
		{
			fl_line(lstx, lsty, n, (int)((m_Data2[n]+1.0f)*scale));
			lstx=n;
			lsty=(int)((m_Data2[n]+1.0f)*scale);
		}
	}
}*/

