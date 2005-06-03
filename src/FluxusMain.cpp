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
m_CameraMode(SCENE),
m_CurrentEditor(0),
m_Init(true),
m_RotX(0),
m_RotY(0),
m_PosX(0),
m_PosY(0),
m_DisY(-10),
m_Frame(-1),
m_Width(x),
m_Height(y),
m_HideScript(false),
m_OSCServer(NULL)
{
	m_Renderer.SetDesiredFPS(100000);
}

void FluxusMain::ResetCamera()
{
	m_RotX=m_RotY=m_PosX=m_PosY=0;
	m_DisY=-10;
}

void FluxusMain::Handle(unsigned char key, int button, int special, int state, int x, int y) 
{
	//cerr<<"key:"<<key<<" button:"<<button<<" special:"<<special<<" state:"<<state<<" x:"<<x<<" y:"<<y<<endl;

	if (key!=0 || special!=-1) 
	{
		if (special==GLUT_KEY_F1) m_CameraMode=SCENE;
		if (special==GLUT_KEY_F2) m_CameraMode=EDITOR;
		if (special==GLUT_KEY_F5) m_Script=m_Editor[m_CurrentEditor].GetText();
	
		// the editor only takes keyboard events
		if (!m_HideScript) m_Editor[m_CurrentEditor].Handle(button,key,special,state,x,y);
	}
	else
	{
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

void FluxusMain::ClearOSCHistory()
{
	if (m_OSCServer!=NULL) m_OSCServer->ClearHistory();
}

void FluxusMain::Render()
{		
	m_Physics.Tick();
  	for (map<string,Lifeforms*>::iterator i=m_Lifeforms.begin(); i!=m_Lifeforms.end(); i++)
  	{
  		i->second->Update();
  	}
  	
	m_Renderer.Render();
	if (!m_HideScript) m_Editor[m_CurrentEditor].Render();
	
	if (m_Frame!=-1)
	{
		char Fnum[5];
		snprintf(Fnum,5,"%04d",m_Frame);
		string Filename=m_FrameName+"-"+string(Fnum)+".tif";
		WriteTiff((char*)Filename.c_str(),"fluxus pixels",0,0,m_Width,m_Height,1);
		//  MAC RLE ----------------------------------------------------^
		m_Frame++;
	}
}

Lifeforms *FluxusMain::GetLifeforms(const string &name)
{
	map<string,Lifeforms*>::iterator i=m_Lifeforms.find(name);
	if (i!=m_Lifeforms.end())
	{
		return i->second;
	}
	return NULL;
}

void FluxusMain::LoadScript(const string &Filename) 
{ 
	FILE *file=fopen(Filename.c_str(),"r");
	if (file)
	{
		fseek(file,0,SEEK_END);
		unsigned int size=ftell(file);
		fseek(file,0,SEEK_SET);		
		
		if (size<=0) 
		{
			fclose(file);
			cerr<<"empty file: "<<Filename<<endl;
			return;
		}
		
		char *buffer = new char[size+1];
		if (buffer)
		{
			if (size!=fread(buffer,1,size,file))	
			{
				delete[] buffer;
				fclose(file);
				cerr<<"read error: "<<Filename<<endl;
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
	
	m_SaveName[m_CurrentEditor]=Filename; // just a precaution
}

void FluxusMain::SaveScript() 
{ 
	if (m_SaveName[m_CurrentEditor]!="")
	{
		FILE *file=fopen(m_SaveName[m_CurrentEditor].c_str(),"w");
		if (file)
		{	
			fwrite(m_Editor[m_CurrentEditor].GetText().c_str(),1,m_Editor[m_CurrentEditor].GetText().size(),file);	
			fclose(file);
		}
		
		Dump("Saved ["+m_SaveName[m_CurrentEditor]+"]");
	}
	else
	{
		Dump("No save name set, not saved...");
	}
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

char FluxusMain::TypeFromOSC(const string &token, unsigned int index)
{
	if (!m_OSCServer) 
	{
		cerr<<"osc server not running..."<<endl;
		return '0';
	}
	
	vector<OSCData*> args;
	if (m_OSCServer->Get(token,args))
	{
		if (index<args.size())
		{
			return args[index]->Type();
		}
	}
	return '0';
}

float FluxusMain::NumberFromOSC(const string &token, unsigned int index) 
{ 
	if (!m_OSCServer) 
	{
		cerr<<"osc server not running..."<<endl;
		return '0';
	}
	
	vector<OSCData*> args;
	if (m_OSCServer->Get(token,args))
	{
		if (index<args.size() && args[index]->Type()=='n')
		{
			return static_cast<OSCNumber*>(args[index])->Value;
		}
	}
	return 0;
}

string FluxusMain::StringFromOSC(const string &token, unsigned int index) 
{ 
	if (!m_OSCServer) 
	{
		cerr<<"osc server not running..."<<endl;
		return "";
	}
	
	vector<OSCData*> args;
	if (m_OSCServer->Get(token,args))
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
	
	return m_OSCServer->InHistory(token);	
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

