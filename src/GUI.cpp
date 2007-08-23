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

#include <FL/Fl.h>
#include "GUI.h"
#include <GL/glut.h>
#include <FL/fl_draw.h>
#include <FL/fl_file_chooser.h>
#include "Utils.h"

using namespace fluxus;

ScriptGUI::ScriptGUI(int w, int h, char *n) :
Fl_Double_Window(w,h,n)
{
	color(FL_DARK_GREEN);
	
	m_TextEditor = new Fl_Code_Editor(0,0,w,h-100,"");
	m_TextBuffer = new Fl_Text_Buffer(1);
	m_TextBuffer->tab_distance(4);
	m_TextEditor->buffer(m_TextBuffer);
    m_TextEditor->textfont(FL_COURIER|FL_BOLD);
    m_TextEditor->textsize(10);
    m_TextEditor->color(FL_DARK_GREEN);
    m_TextEditor->textcolor(FL_BLACK);
	m_TextEditor->box(FL_NO_BOX);
	m_TextEditor->cursor_style(0);
	
	m_Output = new Fl_Text_Display(0,h-100,w,100,"");
   	m_OutputBuffer = new Fl_Text_Buffer;
	m_Output->buffer(m_OutputBuffer);
	m_Output->textfont(FL_COURIER|FL_BOLD);
	m_Output->textsize(10);
	m_Output->color(FL_DARK_GREEN);
	m_Output->textcolor(FL_GREEN);
	m_Output->box(FL_NO_BOX);
	
    resizable(m_TextEditor);

    m_Menu = new Fl_Menu_Button(0,0,w,h);
    m_Menu->type(Fl_Menu_Button::POPUP3);
    m_Menu->add("load", "", (Fl_Callback*)Load,this);
    m_Menu->add("save", "", (Fl_Callback*)Save,this);
    m_Menu->textfont(FL_COURIER|FL_BOLD);
	m_Menu->textsize(10);
}

void ScriptGUI::draw()
{
	Fl_Double_Window::draw();
}

int ScriptGUI::handle(int event)
{
	if (event==FL_KEYDOWN && Fl::event_key()==FL_F+5)
	{
		RunScript();
	}
	return Fl_Double_Window::handle(event);
}

void ScriptGUI::RunScript()
{
	m_TextBuffer->savefile("crash_temp.scm", m_TextBuffer->length());
	ScriptFragment=m_TextBuffer->selection_text();	
	if (ScriptFragment=="") ScriptFragment=m_TextBuffer->text();
}

void ScriptGUI::LoadScript(const string &Filename)
{	
	m_TextBuffer->loadfile(Filename.c_str());
}

void ScriptGUI::Dump(const string &Text)
{
	m_OutputBuffer->insert(m_OutputBuffer->length(), Text.c_str());
	m_Output->insert_position(m_OutputBuffer->length());
	m_Output->show_insert_position();
}
	
inline void ScriptGUI::Load_i(Fl_Menu_Button *o, void *p)
{
	m_TextBuffer->loadfile(fl_file_chooser("load script", "*.scm", NULL), 5);
}
void ScriptGUI::Load(Fl_Menu_Button *o, void *p)
{((ScriptGUI*)p)->Load_i(o, p);}

inline void ScriptGUI::Save_i(Fl_Menu_Button *o, void *p)
{
	m_TextBuffer->savefile(fl_file_chooser("save script", "*.scm", NULL), m_TextBuffer->length());
}
void ScriptGUI::Save(Fl_Menu_Button *o, void *p)
{((ScriptGUI*)p)->Save_i(o, p);}

////////////////////////////////////////////////////////////////

FluxFace::FluxFace(int w, int h, char *n) :
Fl_Double_Window(w,h,n)
{
   color(FL_DARK_YELLOW);
   resizable(this);
}

void FluxFace::New(const string &Name, int x, int y)
{
	show();
	
	int h=0,w=0;
	fl_font(FL_COURIER|FL_BOLD,10);
	fl_measure(Name.c_str(), w, h);
	Fl_Button *newbut = new Fl_Button(x,y,w,h,"");
	newbut->color(FL_DARK_YELLOW);
	newbut->selection_color(FL_DARK_YELLOW);
	newbut->box(FL_FLAT_BOX);
	newbut->down_box(FL_FLAT_BOX);
	m_ButtonMap[Name]=newbut;
	map<string,Fl_Button*>::iterator i = m_ButtonMap.find(Name);
	newbut->label(i->first.c_str());
	newbut->labelfont(FL_COURIER|FL_BOLD);
    newbut->labelsize(10);
    newbut->callback((Fl_Callback*)Do,this);
	add(newbut);
	redraw();
}

void FluxFace::Clear()
{
	for (map<string,Fl_Button*>::iterator i = m_ButtonMap.begin(); i != m_ButtonMap.end(); i++)
	{
		remove(i->second);
	}
	
	redraw();
}

inline void FluxFace::Do_i(Fl_Menu_Button *o, void *p)
{
	m_ExecuteStr=o->label();
}
void FluxFace::Do(Fl_Menu_Button *o, void *p)
{((FluxFace*)p)->Do_i(o, p);}

////////////////////////////////////////////////////////////////

GUI::GUI(int w, int h, char *n, int bufsize) :
Fl_Gl_Window(w,h,n),
m_Physics(&m_Renderer),
m_RotX(0),
m_RotY(0),
m_PosX(0),
m_PosY(0),
m_DisY(-10),
m_Frame(-1)
{
	resizable(this);
	//mode(FL_RGB|FL_DOUBLE|FL_ACCUM|FL_DEPTH);
	m_ScriptWin = new ScriptGUI(300,400,"fluxscript");
	m_ScriptWin->show();
	m_FFTWin = new FFTWindow(bufsize);	
	m_FFTWin->show();
	m_FluxFace = new FluxFace(320,200,"fluxface");
	m_FluxFace->hide();
}

void GUI::ResetCamera()
{
	m_RotX=m_RotY=m_PosX=m_PosY=0;
	m_DisY=-10;
}

int GUI::handle(int event)
{
	switch (event)
	{
		case FL_DRAG:
			if (Fl::event_button()==1)
			{
				m_RotY+=(Fl::event_x()-m_LastMouseX)/4.0f;
				m_RotX+=-(Fl::event_y()-m_LastMouseY)/4.0f;
			}
			if (Fl::event_button()==2)
			{
				m_PosX+=(Fl::event_x()-m_LastMouseX)/50.0f;
				m_PosY+=-(Fl::event_y()-m_LastMouseY)/50.0f;
			}
			if (Fl::event_button()==3)
			{
				m_DisY+=-(Fl::event_y()-m_LastMouseY)/50.0f;
			}
		break;
		case FL_KEYDOWN:
		
		    if (Fl::event_state()==FL_CTRL && Fl::event_key()=='s')
			{
				if (m_ScriptWin->shown())
				{
					m_ScriptWin->hide();
				}
				else
				{
					m_ScriptWin->show();
				}
			}
			
			if (Fl::event_state()==FL_CTRL && Fl::event_key()=='f')
			{
				if (m_FFTWin->shown())
				{
					m_FFTWin->hide();
				}
				else
				{
					m_FFTWin->show();
				}
			}
			
			if (Fl::event_state()==FL_CTRL && Fl::event_key()=='i')
			{
				if (m_FluxFace->shown())
				{
					m_FluxFace->hide();
				}
				else
				{
					m_FluxFace->show();
				}
			}
			
			if (Fl::event_key()=='f')
			{
				fullscreen();
				m_Renderer.SetResolution(w(),h());
			}
			
			if (Fl::event_key()=='w')
			{
				fullscreen_off(100,100,640,480);
				m_Renderer.SetResolution(w(),h());
			}
		break;
		default: break;
	}
	
	 m_Renderer.GetCamera()->init();
	 m_Renderer.GetCamera()->translate(m_PosX,m_PosY,m_DisY);
	 m_Renderer.GetCamera()->roty(-m_RotY);
	 m_Renderer.GetCamera()->rotx(-m_RotX);
	
	m_LastMouseX = Fl::event_x();
	m_LastMouseY = Fl::event_y();
        redraw();

	return Fl_Gl_Window::handle(event);
}

bool GUI::KeyPressed(char b)
{
	return Fl::event_key(b);
}

void GUI::StartDumpFrames(const string &Filename)
{
	m_Frame=0;
	m_FrameName = Filename;
}

void GUI::EndDumpFrames()
{
	m_Frame=-1;
}

void GUI::Render()
{
	if (!valid()) 
	{
		glViewport(0,0,w(),h());
	}
	
	m_Physics.Tick();
  	for (map<string,Lifeforms*>::iterator i=m_Lifeforms.begin(); i!=m_Lifeforms.end(); i++)
  	{
  		i->second->Update();
  	}
  	m_Renderer.Render();
	
	if (m_Frame!=-1)
	{ 
		char Fnum[5];
		snprintf(Fnum,5,"%04d",m_Frame);
		string Filename=m_FrameName+"-"+string(Fnum)+".jpg";
		//string Filename=m_FrameName+"-"+string(Fnum)+".tif";
		WriteJPG((char*)Filename.c_str(),"fluxus pixels",0,0,w(),h(),95,1);
		//WriteTiff((char*)Filename.c_str(),"fluxus pixels",0,0,w(),h(),1);
		//  MAC RLE ----------------------------------------------------^
		m_Frame++;
	}
}

Lifeforms *GUI::GetLifeforms(const string &name)
{
	map<string,Lifeforms*>::iterator i=m_Lifeforms.find(name);
	if (i!=m_Lifeforms.end())
	{
		return i->second;
	}
	return NULL;
}
//////////////////////////////////////////////////////////////////////

void FFTWindow::draw()
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
}

