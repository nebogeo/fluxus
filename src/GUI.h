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

#include <FL/Fl_Gl_Window.h>
#include <FL/Fl_Double_Window.h>
#include <FL/Fl_Button.h>
#include <FL/Fl_Menu_Button.h>
#include <fluxus/Renderer.h>
#include <fluxus/PolyPrimitive.h>
#include <fluxus/Physics.h>
#include <fluxus/Lifeforms.h>
#include "Fl_Code_Editor.h"

namespace fluxus
{

class ScriptGUI : public Fl_Double_Window
{
public:
	ScriptGUI(int w, int h, char *n);
	virtual ~ScriptGUI() {}
	
	virtual int handle(int event);
	virtual void draw();
	
	string ScriptFragment;
	void Dump(const string &Text);
	void LoadScript(const string &Filename);
	void RunScript();
	
private:
	
	Fl_Code_Editor *m_TextEditor;
	Fl_Text_Buffer *m_TextBuffer;
	Fl_Text_Display *m_Output;
	Fl_Text_Buffer *m_OutputBuffer;
	
	Fl_Menu_Button *m_Menu;
	
	inline void Load_i(Fl_Menu_Button *o, void *p);
	static void Load(Fl_Menu_Button *o, void *p);
	inline void Save_i(Fl_Menu_Button *o, void *p);
	static void Save(Fl_Menu_Button *o, void *p);
	
};

/////////////////////////////////////////////////

class FFTWindow : public Fl_Double_Window
{
public:	
	FFTWindow(int length) : Fl_Double_Window(200,100,"fft"),m_Length(length),m_Data(NULL),m_Data2(NULL) {}
	virtual ~FFTWindow() {}
	
	void SetData(float *s, float *s2) { m_Data=s; m_Data2=s2; }
	virtual void draw();

private:
	int m_Length;
	float *m_Data;
	float *m_Data2;
};

/////////////////////////////////////////////////

class FluxFace : public Fl_Double_Window
{
public:	
	FluxFace(int w, int h, char *n);
	virtual ~FluxFace() {}
	
    void New(const string &Name, int x=-1, int y=-1);
	void Clear();
	string GetScriptFragment() { string t=m_ExecuteStr; m_ExecuteStr=""; return t; }
	
private:
	map<string,Fl_Button*> m_ButtonMap;
	string m_ExecuteStr;
	
	inline void Do_i(Fl_Menu_Button *o, void *p);
	static void Do(Fl_Menu_Button *o, void *p);
};

//////////////////////////////////////////////

class GUI : public Fl_Gl_Window
{
public:
	GUI(int w, int h, char *n, int bufsize);
	virtual ~GUI() {}
	
	int handle(int event);
	virtual void draw() { Render(); }
	
	void Render();
	bool KeyPressed(char b);
	
	Renderer *GetRenderer() { return &m_Renderer; }
	Physics *GetPhysics() { return &m_Physics; }
	FluxFace *GetFluxFace() { return m_FluxFace; }
	void AddLifeforms(const string &name) { m_Lifeforms[name]=new Lifeforms; m_Lifeforms[name]->RegisterRenderer(&m_Renderer);}
	Lifeforms *GetLifeforms(const string &name);
    void ClearLifeforms() { m_Lifeforms.clear(); }

    string GetScriptFragment() { string t=m_ScriptWin->ScriptFragment; m_ScriptWin->ScriptFragment=""; return t; }
    void Dump(const string &Text) { m_ScriptWin->Dump(Text); }
    void ResetCamera();
    void LoadScript(const string &Filename) { m_ScriptWin->LoadScript(Filename); }
    void RunScript() { m_ScriptWin->RunScript(); }
	void StartDumpFrames(const string &Filename);
	void EndDumpFrames();

    FFTWindow *GetFFTWindow() { return m_FFTWin; }

protected:
	Renderer m_Renderer;
    Physics  m_Physics;

    map<string,Lifeforms*> m_Lifeforms;

private:
	int m_LastMouseX;
	int m_LastMouseY;
	float m_RotX,m_RotY,m_PosX,m_PosY,m_DisY;
	dVector m_Pos;
	dVector m_Dir;
	
	ScriptGUI *m_ScriptWin;
	FFTWindow *m_FFTWin;
	FluxFace *m_FluxFace;	
	
	int m_Frame;
	string m_FrameName;
};

};
