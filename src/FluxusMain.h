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

#include <fluxus/Renderer.h>
#include <fluxus/PolyPrimitive.h>
#include <fluxus/Physics.h>
#include <fluxus/Lifeforms.h>
#include "GLEditor.h"

using namespace fluxus;

#ifndef FLUXUS_MAIN
#define FLUXUS_MAIN

namespace fluxus
{

class FluxusMain 
{
public:
	FluxusMain(int x, int y, int bufsize);
	virtual ~FluxusMain() {}
	
	void Handle(unsigned char key, int button, int special, int state, int x, int y);
	void Render();
	void Reshape(int width, int height);
	
	bool KeyPressed(char b);
	
	Renderer *GetRenderer() { return &m_Renderer; }
	Physics *GetPhysics() { return &m_Physics; }
	void AddLifeforms(const string &name) { m_Lifeforms[name]=new Lifeforms; m_Lifeforms[name]->RegisterRenderer(&m_Renderer);}
	Lifeforms *GetLifeforms(const string &name);
    void ClearLifeforms() { m_Lifeforms.clear(); }

    string GetScriptFragment() { string temp=m_Script; m_Script=""; return temp; }
    void Dump(const string &Text) { cerr<<Text<<endl; }
    void ResetCamera();
    void LoadScript(const string &Filename);
    void SetSaveName(const string &s) { m_SaveName=s; }
    void SaveScript();
    void RunScript() {  }
	void StartDumpFrames(const string &Filename);
	void EndDumpFrames();

protected:
	Renderer m_Renderer;
    Physics  m_Physics;

    map<string,Lifeforms*> m_Lifeforms;

private:
	enum CameraMode{SCENE,EDITOR};
	CameraMode m_CameraMode;
	
	GLEditor m_Editor;

	bool m_Init;
	int m_LastMouseX;
	int m_LastMouseY;
	int m_LastButton;
	float m_RotX,m_RotY,m_PosX,m_PosY,m_DisY;
	dVector m_Pos;
	dVector m_Dir;
	int m_Frame;
	string m_FrameName;
	int m_Width;
	int m_Height;
	string m_Script;
	string m_SaveName;
};

};

#endif
