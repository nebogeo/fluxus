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

#include <Renderer.h>
#include <PolyPrimitive.h>
#include <Physics.h>
#include <SearchPaths.h>
#include "GLEditor.h"
#include "OSCServer.h"
#include "Recorder.h"
#include "AudioCollector.h"
#include "Repl.h"

using namespace fluxus;

#ifndef FLUXUS_MAIN
#define FLUXUS_MAIN

namespace fluxus
{

static const int NUM_EDITORS=10;

class FluxusMain 
{
public:
	FluxusMain(int x, int y);
	virtual ~FluxusMain() {
		for(int i=0; i<10; i++)
			delete m_Editor[i];
	}
	
	void Handle(unsigned char key, int button, int special, int state, int x, int y, int mod);
	void Render();
	void Reshape(int width, int height);
	
	void TickRecorder();
	
	bool KeyPressed(char b);
	
	Renderer *GetRenderer() { return &m_Renderer; }
	Physics *GetPhysics() { return &m_Physics; }

    string GetScriptFragment() { string temp=m_Script; m_Script=""; return temp; }
    void Dump(const string &Text) { cerr<<Text<<endl; }
    void ResetCamera();
    void LoadScript(const string &Filename);
    void SourceScript(const string &Filename);
    void SetSaveName(const string &s) { m_SaveName[m_CurrentEditor]=s; }
    void SaveScript();
	void StartDumpFrames(const string &Filename, const string &Type);
	void EndDumpFrames();
	void HideScript() { m_HideScript=!m_HideScript; }
	void HideCursor() { m_ShowCursor=!m_ShowCursor; m_Renderer.ShowCursor(m_ShowCursor); }
	void StartOSC(const string &port);
	char TypeFromOSC(unsigned int index);
	float NumberFromOSC(unsigned int index);
	string StringFromOSC(unsigned int index);
	bool MsgOSC(const string &token);
	string GetLastMsg();
	void StartOSCClient(const string &port);
	void SendOSC(const string &msg, const vector<OSCData*> &args);
	void ShowLocators(bool s) { m_ShowLocators=s; }
	void SetCurrentEditor(int s) { m_CurrentEditor=s; }
	
	float GetMouseX() { return m_LastMouseX; }
	float GetMouseY() { return m_LastMouseY; }
	int GetMouseButton() { return m_CurButton; }
 
    void LoadRecordedCode(const string &Filename) { m_Recorder.Load(Filename); }
    void SaveRecordedCode(const string &Filename) { m_Recorder.Save(Filename); }

	void SetAudio(AudioCollector *s) { m_Audio=s; }
	
	Repl * GetRepl() { return (Repl*)m_Editor[9]; }

protected:
	Renderer m_Renderer;
    Physics  m_Physics;
	EventRecorder m_Recorder;
	AudioCollector *m_Audio;
	
private:
	void HandleImpl(unsigned char key, int button, int special, int state, int x, int y, int mod);
	
	enum CameraMode{SCENE,EDITOR};
	CameraMode m_CameraMode;
	
	GLEditor * m_Editor[NUM_EDITORS];
	int m_CurrentEditor;
	string m_SaveName[NUM_EDITORS];

	bool m_Init;
	int m_LastMouseX;
	int m_LastMouseY;
	int m_LastButton;
	int m_CurButton;
	float m_RotX,m_RotY,m_PosX,m_PosY,m_DisY;	

	// arcball
	float m_MouseClickX, m_MouseClickY;
	dQuat m_RotStart, m_RotNow;
		
	bool m_ShowLocators;
	dVector m_Pos;
	dVector m_Dir;
	int m_Frame;
	string m_FrameName;
	string m_FrameType;
	int m_Width;
	int m_Height;
	string m_Script;
	bool m_HideScript;
	bool m_ShowCursor;

	Server *m_OSCServer;
	Client *m_OSCClient;
};

};

#endif
