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
 
#ifndef N_RENDERER
#define N_RENDERER

#include <sys/time.h>
#include "dada.h"
#include "deque"
#include "map"
#include "vector"
#include "string"
#include "Camera.h"
#include "SceneGraph.h"
#include "ImmediateMode.h"
#include "Light.h"
#include "TexturePainter.h"

using namespace std;

namespace fluxus
{

class State;
class Primitive;

class Renderer
{
public:
	Renderer();
	~Renderer();
	
	//////////////////////////////////////////////////////////////////////
	// Rendering control 
	void Render();
	void Clear();
	
	///////////////////////////////////////////////////////////////////////
	// State accessors
	State *GetState();
	void ApplyState();
	void PushState();
	void PopState();
	// grabs the object state and makes it current
	void Grab(int ID);
	void UnGrab();
	Primitive *Grabbed() { return m_Grabbed; }
	
	//////////////////////////////////////////////////////////////////////
	// Primitive handling
	int          AddPrimitive(Primitive *Prim);
	Primitive   *GetPrimitive(int ID);
	// make sure it's removed from the physics engine too
	void         RemovePrimitive(int ID);
	// moves the primitive (and it's subtree) from it's position in the
	// graph and adds it to the root.
	void         DetachPrimitive(int ID);
	dMatrix      GetGlobalTransform(int ID);
	dBoundingBox GetBoundingBox(int ID);
	// immediate mode (don't delete prim till after Render() - it's immediate-ish :))
	void         RenderPrimitive(Primitive *Prim);
	// Get primitive ID from screen space
	int          Select(int x, int y, int size);
	
	///////////////////////////////////////////////////////////////////////
	// Lights
	int AddLight(Light *l);
	Light *GetLight(int id);
	void ClearLights();

	////////////////////////////////////////////////////////////////////////
	// Camera
	Camera *GetCamera() { return &m_Camera; }

	////////////////////////////////////////////////////////////////////////
	// Scenegraph access
	const SceneGraph &GetSceneGraph()        { return m_World; }
		
	////////////////////////////////////////////////////////////////////////
	// Global state control
	void DrawText(const string &Text);
	void Reinitialise()                      { m_Initialised=false; }
	void SetMotionBlur(bool s, float a=0.02) { m_MotionBlur=s; m_Fade=a; }
	void SetResolution(int x, int y)         { m_Width=x; m_Height=y; m_Initialised=false; }
	void GetResolution(int &x, int &y)       { x=m_Width; y=m_Height; }
	void DrawBuffer(GLenum mode);
	
	void InitTextures() 					 { TexturePainter::Get()->Initialise(); }
	unsigned int LoadTexture(const string &Filename, bool ignorecache=false) { return TexturePainter::Get()->LoadTexture(Filename,ignorecache); }
	void ShowAxis(bool s)                    { m_ShowAxis=s; }
	void ShowCursor(bool s);
	void SetBGColour(const dColour &s)       { m_BGColour=s; }
	void SetClearFrame(bool s)               { m_ClearFrame=s; }
	void SetClearZBuffer(bool s)             { m_ClearZBuffer=s; }
	void SetBackFaceCull(bool s)             { m_BackFaceCull=s; m_Initialised=false; }
	// default to ccw
	void SetFaceOrderClockwise(bool s)       {  m_FaceOrderClockwise=s; m_Initialised=false; }
	void SetDesiredFPS(float s)              { m_Deadline=1/s; }
	void SetFPSDisplay(bool s)               { m_FPSDisplay=s; }
	void SetFog(const dColour &c, float d, float s, float e)    
		{ m_FogColour=c; m_FogDensity=d; m_FogStart=s; m_FogEnd=e; m_Initialised=false; }
	
	double GetTime()                         { return m_Time; }
	double GetDelta()                        { return m_Delta; }
	
	enum stereo_mode_t {noStereo, crystalEyes, colourStereo};
 	bool SetStereoMode(stereo_mode_t mode);
 	stereo_mode_t GetStereoMode(){ return m_StereoMode;}
	void SetColourMask(bool inred, bool ingreen, bool inblue, bool inalpha);
	
private:
	void PreRender(bool PickMode=false);
	void PostRender();
	void RenderLights(bool camera);
	
	bool  m_Initialised;
	bool  m_InitLights;
	int   m_Width,m_Height;
	bool  m_MotionBlur;
	float m_Fade;
	bool  m_ShowAxis;
	Primitive *m_Grabbed;
	dColour m_BGColour;
	bool m_ClearFrame;
	bool m_ClearZBuffer;
	bool m_BackFaceCull;
	bool m_FaceOrderClockwise;
	dColour m_FogColour; 
	float m_FogDensity; 
	float m_FogStart; 
	float m_FogEnd;
	
    deque<State> m_StateStack;
    SceneGraph m_World;
	vector<Light*> m_LightVec;
	Camera m_Camera;
	ImmediateMode m_ImmediateMode;
	
	// info for picking mode
	struct SelectInfo
	{
		int x,y;
		int size;
	};
	
	SelectInfo m_SelectInfo;
	stereo_mode_t m_StereoMode;
	bool m_MaskRed,m_MaskGreen,m_MaskBlue,m_MaskAlpha;
	
	timeval m_LastTime;
	float m_Deadline;
	bool m_FPSDisplay;
	double m_Time;
	double m_Delta;
};
	
};

#endif
