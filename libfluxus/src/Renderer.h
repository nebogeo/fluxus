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
 
#include <sys/time.h>
#include "dada.h"
#include "deque"
#include "map"
#include "vector"
#include "string"
#include "SceneGraph.h"
#include "Light.h"
#include "TexturePainter.h"

#ifndef N_RENDERER
#define N_RENDERER

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
	// Rendering control (retained mode)
	void BeginScene(bool PickMode=false);
	void Render();
	void EndScene();	
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
	// immediate mode (don't delete prim before frame end - it's immediate-ish :))
	void         RenderPrimitive(Primitive *Prim);
	// Get primitive ID from screen space
	int          Select(int x, int y, int size);
	
	///////////////////////////////////////////////////////////////////////
	// Lights
	int AddLight(Light *l);
	Light *GetLight(int id);
	void ClearLights();
	
	////////////////////////////////////////////////////////////////////////
	// Scenegraph access
	const SceneGraph &GetSceneGraph()        { return m_World; }
	void PrintSceneGraph()                   { m_World.Dump(); }
		
	////////////////////////////////////////////////////////////////////////
	// Global state control
	void DrawAxes();
	void DrawText(const string &Text);
	void Reinitialise()                      { m_Initialised=false; }
	void SetMotionBlur(bool s, float a=0.02) { m_MotionBlur=s; m_Fade=a; }
	void SetResolution(int x, int y)         { m_Width=x; m_Height=y; m_Initialised=false; }
	dMatrix *GetCamera()                     { return &m_Camera; }
	void LockCamera(int Prim);
	void UnlockCamera()						 { m_LockedCamera=false; }
	void SetOrtho(bool s)                    { m_Ortho=s; m_Initialised=false; }
	void SetFrustum(float u, float d, float l, float r, float f, float b) 
		{ m_Up=u; m_Down=d; m_Left=l; m_Right=r; m_Front=f; m_Back=b; m_Initialised=false; }
	unsigned int LoadTexture(const string &Filename) { return TexturePainter::Get()->LoadTexture(Filename); }
	void ShowAxis(bool s)                    { m_ShowAxis=s; }
	void SetBGColour(const dColour &s)       { m_BGColour=s; }
	void SetClearFrame(bool s)               { m_ClearFrame=s; }
	void SetClearZBuffer(bool s)             { m_ClearZBuffer=s; }
	void SetBackFaceCull(bool s)             { m_BackFaceCull=s; m_Initialised=false; }
	void SetDepthTest(bool s)                { m_DepthTest=s; m_Initialised=false; }
	// default to ccw
	void SetFaceOrderClockwise(bool s)       {  m_FaceOrderClockwise=s; m_Initialised=false; }
	void SetDesiredFPS(float s)              { m_Deadline=1/s; }
	void SetFPSDisplay(bool s)               { m_FPSDisplay=s; }
	
	typedef void (cb)();
	void SetEngineCallback(cb *s)            { EngineCallback=s; }
	
	struct LibraryEntry
	{	
		int ID;
		dBoundingBox BBox;
	};
	
	// must be called after the render has been run once.
	// used for building a library of shapes to use for
	// compiled primitives. the primitive p may be deleted
	// after this call.
	void AddToLibrary(const string &name, Primitive *p);
	bool GetFromLibrary(const string &name, LibraryEntry &li);

private:
	void RenderLights(bool camera);
	
	bool  m_Initialised;
	int   m_Width,m_Height;
	bool  m_MotionBlur;
	float m_Fade;
	bool  m_InScene;
	bool  m_Ortho;
	int   m_CameraAttached;
	bool  m_LockedCamera;
	bool  m_ShowAxis;
	Primitive *m_Grabbed;
	dColour m_BGColour;
	bool m_ClearFrame;
	bool m_ClearZBuffer;
	bool m_DepthTest;
	float m_Up,m_Down,m_Left,m_Right,m_Front,m_Back;
	bool m_BackFaceCull;
	bool m_FaceOrderClockwise;
	
	dMatrix m_Camera;
    deque<State> m_StateStack;
    SceneGraph m_World;
	vector<Light*> m_LightVec;
	
    /////////////////////////////////////////////////////
    // Immediate-ish mode
    // we need to keep a record of immediate mode requests to
    // potentially save in the flx file. as primitive pointers
    // are stored, prims drawn in immediate mode have to stick
    // around till the end of frame at least.
    struct IMItem
    {
    	State m_State;
    	Primitive *m_Primitive;
   	};
	
   	vector<IMItem*> m_IMRecord;
    // also renders the IM stuff in one batch, as they could have been
    // loaded from an flx file
    void RenderIMPrimitives();
    void ClearIMPrimitives();

    // need to know, so we can delete the IM primitives newed by the loader.
    bool m_LoadedFromFlx;

	// info for picking mode
	struct SelectInfo
	{
		int x,y;
		int size;
	};
	
	SelectInfo m_SelectInfo;
	
	timeval m_LastTime;
	float m_Deadline;
	bool m_FPSDisplay;
	
	map<string,LibraryEntry> m_CompiledLibrary;
	
    cb *EngineCallback;

    friend istream &operator>>(istream &s, Renderer &o);
	friend ostream &operator<<(ostream &s, Renderer &o);
};

istream &operator>>(istream &s, Renderer &o);
ostream &operator<<(ostream &s, Renderer &o);
	
};

#endif
