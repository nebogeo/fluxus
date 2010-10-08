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

// TODO: check this works for Apple's OpenGL
#ifndef GL_POLYGON_OFFSET_EXT
#define GL_POLYGON_OFFSET_EXT ((GLenum)0x8037)
#endif

using namespace std;

namespace Fluxus
{

class State;
class Primitive;

///\mainpage The Fluxus Renderer (libfluxus)
/// This is the low level rendering engine for fluxus, which consists
/// of a scene graph and a selection of different geometry primitives 
/// for rendering realtime graphics.
///
/// This API is written in C++ (this is what 'fluxus the livecoding 
/// environment' binds to the Scheme language) and is designed for speed
/// and flexibility.

//////////////////////////////////////////////////////////////////////
/// This is the main interface for the fluxus renderer, all
/// control should come through this interface
class Renderer
{
public:
	Renderer(bool main = false);
	~Renderer();

	//////////////////////////////////////////////////////////////////////
	///@name Rendering control 
	///@{ 
	void Render();
	void Clear();
	///@}
	
	///////////////////////////////////////////////////////////////////////
	///@name State accessors
	///@{ 
	State *GetState();
	void ApplyState();
	void PushState();
	void PopState();
	/// Grabs the object state and makes it current
	void Grab(int ID);
	void UnGrab();
	Primitive *Grabbed() { return m_Grabbed; }
	///@}
	
	//////////////////////////////////////////////////////////////////////
	///@name Primitive handling
	///@{ 
	int          AddPrimitive(Primitive *Prim);
	Primitive   *GetPrimitive(int ID);
	/// Make sure it's removed from the physics engine too
	void         RemovePrimitive(int ID);
	/// Moves the primitive (and it's subtree) from it's position in the
	/// graph and adds it to the root.
	void         DetachPrimitive(int ID);
	dMatrix      GetGlobalTransform(int ID);
	dBoundingBox GetBoundingBox(int ID);
	/// Immediate mode (don't delete prim till after Render() - when it
	/// will actually be rendered
	void         RenderPrimitive(Primitive *Prim, bool del = false);
	/// Get primitive ID from screen space
	int          Select(unsigned int CamIndex, int x, int y, int size);
	/// Get all primitive IDs from screen space
	int          SelectAll(unsigned int CamIndex, int x, int y, int size, unsigned int **rIDs);
	///@}
	
	///////////////////////////////////////////////////////////////////////
	///@name Lights
	///@{ 
	int AddLight(Light *l);
	Light *GetLight(int id);
	void ClearLights();
	///@}

	////////////////////////////////////////////////////////////////////////
	///@name Camera
	///@{ 
	vector<Camera> &GetCameraVec() { return m_CameraVec; }
	unsigned int AddCamera(const Camera &cam) { m_CameraVec.push_back(cam); return m_CameraVec.size()-1; }
	///@}

	////////////////////////////////////////////////////////////////////////
	///@name Scenegraph access
	///@{
	SceneGraph &GetSceneGraph()        { return m_World; }
	///@}

	enum stereo_mode_t {noStereo, crystalEyes, colourStereo};

	////////////////////////////////////////////////////////////////////////
	///@name Global state control
	///@{
	void DrawText(const string &Text);
	void Reinitialise()                      { m_Initialised=false; }
	void SetMotionBlur(bool s, float a=0.02) { m_MotionBlur=s; m_Fade=a; }
	void SetResolution(int x, int y)         { m_Width=x; m_Height=y; m_Initialised=false; }
	void GetResolution(int &x, int &y)       { x=m_Width; y=m_Height; }
	TexturePainter *GetTexturePainter()      { return TexturePainter::Get(); }
	void ShowAxis(bool s)                    { m_ShowAxis=s; }
	void SetBGColour(const dColour &s)       { m_BGColour=s; }
	void SetClearFrame(bool s)               { m_ClearFrame=s; }
	void SetClearZBuffer(bool s)             { m_ClearZBuffer=s; }
	void SetClearAccum(bool s)               { m_ClearAccum=s; }
	void SetDesiredFPS(float s)              { m_Deadline=1/s; }
	void SetFPSDisplay(bool s)               { m_FPSDisplay=s; }
	void SetFog(const dColour &c, float d, float s, float e)
		{ m_FogColour=c; m_FogDensity=d; m_FogStart=s; m_FogEnd=e; m_Initialised=false; }
	void ShadowLight(unsigned int s)		 { m_ShadowLight=s; }
	void DebugShadows(bool s)				 { m_ShadowVolumeGen.SetDebug(s); }
	void ShadowLength(float s)				 { m_ShadowVolumeGen.SetLength(s); }
	double GetTime()                         { return m_Time; }
	double GetDelta()                        { return m_Delta; }
	bool SetStereoMode(stereo_mode_t mode);
	stereo_mode_t GetStereoMode(){ return m_StereoMode;}
	void PrintInfo();
	///@}

	////////////////////////////////////////////////////////////////////////
	///@name Thin interface to some hardware features
	///@{
	void SetColourMask(bool inred, bool ingreen, bool inblue, bool inalpha);
	void Accum(int mode, float factor);
	void DrawBuffer(GLenum mode);
	void ReadBuffer(GLenum mode);
	///@}


private:
	void PreRender(unsigned int CamIndex, bool PickMode=false);
	void PostRender();
	void RenderLights(bool camera);
	void RenderStencilShadows(unsigned int CamIndex);

	bool  m_MainRenderer;
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
	bool m_ClearAccum;
	dColour m_FogColour;
	float m_FogDensity;
	float m_FogStart;
	float m_FogEnd;
	unsigned int m_ShadowLight;

    deque<State> m_StateStack;
    SceneGraph m_World;
	vector<Light*> m_LightVec;
	vector<Camera> m_CameraVec;
	ImmediateMode m_ImmediateMode;
	ShadowVolumeGen m_ShadowVolumeGen;

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
