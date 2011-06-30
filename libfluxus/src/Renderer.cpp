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

#include "Renderer.h"
#include "State.h"
#include "Primitive.h"
#include "PixelPrimitive.h"
#include "PNGLoader.h"
#include "SearchPaths.h"
#include "PrimitiveIO.h"
#include "ShaderCache.h"
#include "GLSLShader.h"
#include "Trace.h"
#include "FFGLManager.h"
#include <sys/time.h>
#include <stdio.h>
#include <unistd.h>

using namespace Fluxus;

//#define DEBUG_TRACE

// should use glew for this?
#ifndef GL_POLYGON_OFFSET
#define GL_POLYGON_OFFSET GL_POLYGON_OFFSET_EXT
#endif

static const int FRAMES_PER_TIME = 10;
static int TimeCounter = 0;
static timeval StartTime;
static float FPS;

static const int MAXLIGHTS = 8;

Renderer::Renderer(bool main /* = false */) :
m_Initialised(false),
m_InitLights(false),
m_Width(640),
m_Height(480),
m_MotionBlur(false),
m_Fade(0.02f),
m_ShowAxis(false),
m_Grabbed(NULL),
m_ClearFrame(true),
m_ClearZBuffer(true),
m_ClearAccum(false),
m_FogDensity(0),
m_FogStart(0),
m_FogEnd(100),
m_ShadowLight(0),
m_StereoMode(noStereo),
m_MaskRed(true),
m_MaskGreen(true),
m_MaskBlue(true),
m_MaskAlpha(true),
m_Deadline(1/25.0f),
m_FPSDisplay(false),
m_Time(0),
m_Delta(0)
{
	m_MainRenderer = main;

	Clear();

	// stop valgrind complaining
	m_LastTime.tv_sec=0;
	m_LastTime.tv_usec=0;
}

Renderer::~Renderer()
{
	if (m_MainRenderer)
	{
		TexturePainter::Shutdown();
		SearchPaths::Shutdown();
		FFGLManager::Shutdown();
	}
}

/////////////////////////////////////

void Renderer::Clear()
{
	m_ShowAxis = false;

	m_FogDensity = 0;
	m_FogStart = 0;
	m_FogEnd = 100;

	m_FPSDisplay = false;

	m_BGColour = dColour(0, 0, 0, 1);

	m_World.Clear();
	m_StateStack.clear();
	m_CameraVec.clear();
	UnGrab();
	State InitialState;
	m_StateStack.push_back(InitialState);

	// add the default camera
	Camera cam;
	m_CameraVec.push_back(cam);
}

void Renderer::Render()
{
	///\todo collapse all these clears into one call with the bitfield
	if (m_ClearFrame && !m_MotionBlur)
	{
		glClearColor(m_BGColour.r,m_BGColour.g,m_BGColour.b,m_BGColour.a);
		glClear(GL_COLOR_BUFFER_BIT);
	}

	if (m_ClearZBuffer)
	{
		glClear(GL_DEPTH_BUFFER_BIT);
	}

	if (m_ClearAccum)
	{
		glClear(GL_ACCUM_BUFFER_BIT);
	}

	for (unsigned int cam=0; cam<m_CameraVec.size(); cam++)
	{
		// need to clear this even if we aren't using shadows
		m_ShadowVolumeGen.Clear();

		// if we are using multiple cameras, the renderer
		// needs to be reinitialised for each one
		if (m_CameraVec.size()>1) Reinitialise();

		if (m_ShadowLight!=0)
		{
			RenderStencilShadows(cam);
		}
		else
		{
			PreRender(cam);
			m_World.Render(&m_ShadowVolumeGen,cam);
			m_ImmediateMode.Render(cam);
			PostRender();
		}
	}

	m_ImmediateMode.Clear();

	if (m_MainRenderer)
	{
		FFGLManager::Get()->Render();
	}

	timeval ThisTime;
	// stop valgrind complaining
	ThisTime.tv_sec=0;
	ThisTime.tv_usec=0;

	gettimeofday(&ThisTime,NULL);
	m_Delta=(ThisTime.tv_sec-m_LastTime.tv_sec)+
			(ThisTime.tv_usec-m_LastTime.tv_usec)*0.000001f;

	if (m_Delta<m_Deadline)
	{
		//min 1 hz
		if(m_Deadline-m_Delta<1.0f)
		{
			usleep((int)((m_Deadline-m_Delta)*1000000.0f));
		}
	}

	m_LastTime=ThisTime;
	//if (m_Delta>0) m_Time=ThisTime.tv_sec+ThisTime.tv_usec*0.000001f;
	if (m_Delta>0.0f && m_Delta<100.0f) m_Time+=m_Delta;
}

void Renderer::RenderStencilShadows(unsigned int CamIndex)
{
	if (m_LightVec.size()>m_ShadowLight)
	{
		m_ShadowVolumeGen.SetLightPosition(m_LightVec[m_ShadowLight]->GetPosition());
	}
	
	PreRender(CamIndex);
	glDisable(GL_LIGHT0+m_ShadowLight); 
	m_World.Render(&m_ShadowVolumeGen,CamIndex);
	m_ImmediateMode.Render(CamIndex,&m_ShadowVolumeGen);

	glClear(GL_STENCIL_BUFFER_BIT);
	glEnable(GL_STENCIL_TEST);
	glStencilFunc(GL_ALWAYS, 0, ~0);
	glEnable(GL_DEPTH_TEST);
	glDepthFunc(GL_LESS);
	glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE);
	glDepthMask(GL_FALSE);
	glEnable(GL_CULL_FACE);

	glCullFace(GL_BACK);
    glStencilOp(GL_KEEP, GL_KEEP, GL_INCR);
	m_ShadowVolumeGen.GetVolume()->Render();

    glCullFace(GL_FRONT);
    glStencilOp(GL_KEEP, GL_KEEP, GL_DECR);
	m_ShadowVolumeGen.GetVolume()->Render();

	glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);
	glDepthFunc(GL_EQUAL);
	glStencilFunc(GL_EQUAL, 0, ~0);
	glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);

	glEnable(GL_BLEND);
	glBlendFunc(GL_ONE, GL_ONE);
	glCullFace(GL_BACK);

	glEnable(GL_LIGHT0+m_ShadowLight);

	m_World.Render(&m_ShadowVolumeGen,CamIndex);
	m_ImmediateMode.Render(CamIndex);
	m_ImmediateMode.Clear();

	glDepthMask(GL_TRUE);
	glDepthFunc(GL_LEQUAL);
	glStencilFunc(GL_ALWAYS, 0, ~0);
	

	if (m_ShadowVolumeGen.GetDebug())
	{
		m_ShadowVolumeGen.GetVolume()->GetState()->Hints=HINT_WIRE;
		m_ShadowVolumeGen.GetVolume()->Render();
		m_ShadowVolumeGen.GetVolume()->GetState()->Hints=HINT_SOLID;
	}

	PostRender();
}

void Renderer::PreRender(unsigned int CamIndex, bool PickMode)
{
	Camera &Cam = m_CameraVec[CamIndex];
    if (!m_Initialised || PickMode || Cam.NeedsInit())
    {
		GLSLShader::Init();

		glViewport((int)(Cam.GetViewportX()*(float)m_Width),(int)(Cam.GetViewportY()*(float)m_Height),
			(int)(Cam.GetViewportWidth()*(float)m_Width),(int)(Cam.GetViewportHeight()*(float)m_Height));

		#ifdef DEBUG_TRACE
		cerr<<"renderer:"<<this<<" viewport:"<<Cam.GetViewportX()*(float)m_Width<<" "<<Cam.GetViewportY()*(float)m_Height<<" "<<
			Cam.GetViewportWidth()*(float)m_Width<<" "<<Cam.GetViewportHeight()*(float)m_Height<<endl;
		#endif

		glMatrixMode (GL_PROJECTION);
  		glLoadIdentity();
		
  		if (PickMode) 
		{ 		
			GLint viewport[4]={0,0,m_Width,m_Height};
			gluPickMatrix(m_SelectInfo.x,m_Height-m_SelectInfo.y,
						m_SelectInfo.size,m_SelectInfo.size,viewport);
		}
		
  		Cam.DoProjection();
  		
    	glEnable(GL_BLEND);
    	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);	
		glEnable(GL_LIGHTING);
		
		glEnable(GL_CULL_FACE);
  		glCullFace(GL_BACK);
    	glFrontFace(GL_CCW);		 
		
    	glEnable(GL_RESCALE_NORMAL);
		glDisable(GL_COLOR_MATERIAL);

    	glEnableClientState(GL_VERTEX_ARRAY);
		glEnableClientState(GL_NORMAL_ARRAY);
		glEnableClientState(GL_TEXTURE_COORD_ARRAY);
		glEnableClientState(GL_COLOR_ARRAY);
		glEnable(GL_POLYGON_OFFSET);
		
		if (m_FogDensity>0)
		{
			glEnable(GL_FOG);
			glFogf(GL_FOG_MODE, GL_EXP);
			glFogfv(GL_FOG_COLOR, m_FogColour.arr());
			glFogf(GL_FOG_DENSITY, m_FogDensity);
			glFogf(GL_FOG_HINT, GL_DONT_CARE);
			glFogf(GL_FOG_START, m_FogStart);
			glFogf(GL_FOG_END, m_FogEnd);
		}
		else
		{
			glDisable(GL_FOG);
		}	
			
		glLightModeli(GL_LIGHT_MODEL_COLOR_CONTROL,GL_SEPARATE_SPECULAR_COLOR);
	
    	TexturePainter::Get()->Initialise();
    	
    	m_Initialised=true;
	}
	
	if (!m_InitLights)
	{
		// builds the default camera light
		ClearLights();
		m_InitLights=true;
	}
	
	
	glMatrixMode (GL_MODELVIEW);
  	glLoadIdentity();
	
	PushState();
	
	if (m_MotionBlur)
	{
		glEnable(GL_COLOR_MATERIAL);
		glPolygonMode(GL_FRONT,GL_FILL);
		glDisable(GL_DEPTH_TEST);
		glPushMatrix();
		glTranslatef(0,0,-10);
		glBegin(GL_QUADS);
			glColor4f(m_BGColour.r,m_BGColour.g,m_BGColour.b,m_Fade);
			glVertex3f(-10,-10,0);
			glVertex3f(10,-10,0);
			glVertex3f(10,10,0);
			glVertex3f(-10,10,0);
		glEnd();
		glPopMatrix();
		glEnable(GL_DEPTH_TEST);
		glDisable(GL_COLOR_MATERIAL);
	}

	if (m_FPSDisplay && !PickMode)
	{
		State DefaultState;
		m_StateStack.push_back(DefaultState);
		GetState()->Transform.translate(Cam.GetLeft(),Cam.GetBottom(),0);
		GetState()->Colour=dColour(0,0,1);
		char s[32];
		sprintf(s,"%f fps",FPS);
		DrawText(s);
		PopState();
	}

	RenderLights(true); // camera locked
	Cam.DoCamera(this);
	RenderLights(false); // world space
	
	// set the scene info so all primitives can read it (taken from ribbon prim)
	dMatrix ModelView;
	glGetFloatv(GL_MODELVIEW_MATRIX,ModelView.arr());
	dMatrix InvModelView=ModelView.inverse();
	Primitive::SetSceneInfo(InvModelView.transform_no_trans(dVector(0,0,1)),
							InvModelView.transform_no_trans(dVector(0,1,0)));
		
	glColorMask(m_MaskRed,m_MaskGreen,m_MaskBlue,m_MaskAlpha);
}


void Renderer::PostRender()
{
	// clear the texture, if the last primitive assigned one...
	TexturePainter::Get()->DisableAll();
	
	GLSLShader::Unapply();
	glFrontFace(GL_CCW);

	glDisable(GL_DEPTH_TEST);
	if (m_ShowAxis) Primitive::RenderAxes();
	glEnable(GL_DEPTH_TEST);
	glColorMask(true,true,true,true);
	
	PopState();
	
	if (m_FPSDisplay)
	{
    	if (!(TimeCounter%FRAMES_PER_TIME))
    	{
    		timeval TimeNow;
    		gettimeofday(&TimeNow,NULL);  	
    		FPS = (TimeNow.tv_sec-StartTime.tv_sec)+(TimeNow.tv_usec-StartTime.tv_usec)*0.000001f;
    		FPS/=(float)FRAMES_PER_TIME;
    		FPS=1/FPS; 	
    		gettimeofday(&StartTime,NULL);
    	}
    	TimeCounter++;
	}
	
	//if (m_StateStack.size()!=1)
	//{
	//	Trace::Stream<<"State mismatch: stack size "<<m_StateStack.size()<<" at end scene"<<endl;
	//}
}

void Renderer::RenderLights(bool camera)
{
	int n=0;
	for (vector<Light*>::iterator i=m_LightVec.begin(); i!=m_LightVec.end(); i++)
	{
		if (n<MAXLIGHTS && (*i)->GetCameraLock()==camera) 
		{
			(*i)->Render();
		}
		n++;
	}
}

int Renderer::AddLight(Light *l)
{
	l->SetIndex(m_LightVec.size());
	m_LightVec.push_back(l);
	return m_LightVec.size()-1;
}

Light *Renderer::GetLight(int id)
{
	if (id<(int)m_LightVec.size()) return m_LightVec[id];
	else return NULL;
}

void Renderer::ClearLights()
{
	for (unsigned int n=0; n<m_LightVec.size(); n++)
	{
		glDisable(GL_LIGHT0+n);
	}
	
	m_LightVec.clear();
	
	// add a default light
	Light *light=new Light;
	light->SetPosition(dVector(0,0,0));
	light->SetCameraLock(true);
	AddLight(light);
}

int Renderer::Select(unsigned int CamIndex, int x, int y, int size)
{
	static const int SELECT_SIZE=512;
	unsigned int IDs[SELECT_SIZE];
	memset(IDs,0,SELECT_SIZE);
	GLuint ID=0;
	glSelectBuffer(SELECT_SIZE,(GLuint*)IDs);
	glRenderMode(GL_SELECT);
	glInitNames();
	
	m_SelectInfo.x=x;
	m_SelectInfo.y=y;
	m_SelectInfo.size=size;
	
	// the problem here is that select is called mid-scene, so we have to set up for 
	// picking mode here...
	PreRender(CamIndex,true);
	
	// render the scene for picking
	m_World.Render(&m_ShadowVolumeGen,SceneGraph::SELECT);
	
	int hits=glRenderMode(GL_RENDER);
	unsigned int *ptr=IDs, numnames;
	float minz,maxz,closest=1000000;

	// process the hit records
	for (int n=0; n<hits; n++)
	{
		numnames=*ptr;
		ptr++;
		minz = (float) *ptr++/0x7fffffff;
		maxz = (float) *ptr++/0x7fffffff;
				
		// find the closest one
		if (closest>minz) 
		{
			closest=minz;
			ID=*ptr;
		}
		for (unsigned int i=0; i<numnames; i++) *ptr++;
	}
	
	// ... and reset the scene back here so we can carry on afterwards as if nothing
	// has happened...
	m_Initialised=false;
	PreRender(CamIndex);
	
	return ID;
}

int Renderer::SelectAll(unsigned int CamIndex, int x, int y, int size, unsigned int **rIDs)
{
	static const int SELECT_SIZE=512;
	unsigned int IDs[SELECT_SIZE];
	static unsigned int OutputIDs[SELECT_SIZE];

	memset(IDs,0,SELECT_SIZE);
	GLuint ID=0;
	glSelectBuffer(SELECT_SIZE,(GLuint*)IDs);
	glRenderMode(GL_SELECT);
	glInitNames();

	m_SelectInfo.x=x;
	m_SelectInfo.y=y;
	m_SelectInfo.size=size;

	// the problem here is that select is called mid-scene, so we have to set up for 
	// picking mode here...
	PreRender(CamIndex,true);

	// render the scene for picking
	m_World.Render(&m_ShadowVolumeGen,SceneGraph::SELECT);

	int hits=glRenderMode(GL_RENDER);
	unsigned int *ptr=IDs, numnames;
	float minz,maxz,closest=1000000;

	// process the hit records
	for (int n=0; n<hits; n++)
	{
		numnames=*ptr;
		ptr+=3;
		OutputIDs[n] = *ptr; // save ID
		for (unsigned int i=0; i<numnames; i++) *ptr++;
	}

	// ... and reset the scene back here so we can carry on afterwards as if nothing
	// has happened...
	m_Initialised=false;
	PreRender(CamIndex);

	*rIDs = OutputIDs;
	return hits;
}

int Renderer::AddPrimitive(Primitive *Prim)
{
	Prim->SetState(GetState());
	SceneNode *node = new SceneNode(Prim);
	int ret=m_World.AddNode(GetState()->Parent,node);
	m_World.RecalcAABB(node);
	return ret;
}

Primitive *Renderer::GetPrimitive(int ID)
{
	SceneNode *node = (SceneNode*)m_World.FindNode(ID);
	if (node==NULL) return NULL;
	return node->Prim;
}

void Renderer::RemovePrimitive(int ID)
{
	SceneNode *node = (SceneNode*)m_World.FindNode(ID);
	if (node!=NULL)
	{
		if (node->Prim==m_Grabbed) UnGrab();
		m_World.RemoveNode(node);
	}
}

void Renderer::DetachPrimitive(int ID)
{
	SceneNode *node=(SceneNode*)m_World.FindNode(ID);
	if (node) m_World.Detach(node);
}

// immediate mode
void Renderer::RenderPrimitive(Primitive *Prim, bool del /* = false */)
{
	m_ImmediateMode.Add(Prim, GetState(), del);
}

dMatrix Renderer::GetGlobalTransform(int ID)
{
	dMatrix mat;
	SceneNode *node=(SceneNode*)m_World.FindNode(ID);
	if (node) mat=m_World.GetGlobalTransform(node);
	return mat;
}

dBoundingBox Renderer::GetBoundingBox(int ID)
{
	dBoundingBox bbox;
	SceneNode *node=(SceneNode*)m_World.FindNode(ID);
	if (node) m_World.GetBoundingBox(node,bbox);
	return bbox;
}

///////////////////////////////	
// state accessors
State *Renderer::GetState()
{
	if (m_StateStack.empty())
	{
		Trace::Stream<<"Renderer::GetState : State stack is empty"<<endl;
		return NULL;
	}
	
	return &(*m_StateStack.begin());
}

void Renderer::ApplyState()
{
	GetState()->Apply();
}

void Renderer::PushState()
{
	#ifdef DEBUG_TRACE
	//Trace::Stream<<"Renderer::PushState"<<endl;
	#endif
	
	m_StateStack.push_front(*GetState());
}

void Renderer::PopState()
{
	#ifdef DEBUG_TRACE
	//Trace::Stream<<"Renderer::PopState"<<endl;
	#endif
	
	if (m_StateStack.size()<2)
	{
		Trace::Stream<<"Renderer::PopState : only one state left, not popping"<<endl;
	}
	else
	{
		m_StateStack.pop_front();
	}
}

void Renderer::Grab(int ID)
{
	SceneNode *n=(SceneNode *)m_World.FindNode(ID);
	if (n)
	{
		Primitive *p=n->Prim;
		if (p)
		{
			m_Grabbed=p;
		}
	}
}

void Renderer::UnGrab()
{
	m_Grabbed=NULL;
}

//void Renderer::Apply(int id)
//{
//	Primitive *p = GetPrimitive(id);
//	if (p) p->ApplyTransform();
//}

void Renderer::DrawText(const string &Text)
{
	glPushMatrix();	
	GetState()->Apply();
	//glDisable(GL_DEPTH_TEST);
	glDisable(GL_LIGHTING);
	glPushMatrix();
	glRasterPos3f(0.0, 0.0, -1.1);
	for (unsigned int n=0; n<Text.length(); n++)
	{
		glutBitmapCharacter(GLUT_BITMAP_HELVETICA_12, Text.c_str()[n]);
		glTranslatef(1.0f,0.0f,0.0f);
	}
	glPopMatrix();
	glEnable(GL_LIGHTING);
	//glEnable(GL_DEPTH_TEST);
	glPopMatrix();	
}

void Renderer::DrawBuffer(GLenum mode)
{
	glDrawBuffer(mode);
}

void Renderer::ReadBuffer(GLenum mode)
{
	glReadBuffer(mode);
}

bool Renderer::SetStereoMode(stereo_mode_t mode)
{
 	GLboolean stereoWindowTest;
 	switch(mode){
 		case noStereo: m_StereoMode = noStereo;
 			return true;
 		case crystalEyes:
 			//test for a stereo window
 			glGetBooleanv (GL_STEREO, &stereoWindowTest);
 			if(stereoWindowTest){
 				m_StereoMode = crystalEyes;
 				return true;
 			} else {
 				m_StereoMode = noStereo;
 				return false;
 			}
 		case colourStereo:
 			m_StereoMode = colourStereo;
 			return true;
 	};
 	return false;
}

void Renderer::SetColourMask(bool inred, bool ingreen, bool inblue, bool inalpha)
{
	m_MaskRed=inred;
	m_MaskGreen=ingreen;
	m_MaskBlue=inblue;
	m_MaskAlpha=inalpha;
}

void Renderer::Accum(int mode, float factor)
{
	glAccum(mode,factor);
}

void Renderer::PrintInfo()
{
	Trace::Stream<<"Fluxus Version "<<FLUXUS_MAJOR_VERSION<<"."<<FLUXUS_MINOR_VERSION<<endl;
	Trace::Stream<<"Textures Cached:"<<endl;
	TexturePainter::Get()->Dump();
	Trace::Stream<<"Primitives Cached:"<<endl;
	PrimitiveIO::Dump();
	Trace::Stream<<"Shaders cached:"<<endl;
	ShaderCache::Dump();	
	Trace::Stream<<"Scenegraph:"<<endl;
	m_World.Dump();	
	Trace::Stream<<"NumRendered:"<<m_World.GetNumRendered()<<endl;
	Trace::Stream<<"HighWater:"<<m_World.GetHighWater()<<endl;
}
