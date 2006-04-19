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
#include "PNGLoader.h"
#include <sys/time.h>
#include <stdio.h>
#include <unistd.h>

using namespace fluxus;

//#define DEBUG_TRACE

static const int FRAMES_PER_TIME = 10;
static int TimeCounter = 0;
static timeval StartTime;
static float FPS;

static const int MAXLIGHTS = 8;

Renderer::Renderer() :
m_Initialised(false),
m_Width(640),
m_Height(480),
m_MotionBlur(false),
m_Fade(0.02f),
m_InScene(false),
m_Ortho(false),
m_LockedCamera(false),
m_CameraLag(0),
m_ShowAxis(false),
m_Grabbed(NULL),
m_ClearFrame(true),
m_ClearZBuffer(false),
m_DepthTest(true),
m_Up(-1),
m_Down(1),
m_Left(-0.75),
m_Right(0.75),
m_Front(1),
m_Back(10000),
m_OrthZoom(1.0f),
m_BackFaceCull(true),
m_FaceOrderClockwise(false),
m_FogDensity(0), 
m_FogStart(0),
m_FogEnd(100),
m_FeedBack(false),
m_FeedBackData(NULL),
m_FeedBackID(0),
m_LoadedFromFlx(false),
m_Deadline(1/25.0f),
m_FPSDisplay(false),
m_Time(0),
m_Delta(0),
EngineCallback(NULL)
{
	State InitialState;
	m_StateStack.push_back(InitialState);
	
	// add a default light
	Light *light=new Light;
	light->SetPosition(dVector(0,0,0));
	light->SetCameraLock(true);
	AddLight(light);

	InitFeedback();
}

Renderer::~Renderer()
{
	TexturePainter::Shutdown();
}

/////////////////////////////////////	
// retained mode
void Renderer::BeginScene(bool PickMode)
{
	if (m_InScene)
	{
		cerr<<"Renderer::BeginScene : already in scene, aborting"<<endl;
		return;
	}
	
	#ifdef DEBUG_TRACE
    cerr<<"Renderer::BeginScene"<<endl;
    #endif
    if (!m_Initialised || PickMode)
    {
    	glViewport(0,0,m_Width,m_Height);

    	glMatrixMode (GL_PROJECTION);
  		glLoadIdentity();
		
  		if (PickMode) 
		{ 		
			GLint viewport[4]={0,0,m_Width,m_Height};
			gluPickMatrix(m_SelectInfo.x,m_Height-m_SelectInfo.y,
						m_SelectInfo.size,m_SelectInfo.size,viewport);
		}
		
  		if (m_Ortho) glOrtho(m_Down*m_OrthZoom,m_Up*m_OrthZoom,m_Right*m_OrthZoom,m_Left*m_OrthZoom,m_Front,m_Back);
  		else glFrustum(m_Up,m_Down,m_Left,m_Right,m_Front,m_Back);
		
    	glEnable(GL_BLEND);
    	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);	
		glEnable(GL_LIGHTING);
		
		if (m_BackFaceCull)
		{
			glEnable(GL_CULL_FACE);
  			glCullFace(GL_BACK);
    	}
		else
		{
			glDisable(GL_CULL_FACE);
		}
		
		if (m_FaceOrderClockwise) glFrontFace(GL_CW);
		else glFrontFace(GL_CCW);
		
		if (m_DepthTest) glEnable(GL_DEPTH_TEST);
		else glDisable(GL_DEPTH_TEST);
		 
    	glEnable(GL_RESCALE_NORMAL);
		glDisable(GL_COLOR_MATERIAL);

    	glEnableClientState(GL_VERTEX_ARRAY);
		glEnableClientState(GL_NORMAL_ARRAY);
		glEnableClientState(GL_TEXTURE_COORD_ARRAY);
		glEnableClientState(GL_COLOR_ARRAY);

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
	
	if (m_ClearFrame)
	{
		glClearColor(m_BGColour.r,m_BGColour.g,m_BGColour.b,m_BGColour.a);	
		if (m_MotionBlur || m_FeedBack) glClear(GL_DEPTH_BUFFER_BIT);
		else glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);	
	}
	
	if (m_ClearZBuffer)
	{
		glClear(GL_DEPTH_BUFFER_BIT);
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
	
	if (m_FeedBack)
	{       	
		
		glEnable(GL_TEXTURE_2D);
		glDisable(GL_DEPTH_TEST);
		glDisable(GL_LIGHTING);
		glPolygonMode(GL_FRONT,GL_FILL);
		glBindTexture(GL_TEXTURE_2D,m_FeedBackID);
		
		glPushMatrix();
		glMultMatrixf(m_FeedBackMat.arr());
		glTranslatef(0,0,-10);
		glBegin(GL_QUADS);
			glTexCoord2f(0,0);
			glVertex3f(-10,-10,0);
			glTexCoord2f(1,0);
			glVertex3f(10,-10,0);
			glTexCoord2f(1,1);
			glVertex3f(10,10,0);
			glTexCoord2f(0,1);
			glVertex3f(-10,10,0);
		glEnd();
		glPopMatrix();
		
		glEnable(GL_DEPTH_TEST);
		glEnable(GL_LIGHTING);
		glDisable(GL_TEXTURE_2D);
	}
	

	
	if (m_FPSDisplay)
	{
		PushState();
		GetState()->Transform.translate(m_Up,m_Left,0);
		GetState()->Colour=dColour(0,0,1);
		char s[32];
		sprintf(s,"%f fps",FPS);
    	if (!PickMode) DrawText(s);
    	PopState();
	}
	
	RenderLights(true); // camera locked
	
	//GetState()->Transform*=m_Camera;
	glMultMatrixf(m_Camera.arr());
	
	RenderLights(false); // world space
	
	if (m_LockedCamera)
	{
		Primitive *p = GetPrimitive(m_CameraAttached);
		if (p)
		{
			if (m_CameraLag!=0)
			{
				m_LockedMatrix.blend(p->GetState()->Transform.inverse(),m_CameraLag);
			}
			else
			{
				m_LockedMatrix=p->GetState()->Transform.inverse();
			}
			
			glMultMatrixf(m_LockedMatrix.arr());		
		}
		else m_LockedCamera=false;
	}

}

void Renderer::Render()
{
	if (!m_LoadedFromFlx) ClearIMPrimitives();
	
	BeginScene();
	glPushMatrix();
	if (EngineCallback) EngineCallback();
	glPopMatrix();
	m_World.Render();
	RenderIMPrimitives();	
	EndScene();
	
	if (m_LoadedFromFlx) ClearIMPrimitives();
	
	timeval ThisTime;
	gettimeofday(&ThisTime,NULL);
	m_Delta=(ThisTime.tv_sec-m_LastTime.tv_sec)+
			(ThisTime.tv_usec-m_LastTime.tv_usec)*0.000001f;

	if (m_Delta<m_Deadline)
	{
		//min 1 hz
		if(m_Deadline-m_Delta<1.0f)
		{
			usleep((int)((m_Deadline-m_Delta)*1000000));
		}
	}
	
	m_LastTime=ThisTime;
	if (m_Delta>0) m_Time+=m_Delta;
}

void Renderer::EndScene()
{
	#ifdef DEBUG_TRACE
	cerr<<"Renderer::EndScene"<<endl;
	#endif
	
	// clear the texture, if the last primitive assigned one...
	glDisable(GL_TEXTURE_2D);

	if (m_ShowAxis) DrawAxes();
	
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
	
	if (m_FeedBack)
	{  
		// grab the display
		glPixelStorei(GL_PACK_ALIGNMENT, 1);
		glReadPixels(0, 0, m_Width, m_Height, GL_RGB, GL_UNSIGNED_BYTE, m_FeedBackData);
		glDeleteTextures(1,&m_FeedBackID);
		glGenTextures(1,&m_FeedBackID);
		glBindTexture(GL_TEXTURE_2D,m_FeedBackID);
		//glTexImage2D(GL_TEXTURE_2D, 0, 3, m_Width, m_Height, 0, GL_RGB, GL_UNSIGNED_BYTE, m_FeedBackData);
		gluBuild2DMipmaps(GL_TEXTURE_2D,3,m_Width, m_Height,GL_RGB,GL_UNSIGNED_BYTE,m_FeedBackData);
	}
	
	//if (m_StateStack.size()!=1)
	//{
	//	cerr<<"State mismatch: stack size "<<m_StateStack.size()<<" at end scene"<<endl;
	//}
}

void Renderer::RenderLights(bool camera)
{
	int n=0;
	for (vector<Light*>::iterator i=m_LightVec.begin(); i!=m_LightVec.end(); i++)
	{
		if (n<MAXLIGHTS && (*i)->GetCameraLock()==camera) 
		{
			(*i)->Render(n);
		}
		n++;
	}
}

int Renderer::AddLight(Light *l)
{
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
	m_LightVec.clear();
}

int Renderer::Select(int x, int y, int size)
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
	BeginScene(true);
	
	// render the scene for picking
	m_World.Render(SceneGraph::SELECT);
	
	// ... and reset the scene back here so we can carry on afterwards as if nothing
	// has happened...
	m_Initialised=false;
	BeginScene();
	
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
	return ID;
}

dMatrix Renderer::GetProjection()
{
	dMatrix Projection;
	glGetFloatv(GL_PROJECTION_MATRIX,Projection.arr());
	return Projection;
}

void Renderer::LockCamera(int Prim)
{
     Primitive *p = GetPrimitive(Prim);
     if (!p)
     {
     	m_CameraAttached=0;
     	m_LockedCamera=false;
     	return;
     }
     m_LockedCamera=true;
     m_CameraAttached=Prim;
}

void Renderer::Clear()
{
	m_World.Clear();
	m_StateStack.clear();
	UnGrab();
	
	State InitialState;
	m_StateStack.push_back(InitialState);
}	

int Renderer::AddPrimitive(Primitive *Prim)
{
	Prim->SetState(GetState());
	SceneNode *node = new SceneNode(Prim);
	return m_World.AddNode(GetState()->Parent,node);
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

////////////////////////////	
// immediate mode
void Renderer::RenderPrimitive(Primitive *Prim)
{	
	// store the primitive and current state
	// to err, render them later!!!
	IMItem *newitem = new IMItem;
	newitem->m_State = *GetState();
	newitem->m_Primitive = Prim;
	m_IMRecord.push_back(newitem);
	
	// render hints need to be in the primitives state to be picked up
	// annoying... make sure this doesn't happed too much
	Prim->GetState()->Hints=newitem->m_State.Hints;
}

void Renderer::RenderIMPrimitives()
{
	for(vector<IMItem*>::iterator i=m_IMRecord.begin(); i!=m_IMRecord.end(); ++i)
	{
		glPushMatrix();
		(*i)->m_State.Apply();
		(*i)->m_Primitive->Render();
		glPopMatrix();
	}
}

void Renderer::ClearIMPrimitives()
{
	for(vector<IMItem*>::iterator i=m_IMRecord.begin(); i!=m_IMRecord.end(); ++i)
	{
		// only delete the primitive pointer if we've created it...
		if (m_LoadedFromFlx) delete (*i)->m_Primitive;
		delete *i;
	}
	
	m_IMRecord.clear();
}

///////////////////////////////	
// state accessors
State *Renderer::GetState()
{
	if (m_StateStack.empty())
	{
		cerr<<"Renderer::GetState : State stack is empty"<<endl;
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
	cerr<<"Renderer::PushState"<<endl;
	#endif
	
	m_StateStack.push_front(*GetState());
}

void Renderer::PopState()
{
	#ifdef DEBUG_TRACE
	cerr<<"Renderer::PopState"<<endl;
	#endif
	
	if (m_StateStack.size()<2)
	{
		cerr<<"Renderer::PopState : only one state left, not popping"<<endl;
	}
	else
	{
		m_StateStack.pop_front();
	}
}

void Renderer::DrawAxes()
{
	glLineWidth(1);
    glDisable(GL_DEPTH_TEST);
	glDisable(GL_LIGHTING);
	glBegin(GL_LINES);
		glColor3f(1,0,0);
		glVertex3f(0,0,0);
		glVertex3f(1,0,0);
	
		glColor3f(0,1,0);
		glVertex3f(0,0,0);
		glVertex3f(0,1,0);
		
		glColor3f(0,0,1);
		glVertex3f(0,0,0);
		glVertex3f(0,0,1);
	glEnd();

    glColor3f(1, 0, 0);
    glRasterPos3f(1.1, 0.0, 0.0);
    glutBitmapCharacter(GLUT_BITMAP_HELVETICA_12, 'x');
    glColor3f(0, 1, 0);
    glRasterPos3f(0.0, 1.1, 0.0);
    glutBitmapCharacter(GLUT_BITMAP_HELVETICA_12, 'y');
    glColor3f(0, 0, 1);
    glRasterPos3f(0.0, 0.0, 1.1);
    glutBitmapCharacter(GLUT_BITMAP_HELVETICA_12, 'z');
    glEnable(GL_LIGHTING);
	glEnable(GL_DEPTH_TEST);
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

void Renderer::AddToLibrary(const string &name, Primitive *p)
{
	map<string,LibraryEntry>::iterator i = m_CompiledLibrary.find(name);
	if (i==m_CompiledLibrary.end())
	{
		int list = glGenLists(1);
		glNewList(list,GL_COMPILE);
		p->Render();
		glEndList();
		LibraryEntry newentry;
		newentry.ID=list;
		newentry.BBox=p->GetBoundingBox();
		m_CompiledLibrary[name]=newentry;
	}
	else
	{
		cerr<<name<<" already exists in the library"<<endl;
	}
}	

bool Renderer::GetFromLibrary(const string &name, Renderer::LibraryEntry &li)
{
	map<string,LibraryEntry>::iterator i = m_CompiledLibrary.find(name);
	if (i!=m_CompiledLibrary.end())
	{
		li=i->second;
		return true;
	}
	
	cerr<<name<<" doesn't exist in the library"<<endl;
	return false;
}

void Renderer::InitFeedback()
{
	if (m_FeedBackData)	delete[] m_FeedBackData;
	m_FeedBackData = new char[m_Width * m_Height * 3];
	if (m_FeedBackID==0) glGenTextures(1,&m_FeedBackID);
}

void Renderer::ShowCursor(bool s)
{
	if (s)
	{
		glutSetCursor(GLUT_CURSOR_INHERIT); 
	}
	else 
	{
		glutSetCursor(GLUT_CURSOR_NONE); 
	}

}
