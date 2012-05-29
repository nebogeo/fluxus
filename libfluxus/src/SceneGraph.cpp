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

#include "SceneGraph.h"
#include "PolyPrimitive.h"
#include "PixelPrimitive.h"

using namespace Fluxus;

SceneGraph::SceneGraph() :
m_NumRendered(0),
m_HighWater(0)
{
	// need to reset to having a root node present
	Clear();
}

SceneGraph::~SceneGraph()
{
}

void SceneGraph::Render(ShadowVolumeGen *shadowgen, unsigned int camera, Mode rendermode)
{
	glGetFloatv(GL_MODELVIEW_MATRIX,m_TopTransform.arr());
	
	// get the frustum planes for culling later on
	dMatrix total;
	glGetFloatv(GL_PROJECTION_MATRIX,total.arr());
	total=total*m_TopTransform;
	GetFrustumPlanes(m_FrustumPlanes, total, false);
	
	unsigned int cameracode = 1<<camera;

	m_NumRendered=0;

	// render all the children of the root
	for (vector<Node*>::iterator i=m_Root->Children.begin(); i!=m_Root->Children.end(); ++i)
	{
		RenderWalk((SceneNode*)*i,0,cameracode,shadowgen,rendermode);
	}

	// now render the depth sorted primitives:
	m_DepthSorter.Render();
	m_DepthSorter.Clear();
	
	if (m_NumRendered>m_HighWater) m_HighWater=m_NumRendered;
}

void SceneGraph::RenderWalk(SceneNode *node,  int depth, unsigned int cameracode, ShadowVolumeGen *shadowgen, Mode rendermode)
{
	// max gl matrix stack is 32
	/*if (depth>=30)
	{
		Trace::Stream<<"SceneGraph::RenderWalk: max stack reached"<<endl;
		return;
	}*/

	if ((node->Prim->GetVisibility()&cameracode)==0) return;
	if (rendermode==SELECT && !node->Prim->IsSelectable()) return;

	dMatrix parent;
	// see if we need the parent (result of all the parents) transform
	if (node->Prim->GetState()->Hints & HINT_DEPTH_SORT)
	{
		glGetFloatv(GL_MODELVIEW_MATRIX,parent.arr());
	}

	glPushMatrix();

	// if we are a lazy parent then we need to ignore
	// the effects of the heirachical transform - we
	// treat their transform as a world space one
	if (node->Prim->GetState()->Hints & HINT_LAZY_PARENT)
	{
		glLoadMatrixf(m_TopTransform.arr());
	}


	node->Prim->ApplyState();

	if (!(node->Prim->GetState()->Hints & HINT_FRUSTUM_CULL) || FrustumClip(node))
	{
		if (node->Prim->GetState()->Hints & HINT_DEPTH_SORT)
		{
			// render it later, and after depth sorting
			m_DepthSorter.Add(parent,node->Prim,node->ID);
		}
		else
		{
			glPushName(node->ID);
			node->Prim->Prerender();
			node->Prim->Render();
			glPopName();
		}

		m_NumRendered++;
		depth++;

		for (vector<Node*>::iterator i=node->Children.begin(); i!=node->Children.end(); ++i)
		{
			RenderWalk((SceneNode*)*i,depth,cameracode,shadowgen,rendermode);
		}
	}

	node->Prim->UnapplyState();
	glPopMatrix();

	if (node->Prim->GetState()->Hints & HINT_CAST_SHADOW)
	{
		shadowgen->Generate(node->Prim);
	}
}

// from Fast Extraction of Viewing Frustum Planes from the World-View-Projection Matrix
// by Gil Gribb and Klaus Hartmann, thanks to flipcode
void SceneGraph::GetFrustumPlanes(dPlane *planes, dMatrix m, bool normalise)
{
  // Left clipping plane
  planes[0].a = m.m[0][3] + m.m[0][0];
  planes[0].b = m.m[1][3] + m.m[1][0];
  planes[0].c = m.m[2][3] + m.m[2][0];
  planes[0].d = m.m[3][3] + m.m[3][0];
  // Right clipping nlape
  planes[1].a = m.m[0][3] - m.m[0][0];
  planes[1].b = m.m[1][3] - m.m[1][0];
  planes[1].c = m.m[2][3] - m.m[2][0];
  planes[1].d = m.m[3][3] - m.m[3][0];
  // Top clipping pl nea
  planes[2].a = m.m[0][3] - m.m[0][1];
  planes[2].b = m.m[1][3] - m.m[1][1];
  planes[2].c = m.m[2][3] - m.m[2][1];
  planes[2].d = m.m[3][3] - m.m[3][1];
  // Bottom clippingapl ne
  planes[3].a = m.m[0][3] + m.m[0][1];
  planes[3].b = m.m[1][3] + m.m[1][1];
  planes[3].c = m.m[2][3] + m.m[2][1];
  planes[3].d = m.m[3][3] + m.m[3][1];
  // Near clipping peanl
  planes[4].a = m.m[0][3] + m.m[0][2];
  planes[4].b = m.m[1][3] + m.m[1][2];
  planes[4].c = m.m[2][3] + m.m[2][2];
  planes[4].d = m.m[3][3] + m.m[3][2];
  // Far clipping pl nea
  planes[5].a = m.m[0][3] - m.m[0][2];
  planes[5].b = m.m[1][3] - m.m[1][2];
  planes[5].c = m.m[2][3] - m.m[2][2];
  planes[5].d = m.m[3][3] - m.m[3][2];
  
  // Normalize the plane equations, if requested
  if (normalise)
  {
      planes[0].normalise();
      planes[1].normalise();
      planes[2].normalise();
      planes[3].normalise();
      planes[4].normalise();
      planes[5].normalise();
  }
}


bool SceneGraph::FrustumClip(SceneNode *node)
{
	// do the frustum clip
  	for (int n=0; n<6; n++)
	{	
		if (!Intersect(m_FrustumPlanes[n], node, 0))
		{
			return false;
		}
	}
	return true;
}

void SceneGraph::CohenSutherland(const dVector &p, char &cs)
{
	char t=0;
	if (p.z>0.0f) // in front of the camera
	{
		if (p.x>1)       t |= 0x01;
		else if (p.x<-1) t |= 0x02;
		if (p.y>1)       t |= 0x04;
		else if (p.y<-1) t |= 0x08;
	}
	else // behind the camera
	{
		if (p.x<-1)    t |= 0x01;
		else if (p.x>1) t |= 0x02;
		if (p.y<-1)     t |= 0x04;
		else if (p.y>1) t |= 0x08;
	}
	cs&=t;
}

void SceneGraph::Detach(SceneNode *node)
{
	if (node->Parent!=m_Root)
	{
		// keep the concatenated transform
		node->Prim->GetState()->Transform=GetGlobalTransform(node);

		// move node to the root
		node->Parent->RemoveChild(node->ID);
		m_Root->Children.push_back(node);
		node->Parent=m_Root;
	}
}

dMatrix SceneGraph::GetGlobalTransform(const SceneNode *node) const
{
	dMatrix Mat,Ret;

	list<const SceneNode*> Path;

	const SceneNode* current=node;

	// iterate back up the tree storing parents...
	// lazy parent objects are treated as non-heirachical,
	// so we can stop if we find one of them - and
	// use it's transform as world space
	bool foundlazy=false;
	while(current!=NULL && !foundlazy)
	{
		if (current && current->Prim)
		{
			Path.push_front(current);
			foundlazy = current->Prim->GetState()->Hints & HINT_LAZY_PARENT;
		}
		current=(const SceneNode*)current->Parent;
	}

	// concatenate the matrices together to get the global
	for (list<const SceneNode*>::iterator i=Path.begin(); i!=Path.end(); ++i)
	{
		Mat*=(*i)->Prim->GetState()->Transform;
	}

	return Mat;
}

void SceneGraph::GetBoundingBox(SceneNode *node, dBoundingBox &result)
{
	dMatrix mat;
	GetBoundingBox(node, mat, result);
}

void SceneGraph::GetBoundingBox(SceneNode *node, dMatrix mat, dBoundingBox &result)
{
	if (!node) return;

	if (node->Prim)
	{
		dVector point(0,0,0);
		dBoundingBox bbox=node->Prim->GetBoundingBox(mat);
		result.expand(bbox);
		mat*=node->Prim->GetState()->Transform;
    }

	for (vector<Node*>::iterator i=node->Children.begin(); i!=node->Children.end(); ++i)
	{
		GetBoundingBox((SceneNode*)*i,mat,result);
	}
}

void SceneGraph::GetNodes(const Node *node, vector<const SceneNode*> &nodes) const
{
	nodes.push_back(static_cast<const SceneNode*>(node));

	for (vector<Node*>::const_iterator i=node->Children.begin();
			i!=node->Children.end(); i++)
	{
		GetNodes(*i,nodes);
	}
}

void SceneGraph::GetConnections(const Node *node, vector<pair<const SceneNode*,const SceneNode*> > &connections) const
{
	for (vector<Node*>::const_iterator i=node->Children.begin();
			i!=node->Children.end(); i++)
	{
		connections.push_back(pair<const SceneNode *,const SceneNode *>
								(static_cast<const SceneNode*>(node),
								 static_cast<const SceneNode*>(*i)));
		GetConnections(*i,connections);
	}
}

void SceneGraph::Clear()
{
	Tree::Clear();
	SceneNode *root = new SceneNode(NULL);
	AddNode(0,root);
}
	
void SceneGraph::RecalcAABB(SceneNode *node)
{
	node->m_GlobalAABB=node->Prim->GetBoundingBox(GetGlobalTransform(node));
}

bool SceneGraph::Intersect(const SceneNode *a, const SceneNode *b, float threshold)
{
	return b->m_GlobalAABB.inside(a->m_GlobalAABB, threshold);
}

bool SceneGraph::Intersect(const dVector &point, const SceneNode *node, float threshold)
{
	return node->m_GlobalAABB.inside(point,threshold);
}

bool SceneGraph::Intersect(const dPlane &plane, const SceneNode *node, float threshold)
{
	return node->m_GlobalAABB.inside(plane,threshold);
}

void SceneGraph::RenderAxes()
{
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
    glEnable(GL_LIGHTING);
}

