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

using namespace Fluxus;

SceneGraph::SceneGraph()
{
	// need to reset to having a root node present
	Clear();
}

SceneGraph::~SceneGraph()
{
}

void SceneGraph::Render(ShadowVolumeGen *shadowgen, unsigned int camera, Mode rendermode)
{
	//RenderWalk((SceneNode*)m_Root,0);

	glGetFloatv(GL_MODELVIEW_MATRIX,m_TopTransform.arr());	

	unsigned int cameracode = 1<<camera;

	// render all the children of the root
	for (vector<Node*>::iterator i=m_Root->Children.begin(); i!=m_Root->Children.end(); ++i)
	{
		RenderWalk((SceneNode*)*i,0,cameracode,shadowgen,rendermode);
	}
	
	// now render the depth sorted primitives:
	m_DepthSorter.Render();
	m_DepthSorter.Clear();
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
	
	///\todo fix frustum culling
	//if (!FrustumClip(node))
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

		depth++;

		for (vector<Node*>::iterator i=node->Children.begin(); i!=node->Children.end(); ++i)
		{
			RenderWalk((SceneNode*)*i,depth,cameracode,shadowgen,rendermode);
		}
	}
	glPopMatrix();
	
	if (node->Prim->GetState()->Hints & HINT_CAST_SHADOW)
	{
		shadowgen->Generate(node->Prim);
	}
}

// this is working the wrong way - need to write proper 
// frustum culling by building planes from the camera 
// frustum and checking is any points are inside
bool SceneGraph::FrustumClip(SceneNode *node)
{
	// do the frustum clip
	dBoundingBox box; 
	GetBoundingBox(node,box);
	dMatrix mat,proj;
	glGetFloatv(GL_MODELVIEW_MATRIX,mat.arr());
	glGetFloatv(GL_PROJECTION_MATRIX,proj.arr());
	mat=proj*mat;
	char cs = 0xff;
	
	dVector p = mat.transform_persp(box.min);
	CohenSutherland(p,cs);
 	p=mat.transform_persp(box.max);
	CohenSutherland(p,cs);
 	p=mat.transform_persp(dVector(box.min.x,box.min.y,box.max.z));
	CohenSutherland(p,cs);
	p=mat.transform_persp(dVector(box.min.x,box.max.y,box.min.z));
	CohenSutherland(p,cs);
 	p=mat.transform_persp(dVector(box.min.x,box.max.y,box.max.z));
	CohenSutherland(p,cs);
	p=mat.transform_persp(dVector(box.max.x,box.min.y,box.min.z));
	CohenSutherland(p,cs);
 	p=mat.transform_persp(dVector(box.max.x,box.min.y,box.max.z));
	CohenSutherland(p,cs);
	p=mat.transform_persp(dVector(box.max.x,box.max.y,box.min.z));
	CohenSutherland(p,cs);

	return cs!=0;
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
		dBoundingBox bbox=node->Prim->GetBoundingBox();
		bbox.min=mat.transform(bbox.min);
		bbox.max=mat.transform(bbox.max);
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
