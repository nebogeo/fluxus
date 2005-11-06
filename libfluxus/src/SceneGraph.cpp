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

using namespace fluxus;

SceneGraph::SceneGraph()
{
	// need to reset to having a root node present
	Clear();
}

SceneGraph::~SceneGraph()
{
}

void SceneGraph::Render(Mode rendermode)
{
	//RenderWalk((SceneNode*)m_Root,0);
	
	// render all the children of the root
	for (vector<Node*>::iterator i=m_Root->Children.begin(); i!=m_Root->Children.end(); ++i)
	{
		RenderWalk((SceneNode*)*i,0,rendermode);
	}
}

void SceneGraph::RenderWalk(SceneNode *node, int depth, Mode rendermode)
{
	// max gl matrix stack is 32 
	/*if (depth>=30)
	{
		cerr<<"SceneGraph::RenderWalk: max stack reached"<<endl;
		return;
	}*/
	
	if (node->Prim->Hidden()) return;
	if (rendermode==SELECT && !node->Prim->IsSelectable()) return;
	
	glPushMatrix();		
	node->Prim->ApplyState();
	glPushName(node->ID);
	node->Prim->Render();
	glPopName();
    
	depth++;

	for (vector<Node*>::iterator i=node->Children.begin(); i!=node->Children.end(); ++i)
	{
		RenderWalk((SceneNode*)*i,depth,rendermode);
	}
	glPopMatrix();
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

dMatrix SceneGraph::GetGlobalTransform(SceneNode *node)
{
	dMatrix Mat,Ret;
	
	list<SceneNode*> Path;
	
	SceneNode* current=node;
	// iterate back up the tree storing parents...
	while(current!=NULL)
	{
		if (current && current->Prim) Path.push_front(current);
		current=(SceneNode*)current->Parent;
	}
	
	// concatenate the matrices together to get the global
	for (list<SceneNode*>::iterator i=Path.begin(); i!=Path.end(); ++i)
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
		mat*=node->Prim->GetState()->Transform;
		bbox.min=mat.transform(bbox.min);
		bbox.max=mat.transform(bbox.max);
		result.expand(bbox);
    }

	for (vector<Node*>::iterator i=node->Children.begin(); i!=node->Children.end(); ++i)
	{
		GetBoundingBox((SceneNode*)*i,mat,result);
	}
}

void SceneGraph::Clear()
{
	Tree::Clear();
	SceneNode *root = new SceneNode(NULL);
	AddNode(0,root);
}
