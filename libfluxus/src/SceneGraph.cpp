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

void SceneGraph::Render()
{
	//RenderWalk((SceneNode*)m_Root,0);
	
	// render all the children of the root
	for (vector<Node*>::iterator i=m_Root->Children.begin(); i!=m_Root->Children.end(); ++i)
	{
		RenderWalk((SceneNode*)*i,0);
	}
}

void SceneGraph::RenderWalk(SceneNode *node, int depth)
{
	// max gl matrix stack is 32 
	/*if (depth>=30)
	{
		cerr<<"SceneGraph::RenderWalk: max stack reached"<<endl;
		return;
	}*/
	
	if (node->Prim->Hidden()) return;

	glPushMatrix();		
	node->Prim->ApplyState();
	int Texture=node->Prim->GetState()->Texture;
	glPushName(node->ID);
	if (Texture>=0)
	{ 
		glEnable(GL_TEXTURE_2D);
		m_TexturePainter.SetCurrent(Texture);
		node->Prim->Render();
		glDisable(GL_TEXTURE_2D);
	}
	else
	{
		node->Prim->Render();
	}
	glPopName();
    
	depth++;

	for (vector<Node*>::iterator i=node->Children.begin(); i!=node->Children.end(); ++i)
	{
		RenderWalk((SceneNode*)*i,depth);
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

istream &fluxus::operator>>(istream &s, SceneNode &o)
{
	s.ignore(3);
	bool PrimExists = false;
	s.read((char*)&PrimExists,sizeof(bool));
	if (PrimExists)
	{
		o.Prim = new PolyPrimitive;
		s>>*(PolyPrimitive*)o.Prim;
	}
	int num=0;
	s.read((char*)&num,sizeof(int));
	for (int n=0; n<num; n++)
	{
		SceneNode* newnode = new SceneNode(NULL);
		s>>*newnode;
		o.Children.push_back(newnode);
	}
	
	return s;
}

ostream &fluxus::operator<<(ostream &s, SceneNode &o)
{
	s.write("sgn",3);
	bool PrimExists = o.Prim;
	s.write((char*)&PrimExists,sizeof(bool));
	if (o.Prim)
	{
		s<<*(PolyPrimitive*)o.Prim;
	}
	int num=o.Children.size();
	s.write((char*)&num,sizeof(int));
	for (vector<Node*>::iterator i=o.Children.begin(); i!=o.Children.end(); ++i)
	{
		s<<*((SceneNode*)*i);
	}
	
	return s;
}

istream &fluxus::operator>>(istream &s, SceneGraph &o)
{
	s.ignore(3);
	o.m_Root = new SceneNode(NULL);
	s>>*(SceneNode*)o.m_Root;
	return s;
}

ostream &fluxus::operator<<(ostream &s, SceneGraph &o)
{
	s.write("sgh",3);
	s<<*(SceneNode*)o.m_Root;
	return s;
}
