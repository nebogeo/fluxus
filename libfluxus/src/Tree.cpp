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

#include "Tree.h"
#include "Trace.h"

using namespace Fluxus;

void Node::RemoveChild(int ID)
{
	for (vector<Node*>::iterator i=Children.begin(); i!=Children.end(); ++i)
	{
		if ((*i)->ID==ID)
		{
			Children.erase(i);
			return;
		}
	}
	
	Trace::Stream<<"Node::RemoveChild : could not find "<<ID<<endl;
}

//////////////////////////////////////////////////////////////////

Tree::Tree()
{
	m_CurrentID=1;
	m_Root=NULL;
}

Tree::~Tree()
{
    RemoveNode(m_Root);
}

int Tree::AddNode(int ParentID, Node *node)
{
	if (m_Root!=NULL)
	{
		Node *parent=FindNode(ParentID);
		if (!parent)
		{
			Trace::Stream<<"Tree::AddNode : can't find parent node "<<ParentID<<endl;
			return 0;
		}
		
		node->ID=m_CurrentID++;
		parent->Children.push_back(node);
		node->Parent=parent;
	}
	else
	{
		node->ID=m_CurrentID++;
	    m_Root=node;
	}
	
	// store in the node map for quick searching...
	m_NodeMap[node->ID]=node;
	
	return node->ID;
}

Node *Tree::FindNode(int ID) const
{
	map<int,Node*>::const_iterator i=m_NodeMap.find(ID);
	if (i!=m_NodeMap.end())
	{
		return i->second;
	}
	
	return NULL;
}

void Tree::RemoveNode(Node *node)
{
	if (node==NULL) return;
	
	// remove from node map
	map<int,Node*>::iterator i=m_NodeMap.find(node->ID);
	if (i!=m_NodeMap.end())
	{
		m_NodeMap.erase(i);
	}
	
	// if not root, remove ourself from our parent's child vector
	if (node->Parent)
	{
		node->Parent->RemoveChild(node->ID);
	}
	
	RemoveNodeWalk(node);
}

void Tree::RemoveNodeWalk(Node *node)
{
	if (node==NULL) return;
	for (vector<Node*>::iterator i=node->Children.begin(); i!=node->Children.end(); ++i)
	{
		RemoveNodeWalk(*i);
	}
	
	// remove from node map
	map<int,Node*>::iterator i=m_NodeMap.find(node->ID);
	if (i!=m_NodeMap.end())
	{
		m_NodeMap.erase(i);
	}
	
	delete node;
}

bool Tree::IsDecendedFrom(Node *Parent, Node *Child) const
{
	Node *current = Child;
	
	// iterate back up the tree looking at parents...
	while(current!=NULL)
	{
		if (current==Parent)
		{
			return true;
		}
		current=current->Parent;
	}
	return false;
}

void Tree::Dump(int Depth,Node *node) const
{
	if (node==NULL) node=m_Root;
	if (node==NULL) return;
	
	for (int n=0; n<Depth; n++) Trace::Stream<<" ";
	Trace::Stream<<node->ID<<endl;
	
	Depth++;
	
	for (vector<Node*>::iterator i=node->Children.begin(); i!=node->Children.end(); ++i)
	{
		Dump(Depth,*i);
	}
}
