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

#include <vector>
#include <map>
#include <iostream>

#ifndef N_TREE
#define N_TREE

using namespace std;

namespace fluxus
{

class Node
{
public:
	Node() : Parent(NULL) {}
	virtual ~Node() {}
	
	void RemoveChild(int ID);
	
	Node *Parent;
	vector<Node*> Children;
	int ID;
};

class Tree
{
public:
    Tree();
    virtual ~Tree();

    virtual int AddNode(int ParentID, Node *);
    virtual Node *FindNode(int ID) const;
    virtual void RemoveNode(Node *node);
    virtual void Clear() { if (m_Root) RemoveNode(m_Root); m_Root=NULL; m_CurrentID=1; }
    virtual void Dump(int Depth=0,Node *node=NULL) const;
	bool IsDecendedFrom(Node *Parent, Node *Child) const;

protected:
	void RemoveNodeWalk(Node *node);
	
	map<int,Node*> m_NodeMap;
	Node *m_Root;
    int m_CurrentID;
};

}

#endif
