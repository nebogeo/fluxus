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

#include <iostream>
#include "Tree.h"
#include "Primitive.h"
#include "State.h"
#include "TexturePainter.h"

using namespace std;

#ifndef N_SCENEGRAPH
#define N_SCENEGRAPH

namespace fluxus
{

class SceneNode : public Node
{
public:
	SceneNode(Primitive *p) : Prim(p) {}
	virtual ~SceneNode() { if (Prim) delete Prim; }
	Primitive *Prim;
};

istream &operator>>(istream &s, SceneNode &o);
ostream &operator<<(ostream &s, SceneNode &o);

class SceneGraph : public Tree
{
public:
	SceneGraph();
	~SceneGraph();

	void Render();
	unsigned int LoadTexture(const string &Filename) { return m_TexturePainter.LoadTexture(Filename); }
	virtual void Clear();
	void Detach(SceneNode *node);
	dMatrix GetGlobalTransform(SceneNode *node);
	void GetBoundingBox(SceneNode *node, dBoundingBox &result);

private:
	TexturePainter m_TexturePainter;
	void RenderWalk(SceneNode *node, int depth);
	void GetBoundingBox(SceneNode *node, dMatrix mat, dBoundingBox &result);
	
	friend istream &operator>>(istream &s, SceneGraph &o);
	friend ostream &operator<<(ostream &s, SceneGraph &o);
};

istream &operator>>(istream &s, SceneGraph &o);
ostream &operator<<(ostream &s, SceneGraph &o);

}

#endif
