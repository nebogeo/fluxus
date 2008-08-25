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
#include "ShadowVolumeGen.h"
#include "DepthSorter.h"

using namespace std;

#ifndef N_SCENEGRAPH
#define N_SCENEGRAPH

namespace Fluxus
{

/////////////////////////////////////
/// A scene graph node 
/// The state is contained within the 
/// primitive itself.
class SceneNode : public Node
{
public:
	SceneNode(Primitive *p) : Prim(p) {}
	virtual ~SceneNode() { if (Prim) delete Prim; }
	Primitive *Prim;
};

istream &operator>>(istream &s, SceneNode &o);
ostream &operator<<(ostream &s, SceneNode &o);

/////////////////////////////////////
/// A scene graph
class SceneGraph : public Tree
{
public:
	SceneGraph();
	~SceneGraph();

	enum Mode{RENDER,SELECT};

	/// Traverses the graph depth first, rendering
	/// all nodes
	void Render(ShadowVolumeGen *shadowgen, unsigned int camera, Mode rendermode=RENDER);
	
	/// Clears the graph of all primitives
	virtual void Clear();
	
	/// Parents the node to the root, and sets its 
	/// transform to keep it physically in the same 
	/// place in the world. 
	///\todo make the maintain transform optional
	void Detach(SceneNode *node);
	
	/// Gets the world space transfrom of the node
	dMatrix GetGlobalTransform(const SceneNode *node) const;
	
	/// Gets the bounding box of the node, and all 
	/// its children too
	void GetBoundingBox(SceneNode *node, dBoundingBox &result);
	
	/// A utility for getting all the node in a subtree, as a flat list
	void GetNodes(const Node *node, vector<const SceneNode*> &nodes) const;

	/// A utility for getting all the connections in a subtree, as a flat list
	void GetConnections(const Node *node, 
		vector<pair<const SceneNode*,const SceneNode*> > &connections) const;

private:
	void RenderWalk(SceneNode *node, int depth, unsigned int cameracode, ShadowVolumeGen *shadowgen, Mode rendermode);
	void GetBoundingBox(SceneNode *node, dMatrix mat, dBoundingBox &result);
	bool FrustumClip(SceneNode *node);
	void CohenSutherland(const dVector &p, char &cs);
	
	DepthSorter m_DepthSorter;
	dMatrix m_TopTransform;
};

}

#endif
