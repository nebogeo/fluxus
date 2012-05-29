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

#ifndef N_SCENEGRAPH
#define N_SCENEGRAPH

#include <iostream>
#include "Tree.h"
#include "Primitive.h"
#include "State.h"
#include "ShadowVolumeGen.h"
#include "DepthSorter.h"

using namespace std;

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
	dBoundingBox m_GlobalAABB;
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

	void RecalcAABB(SceneNode *node);

	///Bounding box intersections, for higher accuracy, see the evaluators
	///\todo fix const correctness from here...
	bool Intersect(const SceneNode *a, const SceneNode *b, float threshold);
	bool Intersect(const dVector &point, const SceneNode *node, float threshold);
	bool Intersect(const dPlane &plane, const SceneNode *node, float threshold);

	/// Some statistics
	unsigned int GetNumRendered() { return m_NumRendered; }
	unsigned int GetHighWater() { return m_HighWater; }

	/// Render origin
	static void RenderAxes();

private:
	void RenderWalk(SceneNode *node, int depth, unsigned int cameracode, ShadowVolumeGen *shadowgen, Mode rendermode);
	void GetBoundingBox(SceneNode *node, dMatrix mat, dBoundingBox &result);
	bool FrustumClip(SceneNode *node);
	void CohenSutherland(const dVector &p, char &cs);
	void GetFrustumPlanes(dPlane *planes, dMatrix m, bool normalise);

	DepthSorter m_DepthSorter;
	dMatrix m_TopTransform;
	dPlane m_FrustumPlanes[6];

	unsigned int m_NumRendered;
	unsigned int m_HighWater;
};

}

#endif

