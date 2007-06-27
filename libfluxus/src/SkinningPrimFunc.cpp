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

#include "SkinningPrimFunc.h"
#include "Primitive.h"
#include "SceneGraph.h"

using namespace Fluxus;

SkinningPrimFunc::SkinningPrimFunc()
{
}

SkinningPrimFunc::~SkinningPrimFunc()
{
}

void SkinningPrimFunc::Run(Primitive &prim, const SceneGraph &world)
{
	int rootid = GetArg<int>("skeleton-root",0);
	int bindposerootid = GetArg<int>("bindpose-root",0);
	bool skinnormals = GetArg<int>("skin-normals",0);
	vector<dVector> *p = prim.GetDataVec<dVector>("p");
	vector<dVector> *pref = prim.GetDataVec<dVector>("pref");
	vector<dVector> *n = NULL;
	vector<dVector> *nref = NULL;
	
	if (!pref)
	{
		///\todo sort out a proper error messaging thing
		cerr<<"SkinningPrimFunc::Run: aborting: primitive needs a pref (copy of p)"<<endl;
		return;
	}
	
	if (skinnormals)
	{
		n = prim.GetDataVec<dVector>("n");
		nref = prim.GetDataVec<dVector>("nref");
		if (!nref)
		{
			cerr<<"SkinningPrimFunc::Run: aborting: primitive needs an nref (copy of n)"<<endl;
			return;
		}
	}
	
	const SceneNode *root = static_cast<const SceneNode *>(world.FindNode(rootid));
	if (!root)
	{
		cerr<<"GenSkinWeightsPrimFunc::Run: couldn't find skeleton root node "<<rootid<<endl;
		return;
	}
	
	const SceneNode *bindposeroot = static_cast<const SceneNode *>(world.FindNode(bindposerootid));
	if (!root)
	{
		cerr<<"GenSkinWeightsPrimFunc::Run: couldn't find bindopose skeleton root node "<<bindposerootid<<endl;
		return;
	}
	
	// get the nodes as flat lists
	vector<const SceneNode*> skeleton;
	world.GetNodes(root, skeleton);
	vector<const SceneNode*> bindposeskeleton;
	world.GetNodes(bindposeroot, bindposeskeleton);

	if (skeleton.size()!=bindposeskeleton.size())
	{
		cerr<<"SkinningPrimFunc::Run: aborting: skeleton sizes do not match! "<<
			skeleton.size()<<" vs "<<bindposeskeleton.size()<<endl;
		return;
	}
	
	// make a vector of all the transforms
	vector<dMatrix> transforms;
	for (unsigned int i=0; i<skeleton.size(); i++)
	{
		transforms.push_back(world.GetGlobalTransform(skeleton[i])*
        					 world.GetGlobalTransform(bindposeskeleton[i]).inverse());
	}
	
	// get pointers to all the weights
	vector<vector<float>*> weights;
	for (unsigned int bone=0; bone<skeleton.size(); bone++)
	{
		char wname[235];
		snprintf(wname,256,"w%d",bone);
		vector<float> *w = prim.GetDataVec<float>(wname);
		if (w==NULL)
		{
			cerr<<"SkinningPrimFunc::Run: can't find weights, aborting"<<endl;
			return;
		}
		weights.push_back(w);
	}
	
	for (unsigned int i=0; i<prim.Size(); i++)
	{
		dMatrix mat;
		mat.zero();
		for	(unsigned int bone=0; bone<skeleton.size(); bone++)
		{
			mat+=(transforms[bone]*(*weights[bone])[i]);
		}
		
		(*p)[i]=mat.transform((*pref)[i]);
		
		if (skinnormals)
		{
			(*n)[i]=mat.transform_no_trans((*nref)[i]);
		}
	}
}
