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

#include <float.h>
#include "GenSkinWeightsPrimFunc.h"
#include "Primitive.h"
#include "SceneGraph.h"

using namespace Fluxus;

GenSkinWeightsPrimFunc::GenSkinWeightsPrimFunc()
{
}

GenSkinWeightsPrimFunc::~GenSkinWeightsPrimFunc()
{
}

void GenSkinWeightsPrimFunc::Run(Primitive &prim, const SceneGraph &world)
{
	vector<int> skeleton = GetArg<vector<int> >("skeleton",vector<int>());
	float sharpness = GetArg<float>("sharpness",0);
	vector<dVector> *p = prim.GetDataVec<dVector>("p");
	vector<TypedPData<float> *> weights;
	int bone=0;
	
	// first pass, put inverse distances for each bone
	for (unsigned int i=0; i<skeleton.size()-1; i++)
	{
		// get the bone
		const SceneNode *thisbonenode = (const SceneNode *)world.FindNode(skeleton[i]);
		const SceneNode *nextbonenode = (const SceneNode *)world.FindNode(skeleton[i+1]);
		if (thisbonenode && nextbonenode)
		{
			// make a skinweight pdata array
			weights.push_back(new TypedPData<float>(prim.Size()));

			// find the bone position
			dVector startbone = world.GetGlobalTransform(thisbonenode).transform(dVector(0,0,0));
			dVector endbone   = world.GetGlobalTransform(nextbonenode).transform(dVector(0,0,0));

			for (unsigned int n=0; n<prim.Size(); n++)
			{
				float d=dGeometry::pointlinedist((*p)[n],startbone,endbone);
				if (d==0) weights[bone]->m_Data[n]=2;
				else weights[bone]->m_Data[n]=(1/d);
			}
		}
		else
		{
			cerr<<"GenSkinWeightsPrimFunc::Run: aborting - couldn't find bone id "<<skeleton[i]<<endl;
			return;
		}
		bone++;
	}				
	
	weights.push_back(new TypedPData<float>(prim.Size()));
	for (unsigned int n=0; n<prim.Size(); n++)
	{
		weights[bone]->m_Data[n]=0;
	}
	
	// second pass, pow the weights to allow 
	// us to control the creasing
	for (unsigned int n=0; n<prim.Size(); n++)
	{
		for (unsigned int bone=0; bone<weights.size(); bone++)
		{
			weights[bone]->m_Data[n]=powf(weights[bone]->m_Data[n],sharpness);
		}
	}
	
	// third pass, normalise the weights
	for (unsigned int n=0; n<prim.Size(); n++)
	{
		float m=0;
		for (unsigned int bone=0; bone<weights.size(); bone++)
		{
			m+=weights[bone]->m_Data[n];
		}
		
		for (unsigned int bone=0; bone<weights.size(); bone++)
		{
			weights[bone]->m_Data[n]/=m;
		}
	}
	
	
	// finally, add the weights to the primitive
	for (unsigned int bone=0; bone<weights.size(); bone++)
	{
		char wname[256];
		snprintf(wname,256,"w%d",bone);
		prim.AddData(wname, weights[bone]);
	}
}
