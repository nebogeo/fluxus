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

#include <stdio.h>
#include "SkinWeightsToVertColsPrimFunc.h"
#include "Primitive.h"
#include "SceneGraph.h"

using namespace Fluxus;

SkinWeightsToVertColsPrimFunc::SkinWeightsToVertColsPrimFunc()
{
}

SkinWeightsToVertColsPrimFunc::~SkinWeightsToVertColsPrimFunc()
{
}

void SkinWeightsToVertColsPrimFunc::Run(Primitive &prim, const SceneGraph &world)
{
	// find out how many sets of weights there are
	bool found=true;
	unsigned int numbones=0;
	unsigned int size=0;
	char wname[256];
	char type=0;

	while(found)
	{
		snprintf(wname,256,"w%d",numbones);
		found=prim.GetDataInfo(wname, type, size);
		if (found) numbones++;
	}

	// make some random colours
	vector<dColour> colours;
	for (unsigned int bone=0; bone<numbones; bone++)
	{
		colours.push_back(dColour(RandFloat(),RandFloat(),RandFloat()));
	}

	// get pointers to all the weights
	vector<vector<float>*> weights;
	for (unsigned int bone=0; bone<numbones; bone++)
	{
		snprintf(wname,256,"w%d",bone);
		weights.push_back(prim.GetDataVec<float>(wname));
	}

	for (unsigned int n=0; n<prim.Size(); n++)
	{
		dColour col;
		for	(unsigned int bone=0; bone<numbones; bone++)
		{
			col+=colours[bone]*(*weights[bone])[n];
		}
		///\todo make sure c exists...
		prim.SetData("c", n, col);
	}

}

