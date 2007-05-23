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

#include <set>

#include "Primitive.h"

#ifndef N_BLOBBYPRIM
#define N_BLOBBYPRIM

namespace fluxus
{


class BlobbyPrimitive : public Primitive
{
public:
	
	BlobbyPrimitive(int dimx, int dimy, int dimz, dVector size);
	virtual  ~BlobbyPrimitive();
	
	virtual void AddInfluence(const dVector &Vert, float Strength);	
	virtual void Render();
	virtual dBoundingBox GetBoundingBox();
	virtual void RecalculateNormals(bool smooth);
	virtual void ApplyTransform(bool ScaleRotOnly=false);
	virtual string GetTypeName() { return "BlobbyPrimitive"; }
	
protected:
	class Triangle 
	{
	public:
		dVector p[3];
	};

	class Cell 
	{
	public:
		dVector p[8];
		float val[8];
		dColour col[8];
	};
	
	void Draw(float isolevel, bool calcnormals, bool colour);
	void Interpolate(dVertex &vert, float isolevel, int cell, int a, int b);
	float Sample(const dVector &pos);
	float SampleCol(const dVector &pos, dColour &col);
		
	virtual void PDataDirty();
	
	vector<dVector> *m_PosData;
	vector<float> *m_StrengthData;
	vector<dColour> *m_ColData;
	
	vector<Cell> m_Voxels;
};

};

#endif
