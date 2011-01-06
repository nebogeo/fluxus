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

#ifndef N_VOXELPRIM
#define N_VOXELPRIM

#include "Primitive.h"

namespace Fluxus
{

class BlobbyPrimitive;

//////////////////////////////////////////////////////
class VoxelPrimitive : public Primitive
{
public:
	VoxelPrimitive(unsigned int w, unsigned int h, unsigned int d);
	VoxelPrimitive(const VoxelPrimitive &other);
	virtual ~VoxelPrimitive();
	
	///////////////////////////////////////////////////
	///@name Primitive Interface
	///@{
	virtual VoxelPrimitive* Clone() const;
	virtual void Render();
	virtual dBoundingBox GetBoundingBox(const dMatrix &space);
	virtual void ApplyTransform(bool ScaleRotOnly=false);
	virtual string GetTypeName() { return "VoxelPrimitive"; }
	virtual Evaluator *MakeEvaluator() { return NULL; }
	///@}
	
	///////////////////////////////////////////////////
	///@name Voxel operations
	///@{
	unsigned int GetWidth() { return m_Width; }
	unsigned int GetHeight() { return m_Height; }
	unsigned int GetDepth() { return m_Depth; }
	void SphereInfluence(const dVector &pos, const dColour &col, float pow);
	void SphereSolid(const dVector &pos, const dColour &col, float radius);
	void BoxSolid(const dVector &topleft, const dVector &botright, const dColour &col);
	void Threshold(float value);
	void CalcGradient();
	void PointLight(dVector lightpos, dColour col);
	BlobbyPrimitive *ConvertToBlobby();
	///@}
	
protected:

	virtual void PDataDirty();
	unsigned int Index(unsigned int x, unsigned int y, unsigned int z);
	dVector Position(unsigned int index);
	dColour SafeRef(unsigned int x, unsigned int y, unsigned int z);

private:

	vector<dColour,FLX_ALLOC(dColour) > *m_ColData;
	vector<dColour,FLX_ALLOC(dColour) > *m_GradData;
	unsigned int m_Width;
	unsigned int m_Height;
	unsigned int m_Depth;
};

}

#endif
