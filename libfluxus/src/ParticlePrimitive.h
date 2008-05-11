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

#include "Primitive.h"

#ifndef N_PARTICLEPRIM
#define N_PARTICLEPRIM

namespace Fluxus
{

//////////////////////////////////////////////////////
/// The particle system primitive
class ParticlePrimitive : public Primitive
{
public:
	ParticlePrimitive();
	ParticlePrimitive(const ParticlePrimitive &other);
	virtual ~ParticlePrimitive();
	
	///////////////////////////////////////////////////
	///@name Primitive Interface
	///@{
	virtual ParticlePrimitive* Clone() const;
	virtual void Render();
	virtual dBoundingBox GetBoundingBox();
	virtual void ApplyTransform(bool ScaleRotOnly=false);
	virtual string GetTypeName() { return "ParticlePrimitive"; }
	///@}
	
	void AddParticle(const dVector &v, const dColour &c, const dVector &s) 
	{ 
		m_VertData->push_back(v); 
		m_ColData->push_back(c); 
		m_SizeData->push_back(s); 
		m_RotateData->push_back(0); 
	}

protected:

	virtual void PDataDirty();

private:

	vector<dVector> *m_VertData;
	vector<dColour> *m_ColData;
	vector<dVector> *m_SizeData;
	vector<float> *m_RotateData;
};

}

#endif
