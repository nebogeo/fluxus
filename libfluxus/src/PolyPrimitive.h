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

#include "Renderer.h"
#include "Primitive.h"

#ifndef N_POLYPRIM
#define N_POLYPRIM

namespace fluxus
{

class PolyPrimitive : public Primitive
{
public:
	enum Type{TRISTRIP,QUADS,TRILIST,TRIFAN,POLYGON};
	
	PolyPrimitive(Type t=TRISTRIP);
	virtual  ~PolyPrimitive();
	
	virtual void AddVertex(const dVertex &Vert);	
	virtual void Render();
	virtual dBoundingBox GetBoundingBox();
	virtual void RecalculateNormals();
	virtual void ApplyTransform(bool ScaleRotOnly=false);
	virtual string GetTypeName() { return "PolyPrimitive"; }

	void VertColours(bool s) { m_VertColours=s; }
	
protected:

	virtual void PDataDirty();
	
	Type m_Type;
	bool m_VertColours;
	vector<vector<int> > m_ConnectedVerts;
	vector<dVector> *m_VertData;
	vector<dVector> *m_NormData;
	vector<dColour> *m_ColData;
	vector<dVector> *m_TexData;
};

};

#endif
