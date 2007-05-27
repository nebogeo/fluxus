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
	virtual void RecalculateNormals(bool smooth);
	virtual void ApplyTransform(bool ScaleRotOnly=false);
	virtual string GetTypeName() { return "PolyPrimitive"; }
	
	Type GetType() { return m_Type; }

	const vector<vector<int> > &GetConnectedVerts() { GenerateTopology(); return m_ConnectedVerts; }
	const vector<dVector> &GetGeometricNormals() { GenerateTopology(); return m_GeometricNormals; }
	const vector<vector<pair<int,int> > > &GetUniqueEdges() { CalculateUniqueEdges(); return m_UniqueEdges; }

	void SetIndexMode(bool s) { m_IndexMode=s; }
	bool IsIndexed() { return m_IndexMode; }
	vector<unsigned int> &GetIndex() { return m_IndexData; }
	void ConvertToIndexed();
	
protected:

	void GenerateTopology();
	
	void CalculateConnected();
	void CalculateGeometricNormals();
	void CalculateUniqueEdges();
	
	void UniqueEdgesFindShared(pair<int,int> edge, set<pair<int,int> > firstpass, set<pair<int,int> > &stored);
	
	virtual void PDataDirty();

	void RecalculateNormalsIndexed();
	
	vector<vector<int> > m_ConnectedVerts;
	vector<dVector> m_GeometricNormals;
	vector<vector<int> > m_Faces;
	vector<vector<pair<int,int> > >  m_UniqueEdges;
	
	bool m_IndexMode;
	vector<unsigned int> m_IndexData;
	
	Type m_Type;
	vector<dVector> *m_VertData;
	vector<dVector> *m_NormData;
	vector<dColour> *m_ColData;
	vector<dVector> *m_TexData;
};

};

#endif
