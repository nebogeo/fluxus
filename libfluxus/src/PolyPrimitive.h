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
#include "PolyEvaluator.h"

#ifndef N_POLYPRIM
#define N_POLYPRIM

namespace Fluxus
{

///////////////////////////////////////////////////
/// A Polygon primitive
class PolyPrimitive : public Primitive
{
public:
	// Todo: sort out these silly names, add quadstrip
	enum Type{TRISTRIP,QUADS,TRILIST,TRIFAN,POLYGON};
	
	PolyPrimitive(Type t=TRISTRIP);
	PolyPrimitive(const PolyPrimitive &other);
	virtual ~PolyPrimitive();
	
	///////////////////////////////////////////////////
	///@name Primitive Interface
	///@{
	virtual PolyPrimitive *Clone() const;
	virtual void Render();
	virtual dBoundingBox GetBoundingBox();
	virtual void RecalculateNormals(bool smooth);
	virtual void ApplyTransform(bool ScaleRotOnly=false);
	virtual string GetTypeName() { return "PolyPrimitive"; }
	virtual Evaluator *MakeEvaluator() { return new PolyEvaluator(this); }
	///@}
	
	Type GetType() const { return m_Type; }
	
	/// Add a new vertex to the primitive
	virtual void AddVertex(const dVertex &Vert);
		
	/// Clears out the primitive
	void Clear();

	///////////////////////////////////////////////////
	///@name Topology functions
	/// Functions to get topological information 
	/// about the primitive. These are lazily computed
	/// and stored as they can take some time.
	///@{

	/// Connected verts is a list of lists of vertices 
	/// which are coincident. If this polyprimitive is 
	/// indexed the coincident verts are calculated by
	/// looking at the index values and the position 
	/// of the index is stored, otherwise it's done by 	
	/// looking at the actual vertex positions, with a 
	/// small allowed error, and the index is stored.
	const vector<vector<int> > &GetConnectedVerts() { GenerateTopology(); return m_ConnectedVerts; }
	
	/// Unique edges is a list of coincident edges in 
	/// the topology, formed by pairs of vert indexes, 
	/// or index positions if the poly is indexed.
	const vector<vector<pair<int,int> > > &GetUniqueEdges() { CalculateUniqueEdges(); return m_UniqueEdges; }
	
	/// In indexed mode there is a geometric normal 
	/// for every index
	const vector<dVector> &GetGeometricNormals() { GenerateTopology(); return m_GeometricNormals; }
	///@}

	//////////////////////////////////////////////////
	///@name Indexed mode access
	///@{
	void SetIndexMode(bool s) { m_IndexMode=s; }
	bool IsIndexed() const { return m_IndexMode; }
	vector<unsigned int> &GetIndex() { return m_IndexData; }
	const vector<unsigned int> &GetIndexConst() const { return m_IndexData; }
	/// Look at coincident verts and compress the poly
	/// primitive into an indexed form
	void ConvertToIndexed();
	///@}
	
	
protected:

	virtual void PDataDirty();
	
	// Topology generation commands
	void GenerateTopology();
	void CalculateConnected();
	void CalculateGeometricNormals();
	void CalculateUniqueEdges();
	void UniqueEdgesFindShared(pair<int,int> edge, set<pair<int,int> > firstpass, set<pair<int,int> > &stored);
	void RecalculateNormalsIndexed();
	
	vector<vector<int> > m_ConnectedVerts;
	vector<dVector> m_GeometricNormals;
	vector<vector<pair<int,int> > > m_UniqueEdges;
	
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
