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


// Generates a shadow volume poly primitive for the supplied 
// primitives and light position

// Can we accelerate the process by caching as much as possible, and 
// changing only when light positions/primitive transforms/deform?

#ifndef N_SHADOWGEN
#define N_SHADOWGEN

#include "Primitive.h"
#include "PolyPrimitive.h"
#include "NURBSPrimitive.h"

namespace Fluxus
{

/////////////////////////////////////
/// Generates shadow volumes from 
/// extruding light silhouette edges. These 
/// volumes are then concatenated into a 
/// single polygon primitive for rendering 
/// into a stencil buffer for shadow 
/// rendering.
class ShadowVolumeGen
{
public:
	ShadowVolumeGen();
	~ShadowVolumeGen();	

	/// Sets the light position to generate the silhouette 
	/// edges and shadow volume from 
	void SetLightPosition(dVector pos) { Clear(); m_LightPosition=pos; }
	
	/// Clears the current shadow volume - should call this at 
	/// start of the frame, or after rendering the shadows.
	void Clear();
	
	/// Adds the volumes for a primitive to the polygon 
	/// primitive
	void Generate(Primitive *prim);
	
	/// Gets the volume generated so far
	PolyPrimitive *GetVolume();
	
	/// Sets the length to extrude the volume by, in world space
	void SetLength(float s) { m_Length=s; }

	/// Sets the amount to expand the volume by, to minimise 
	/// silhouette edge artifacts
	void SetExpand(float s) { m_Expand=s; }
	
	///@name Accessors for debug mode
	/// When in debug mode, the silhouette edges are drawn, 
	/// indicating direction by colour gradient
	///@{
	void SetDebug(bool s) { m_Debug=s; }
	bool GetDebug() { return m_Debug; }
	///@}
	
private:

	typedef pair<int,int> EdgeType;
	typedef vector<EdgeType> EdgeContainer;
	typedef vector<EdgeContainer> SharedEdgeContainer;

	void PolyGen(PolyPrimitive *src);
	void NURBSGen(NURBSPrimitive *src);
	
	void AddEdge(dVector start, dVector end);
	int FindNextEdge(unsigned int index, vector<pair<dVector,dVector> > &silhouette, bool &flip);

	PolyPrimitive m_ShadowVolume;
	dVector m_LightPosition;
	float m_Length;
	float m_Expand;
	bool m_Debug;
};

};

#endif
