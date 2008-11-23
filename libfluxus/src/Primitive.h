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

#include <string>
#include <map>
#include <assert.h>
#include "State.h"
#include "PDataContainer.h"
#include "Evaluator.h"

#ifndef N_PRIM
#define N_PRIM

namespace Fluxus
{

//////////////////////////////////////////////////
/// The base primitive class. 
class Primitive : public PDataContainer
{
public:
	Primitive();
	Primitive(const Primitive &other);
	virtual ~Primitive();

	///////////////////////////////////////////////////
	///@name Abstract Primitive Interface
	///@{
	virtual Primitive *Clone() const=0;
	virtual void Render()=0;
	virtual dBoundingBox GetBoundingBox(const dMatrix &space)=0;
	virtual void ApplyTransform(bool ScaleRotOnly=false)=0;
	virtual Evaluator *MakeEvaluator()=0;
	///@}
    
	/// This needs to be set appropriately for all derived types
	virtual string GetTypeName()    { return "Primitive"; }
		
	/// Only makes sense for certain primitive types
	virtual void RecalculateNormals(bool smooth) {}
	
	///////////////////////////////////////////////////
	///@name Primitive Interface
	///@{
	void RenderBoundingBox();
	static void RenderAxes();
	void Prerender();
	void ApplyState()               { m_State.Apply(); }
	
	/// The primitives state stores everything 
	/// general to all primitives
	void SetState(State *s)         { assert(s); m_State=*s; }
	State *GetState()               { return &m_State; }
	
	/// A hint if we are in the physics system or not
	void SetPhysicalHint(bool s)    { m_IsPhysical=s; }
	bool IsPhysicalHint()           { return m_IsPhysical; }
	
	/// Visibility status bitfield - prevents rendering for different cameras
	unsigned int GetVisibility()    { return m_Visibility; }
	void SetVisibility(unsigned int s) { m_Visibility=s; } 
	
	/// Whether we should be included in the selection pass
	bool IsSelectable()      		{ return m_Selectable; }
	void Selectable(bool s)      	{ m_Selectable=s; }
	///@}

protected:
	State m_State;
	
private:
	
	///\todo: make these into an enum/bitfield?
	bool  m_IsPhysical;
	unsigned int m_Visibility;
	bool  m_Selectable;
};

};

#endif
