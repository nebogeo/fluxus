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
#include <assert.h>
#include "State.h"

#ifndef N_PRIM
#define N_PRIM

namespace fluxus
{

class Primitive
{
public:
	Primitive() : m_IsPhysical(false),m_Hidden(false) {}
	virtual ~Primitive() {}
	virtual void Render()=0;
	virtual dBoundingBox GetBoundingBox()=0;
	virtual void ApplyTransform(bool ScaleRotOnly=false)=0;
    virtual string GetTypeName()    { return "Primitive"; }
	
	// for per vertex/cv/etc data manipulation
	virtual unsigned int GetDataSize() { return 0; }	
	virtual void SetData(char t, unsigned int i, dVector v) {}
	virtual dVector GetData(char t, unsigned int i) { return dVector(); }
	virtual void RecalculateNormals() {}
	virtual void Finalise() {} // setdata unfinalises some prims - use this to optimise again
	
	void RenderBoundingBox();
	
	void ApplyState()               { m_State.Apply(); }
	void SetState(State *s)         { assert(s); m_State=*s; }
	void SetPhysicalHint(bool s)    { m_IsPhysical=s; }
	bool IsPhysicalHint()           { return m_IsPhysical; }
	bool Hidden()      		        { return m_Hidden; }
	void Hide(bool s)      		    { m_Hidden=s; }
	State *GetState()               { return &m_State; }
	
	
protected:
	State m_State;
	
private:
	bool  m_IsPhysical;
	bool  m_Hidden;
	
	friend istream &operator>>(istream &s, Primitive &o);
	friend ostream &operator<<(ostream &s, Primitive &o);
};

istream &operator>>(istream &s, Primitive &o);
ostream &operator<<(ostream &s, Primitive &o);

};

#endif
