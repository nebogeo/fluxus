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

#ifndef FLUXUSLIGHT
#define FLUXUSLIGHT

#include <string>
#include <assert.h>
#include "dada.h"

namespace Fluxus
{

//////////////////////////////////////////////////////
/// The fluxus light
/// This is a fairly simple and stupid abstraction 
/// of OpenGL lights.
class Light
{
public:
	Light();
	virtual ~Light();
	///////////////////////////
	///@name Renderer Interface
	///@{
	void Render();
	///@}
	
	enum Type {POINT,DIRECTIONAL,SPOT};

	///////////////////////////
	///@name Accessors
	///@{
	void SetType(Type s) { m_Type=s; }
	void SetIndex(int s);
	void SetAmbient(dColour s);
	void SetDiffuse(dColour s);
	void SetSpecular(dColour s);
	void SetSpotAngle(float s);
	void SetSpotExponent(float s);
	void SetPosition(dVector s);
	void SetAttenuation(int type, float s);
	void SetDirection(dVector s);
	dVector GetPosition() { return m_Position; }
	///@}
	
	///////////////////////////
	///@name Camera Locking
	/// Whether the light is to be locked
	/// onto the camera
	///@{
	void SetCameraLock(bool s)  { m_CameraLock=s; }
	bool GetCameraLock()   { return m_CameraLock; }
	///@}
	
	
protected:

	int m_Index;
	dColour m_Ambient;
	dColour m_Diffuse;
	dColour m_Specular;
	dVector m_Position;
	dVector m_Direction;
	
	Type m_Type;
	bool m_CameraLock;
	
private:
	
	friend istream &operator>>(istream &s, Light &o);
	friend ostream &operator<<(ostream &s, Light &o);
};

istream &operator>>(istream &s, Light &o);
ostream &operator<<(ostream &s, Light &o);

};

#endif
