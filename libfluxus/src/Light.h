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
#include "dada.h"

#ifndef FLUXUSLIGHT
#define FLUXUSLIGHT

namespace fluxus
{

class Light
{
public:
	Light();
	virtual ~Light() {}
	void Render(int index=0);

	void SetAmbient(dColour s)  { m_Ambient=s; }
	void SetDiffuse(dColour s)  { m_Diffuse=s; }
	void SetSpecular(dColour s) { m_Specular=s; }
	void SetPosition(dVector s) { m_Position=s; }
	void SetCameraLock(bool s)  { m_CameraLock=s; }
	bool GetCameraLock()   { return m_CameraLock; }
	
protected:
	dColour m_Ambient;
	dColour m_Diffuse;
	dColour m_Specular;
	dVector m_Position;
	bool m_CameraLock;
	
private:
	
	friend istream &operator>>(istream &s, Light &o);
	friend ostream &operator<<(ostream &s, Light &o);
};

istream &operator>>(istream &s, Light &o);
ostream &operator<<(ostream &s, Light &o);

};

#endif
