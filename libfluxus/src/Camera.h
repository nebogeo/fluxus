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
 
#ifndef FLUX_CAMERA
#define FLUX_CAMERA

#include "Primitive.h"

namespace Fluxus
{

//////////////////////////////////////////////////////
/// The fluxus camera
class Camera
{
public:
	Camera();
	~Camera();

	/////////////////////////////////////////////
	///@name Renderer interface
	///@{
	
	/// Whether the camera needs the renderer to
	/// reinitialise itself - due to the camera
	/// having changed
	bool NeedsInit() { return !m_Initialised; }
	
	/// Apply the projection matrix to the stack
	void DoProjection();
	
	/// Apply the camera matrix to the stack
	void DoCamera();
	///@}
	
	/////////////////////////////////////////////
	///@name Accessors
	///@{
	dMatrix *GetMatrix()                     { return &m_Transform; }
	void SetMatrix(const dMatrix &m)         { m_Transform=m; }
	dMatrix *GetLockedMatrix()               { return &m_LockedMatrix; }
	dMatrix GetProjection();
	float GetUp() { return m_Up; }
	float GetLeft() { return m_Left; }
	/// Lock the camera to this primitive's position
	void LockCamera(Primitive *p);
	void SetCameraLag(float s)               { m_CameraLag=s; }
	void SetOrtho(bool s)                    { m_Ortho=s; m_Initialised=false; }
	void SetOrthoZoom(float s)				 { m_OrthZoom=s; m_Initialised=false; }
	void SetFrustum(float u, float d, float l, float r) { m_Up=u; m_Down=d; m_Left=l; m_Right=r; m_Initialised=false; }
	void SetClip(float f, float b)           { m_Front=f; m_Back=b; m_Initialised=false; }
	///@}

private:

	bool m_Initialised;
	dMatrix m_Transform;
 	bool  m_Ortho;
	Primitive *m_CameraAttached;
	float m_CameraLag;
	dMatrix  m_LockedMatrix;
	float m_Up,m_Down,m_Left,m_Right,m_Front,m_Back;
	float m_OrthZoom;
};

}

#endif
