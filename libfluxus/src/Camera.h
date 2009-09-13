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

class Renderer;
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
	bool NeedsInit();

	/// Apply the projection matrix to the stack
	void DoProjection();

	/// Apply the camera matrix to the stack
	void DoCamera(Renderer * renderer);
	///@}

	/////////////////////////////////////////////
	///@name Accessors
	///@{
	dMatrix *GetMatrix()                     { return &m_Transform; }
	void SetMatrix(const dMatrix &m)         { m_Transform=m; }
	dMatrix *GetLockedMatrix()               { return &m_LockedMatrix; }
	dMatrix GetProjection();
	void SetProjection(const dMatrix &m);
	float GetTop() { return m_Top; }
	float GetLeft() { return m_Left; }
	float GetBottom() { return m_Bottom; }
	float GetRight() { return m_Right; }
	/// Lock the camera to this primitive's position
	void LockCamera(int p);
	void SetCameraLag(float s)               { m_CameraLag=s; }
	void SetOrtho(bool s)                    { m_Ortho=s; m_CustomProjection=false; m_Initialised=false; }
	void SetOrthoZoom(float s)				 { m_OrthZoom=s; m_Initialised=false; }
	void SetFrustum(float l, float r, float b, float t) { m_Left=l; m_Right=r; m_Bottom=b; m_Top=t; m_CustomProjection=false; m_Initialised=false; }
	void SetClip(float f, float b)           { m_Front=f; m_Back=b; m_Initialised=false; }
	void SetViewport(float x, float y, float w, float h)
		{ m_ViewX=x; m_ViewY=y; m_ViewWidth=w; m_ViewHeight=h; }
	float GetViewportX()			 { return m_ViewX; }
	float GetViewportY()			 { return m_ViewY; }
	float GetViewportWidth()		 { return m_ViewWidth; }
	float GetViewportHeight()		 { return m_ViewHeight; }
	///@}

private:

	bool m_Initialised;
	dMatrix m_Transform;
	dMatrix m_CustomProjectionMatrix;
	bool m_Ortho;
	bool m_CustomProjection;
	int m_CameraAttached;
	float m_CameraLag;
	dMatrix  m_LockedMatrix;
	float m_Left,m_Right,m_Bottom,m_Top,m_Front,m_Back;
	float m_OrthZoom;
	float m_ViewX,m_ViewY,m_ViewWidth,m_ViewHeight;
	bool m_FirstAttach;
};

}

#endif

