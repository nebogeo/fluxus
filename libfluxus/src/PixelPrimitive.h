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

#ifndef N_PIXELPRIM
#define N_PIXELPRIM

#include "Primitive.h"
#include "Renderer.h"
#include "Physics.h"

namespace Fluxus
{

//////////////////////////////////////////////////////
/// A pixel primitive - this allows you to make
/// procedural textures, as the pixel values are
/// accessable as pdata in this primitive. The
/// resulting texture can then be uploaded and
/// applied to other primitives.
class PixelPrimitive : public Primitive
{
public:
	PixelPrimitive(unsigned int w, unsigned int h, bool RendererActive = false);
	PixelPrimitive(const PixelPrimitive &other);
	virtual ~PixelPrimitive();

	///////////////////////////////////////////////////
	///@name Primitive Interface
	///@{
	virtual PixelPrimitive* Clone() const;
	virtual void Render();
	virtual dBoundingBox GetBoundingBox(const dMatrix &space);
	virtual void RecalculateNormals(bool smooth) {}
	virtual void ApplyTransform(bool ScaleRotOnly=false);
	virtual string GetTypeName() { return "PixelPrimitive"; }
	virtual Evaluator *MakeEvaluator() { return NULL; }
	///@}

	/// Create a new FBO and release the old one if exists
	void ResizeFBO(int w, int h);

	/// Upload the texture to the graphics card
	void Upload();

	/// Download the texture from the graphics card
	void Download();

	/// Load a png file into this primitive
	void Load(const string &filename);

	/// Save to a png file
	void Save(const string &filename) const;

	/// Get the uploaded texture ID - call Upload() first.
	unsigned int GetTexture() { return m_Texture; }

	/// Get the width
	unsigned int GetWidth() { return m_Width; }

	/// Get the height
	unsigned int GetHeight() { return m_Height; }

	void Bind();
	void Unbind();

	Renderer *GetRenderer() { return m_Renderer; }
	Physics *GetPhysics() { return m_Physics; }

	/// Activate the renderer
	void ActivateRenderer(bool active) { m_RendererActive = active; }

protected:

	virtual void PDataDirty();

	void DownloadPData();
	void UploadPData();

	vector<dVector> m_Points;
	vector<dColour> *m_ColourData;

	unsigned int m_Texture;
	unsigned int m_DepthBuffer;
	unsigned int m_FBO;

	unsigned int m_Width;
	unsigned int m_Height;
	unsigned int m_FBOWidth;
	unsigned int m_FBOHeight;
	float m_FBOMaxS;
	float m_FBOMaxT;

	Renderer *m_Renderer;
	Physics *m_Physics;

	bool m_ReadyForUpload;
	bool m_ReadyForDownload;
	bool m_FBOSupported;
	bool m_RendererActive;
};

};

#endif

