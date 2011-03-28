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
	PixelPrimitive(unsigned int w, unsigned int h, bool RendererActive = false,
			unsigned txtcount = 1);
	PixelPrimitive(const PixelPrimitive &other);
	virtual ~PixelPrimitive();

	///////////////////////////////////////////////////
	///@name Primitive Interface
	///@{
	virtual PixelPrimitive* Clone() const;
	virtual void Render();
	// TODO: check maximum textures supported by hardware
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
	void Download(unsigned handle = 0 );

	/// Load a png file into this primitive
	void Load(const string &filename);

	/// Save to a png file
	void Save(const string &filename) const;

	/// Get the uploaded texture ID - call Upload() first.
	unsigned int GetTexture(unsigned i = 0) { return m_Textures[i % m_MaxTextures]; }

	/// Set the texture ID used when rendering the primitive
	void SetDisplayTexture(unsigned id) { m_DisplayTexture = id; }

	/// Set the texture ID used when rendering into the primitive
	void SetRenderTexture(unsigned id);

	/// Get the texture ID used for rendering
	unsigned GetRenderTexture() { return m_RenderTexture; }

	/// Get the width
	unsigned int GetWidth() { return m_Width; }

	/// Get the height
	unsigned int GetHeight() { return m_Height; }

	void Bind(int texture = -1);
	void Unbind();

	/// Get the FBO width
	unsigned int GetFBOWidth() { return m_FBOWidth; }

	/// Get the FBO height
	unsigned int GetFBOHeight() { return m_FBOHeight; }

	/// Get the FBO handle
	unsigned int GetFBO() { return m_FBO; }

	Renderer *GetRenderer() { return m_Renderer; }
	Physics *GetPhysics() { return m_Physics; }

	/// Activate the renderer
	void ActivateRenderer(bool active) { m_RendererActive = active; }

protected:

	virtual void PDataDirty();

	void DownloadPData();
	void UploadPData();

	vector<dVector,FLX_ALLOC(dVector) > m_Points;
	vector<dColour,FLX_ALLOC(dColour) > *m_ColourData;

	unsigned m_MaxTextures;
	unsigned *m_Textures; // attached textures

	unsigned m_DisplayTexture; // id for rendering the primitive on screen
	unsigned m_RenderTexture; // id for rendering into the fbo
	unsigned m_RenderTextureIndex; // index for rendering into the fbo
	unsigned GetTextureIndex(unsigned id);

	unsigned m_DepthBuffer;
	unsigned m_FBO;

	unsigned m_Width;
	unsigned m_Height;
	unsigned m_FBOWidth;
	unsigned m_FBOHeight;
	float m_FBOMaxS;
	float m_FBOMaxT;

	Renderer *m_Renderer;
	Physics *m_Physics;

	bool m_ReadyForUpload;
	bool m_ReadyForDownload;
	unsigned m_DownloadTextureHandle;
	bool m_FBOSupported;
	bool m_RendererActive;
};

};

#endif

