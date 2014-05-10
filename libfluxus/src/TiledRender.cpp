// Copyright (C) 2009 Dave Griffiths
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

#include "TiledRender.h"
#include "Utils.h"

namespace Fluxus
{

unsigned char *TiledRender(Renderer *renderer, int width, int height)
{
	int scrw=0,scrh=0;
	renderer->GetResolution(scrw,scrh);

	unsigned int tilesx=1;
	unsigned int tilewidth=width;
	while (tilewidth>(unsigned int)scrw)
	{
		tilewidth/=2;
		tilesx*=2;
	}

	unsigned int tilesy=1;
	unsigned int tileheight=height;
	while (tileheight>(unsigned int)scrh)
	{
		tileheight/=2;
		tilesy*=2;
	}
	renderer->SetResolution(tilewidth,tileheight);

	float tilevpwidth = 1/(float)tilesx;
	float tilevpheight = 1/(float)tilesy;

	// assume just main camera
	Camera* camera = &(*renderer->GetCameraVec().begin());
	float left = camera->GetLeft();
	float right = camera->GetRight();
	float bottom = camera->GetBottom();
	float top = camera->GetTop();

	float frstwidth=right-left;
	float frstheight=top-bottom;
	float frsttilewidth=frstwidth/(float)tilesx;
	float frsttileheight=frstheight/(float)tilesy;

	unsigned char **tiles = new unsigned char *[tilesx*tilesy];
	unsigned int tile=0;
	for (unsigned int y=0; y<tilesy; y++)
	{
		for (unsigned int x=0; x<tilesx; x++)
		{
			float frstl=left+(x/(float)tilesx)*frstwidth;
			float frstr=frstl+frsttilewidth;
			float frstb=bottom+(y/(float)tilesy)*frstheight;
			float frstt=frstb+frsttileheight;
			camera->SetFrustum(frstl,frstr,frstb,frstt);
			renderer->Render();
			tiles[tile] = GetScreenBuffer(0, 0, tilewidth, tileheight, 1);
			tile++;
		}
	}

	// put things back as we found them...
	camera->SetFrustum(left,right,bottom,top);
	renderer->SetResolution(scrw,scrh);

	// splice the tiles together
	unsigned char *final = new unsigned char[width*height*3];
	unsigned int dstpos = 0;
	for (unsigned int tile=0; tile<tilesx*tilesy; tile++)
	{
		// find the x y pos of the corner of this tile
		unsigned int xorg=(tile%tilesx)*tilewidth;
		unsigned int yorg=(tile/tilesx)*tileheight;

		for (unsigned int x=0; x<tilewidth; x++)
		{
			for (unsigned int y=0; y<tileheight; y++)
			{
				unsigned int dst=(y+yorg)*width+(x+xorg);
				final[dst*3]=tiles[tile][(y*tilewidth+x)*3];
				final[dst*3+1]=tiles[tile][(y*tilewidth+x)*3+1];
				final[dst*3+2]=tiles[tile][(y*tilewidth+x)*3+2];
			}
		}
		delete[] tiles[tile];
	}
	delete[] tiles;
	return final;
}

};
