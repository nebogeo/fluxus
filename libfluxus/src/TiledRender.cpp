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

void TiledRender(const string &filename, Renderer *renderer, int width, int height)
{
	int scrw=0,scrh=0;
	renderer->GetResolution(scrw,scrh);

	unsigned int tilesx=1;
	unsigned int tilewidth=width;
	while (tilewidth>scrw) 
	{
		tilewidth/=2;
		tilesx+=1;
	}

	unsigned int tilesy=1;
	unsigned int tileheight=height;
	while (tileheight>scrh) 
	{
		tileheight/=2;
		tilesy+=1;
	}
	
	float tilevpwidth = 1/(float)tilesx;
	float tilevpheight = 1/(float)tilesy;

	unsigned char **tiles = new unsigned char *[tilesx*tilesy];
	unsigned int tile=0;
	for (unsigned int y=0; y<tilesy; y++)
	{
		for (unsigned int x=0; x<tilesx; x++)
		{
			cerr<<"rendering tile "<<x<<", "<<y<<endl;
			// assume just main camera??? :/
			Camera* camera = &(*renderer->GetCameraVec().begin());
			camera->SetViewport(x/(float)tilesx,y/(float)tilesy,tilevpwidth,tilevpheight);
			renderer->Render();
			tiles[tile] = GetScreenBuffer(0, 0, tilewidth, tileheight, 1);
			tile++;
		}
	}

	// splice!
	unsigned char *final = new unsigned char[width*height];
	unsigned int dstpos = 0;
	for (unsigned int tile=0; tile<tilesx*tilesy; tile++)
	{
		for (unsigned int x=0; x<tilewidth; x++)
		{	
			for (unsigned int y=0; y<tilewidth; y++)
			{
				final[dstpos++]=tiles[tile][(x+y*tilewidth)*3];
				final[dstpos++]=tiles[tile][(x+y*tilewidth)*3+1];
				final[dstpos++]=tiles[tile][(x+y*tilewidth)*3+2];
			}
		}
		//delete[] tiles[tx+ty*tilesx];	
	}
	//delete[] tiles;	
    WriteJPGt(final, filename.c_str(), "tile rendered by fluxus", 0, 0, width, height, 1);
	//delete[] final;*/

}

};
