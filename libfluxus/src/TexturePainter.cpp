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

#include "State.h"
#include "TexturePainter.h"
#include "PNGLoader.h"
#include "SearchPaths.h"

using namespace fluxus;

TexturePainter *TexturePainter::m_Singleton=NULL;

TexturePainter::TexturePainter() 
{
}

TexturePainter::~TexturePainter()
{
}

void TexturePainter::Initialise()
{
	for (int c=0; c<MAX_TEXTURES; c++)
	{
		glActiveTexture(GL_TEXTURE0+c);
		glClientActiveTexture(GL_TEXTURE0+c);
		glEnableClientState(GL_TEXTURE_COORD_ARRAY);

		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);

    	glMatrixMode(GL_TEXTURE);
    	glLoadIdentity();
	}
	glClientActiveTexture(GL_TEXTURE0);
}

int TexturePainter::LoadTexture(const string &Filename, bool ignorecache)
{
	string Fullpath = SearchPaths::Get()->GetFullPath(Filename);
	
	// see if we've loaded this one already
	map<string,int>::iterator i=m_LoadedMap.find(Fullpath);
	if (i!=m_LoadedMap.end())
	{
		if (!ignorecache)
		{
			return i->second;
		}
		else
		{
			// remove the old texture
			glDeleteTextures(1,(GLuint*)&i->second);
			delete m_TextureMap[i->second];
		}
	}
	
	
	GLuint ID=0;
	
	char *ImageData;
    TextureDesc *desc = new TextureDesc;
    ImageData=PNGLoader::Load(Fullpath,desc->Width,desc->Height,desc->Format);
	
	if (ImageData!=NULL)
    {
		// upload to card...
		glGenTextures(1,&ID);
    	glBindTexture(GL_TEXTURE_2D,ID);
		if (desc->Format==RGB)
    	{
			gluBuild2DMipmaps(GL_TEXTURE_2D,3,desc->Width, desc->Height,GL_RGB,GL_UNSIGNED_BYTE,ImageData);
       		
			// test texture upload
			/*glTexImage2D(GL_PROXY_TEXTURE_2D, 0, GL_RGB, desc->Width, desc->Height, 0, GL_RGB, GL_UNSIGNED_BYTE, NULL);
			
			int width = 0;
			int height = 0;
     		glGetTexLevelParameteriv (GL_PROXY_TEXTURE_2D, 0, GL_TEXTURE_WIDTH, &width);
     		glGetTexLevelParameteriv (GL_PROXY_TEXTURE_2D, 0, GL_TEXTURE_HEIGHT, &height);
			
			if (width!=desc->Width || height!=desc->Height) 
	  		{
				cerr<<"TexturePainter::LoadTexture: not enough gfx memory to load texture ["<<Fullpath<<"]"<<endl;
			}
			else
			{
				glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, desc->Width, desc->Height, 0, GL_RGB, GL_UNSIGNED_BYTE, ImageData);
			}*/
    	}
    	else
    	{
    		gluBuild2DMipmaps(GL_TEXTURE_2D,4,desc->Width, desc->Height,GL_RGBA,GL_UNSIGNED_BYTE,ImageData);
			
			// test texture upload
			/*glTexImage2D(GL_PROXY_TEXTURE_2D, 0, GL_RGBA, desc->Width, desc->Height, 0, GL_RGBA, GL_UNSIGNED_BYTE, NULL);
			
			int width = 0;
			int height = 0;
     		glGetTexLevelParameteriv (GL_PROXY_TEXTURE_2D, 0, GL_TEXTURE_WIDTH, &width);
     		glGetTexLevelParameteriv (GL_PROXY_TEXTURE_2D, 0, GL_TEXTURE_HEIGHT, &height);
			
			if (width!=desc->Width || height!=desc->Height) 
	  		{
				cerr<<"TexturePainter::LoadTexture: not enough gfx memory to load texture ["<<Fullpath<<"]"<<endl;
			}
			else
			{
       			glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, desc->Width, desc->Height, 0, GL_RGBA, GL_UNSIGNED_BYTE, ImageData);
			}*/
    	}
		delete[] ImageData;
		
		m_TextureMap[ID]=desc;
		m_LoadedMap[Fullpath]=ID;
		return ID;
	}
	
	m_LoadedMap[Fullpath]=-1;
    return -1;
}

bool TexturePainter::SetCurrent(int *ids)
{
	bool ret=false;
	
	for (int c=0; c<MAX_TEXTURES; c++)
	{
		glActiveTexture(GL_TEXTURE0+c);
		
		map<unsigned int,TextureDesc*>::iterator i=m_TextureMap.find(ids[c]);
		if (i!=m_TextureMap.end())
		{
			glEnable(GL_TEXTURE_2D);
			glBindTexture(GL_TEXTURE_2D,ids[c]);
			if (c==0) glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
			else glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
			ret=true;
		}
		else
		{
			glDisable(GL_TEXTURE_2D);
		}
	}
	
	return ret;
}

void TexturePainter::DisableAll()
{
	for (int c=0; c<MAX_TEXTURES; c++)
	{
		glActiveTexture(GL_TEXTURE0+c);
		glDisable(GL_TEXTURE_2D);
	}
}







