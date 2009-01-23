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

#ifndef __APPLE__
#include <GL/gl.h> 
#else
#include <OpenGL/gl.h>
#include <OpenGL/glext.h>
#endif 
#include <stdlib.h> 

GLubyte *GetScreenBuffer(int x, int y, unsigned int width, unsigned int height, int super=1);
int WriteTiff(const char *filename, const char *description, int x, int y, int width, int height, int compression, int super=1);
int WriteJPG(const char *filename, const char *description, int x, int y, int width, int height, int quality, int super=1);
int WritePPM(const char *filename, const char *description, int x, int y, int width, int height, int quality, int super=1);
int WriteJPGt(GLubyte *image, const char *filename, const char *description, int x, int y, int width, int height, int quality, int super=1);
