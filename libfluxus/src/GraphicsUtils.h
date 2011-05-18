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

#ifndef GRAPHICS_UTILS
#define GRAPHICS_UTILS

//#include "Renderer.h"
#include "PolyPrimitive.h"
#include "TextPrimitive.h"
#include "NURBSPrimitive.h"

namespace Fluxus
{

void MakeCube(PolyPrimitive *p, float size = 1);
void MakeCylinder(PolyPrimitive *p, float height, float radius, int hsegments, int rsegments);
void MakeSphere(PolyPrimitive *p, float radius, int hsegments, int rsegments);
void MakeIcosphere(PolyPrimitive *p, int level);
void MakeTorus(PolyPrimitive *p, float innerradius, float outerradius, int hsegments, int rsegments);
void MakePlane(PolyPrimitive *p);
void MakePlane(PolyPrimitive *p, int xsegs, int ysegs);
void MakeNURBSSphere(NURBSPrimitive *p, float radius, int hsegments, int rsegments);
void MakeNURBSPlane(NURBSPrimitive *p, int usegments, int vsegments);
void MakeTeapot(PolyPrimitive *p);

//static dVector MidpointOnSphere(dVector &a, dVector &b);
//static void MakeIcosphereFace(PolyPrimitive *p, dVector &a, dVector &b, dVector &c, int level);

}

#endif
