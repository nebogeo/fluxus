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

#include "dada.h"
#include "Geometry.h"

using namespace Fluxus;

float Fluxus::PointLineDist(const dVector &p, const dVector &start, const dVector &end)
{
    float linemag;
    dVector intersection;
 
    float len = end.dist(start);
 
    float t = ((p.x-start.x)*(end.x-start.x) +
               (p.y-start.y)*(end.y-start.y) +
               (p.z-start.z)*(end.z-start.z)) / (len*len);
 
    if (t<0.0f) // off the end
	{
		return p.dist(start);
	}
    if (t>1.0f) // off the end
	{
		return p.dist(end);
	}
	
    intersection.x = start.x+t*(end.x-start.x);
    intersection.y = start.y+t*(end.y-start.y);
    intersection.z = start.z+t*(end.z-start.z);
 
    return p.dist(intersection);
}

/*bool IntersectLineQuad(const dVector &start, const dVector &end, 
		const dVector &a, const dVector &b, const dVector &c, const dVector &d,
		dVector &bary)
{
	return IntersectLineTriangle(start,end,a,b,c,intersections) || 
	       IntersectLineTriangle(start,end,b,c,d,intersections);
}*/

bool Fluxus::IntersectLineTriangle(const dVector &start, const dVector &end, 
	const dVector &a, const dVector &b, const dVector &c, 
	dVector &bary)
{
	dVector edge1 = b-a;
	dVector edge2 = c-a;
	dVector ray = end-start;
	dVector s1 = ray.cross(edge2);

	// degenerate triangle!
	float divisor = s1.dot(edge1);
	if (divisor==0.0) return false;
	float invdiv = 1/divisor;

	dVector distance = start-a;
	bary.x = distance.dot(s1)*invdiv;
	if (bary.x<0.0 || bary.x>1.0) return false;


	dVector s2 = distance.cross(edge1);
	bary.y = ray.dot(s2)*invdiv;
	if (bary.y<0.0 || (bary.x+bary.y)>1.0) return false;
	
	bary.z=1-(bary.x+bary.y);
	return true;
}

