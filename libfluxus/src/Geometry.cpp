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

// returns the parametric distance along the line, or -1 for no intersection
float Fluxus::IntersectLineTriangle(const dVector &start, const dVector &end, 
	const dVector &ta, const dVector &tb, const dVector &tc, 
	dVector &bary)
{
    dVector u = ta-tc;
    dVector v = tb-tc;
    dVector n = v.cross(u);

    if (n.mag()==0) return -1;
    
    dVector ray = end-start;
    dVector w0 = start-tc;
    float a = -n.dot(w0);
    float b = n.dot(ray);

    // if b is small, ray is parallel
    if (b==0) return -1;
    //     if a==0 then the ray is in the plane

    float r = a/b;
    if (r<0) return -1;
    if (r>1) return -1;
    dVector I = start+(ray*r);
    float uu = u.dot(u);
    float uv = u.dot(v);
    float vv = v.dot(v);
    dVector w = I-tc;
    float wu = w.dot(u);
    float wv = w.dot(v);
    float D = uv*uv - uu*vv;

    bary.x=(uv*wv - vv*wu)/D;
    if (bary.x<0 || bary.x>1.0) return -1;
    bary.y=(uv*wu - uu*wv)/D;
    if (bary.y<0 || bary.y>1.0) return -1;
    bary.z=1-(bary.x+bary.y);
	if (bary.z<0 || bary.z>1.0) return -1;
    return r;
}

