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

#include "PolyPrimitive.h"
#include "GraphicsUtils.h"

using namespace Fluxus;

///\todo preallocate all these arrays

void Fluxus::MakeCube(PolyPrimitive *p, float size)
{
    dVector boxv0; dVector boxv1;
    dVector boxv2; dVector boxv3;
    dVector boxv4; dVector boxv5;
    dVector boxv6; dVector boxv7;
    dVector Normal;

    size/=2.0f;

    boxv0.x = -size; boxv0.y = -size; boxv0.z = -size;
    boxv1.x =  size; boxv1.y = -size; boxv1.z = -size;
    boxv2.x =  size; boxv2.y =  size; boxv2.z = -size;
    boxv3.x = -size; boxv3.y =  size; boxv3.z = -size;
    boxv4.x = -size; boxv4.y = -size; boxv4.z =  size;
    boxv5.x =  size; boxv5.y = -size; boxv5.z =  size;
    boxv6.x =  size; boxv6.y =  size; boxv6.z =  size;
    boxv7.x = -size; boxv7.y =  size; boxv7.z =  size;

	Normal=dVector(0,0,-1);
    p->AddVertex(dVertex(boxv3,Normal,0,0));
    p->AddVertex(dVertex(boxv2,Normal,1,0));
    p->AddVertex(dVertex(boxv1,Normal,1,1));
    p->AddVertex(dVertex(boxv0,Normal,0,1));

    Normal=dVector(0,-1,0);
    p->AddVertex(dVertex(boxv1,Normal,0,0));
    p->AddVertex(dVertex(boxv5,Normal,1,0));
    p->AddVertex(dVertex(boxv4,Normal,1,1));
    p->AddVertex(dVertex(boxv0,Normal,0,1));

    Normal=dVector(0,1,0);
    p->AddVertex(dVertex(boxv3,Normal,0,0));
    p->AddVertex(dVertex(boxv7,Normal,1,0));
    p->AddVertex(dVertex(boxv6,Normal,1,1));
    p->AddVertex(dVertex(boxv2,Normal,0,1));

    Normal=dVector(0,0,1);
    p->AddVertex(dVertex(boxv4,Normal,0,0));
    p->AddVertex(dVertex(boxv5,Normal,1,0));
    p->AddVertex(dVertex(boxv6,Normal,1,1));
    p->AddVertex(dVertex(boxv7,Normal,0,1));

    Normal=dVector(-1,0,0);
    p->AddVertex(dVertex(boxv4,Normal,0,0));
    p->AddVertex(dVertex(boxv7,Normal,1,0));
    p->AddVertex(dVertex(boxv3,Normal,1,1));
    p->AddVertex(dVertex(boxv0,Normal,0,1));

    Normal=dVector(1,0,0);
    p->AddVertex(dVertex(boxv2,Normal,0,0));
    p->AddVertex(dVertex(boxv6,Normal,1,0));
    p->AddVertex(dVertex(boxv5,Normal,1,1));
    p->AddVertex(dVertex(boxv1,Normal,0,1));
}

void Fluxus::MakePlane(PolyPrimitive *p)
{
    p->AddVertex(dVertex(dVector(-0.5,-0.5,0),dVector(0,0,1),0,0));
    p->AddVertex(dVertex(dVector(0.5,-0.5,0),dVector(0,0,1),1,0));
    p->AddVertex(dVertex(dVector(0.5,0.5,0),dVector(0,0,1),1,1));
    p->AddVertex(dVertex(dVector(-0.5,0.5,0),dVector(0,0,1),0,1));

    p->AddVertex(dVertex(dVector(-0.5,0.5,0),dVector(0,0,-1),1,1));
    p->AddVertex(dVertex(dVector(0.5,0.5,0),dVector(0,0,-1),0,1));
    p->AddVertex(dVertex(dVector(0.5,-0.5,0),dVector(0,0,-1),0,0));
    p->AddVertex(dVertex(dVector(-0.5,-0.5,0),dVector(0,0,-1),1,0));
}

void Fluxus::MakePlane(PolyPrimitive *p, int xsegs, int ysegs)
{
	float usegsize=1/(float)xsegs;
	float vsegsize=1/(float)ysegs;

	for (int x=0; x<xsegs; x++)
	{
		for (int y=0; y<ysegs; y++)
		{
			float u=x/(float)xsegs;
			float v=y/(float)ysegs;			
			p->AddVertex(dVertex(dVector(u-0.5f,v-0.5f,0),dVector(0,0,1),u,v));
			p->AddVertex(dVertex(dVector(u+usegsize-0.5f,v-0.5f,0),dVector(0,0,1),u+usegsize,v));
			p->AddVertex(dVertex(dVector(u+usegsize-0.5f,v+vsegsize-0.5f,0),dVector(0,0,1),u+usegsize,v+vsegsize));
			p->AddVertex(dVertex(dVector(u-0.5f,v+vsegsize-0.5f,0),dVector(0,0,1),u,v+vsegsize));
		}
	}
}

void Fluxus::MakeCylinder(PolyPrimitive *p, float height, float radius, int hsegments, int rsegments)
{
	float heightpersegment = height/hsegments;
	float radpersegment = (360/rsegments)*DEG_CONV;
	for (int j=0; j<hsegments; j++)
	{
		for (int i=0; i<rsegments; i++)
		{
			dVector point(sin(i*radpersegment)*radius,0,cos(i*radpersegment)*radius);
			dVector point2(sin((i+1)*radpersegment)*radius,0,cos((i+1)*radpersegment)*radius);

			dVector ntemp(sin(i*radpersegment)*(radius+1.0),0,cos(i*radpersegment)*(radius+1.0));
			dVector ntemp2(sin((i+1)*radpersegment)*(radius+1.0),0,cos((i+1)*radpersegment)*(radius+1.0));
			dVector n(ntemp-point);
			dVector n2(ntemp2-point2);
			n.normalise();
			n2.normalise();

			p->AddVertex(dVertex(dVector(point2.x,j*heightpersegment,point2.z),n2,(i+1)/(float)rsegments,j*heightpersegment));
			p->AddVertex(dVertex(dVector(point.x,(j+1)*heightpersegment,point.z),n,i/(float)rsegments,(j+1)*heightpersegment));
			p->AddVertex(dVertex(dVector(point.x,j*heightpersegment,point.z),n,i/(float)rsegments,j*heightpersegment));

			p->AddVertex(dVertex(dVector(point2.x,j*heightpersegment,point2.z),n2,(i+1)/(float)rsegments,j*heightpersegment));
			p->AddVertex(dVertex(dVector(point2.x,(j+1)*heightpersegment,point2.z),n2,(i+1)/(float)rsegments,(j+1)*heightpersegment));
			p->AddVertex(dVertex(dVector(point.x,(j+1)*heightpersegment,point.z),n,i/(float)rsegments,(j+1)*heightpersegment));
		}
	}

	// cap the cylinder at both ends
	dVector centre(0,0,0);
	dVector normal(0,-1,0);

	for (int i=0; i<rsegments; i++)
	{
		dVector point(sin(i*radpersegment)*radius,0,cos(i*radpersegment)*radius);
		dVector point2(sin((i+1)*radpersegment)*radius,0,cos((i+1)*radpersegment)*radius);
		p->AddVertex(dVertex(dVector(point2.x, centre.y, point2.z), normal, point2.x / 2 + .5, point2.z / 2 + .5));
		p->AddVertex(dVertex(dVector(point.x, centre.y, point.z), normal, point.x / 2 + .5, point.z / 2 + .5));
		p->AddVertex(dVertex(centre, normal, 0.5, 0.5));
	}

	centre=dVector(0,height,0);
	normal=dVector(0,1,0);
	for (int i=0; i<rsegments; i++)
	{
		dVector point(sin(i*radpersegment)*radius,0,cos(i*radpersegment)*radius);
		dVector point2(sin((i+1)*radpersegment)*radius,0,cos((i+1)*radpersegment)*radius);
		p->AddVertex(dVertex(centre, normal, 0.5, 0.5));
		p->AddVertex(dVertex(dVector(point.x, centre.y, point.z), normal, point.x / 2 + .5, point.z / 2 + .5));
		p->AddVertex(dVertex(dVector(point2.x, centre.y, point2.z), normal, point2.x / 2 + .5, point2.z / 2 + .5));
	}
}

void Fluxus::MakeSphere(PolyPrimitive *p, float radius, int hsegments, int rsegments)
{
	float radpersegment = (360/(float)rsegments)*DEG_CONV;
	for (int j=0; j<hsegments; j++)
	{
		float scale[2],height[2],nheight[2];
		scale[0] = sin((j/(float)hsegments)*180*DEG_CONV);
		scale[1] = sin(((j+1)/(float)hsegments)*180*DEG_CONV);
		height[0] = cos((j/(float)hsegments)*180*DEG_CONV)*radius;
		height[1] = cos(((j+1)/(float)hsegments)*180*DEG_CONV)*radius;
		nheight[0] = cos((j/(float)hsegments)*180*DEG_CONV)*(radius+1.0f);
		nheight[1] = cos(((j+1)/(float)hsegments)*180*DEG_CONV)*(radius+1.0f);

		for (int i=0; i<rsegments; i++)
		{
			dVector point[2],npoint[2];
			point[0]=dVector(sin(i*radpersegment)*radius,0,cos(i*radpersegment)*radius);
			point[1]=dVector(sin((i+1)*radpersegment)*radius,0,cos((i+1)*radpersegment)*radius);
			npoint[0]=dVector(sin(i*radpersegment)*(radius+1.0f),0,cos(i*radpersegment)*(radius+1.0f));
			npoint[1]=dVector(sin((i+1)*radpersegment)*(radius+1.0f),0,cos((i+1)*radpersegment)*(radius+1.0f));

			dVector tex[4];
			tex[0]=dVector((i+1)/(float)rsegments,j/(float)hsegments,0);
			tex[1]=dVector(i/(float)rsegments,(j+1)/(float)hsegments,0);
			tex[2]=dVector(i/(float)rsegments,j/(float)hsegments,0);
			tex[3]=dVector((i+1)/(float)rsegments,(j+1)/(float)hsegments,0);

			dVector verts[4];
			verts[0]=dVector(point[1].x*scale[0],height[0],point[1].z*scale[0]);
			verts[1]=dVector(point[0].x*scale[1],height[1],point[0].z*scale[1]);
			verts[2]=dVector(point[0].x*scale[0],height[0],point[0].z*scale[0]);
			verts[3]=dVector(point[1].x*scale[1],height[1],point[1].z*scale[1]);

			dVector normals[4];
			normals[0]=(dVector(npoint[1].x*scale[0],nheight[0],npoint[1].z*scale[0])-verts[0]).normalise();
			normals[1]=(dVector(npoint[0].x*scale[1],nheight[1],npoint[0].z*scale[1])-verts[1]).normalise();
			normals[2]=(dVector(npoint[0].x*scale[0],nheight[0],npoint[0].z*scale[0])-verts[2]).normalise();
			normals[3]=(dVector(npoint[1].x*scale[1],nheight[1],npoint[1].z*scale[1])-verts[3]).normalise();

			p->AddVertex(dVertex(verts[2],normals[2],tex[2].x,tex[2].y));
			p->AddVertex(dVertex(verts[1],normals[1],tex[1].x,tex[1].y));
			p->AddVertex(dVertex(verts[0],normals[0],tex[0].x,tex[0].y));

			p->AddVertex(dVertex(verts[0],normals[0],tex[0].x,tex[0].y));
			p->AddVertex(dVertex(verts[1],normals[1],tex[1].x,tex[1].y));
			p->AddVertex(dVertex(verts[3],normals[3],tex[3].x,tex[3].y));
		}
	}
}

static dVector Fluxus::MidpointOnSphere(dVector &a, dVector &b)
{
	dVector midpoint = (a + b) * 0.5;
	dVector radial = midpoint.normalise();
	return radial;
}

static void Fluxus::MakeIcosphereFace(PolyPrimitive *p, dVector &a, dVector &b, dVector &c, int level)
{
	if (level <= 1)
	{
		dVector ta, tb, tc;

		// cartesian to spherical coordinates
		ta.x = atan2(a.z, a.x) / (2 * M_PI) + .5;
		ta.y = acos(a.y) / M_PI;
		tb.x = atan2(b.z, b.x) / (2 * M_PI) + .5;
		tb.y = acos(b.y) / M_PI;
		tc.x = atan2(c.z, c.x) / (2 * M_PI) + .5;
		tc.y = acos(c.y) / M_PI;

		// texture wrapping coordinate limits
		const float mint = .25;
		const float maxt = 1 - mint;

		// fix north and south pole textures
		if ((a.x == 0) && ((a.y == 1) || (a.y == -1)))
		{
			ta.x = (tb.x + tc.x) / 2;
			if (((tc.x < mint) && (tb.x > maxt)) ||
				((tb.x < mint) && (tc.x > maxt)))
			{
				ta.x += .5;
			}
		}
		else
		if ((b.x == 0) && ((b.y == 1) || (b.y == -1)))
		{
			tb.x = (ta.x + tc.x) / 2;
			if (((tc.x < mint) && (ta.x > maxt)) ||
				((ta.x < mint) && (tc.x > maxt)))
			{
				tb.x += .5;
			}
		}
		else
		if ((c.x == 0) && ((c.y == 1) || (c.y == -1)))
		{
			tc.x = (ta.x + tb.x) / 2;
			if (((ta.x < mint) && (tb.x > maxt)) ||
				((tb.x < mint) && (ta.x > maxt)))
			{
				tc.x += .5;
			}
		}

		// fix texture wrapping
		if ((ta.x < mint) && (tc.x > maxt))
		{
			if (tb.x < mint)
				tc.x -= 1;
			else
				ta.x += 1;
		}
		else
		if ((ta.x < mint) && (tb.x > maxt))
		{
			if (tc.x < mint)
				tb.x -= 1;
			else
				ta.x += 1;
		}
		else
		if ((tc.x < mint) && (tb.x > maxt))
		{
			if (ta.x < mint)
				tb.x -= 1;
			else
				tc.x += 1;
		}
		else
		if ((ta.x > maxt) && (tc.x < mint))
		{
			if (tb.x < mint)
				ta.x -= 1;
			else
				tc.x += 1;
		}
		else
		if ((ta.x > maxt) && (tb.x < mint))
		{
			if (tc.x < mint)
				ta.x -= 1;
			else
				tb.x += 1;
		}
		else
		if ((tc.x > maxt) && (tb.x < mint))
		{
			if (ta.x < mint)
				tc.x -= 1;
			else
				tb.x += 1;
		}

		p->AddVertex(dVertex(a, a, ta.x, ta.y));
		p->AddVertex(dVertex(c, c, tc.x, tc.y));
		p->AddVertex(dVertex(b, b, tb.x, tb.y));
	}
	else
	{
		dVector ab = MidpointOnSphere(a, b);
		dVector bc = MidpointOnSphere(b, c);
		dVector ca = MidpointOnSphere(c, a);

		level--;
		MakeIcosphereFace(p, a, ab, ca, level);
		MakeIcosphereFace(p, ab, b, bc, level);
		MakeIcosphereFace(p, ca, bc, c, level);
		MakeIcosphereFace(p, ab, bc, ca, level);
	}
}

/*
 * Based on the explanation and code by Paul Bourke and Craig Reynolds:
 * http://local.wasp.uwa.edu.au/~pbourke/geometry/platonic/
*/
void Fluxus::MakeIcosphere(PolyPrimitive *p, int level)
{
	float sqrt5 = sqrt(5.0);
	float phi = (1.0 + sqrt5) * 0.5;
	float ratio = sqrt(10.0f + (2.0 * sqrt5)) / (4.0 * phi);
	float a = (1 / ratio) * 0.5;
	float b = (1 / ratio) / (2.0 * phi);

	dVector v[12] = {
		dVector( 0,  b, -a),
		dVector( b,  a,  0),
		dVector(-b,  a,  0),
		dVector( 0,  b,  a),
		dVector( 0, -b,  a),
		dVector(-a,  0,  b),
		dVector( 0, -b, -a),
		dVector( a,  0, -b),
		dVector( a,  0,  b),
		dVector(-a,  0, -b),
		dVector( b, -a,  0),
		dVector(-b, -a,  0)
	};

	MakeIcosphereFace(p, v[0], v[1], v[2], level);
	MakeIcosphereFace(p, v[3], v[2], v[1], level);
	MakeIcosphereFace(p, v[3], v[4], v[5], level);
	MakeIcosphereFace(p, v[3], v[8], v[4], level);
	MakeIcosphereFace(p, v[0], v[6], v[7], level);
	MakeIcosphereFace(p, v[0], v[9], v[6], level);
	MakeIcosphereFace(p, v[4], v[10], v[11], level);
	MakeIcosphereFace(p, v[6], v[11], v[10], level);
	MakeIcosphereFace(p, v[2], v[5], v[9], level);
	MakeIcosphereFace(p, v[11], v[9], v[5], level);
	MakeIcosphereFace(p, v[1], v[7], v[8], level);
	MakeIcosphereFace(p, v[10], v[8], v[7], level);
	MakeIcosphereFace(p, v[3], v[5], v[2], level);
	MakeIcosphereFace(p, v[3], v[1], v[8], level);
	MakeIcosphereFace(p, v[0], v[2], v[9], level);
	MakeIcosphereFace(p, v[0], v[7], v[1], level);
	MakeIcosphereFace(p, v[6], v[9], v[11], level);
	MakeIcosphereFace(p, v[6], v[10], v[7], level);
	MakeIcosphereFace(p, v[4], v[11], v[5], level);
	MakeIcosphereFace(p, v[4], v[8], v[10], level);
}

void Fluxus::MakeTorus(PolyPrimitive *p, float innerradius, float outerradius, int hsegments, int rsegments)
{
	float radperouter = (360/(float)rsegments)*DEG_CONV;
	float radperinner = (360/(float)hsegments)*DEG_CONV;

	for(int j=0; j<rsegments; j++)
	{
		float cpsi = cos(j*radperouter);
		float spsi = sin(j*radperouter);
		float cpsi2 = cos((j+1)*radperouter);
		float spsi2 = sin((j+1)*radperouter);

		for(int i=0; i<hsegments; i++)
		{
			float cphi = cos(i*radperinner);
			float sphi = sin(i*radperinner);
			float cphi2 = cos((i+1)*radperinner);
			float sphi2 = sin((i+1)*radperinner);

			dVector verts[4];
			verts[0].x = cpsi * (outerradius + cphi * innerradius);
			verts[0].y = spsi * (outerradius + cphi * innerradius);
			verts[0].z = sphi * innerradius;
			verts[1].x = cpsi * (outerradius + cphi2 * innerradius);
			verts[1].y = spsi * (outerradius + cphi2 * innerradius);
			verts[1].z = sphi2 * innerradius;
			verts[2].x = cpsi2 * (outerradius + cphi2 * innerradius);
			verts[2].y = spsi2 * (outerradius + cphi2 * innerradius);
			verts[2].z = sphi2 * innerradius;
			verts[3].x = cpsi2 * (outerradius + cphi * innerradius);
			verts[3].y = spsi2 * (outerradius + cphi * innerradius);
			verts[3].z = sphi * innerradius;

			dVector normals[4];
			normals[0].x = cpsi*cphi;
			normals[0].y = spsi*cphi;
			normals[0].z = sphi;
			normals[1].x = cpsi*cphi2;
			normals[1].y = spsi*cphi2;
			normals[1].z = sphi2;
			normals[2].x = cpsi2*cphi2;
			normals[2].y = spsi2*cphi2;
			normals[2].z = sphi2;
			normals[3].x = cpsi2*cphi;
			normals[3].y = spsi2*cphi;
			normals[3].z = sphi;

			dVector tex[4];
			tex[0]=dVector(i/(float)rsegments,j/(float)hsegments,0);
			tex[1]=dVector((i+1)/(float)rsegments,j/(float)hsegments,0);
			tex[2]=dVector((i+1)/(float)rsegments,(j+1)/(float)hsegments,0);
			tex[3]=dVector(i/(float)rsegments,(j+1)/(float)hsegments,0);

			p->AddVertex(dVertex(verts[3],normals[3],tex[3].x,tex[3].y));
			p->AddVertex(dVertex(verts[2],normals[2],tex[2].x,tex[2].y));
			p->AddVertex(dVertex(verts[1],normals[1],tex[1].x,tex[1].y));
			p->AddVertex(dVertex(verts[0],normals[0],tex[0].x,tex[0].y));
		}
	}
}

void Fluxus::MakeNURBSSphere(NURBSPrimitive *p, float radius, int hsegments, int rsegments)
{
	p->Init(3,3,hsegments,rsegments);

	for (int n=-3; n<=hsegments+2; n++) p->AddUKnot(n/(float)hsegments);
	for (int n=-1; n<=rsegments; n++) p->AddVKnot(n/(float)rsegments);

	float radpersegment = (360/(float)(rsegments-3))*DEG_CONV;
	for (int j=-1; j<=hsegments+1; j++)
	{
		float scale = sin((j/(float)hsegments)*180*DEG_CONV);
		float height = cos((j/(float)hsegments)*180*DEG_CONV)*radius;

		for (int i=0; i<rsegments; i++)
		{
			p->AddCV(dVector(sin(i*radpersegment)*radius*scale,height,cos(i*radpersegment)*radius*scale));
			p->AddN(dVector(sin(i*radpersegment)*scale,height,cos(i*radpersegment)*scale));
			p->AddColour(dColour(1,1,1));
			p->AddTex(dVector(i/(float)rsegments,j/(float)hsegments,0));
		}
	}
}

void Fluxus::MakeNURBSPlane(NURBSPrimitive *p, int usegments, int vsegments)
{
	p->Init(3,3,usegments+1,vsegments+1);

	// wangle the knots so the surface reaches
	// to the edge of the vertex grid
	p->AddUKnot(0);
	p->AddUKnot(0);
	for (int n=0; n<usegments; n++) p->AddUKnot(n/((float)usegments-1.0f));
	p->AddUKnot(1);
	p->AddUKnot(1);

	p->AddVKnot(0);
	p->AddVKnot(0);
	for (int n=0; n<vsegments; n++) p->AddVKnot(n/((float)vsegments-1.0f));
	p->AddVKnot(1);
	p->AddVKnot(1);

	for (int i=0; i<usegments+1; i++)
	{
		for (int j=0; j<vsegments+1; j++)
		{
			p->AddCV(dVector(i/(float)usegments-0.5,j/(float)vsegments-0.5,0));
			p->AddN(dVector(0,0,1));
			p->AddColour(dColour(1,1,1));
			p->AddTex(dVector(i/(float)usegments,j/(float)vsegments,0));
		}
	}
}

