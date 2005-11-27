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

using namespace fluxus;

void fluxus::MakeCube(PolyPrimitive *p, float size)
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

void fluxus::MakePlane(PolyPrimitive *p)
{
    p->AddVertex(dVertex(dVector(-0.5,-0.5,0),dVector(0,0,1),0,0));
    p->AddVertex(dVertex(dVector(0.5,-0.5,0),dVector(0,0,1),1,0));
    p->AddVertex(dVertex(dVector(0.5,0.5,0),dVector(0,0,1),1,1));
    p->AddVertex(dVertex(dVector(-0.5,0.5,0),dVector(0,0,1),0,1));

    p->AddVertex(dVertex(dVector(-0.5,0.5,0),dVector(0,0,-1),0,1));
    p->AddVertex(dVertex(dVector(0.5,0.5,0),dVector(0,0,-1),1,1));
    p->AddVertex(dVertex(dVector(0.5,-0.5,0),dVector(0,0,-1),1,0));
    p->AddVertex(dVertex(dVector(-0.5,-0.5,0),dVector(0,0,-1),0,0));
}

void fluxus::MakePlane(PolyPrimitive *p, int xsegs, int ysegs)
{
	float usegsize=1/(float)xsegs;
	float vsegsize=1/(float)ysegs;
	
	for (int x=0; x<xsegs; x++)
	{
		for (int y=0; y<ysegs; y++)
		{
			float u=x/(float)xsegs;
			float v=y/(float)ysegs;	
    		p->AddVertex(dVertex(dVector(u,v,0),dVector(0,0,1),u,v));
    		p->AddVertex(dVertex(dVector(u+usegsize,v,0),dVector(0,0,1),u+usegsize,v));
    		p->AddVertex(dVertex(dVector(u+usegsize,v+vsegsize,0),dVector(0,0,1),u+usegsize,v+vsegsize));
    		p->AddVertex(dVertex(dVector(u,v+vsegsize,0),dVector(0,0,1),u,v+vsegsize));
		}
	}
}

void fluxus::MakeCylinder(PolyPrimitive *p, float height, float radius, int hsegments, int rsegments)
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
			
			p->AddVertex(dVertex(dVector(point2.x,j*heightpersegment,point2.z),n2,i/(float)rsegments,j*heightpersegment));			
			p->AddVertex(dVertex(dVector(point.x,(j+1)*heightpersegment,point.z),n,i/(float)rsegments,(j+1)*heightpersegment));
			p->AddVertex(dVertex(dVector(point.x,j*heightpersegment,point.z),n,i/(float)rsegments,j*heightpersegment));
			
			p->AddVertex(dVertex(dVector(point2.x,j*heightpersegment,point2.z),n2,i/(float)rsegments,j*heightpersegment));
			p->AddVertex(dVertex(dVector(point2.x,(j+1)*heightpersegment,point2.z),n2,i/(float)rsegments,(j+1)*heightpersegment));
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
		p->AddVertex(dVertex(dVector(point2.x,centre.y,point2.z),normal,0,i/(float)rsegments));
		p->AddVertex(dVertex(dVector(point.x,centre.y,point.z),normal,0,i/(float)rsegments));
		p->AddVertex(dVertex(centre,normal,0,0));			
	}
		
	centre=dVector(0,height,0);
	normal=dVector(0,1,0);
	for (int i=0; i<rsegments; i++)
	{
		dVector point(sin(i*radpersegment)*radius,0,cos(i*radpersegment)*radius);
		dVector point2(sin((i+1)*radpersegment)*radius,0,cos((i+1)*radpersegment)*radius);
		p->AddVertex(dVertex(centre,normal,1,1));			
		p->AddVertex(dVertex(dVector(point.x,centre.y,point.z),normal,1,i/(float)rsegments));
		p->AddVertex(dVertex(dVector(point2.x,centre.y,point2.z),normal,1,i/(float)rsegments));
	}	
}

void fluxus::MakeSphere(PolyPrimitive *p, float radius, int hsegments, int rsegments)
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

void fluxus::MakeNURBSSphere(NURBSPrimitive *p, float radius, int hsegments, int rsegments)
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
			p->AddTex(dVector(i/(float)rsegments,j/(float)hsegments,0));
		}
	}
}

void fluxus::MakeNURBSPlane(NURBSPrimitive *p, int usegments, int vsegments)
{
	p->Init(3,3,usegments,vsegments);
	
	for (int n=-1; n<=usegments+1; n++) p->AddUKnot(n/(float)usegments);
	for (int n=-1; n<=vsegments+1; n++) p->AddVKnot(n/(float)vsegments);
	
	for (int j=0; j<vsegments; j++)
	{
		for (int i=0; i<usegments; i++)
		{			
			p->AddCV(dVector(i/(float)usegments,0,j/(float)vsegments));
			p->AddN(dVector(0,1,0));
			p->AddTex(dVector(i/(float)usegments,j/(float)vsegments,0));
		}
	}
}
