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

#include "PolyEvaluator.h"
#include "PolyPrimitive.h"
#include "Geometry.h"

using namespace Fluxus;

PolyEvaluator::PolyEvaluator(const PolyPrimitive *prim) :
m_Prim(prim)
{
	assert(m_Prim!=NULL);
}

PolyEvaluator::~PolyEvaluator()
{
}

//////////////////////////////////////////////

Evaluator::Point PolyEvaluator::ClosestPoint(const dVector &position)
{
	return Point();
}

//////////////////////////////////////////////

bool PolyEvaluator::IntersectLine(const dVector &start, const dVector &end, vector<Point> &points)
{
	switch (m_Prim->GetType())
	{
		case PolyPrimitive::TRISTRIP: return IntersectTriStrip(start,end,points); break;
		case PolyPrimitive::QUADS: return IntersectQuads(start,end,points); break;
		case PolyPrimitive::TRILIST: return IntersectTriList(start,end,points); break;
		case PolyPrimitive::TRIFAN: return IntersectTriFan(start,end,points); break;
		case PolyPrimitive::POLYGON: return IntersectPolygon(start,end,points); break;
	};
	
	return false;
}

bool PolyEvaluator::IntersectTriStrip(const dVector &start, const dVector &end, vector<Point> &points)
{
	return false;
}

bool PolyEvaluator::IntersectQuads(const dVector &start, const dVector &end, vector<Point> &points)
{
	dVector bary;
	bool found=false;
	unsigned int i=0;
	while(i<m_Prim->Size())
	{
		unsigned int i1=i++;
		unsigned int i2=i++;
		unsigned int i4=i++;
		unsigned int i3=i++;
		
		if (IntersectLineTriangle(start,end,m_Prim->GetData<dVector>("p",i1),
										m_Prim->GetData<dVector>("p",i2),
										m_Prim->GetData<dVector>("p",i3),bary))
		{
			points.push_back(InterpolatePData(bary,i1,i2,i3));
			found=true;
		}
		else if (IntersectLineTriangle(start,end,m_Prim->GetData<dVector>("p",i2),
									 		 	 m_Prim->GetData<dVector>("p",i3),
												 m_Prim->GetData<dVector>("p",i4),bary))
		{
			points.push_back(InterpolatePData(bary,i2,i3,i4));
			found=true;
		}
	}

	return found;
}

bool PolyEvaluator::IntersectTriList(const dVector &start, const dVector &end, vector<Point> &points)
{	
	dVector bary;
	bool found=false;
	unsigned int i=0;
	while(i<m_Prim->Size())
	{
		unsigned int i1=i++;
		unsigned int i2=i++;
		unsigned int i3=i++;
		
		if (IntersectLineTriangle(start,end,m_Prim->GetData<dVector>("p",i1),
											m_Prim->GetData<dVector>("p",i2),
											m_Prim->GetData<dVector>("p",i3),bary))
		{
			points.push_back(InterpolatePData(bary,i1,i2,i3));
			found=true;
		}
	}

	return found;
}

bool PolyEvaluator::IntersectTriFan(const dVector &start, const dVector &end, vector<Point> &points)
{
	return false;
}

bool PolyEvaluator::IntersectPolygon(const dVector &start, const dVector &end, vector<Point> &points)
{
	return false;
}

////////////////////////////////////////////////

// todo - extend to arbitrary numbers of elements, and put in Evaluator
Evaluator::Point PolyEvaluator::InterpolatePData(dVector bary, unsigned int i1, unsigned int i2, unsigned int i3)
{
	Evaluator::Point point;
	vector<string> names;
	m_Prim->GetDataNames(names);

	for (vector<string>::iterator i=names.begin(); i!=names.end(); ++i)
	{
		char type=0;
		unsigned int size=0;
		m_Prim->GetDataInfo(*i, type, size);
		
		Blend *blend;
		
		switch(type)
		{
			case 'f': blend = new TypedBlend<float>('f',m_Prim->GetData<float>(*i,i1)*bary.x +
												    m_Prim->GetData<float>(*i,i2)*bary.y +
											   	    m_Prim->GetData<float>(*i,i3)*bary.z); break;
			case 'v': blend = new TypedBlend<dVector>('v',m_Prim->GetData<dVector>(*i,i1)*bary.x +
												  m_Prim->GetData<dVector>(*i,i2)*bary.y +
												  m_Prim->GetData<dVector>(*i,i3)*bary.z); break;
			case 'c': blend = new TypedBlend<dColour>('c',m_Prim->GetData<dColour>(*i,i1)*bary.x +
												  m_Prim->GetData<dColour>(*i,i2)*bary.y +
												  m_Prim->GetData<dColour>(*i,i3)*bary.z); break;
			case 'm': blend = new TypedBlend<dMatrix>('m',m_Prim->GetData<dMatrix>(*i,i1)*bary.x +
												  m_Prim->GetData<dMatrix>(*i,i2)*bary.y +
												  m_Prim->GetData<dMatrix>(*i,i3)*bary.z); break;
			default: 
				cerr<<"unknown pdata type in PolyEvaluator::InterpolatePData: "<<type<<endl; 
				assert(0); 
				break;
		};
		
		blend->m_Name=*i;
		
		point.m_Blends.push_back(blend);
	}
	
	return point;
} 

