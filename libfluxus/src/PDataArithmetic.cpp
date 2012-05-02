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

#include "PDataArithmetic.h"

using namespace Fluxus;

template <>
PData *AddOperator::Operate(TypedPData<float> *a, float b)
{
	for (vector<float,FLX_ALLOC(float) >::iterator i=a->m_Data.begin(); i!=a->m_Data.end(); i++)
	{
		*i+=b;
	}
	
	return NULL;
}

template <>
PData *AddOperator::Operate(TypedPData<dVector> *a, float b)
{
	for (vector<dVector,FLX_ALLOC(dVector) >::iterator i=a->m_Data.begin(); i!=a->m_Data.end(); i++)
	{
		i->x+=b;
		i->y+=b;
		i->z+=b;
	}
	return NULL;
}

template <>
PData *AddOperator::Operate(TypedPData<dVector> *a, dVector b)
{
	for (vector<dVector,FLX_ALLOC(dVector) >::iterator i=a->m_Data.begin(); i!=a->m_Data.end(); i++)
	{
		i->x+=b.x;
		i->y+=b.y;
		i->z+=b.z;
	}
	return NULL;
}

template <>
PData *AddOperator::Operate(TypedPData<float> *a, TypedPData<float> *b)
{
	for (unsigned int i=0; i<a->Size(); i++) a->m_Data[i]+=b->m_Data[i];
	return NULL;
}

template <>
PData *AddOperator::Operate(TypedPData<dVector> *a, TypedPData<float> *b)
{
	for (unsigned int i=0; i<a->Size(); i++) 
	{
		a->m_Data[i].x+=b->m_Data[i];
		a->m_Data[i].y+=b->m_Data[i];
		a->m_Data[i].z+=b->m_Data[i];
	}
	return NULL;
}

template <>
PData *AddOperator::Operate(TypedPData<dVector> *a, TypedPData<dVector> *b)
{
	for (unsigned int i=0; i<a->Size(); i++) a->m_Data[i]+=b->m_Data[i];
	return NULL;
}

template <>
PData *AddOperator::Operate(TypedPData<dColour> *a, float b)
{
	for (vector<dColour,FLX_ALLOC(dColour) >::iterator i=a->m_Data.begin(); i!=a->m_Data.end(); i++)
	{
		i->r+=b;
		i->g+=b;
		i->b+=b;
	}
	return NULL;
}

template <>
PData *AddOperator::Operate(TypedPData<dColour> *c, TypedPData<float> *d)
{
	for (unsigned int i=0; i<c->Size(); i++)
	{
		c->m_Data[i].r+=d->m_Data[i];
		c->m_Data[i].g+=d->m_Data[i];
		c->m_Data[i].b+=d->m_Data[i];
	}
	return NULL;
}

template <>
PData *AddOperator::Operate(TypedPData<dColour> *c, dColour d)
{
	for (vector<dColour,FLX_ALLOC(dColour) >::iterator i=c->m_Data.begin(); i!=c->m_Data.end(); i++)
	{
		i->r+=d.r;
		i->g+=d.g;
		i->b+=d.b;
		i->a+=d.a;
	}
	return NULL;
}

template <>
PData *AddOperator::Operate(TypedPData<dColour> *c, TypedPData<dColour> *d)
{
	for (unsigned int i=0; i<c->Size(); i++) c->m_Data[i]+=d->m_Data[i];
	return NULL;
}
////////////////////////////////////////////////////////////////////////////

template <>
PData *MultOperator::Operate(TypedPData<float> *a, float b)
{
	for (vector<float,FLX_ALLOC(float) >::iterator i=a->m_Data.begin(); i!=a->m_Data.end(); i++)
	{
		(*i)*=b;
	}
	return NULL;
}

template <>
PData *MultOperator::Operate(TypedPData<dVector> *a, float b)
{
	for (vector<dVector,FLX_ALLOC(dVector) >::iterator i=a->m_Data.begin(); i!=a->m_Data.end(); i++)
	{
		(*i)*=b;
	}
	return NULL;
}

template <>
PData *MultOperator::Operate(TypedPData<dColour> *a, float b)
{
	for (vector<dColour,FLX_ALLOC(dColour) >::iterator i=a->m_Data.begin(); i!=a->m_Data.end(); i++)
	{
		(*i)*=b;
	}
	return NULL;
}

template <>
PData *MultOperator::Operate(TypedPData<dVector> *a, dVector b)
{
	for (vector<dVector,FLX_ALLOC(dVector) >::iterator i=a->m_Data.begin(); i!=a->m_Data.end(); i++)
	{
		(*i).x*=b.x;
		(*i).y*=b.y;
		(*i).z*=b.z;
	}
	return NULL;
}

template <>
PData *MultOperator::Operate(TypedPData<float> *a, TypedPData<float> *b)
{
	for (unsigned int i=0; i<a->Size(); i++) a->m_Data[i]*=b->m_Data[i];
	return NULL;
}

template <>
PData *MultOperator::Operate(TypedPData<dVector> *a, TypedPData<float> *b)
{
	for (unsigned int i=0; i<a->Size(); i++) 
	{
		a->m_Data[i]*=b->m_Data[i];
	}
	return NULL;
}

template <>
PData *MultOperator::Operate(TypedPData<dVector> *a, TypedPData<dVector> *b)
{
	for (unsigned int i=0; i<a->Size(); i++)
	{
		a->m_Data[i].x*=b->m_Data[i].x;
		a->m_Data[i].y*=b->m_Data[i].y;
		a->m_Data[i].z*=b->m_Data[i].z;
	}
	return NULL;
}

///////////////////////////////////////////////////////

template <>
PData *SineOperator::Operate(TypedPData<float> *a, TypedPData<float> *b)
{
	for (unsigned int i=0; i<a->Size(); i++)
	{
		a->m_Data[i]=sin(b->m_Data[i]);
	}
	return NULL;
}

///////////////////////////////////////////////////////

template <>
PData *CosineOperator::Operate(TypedPData<float> *a, TypedPData<float> *b)
{
	for (unsigned int i=0; i<a->Size(); i++)
	{	
		a->m_Data[i]=cos(b->m_Data[i]);	
	}
	return NULL;
}

///////////////////////////////////////////////////////

template <>
PData *ClosestOperator::Operate(TypedPData<dVector> *a, dVector b)
{
	float closestdist=999999;
	dVector closest;
	for (unsigned int i=0; i<a->Size(); i++)
	{	
		float dist = a->m_Data[i].dist(b);	
		if (dist<closestdist)
		{
			closestdist=dist;
			closest=a->m_Data[i];
		}
		
	}
	
	TypedPData<dVector> *ret = new TypedPData<dVector>;
	ret->m_Data.push_back(closest);
	return ret;
}

template <>
PData *ClosestOperator::Operate(TypedPData<dVector> *a, float b)
{
	// use the float as the index
	unsigned int index=(unsigned int)b;
	float closestdist=999999;
	dVector closest;
	for (unsigned int i=0; i<a->Size(); i++)
	{	
		if (i!=index)
		{
			float dist = a->m_Data[i].dist(a->m_Data[index]);	
			if (dist<closestdist)
			{
				closestdist=dist;
				closest=a->m_Data[i];
			}
		}
	}
	
	TypedPData<dVector> *ret = new TypedPData<dVector>;
	ret->m_Data.push_back(closest);
	return ret;
}

