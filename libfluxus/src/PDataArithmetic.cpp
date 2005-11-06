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

using namespace fluxus;

template <>
PData *AddOperator::Operate(TypedPData<float> *a, float b)
{
	for (vector<float>::iterator i=a->m_Data.begin(); i!=a->m_Data.end(); i++)
	{
		*i+=b;
	}
	
	return NULL;
}

template <>
PData *AddOperator::Operate(TypedPData<dVector> *a, float b)
{
	for (vector<dVector>::iterator i=a->m_Data.begin(); i!=a->m_Data.end(); i++)
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
	for (vector<dVector>::iterator i=a->m_Data.begin(); i!=a->m_Data.end(); i++)
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

////////////////////////////////////////////////////////////////////////////

template <>
PData *MultOperator::Operate(TypedPData<float> *a, float b)
{
	for (vector<float>::iterator i=a->m_Data.begin(); i!=a->m_Data.end(); i++)
	{
		(*i)*=b;
	}
	return NULL;
}

template <>
PData *MultOperator::Operate(TypedPData<dVector> *a, float b)
{
	for (vector<dVector>::iterator i=a->m_Data.begin(); i!=a->m_Data.end(); i++)
	{
		(*i)*=b;
	}
	return NULL;
}

template <>
PData *MultOperator::Operate(TypedPData<dVector> *a, dVector b)
{
	for (vector<dVector>::iterator i=a->m_Data.begin(); i!=a->m_Data.end(); i++)
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

