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

#ifndef N_EVALUATOR
#define N_EVALUATOR

#include <string>
#include <map>
#include <assert.h>
#include "State.h"
#include "PDataContainer.h"

namespace Fluxus
{

//////////////////////////////////////////////////
/// The base Evaluator class. 
/// Abstract interface for primitive evaluators
class Evaluator
{
public:
	Evaluator();
	virtual ~Evaluator();
	
	class Blend 
	{
	public:
		Blend(char t): m_Type(t) {}
		string m_Name;
		char m_Type;
	};

	template<class T>
	class TypedBlend : public Blend
	{	
	public:		
		TypedBlend(char t, T b) : Blend(t), m_Blend(b) {}
		T m_Blend;
	};
	
	class Point
	{
	public:
		vector<Blend*> m_Blends;
	};
	
	virtual bool IntersectLine(const dVector &start, const dVector &end,  vector<Point> &points)=0;
	virtual Point ClosestPoint(const dVector &position)=0;
	
private:


};

}

#endif
