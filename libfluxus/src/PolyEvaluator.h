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

#ifndef N_POLYEVALUATOR
#define N_POLYEVALUATOR

#include <string>
#include <map>
#include <assert.h>
#include "Evaluator.h"

namespace Fluxus
{

class PolyPrimitive;

//////////////////////////////////////////////////
/// The base Evaluator class. 
/// Abstract interface for primitive evaluators
class PolyEvaluator : public Evaluator
{
public:
	PolyEvaluator(const PolyPrimitive *prim);
	virtual ~PolyEvaluator();
	
	virtual bool IntersectLine(const dVector &start, const dVector &end, vector<Point> &points);
	virtual Point ClosestPoint(const dVector &position);
	
private:
	const PolyPrimitive *m_Prim;

	bool IntersectTriStrip(const dVector &start, const dVector &end, vector<Point> &points);
	bool IntersectQuads(const dVector &start, const dVector &end, vector<Point> &points);
	bool IntersectTriList(const dVector &start, const dVector &end, vector<Point> &points);
	bool IntersectTriFan(const dVector &start, const dVector &end, vector<Point> &points);
	bool IntersectPolygon(const dVector &start, const dVector &end, vector<Point> &points);

  Point InterpolatePData(float t, dVector bary, unsigned int i1, unsigned int i2, unsigned int i3);

};

}

#endif
