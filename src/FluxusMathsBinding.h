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

#include "FluxusBinding.h"

#ifndef FLUXUS_MATHS_BINDING
#define FLUXUS_MATHS_BINDING

class FluxusMathsBinding : public FluxusBinding
{
public:
	void RegisterProcs();
	
	static SCM vmul(SCM s_a, SCM s_b);
	static SCM vadd(SCM s_a, SCM s_b);
	static SCM vsub(SCM s_a, SCM s_b);
	static SCM vdiv(SCM s_a, SCM s_b);
	static SCM vtransform(SCM s_v, SCM s_m);
	static SCM vtransform_rot(SCM s_v, SCM s_m);
	static SCM vnormalise(SCM s_v);
	static SCM vdot(SCM s_a, SCM s_b);
	static SCM vmag(SCM s_a);
	static SCM vdist(SCM s_a, SCM s_b);
	static SCM vcross(SCM s_a, SCM s_b);
	static SCM mmul(SCM s_a, SCM s_b);
	static SCM madd(SCM s_a, SCM s_b);
	static SCM msub(SCM s_a, SCM s_b);
	static SCM mdiv(SCM s_a, SCM s_b);
	static SCM mident();
	static SCM mtranslate(SCM s_v);
	static SCM mrotate(SCM s_v);
	static SCM mscale(SCM s_v);
	static SCM mtranspose(SCM s_a);
	static SCM minverse(SCM s_a);
	static SCM maim(SCM s_a, SCM s_b);
	static SCM qaxisangle(SCM s_axis, SCM s_angle);
	static SCM qmul(SCM s_q, SCM s_q);
	static SCM qnormalise(SCM s_q);
	static SCM qtomatrix(SCM s_q);

};
#endif
