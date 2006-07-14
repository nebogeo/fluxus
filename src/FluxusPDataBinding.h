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

#ifndef FLUXUS_PDATA_BINDING
#define FLUXUS_PDATA_BINDING

class FluxusPDataBinding : public FluxusBinding
{
public:
	void RegisterProcs();
	
	static SCM pdata_size();
	static SCM pdata_get(SCM s_t, SCM s_i);
	static SCM pdata_set(SCM s_t, SCM s_i, SCM s_v);
	static SCM pdata_add(SCM s_name, SCM s_type);
	static SCM pdata_copy(SCM s_s, SCM s_d);
	static SCM finalise();
	static SCM recalc_normals(SCM s_b);
	static SCM pdata_op(SCM s_op, SCM s_pd, SCM s_oper);

};
#endif
