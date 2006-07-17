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

#include <libguile.h>
#include "Renderer.h"

#ifndef FLUXUS_SCHEME_PRIM
#define FLUXUS_SCHEME_PRIM

class SchemePrim
{
public:
	SchemePrim(int ID, fluxus::Renderer *owner);
	~SchemePrim();

	int GetID() { return m_ID; }

	static void Init();
	static size_t Free(SCM smob); 
	static SCM Mark(SCM smob);
	static int Print(SCM smob, SCM port, scm_print_state *pstate);
	static void Assert(SCM smob);
	static SCM Equal(SCM a, SCM b);
	static scm_t_bits Tag;

private:
	int m_ID;
	fluxus::Renderer *m_Owner;
};

#endif
