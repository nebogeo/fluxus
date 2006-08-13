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

#include "SchemePrim.h"

using namespace fluxus;

scm_t_bits SchemePrim::Tag;

SchemePrim::SchemePrim(int ID, Renderer *owner) : 
m_ID(ID),
m_Owner(owner)
{  
}

SchemePrim::~SchemePrim() 
{ 
	// need to refcount this
	//m_Owner->RemovePrimitive(m_ID);
}

void SchemePrim::Init() 
{  
	Tag = scm_make_smob_type ("primitive", sizeof(SchemePrim));
	scm_set_smob_mark(Tag,Mark);
	scm_set_smob_print(Tag,Print);
	scm_set_smob_free(Tag,Free);
	scm_set_smob_equalp(Tag,Equal);

	//scm_c_define_gsubr ("clear-image", 1, 0, 0, clear_image);
	//scm_c_define_gsubr ("make-image", 3, 0, 0, make_image);
}

size_t SchemePrim::Free(SCM smob) 
{  
  	delete (SchemePrim *)SCM_SMOB_DATA(smob);
	return 0;
}

SCM SchemePrim::Mark(SCM smob)
{
	return SCM_UNDEFINED;
}

int SchemePrim::Print (SCM smob, SCM port, scm_print_state *pstate)
{
  SchemePrim *pb = (SchemePrim *)SCM_SMOB_DATA(smob);

  scm_puts ("#<primitive ", port);
  char id[256];
  snprintf(id,256,"%d",pb->GetID());
  scm_puts (id, port);
  scm_puts (">", port);

  // non-zero means success 
  return 1;
}

void SchemePrim::Assert(SCM smob)
{
	scm_assert_smob_type(Tag,smob);  
}

SCM SchemePrim::Equal(SCM a, SCM b)
{
	SchemePrim *aa = (SchemePrim *)SCM_SMOB_DATA(a);
	SchemePrim *bb = (SchemePrim *)SCM_SMOB_DATA(b);
	if (aa->m_ID==bb->m_ID)
	{
		return SCM_BOOL_T;
	}
	return SCM_BOOL_F;
}

