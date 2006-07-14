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

#ifndef FLUXUS_IO_BINDING
#define FLUXUS_IO_BINDING

class FluxusIOBinding : public FluxusBinding
{
public:
	void RegisterProcs();
	
	static SCM edit(SCM s_name);
	static SCM save_name(SCM s_name);
	static SCM source(SCM s_name);
	static SCM start_framedump(SCM s_name, SCM s_type);
	static SCM end_framedump();
	static SCM load_recorded_code(SCM s_name);
	static SCM save_recorded_code(SCM s_name);
	static SCM searchpaths(SCM s_list);
	static SCM full_path(SCM s_filename);	
	static SCM mouse_over();
	static SCM mouse_button(SCM s_b);
	static SCM mouse_x();
	static SCM mouse_y();
	static SCM srandom();
	static SCM time();
	static SCM delta();
	static SCM key_pressed(SCM s_key);
	static SCM keys_down();
	static SCM key_special_pressed(SCM s_key);
	static SCM keys_special_down();
	static SCM desiredfps(SCM s);

};
#endif
