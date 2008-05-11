// Copyright (C) 2008 David Griffiths <dave@pawfal.org>
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

#include <stdio.h>
#include "SearchPaths.h"

SearchPaths *SearchPaths::m_Singleton = NULL;

SearchPaths* SearchPaths::Get()
{
	if (!m_Singleton)
	{
		m_Singleton = new SearchPaths;
	}
	return m_Singleton;
}

	
string SearchPaths::GetFullPath(const string &Filename)
{
	for (vector<string>::iterator i=m_Paths.begin(); i!=m_Paths.end(); i++)
	{
		string file = *i+Filename;
		// should I stat?
		FILE *fd=fopen(file.c_str(),"r");
		if (fd)
		{
			fclose(fd);
			return file;
		}
	}
	return Filename;
}

void SearchPaths::AddPath(const string &Path)
{
	m_Paths.push_back(Path);
}
