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

#include <stdio.h>
#include <iostream>
#include <sys/stat.h>
#include <sys/types.h>
#include "SearchPaths.h"

using namespace Fluxus;

SearchPaths *SearchPaths::m_Singleton = NULL;

SearchPaths* SearchPaths::Get()
{
	if (!m_Singleton)
	{
		m_Singleton = new SearchPaths;
	}
	return m_Singleton;
}

void SearchPaths::Shutdown()
{
	if (m_Singleton)
	{
		delete m_Singleton;
	}
}
	
string SearchPaths::GetFullPath(const string &Filename)
{
	if (Filename!="")
	{
		for (vector<string>::iterator i=m_Paths.begin(); i!=m_Paths.end(); i++)
		{
			string path = *i+Filename;
			struct stat sb;
			if (!stat(path.c_str(), &sb))
			{
				return path;
			}
		}
	}
	return Filename;
}

void SearchPaths::AddPath(const string &Path)
{
	m_Paths.push_back(Path);
}
