#include <stdio.h>
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
	for (vector<string>::iterator i=m_Paths.begin(); i!=m_Paths.end(); i++)
	{
		string path = *i+Filename;
		struct stat sb;
		if (!stat(path.c_str(), &sb))
		{
			return path;
		}
	}
	return Filename;
}

void SearchPaths::AddPath(const string &Path)
{
	m_Paths.push_back(Path);
}
