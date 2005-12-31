#include<vector>
#include<string>

using namespace std;

#ifndef FLUXUS_SEARCHPATHS
#define FLUXUS_SEARCHPATHS

class SearchPaths
{
public:
	static SearchPaths* Get();
	
	string GetFullPath(const string &Filename);
	void AddPath(const string &Path);
	
private:
	SearchPaths() {}
	~SearchPaths() {}
	
	vector<string> m_Paths;
	
	static SearchPaths *m_Singleton;
};

#endif
