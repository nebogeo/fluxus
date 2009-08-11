// Copyright (C) 2009 Gabor Papp
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

#ifndef N_FFGLLOADER
#define N_FFGLLOADER

#include <string>
#include <map>
#include <vector>
#include <deque>

#include "PixelPrimitive.h"
#include "FFGL.h"

using namespace std;

namespace Fluxus
{

class FFGLParameter
{
public:
	FFGLParameter() : id(-1), type('\0') {}
	FFGLParameter(int id, char &type, ParameterDefaultValue &defvalue)
		{ this->id = id; this->type = type; this->defvalue = defvalue; }

	int id;
	char type; // argcheck type 'f' or 's'
	ParameterDefaultValue defvalue;
};

class FFGLPluginInstance;

class FFGLPlugin
{
public:
	enum FFGLType { FFGL_SOURCE, FFGL_EFFECT };

	class Error { };

	FFGLPlugin(const string &filename);
	~FFGLPlugin();

	unsigned Instantiate(int width, int height);
	void Deinstantiate(unsigned instance);

	void Render(PixelPrimitive *output, unsigned instance, ProcessOpenGLStruct *pogl);

	float PluginVersion;
	char PluginID[5];
	char PluginName[17];
	enum FFGLType PluginType;
	string PluginDescription;
	string PluginAbout;

	map<string, FFGLParameter>& GetParameters()
	{
		return m_Parameters;
	}

	int GetDefaultValue(string &name, float *fvalue, const char **svalue);
	int GetParameter(FFGLPluginInstance *pi, string &name, float *fvalue, const char **svalue);
	int SetParameter(FFGLPluginInstance *pi, string &name, string &value);
	int SetParameter(FFGLPluginInstance *pi, string &name, float value);

private:

	void ReadParameters();

	void *m_PluginHandle;
    plugMainType *m_PlugMain;

	/* connects parameter name to id and count */
	map<string, FFGLParameter> m_Parameters;
};

class FFGLPluginInstance
{
public:
	FFGLPluginInstance() : plugin(NULL), instance(0), pogl(NULL) {}
	FFGLPluginInstance(FFGLPlugin *plugin, unsigned instance) : pogl(NULL)
			{ this->plugin = plugin; this->instance = instance; }
	~FFGLPluginInstance();

	void Free();
	void SetPixels(vector<PixelPrimitive *> &pixels);
	void Render();

	FFGLPlugin *plugin;
	unsigned instance;

private:
	ProcessOpenGLStruct *pogl;
	PixelPrimitive *output;
};

class FFGLManager
{
public:
	FFGLManager() { current_id = 0; };
	~FFGLManager();

	static FFGLManager *Get()
	{
		if (m_Singleton == NULL)
			m_Singleton = new FFGLManager();
		return m_Singleton;
	}

	static void Shutdown()
	{
		if (m_Singleton != NULL)
		{
			delete m_Singleton;
			m_Singleton = NULL;
		}
	}

	void ClearInstances();

	void Render();

	unsigned Load(const string &filename, int width, int height);

	bool Empty();
	void Push(unsigned id);
	void Pop();
	FFGLPluginInstance* Current();

private:
	static FFGLManager *m_Singleton;

	/* connects plugin filename to plugin object */
	map<string, FFGLPlugin *> m_LoadedPlugins;
	/* fluxus plugin id to plugin object and FFGL instance id */
	map<unsigned, FFGLPluginInstance *> m_PluginInstances;
	/* fluxus plugin id stack */
	deque<unsigned> m_PluginStack;

	/* current fluxus plugin instance id starting from 1 */
	static unsigned current_id;
};

};

#endif

