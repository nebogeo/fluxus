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

/*
	TODO:
		- clear up states after processopengl
*/

#include <iostream>
#include <string.h>
#ifndef WIN32
#include <dlfcn.h>
#endif

#include "OpenGL.h"
#include "SearchPaths.h"
#include "Trace.h"
#include "FFGLManager.h"

using namespace Fluxus;

FFGLPlugin::FFGLPlugin(const string &filename) :
PluginDescription(""),
PluginAbout(""),
m_CapSetTime(false)
{
#ifndef WIN32 // can't use dlopen

#ifdef __APPLE__
	const string extension = ".dylib";
#else
#ifdef WIN32
	const string extension = ".dll";
#else // LINUX
	const string extension = ".so";
#endif
#endif

	string fullpath = SearchPaths::Get()->GetFullPath(filename);

	/*
	 * RTLD_LOCAL tells the loader to use local symbols within this module to
	 * resolve symbols, otherwise the symbols would be resolved globally which
	 * causes mixing of static variables if the plugins were developed using
	 * the same framework
	 */
	m_PluginHandle = dlopen(fullpath.c_str(), RTLD_NOW | RTLD_LOCAL);
	if (m_PluginHandle == NULL) /* try to load with extension added */
	{
		fullpath = SearchPaths::Get()->GetFullPath(filename + extension);
		m_PluginHandle = dlopen(fullpath.c_str(), RTLD_NOW | RTLD_LOCAL);
	}

#ifdef __APPLE__
	if (m_PluginHandle == NULL) /* try to load as bundle on OSX */
	{
		fullpath = SearchPaths::Get()->GetFullPath(filename + ".bundle/Contents/MacOS/" + filename);
		m_PluginHandle = dlopen(fullpath.c_str(), RTLD_NOW | RTLD_LOCAL);
	}
#endif

	if (m_PluginHandle == NULL)
	{
		Trace::Stream << "FFGL plugin " << filename << ": " << dlerror() << endl;
		throw Error();
	}

	m_PlugMain = (plugMainType *)(unsigned long)dlsym(m_PluginHandle, "plugMain");

	if (m_PlugMain == NULL)
	{
		Trace::Stream << "FFGL plugin " << filename << ": " << dlerror() << endl;
		throw Error();
	}

	plugMainUnion r = m_PlugMain(FF_GETINFO, 0, 0);
	if (r.ivalue == FF_FAIL)
	{
		Trace::Stream << "FFGL plugin " << filename << ": cannot get info" << endl;
		throw Error();
	}
	PlugInfoStruct *pis = r.PISvalue;

	if ((m_PlugMain(FF_GETPLUGINCAPS, (unsigned)FF_CAP_PROCESSOPENGL, 0)).ivalue
			!= FF_SUPPORTED)
	{
		Trace::Stream << "FFGL plugin " << filename << ": no FFGL support" << endl;
		throw Error();
	}

	float api_version = pis->APIMajorVersion + pis->APIMinorVersion/1000.0;
	if (api_version < 1.0) /* should be 1.5, but SDK plugins have wrong version info */
	{
		Trace::Stream << "FFGL plugin " << filename << ": old api version ("
			<< api_version << ")" << endl;
		throw Error();
	}

	strncpy(PluginID, (const char *)(pis->uniqueID), 4);
	PluginID[4] = 0;
	strncpy(PluginName, (const char *)(pis->pluginName), 16);
	PluginName[16] = 0;
	PluginType = pis->pluginType ? FFGL_SOURCE : FFGL_EFFECT;

	r = m_PlugMain(FF_GETEXTENDEDINFO, 0, 0);
	if (r.ivalue == FF_FAIL)
	{
		Trace::Stream << "FFGL plugin " << filename << ": cannot get extended info" << endl;
		throw Error();
	}

	PlugExtendedInfoStruct *peis = r.PEISvalue;
	PluginVersion = peis->PluginMajorVersion + peis->PluginMinorVersion/1000.0;
	if (peis->Description != NULL)
		PluginDescription = peis->Description;
	if (peis->About != NULL)
		PluginAbout = peis->About;

	ReadParameters();

	m_MinInputs = m_PlugMain(FF_GETPLUGINCAPS, (unsigned)FF_CAP_MINIMUMINPUTFRAMES, 0).ivalue;
	if (m_MinInputs == FF_UNSUPPORTED)
	{
		m_MinInputs = (PluginType == FFGL_SOURCE) ? 0 : 1;
	}

	m_MaxInputs = m_PlugMain(FF_GETPLUGINCAPS, (unsigned)FF_CAP_MAXIMUMINPUTFRAMES, 0).ivalue;
	if (m_MaxInputs == FF_UNSUPPORTED)
	{
		m_MaxInputs = (PluginType == FFGL_SOURCE) ? 0 : 1;
	}

	if (m_PlugMain(FF_GETPLUGINCAPS, (unsigned)FF_CAP_SETTIME, 0).ivalue == FF_SUPPORTED)
	{
		m_CapSetTime = true;
	}

	if ((m_PlugMain(FF_INITIALISE, 0, 0)).ivalue == FF_FAIL)
	{
		Trace::Stream << "FFGL plugin " << filename << ": plugin init failed" << endl;
		throw Error();
	}
#endif
}

FFGLPlugin::~FFGLPlugin()
{
#ifndef WIN32
	m_Parameters.clear();
	if (m_PlugMain(FF_DEINITIALISE, 0, 0).ivalue == FF_FAIL)
	{
		Trace::Stream << "FFGL plugin: deinitialise failed" << endl;
	}
	dlclose(m_PluginHandle);
#endif
}

void FFGLPlugin::ReadParameters()
{
	plugMainUnion r;
	unsigned numparameters = m_PlugMain(FF_GETNUMPARAMETERS, 0, 0).ivalue;
	if (numparameters == FF_FAIL)
	{
		Trace::Stream << "FFGL plugin: cannot get number of parameters" << endl;
		throw Error();
	}

	for (unsigned i = 0; i < numparameters; i++)
	{
		char name[17];
		name[16] = 0;
		r = m_PlugMain(FF_GETPARAMETERNAME, i, 0);
		if (r.ivalue == FF_FAIL)
		{
			Trace::Stream << "FFGL plugin: cannot get parameter name" << endl;
			throw Error();
		}
		strncpy(name, r.svalue, 16);
		int l = strlen(name);
		for (int j = 0; j < l; j++)
		{
			if (name[j] == ' ')
				name[j] = '-';
			else
			if ((name[j] >= 'A') && (name[j] <= 'Z'))
				name[j] += 'a' - 'A';
		}

	    r = m_PlugMain(FF_GETPARAMETERTYPE, i, 0);
		if (r.ivalue == FF_FAIL)
		{
			Trace::Stream << "FFGL plugin: cannot get parameter type" << endl;
			throw Error();
		}
		char type = (r.ivalue == FF_TYPE_TEXT) ? 's' : 'f';

		r = m_PlugMain(FF_GETPARAMETERDEFAULT, i, 0);
		if (r.ivalue == FF_FAIL)
		{
			Trace::Stream << "FFGL plugin: cannot get parameter default value" << endl;
			throw Error();
		}

		m_Parameters[name] = FFGLParameter(i, type, r.defvalue);
	}
}

unsigned FFGLPlugin::Instantiate(int width, int height)
{
	FFGLViewportStruct vps;
	vps.x = 0;
	vps.y = 0;
	vps.width = width;
	vps.height = height;

	unsigned instance = m_PlugMain(FF_INSTANTIATEGL, (unsigned long)(&vps), 0).ivalue;
	if (instance == FF_FAIL)
	{
		Trace::Stream << "FFGL plugin: instantiate failed" << endl;
	}

	return instance;
}

void FFGLPlugin::Deinstantiate(unsigned instance)
{
	if (m_PlugMain(FF_DEINSTANTIATEGL, 0, instance).ivalue == FF_FAIL)
	{
		Trace::Stream << "FFGL plugin: deinstantiate failed" << endl;
	}
}

int FFGLPlugin::GetDefaultValue(string &name, float *fvalue, const char **svalue)
{
	map<string, FFGLParameter>::iterator i = m_Parameters.find(name);
	if (i != m_Parameters.end())
	{
		FFGLParameter p = i->second;
		if (p.type == 's')
		{
			*svalue = p.defvalue.svalue;
		}
		else
		{
			*fvalue = p.defvalue.fvalue;
			*svalue = NULL;
		}
		return 1;
	}
	else
		return 0;
}

int FFGLPlugin::GetParameter(FFGLPluginInstance *pi, string &name,
		float *fvalue, const char **svalue)
{
	map<string, FFGLParameter>::iterator i = m_Parameters.find(name);
	if (i == m_Parameters.end())
	{
		return 0;
	}
	FFGLParameter p = i->second;

	plugMainUnion r;
	r = m_PlugMain(FF_GETPARAMETER, p.id, pi->instance);
	if (r.ivalue == FF_FAIL)
	{
		return 0;
	}

	if (p.type == 's')
	{
		*svalue = r.svalue;
	}
	else
	{
		*fvalue = r.fvalue;
		*svalue = NULL;
	}

	return 1;
}

int FFGLPlugin::SetParameter(FFGLPluginInstance *pi, string &name, string &value)
{
	map<string, FFGLParameter>::iterator i = m_Parameters.find(name);
	if (i == m_Parameters.end())
	{
		return 0;
	}
	FFGLParameter p = i->second;
	if (p.type != 's')
	{
		return 0;
	}

	SetParameterStruct sps;

	sps.index = p.id;
	sps.svalue = (char *)value.c_str();
	plugMainUnion r;
	r = m_PlugMain(FF_SETPARAMETER, (unsigned long)(&sps), pi->instance);
	if (r.ivalue == FF_FAIL)
	{
		return 0;
	}

	return 1;
}

int FFGLPlugin::SetParameter(FFGLPluginInstance *pi, string &name, float value)
{
	map<string, FFGLParameter>::iterator i = m_Parameters.find(name);
	if (i == m_Parameters.end())
	{
		return 0;
	}
	FFGLParameter p = i->second;
	if (p.type != 'f')
	{
		return 0;
	}

	SetParameterStruct sps;

	sps.index = p.id;
	sps.fvalue = value;
	plugMainUnion r;
	r = m_PlugMain(FF_SETPARAMETER, (unsigned long)(&sps), pi->instance);
	if (r.ivalue == FF_FAIL)
	{
		return 0;
	}

	return 1;
}

int FFGLPlugin::SetTime(FFGLPluginInstance *pi, double time)
{
	if (!m_CapSetTime)
		return 0;

	plugMainUnion r;
	r = m_PlugMain(FF_SETTIME, (unsigned long)(&time), pi->instance);
	if (r.ivalue == FF_FAIL)
	{
		return 0;
	}

	return 1;
}
void FFGLPlugin::Render(PixelPrimitive *output, unsigned output_txt, unsigned instance, ProcessOpenGLStruct *pogl)
{
	unsigned old_output_txt = output->GetRenderTexture();
	output->SetRenderTexture(output_txt);
	output->Bind();
	glViewport(0, 0, output->GetWidth(), output->GetHeight());
	if (m_PlugMain(FF_PROCESSOPENGL, (unsigned long)pogl,  instance).ivalue == FF_FAIL)
	{
		Trace::Stream << "FFGL plugin: ProcessOpenGL failed" << endl;
	}
	output->Unbind();
	output->SetRenderTexture(old_output_txt);
}

FFGLPluginInstance::~FFGLPluginInstance()
{
	plugin->Deinstantiate(instance);
	Free();
}

void FFGLPluginInstance::Free()
{
	if (pogl != NULL)
	{
		for (unsigned i = 0; i < pogl->numInputTextures; i++)
		{
			delete pogl->inputTextures[i];
		}
		delete [] pogl->inputTextures;
		delete pogl;
	}
}

void FFGLPluginInstance::SetPixels(PixelPrimitive *outputpp, vector<unsigned> &textures)
{
	Free();

	unsigned n = textures.size() - 1;
	unsigned mininputs = plugin->GetMinInputs();
	unsigned maxinputs = plugin->GetMaxInputs();

	if ((n < mininputs) || (n > maxinputs))
	{
		if (mininputs == maxinputs)
		{
			Trace::Stream << "FFGL plugin: expecting " << mininputs <<
				" input texture(s), received " << n << " one(s)" << endl;
		}
		else
		{
			Trace::Stream << "FFGL plugin: expecting between " << mininputs << " and " <<
				maxinputs << " input texture(s), received " << n << " one(s)" << endl;
		}
		return;
	}
	pogl = new ProcessOpenGLStruct;
	pogl->numInputTextures = n;
	pogl->inputTextures = new FFGLTextureStruct*[n];

	output = outputpp;
	output_txt = textures[0];
	pogl->HostFBO = output->GetFBO();

	unsigned width = output->GetWidth();
	unsigned height = output->GetHeight();
	unsigned hwwidth = output->GetFBOWidth();
	unsigned hwheight = output->GetFBOHeight();

	for (unsigned i = 0; i < n; i++)
	{
		FFGLTextureStruct *t = pogl->inputTextures[i] = new FFGLTextureStruct;
		t->Width = width;
		t->Height = height;
		t->HardwareWidth = hwwidth;
		t->HardwareHeight = hwheight;
		t->Handle = textures[i + 1];
	}
}

void FFGLPluginInstance::Render()
{
	if ((pogl != NULL) && m_Active)
	{
		plugin->Render(output, output_txt, instance, pogl);
	}
}

FFGLManager *FFGLManager::m_Singleton = NULL;
unsigned FFGLManager::current_id = 0;

FFGLManager::~FFGLManager()
{
	ClearInstances();
	map<string, FFGLPlugin *>::iterator ii = m_LoadedPlugins.begin();
	for (; ii != m_LoadedPlugins.end(); ++ii)
	{
		FFGLPlugin *p = ii->second;
		delete p;
	}
	m_LoadedPlugins.clear();
}

void FFGLManager::ClearInstances()
{
	m_PluginStack.clear();

	map<unsigned, FFGLPluginInstance *>::iterator i = m_PluginInstances.begin();
	for (; i != m_PluginInstances.end(); ++i)
	{
		FFGLPluginInstance *pi = i->second;
		delete pi;
	}
	m_PluginInstances.clear();
	current_id = 0;
}

unsigned FFGLManager::Load(const string &filename, int width, int height)
{
	FFGLPlugin *plugin;

	map<string, FFGLPlugin *>::iterator i = m_LoadedPlugins.find(filename);

	// check if we have already loaded this plugin
	if (i != m_LoadedPlugins.end())
	{
		plugin = i->second;
	}
	else
	{
		try
		{
			plugin = new FFGLPlugin(filename);
			m_LoadedPlugins[filename] = plugin;
		}
		catch(FFGLPlugin::Error)
		{
			return 0;
		}
	}

	unsigned instance = plugin->Instantiate(width, height);
	if (instance == FF_FAIL)
		return 0;

	current_id++;
	m_PluginInstances[current_id] = new FFGLPluginInstance(plugin, instance);

	return current_id;
}

void FFGLManager::Render()
{
	/* set default OpenGL state */
	glDisable(GL_LIGHTING);
	glDisable(GL_CULL_FACE);
	glDisable(GL_DEPTH_TEST);

	glMatrixMode(GL_PROJECTION);
	glPushMatrix();
	glLoadIdentity();

	glMatrixMode(GL_MODELVIEW);
	glPushMatrix();
	glLoadIdentity();

#ifndef DISABLE_MULTITEXTURE
	if (TexturePainter::Get()->MultitexturingEnabled())
	{
		glActiveTexture(GL_TEXTURE0);
	}
#endif

	glPushAttrib(GL_VIEWPORT_BIT);
	glColor4f(1, 1, 1, 1);

	map<unsigned, FFGLPluginInstance *>::iterator i = m_PluginInstances.begin();
	for (; i != m_PluginInstances.end(); ++i)
	{
		FFGLPluginInstance *pi = i->second;
		pi->Render();
	}

	glPopAttrib();

	/* set back fluxus OpenGL state */
	glMatrixMode(GL_MODELVIEW);
	glPopMatrix();

	glMatrixMode(GL_PROJECTION);
	glPopMatrix();

	glMatrixMode(GL_MODELVIEW);

	glEnable(GL_LIGHTING);
}

bool FFGLManager::Empty()
{
	return m_PluginStack.empty();
}

void FFGLManager::Push(unsigned id)
{
	m_PluginStack.push_front(id);
}

void FFGLManager::Pop()
{
	if (!m_PluginStack.empty())
		m_PluginStack.pop_front();
}

FFGLPluginInstance* FFGLManager::Current()
{
	if (m_PluginStack.empty())
	{
		return NULL;
	}
	else
	{
		map<unsigned, FFGLPluginInstance *>::iterator i =
			m_PluginInstances.find(m_PluginStack.front());

		if (i != m_PluginInstances.end())
			return i->second;
		else
			return NULL;
	}
}

