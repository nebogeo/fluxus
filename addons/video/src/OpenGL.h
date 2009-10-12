#ifndef __OPENGL_H__
#define __OPENGL_H__

#ifdef WIN32
#define _STDCALL_SUPPORTED
#define _M_IX86
#endif

extern "C" {

#ifndef __APPLE__
#include <GL/glut.h>
#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glext.h>
#else
#include <GLUT/glut.h>
#include <OpenGL/gl.h>
#include <OpenGL/glu.h>
#include <OpenGL/glext.h>
#endif
}

#endif

