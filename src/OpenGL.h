
#ifdef WIN32
#define _STDCALL_SUPPORTED
#define _M_IX86
#endif

extern "C" {
#include "GL/glew.h"

#ifndef __APPLE__
#include "GL/glut.h"
#else
#include "GLUT/glut.h"
#endif
}
