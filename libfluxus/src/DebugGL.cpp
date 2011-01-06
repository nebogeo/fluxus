#include <string>

#include "OpenGL.h"
#include "Trace.h"
#include "DebugGL.h"

void Fluxus::CheckGLErrors(const std::string &call)
{
	GLenum status = glGetError();
	if (status == GL_NO_ERROR)
		return;

	const char *status_msg = (const char *)gluErrorString(status);
	if (status_msg == NULL)
		status_msg = "unknown gl error";

	Trace::Stream << call << " - " << status_msg << " (" << status << ")" << std::endl;
}

