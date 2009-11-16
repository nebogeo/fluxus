#ifndef OFUTILS_H
#define OFUTILS_H

#include <cstdio>
#include <cstdarg>
#include <string>

enum ofLogLevel {
	OF_LOG_VERBOSE,
	OF_LOG_NOTICE,
	OF_LOG_WARNING,
	OF_LOG_ERROR,
	OF_LOG_FATAL_ERROR,
	OF_LOG_SILENT
};

inline void ofLog(int logLevel, std::string message)
{
	fprintf(stderr, "%s\n", message.c_str());
}

inline void ofLog(int logLevel, const char* format, ...)
{
	va_list args;
	va_start(args, format);
	vfprintf(stderr, format, args);
	fprintf(stderr, "\n");
	va_end(args);
}

#endif

