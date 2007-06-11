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

#include <string>
#include <map>
#include "PData.h"
#include "Primitive.h"
#include "SceneGraph.h"

#ifndef N_PRIMITIVE_FUNCTION
#define N_PRIMITIVE_FUNCTION

using namespace std;

namespace Fluxus
{

/// A general purpose function for working on primitives
class PrimitiveFunction 
{
public:
	PrimitiveFunction();
	virtual ~PrimitiveFunction();

	/// Base class argument type
	class Arg
	{
	public:
		Arg() {}
		virtual ~Arg() {}
	};

	/// Useful argument type
	template<class T>
	class TypedArg : public Arg
	{
	public:
		TypedArg(T data) : m_Data(data) {}
		T m_Data;
	};

	template<class T>
	void SetArg(const string &name, const T &arg);
	
	void ClearArgs();
	
	/////////////////////////////////////////////////////
	///@name Primitive function abstract interface
	///@{
	
	/// Do the work...
	virtual void Run(Primitive &prim, const SceneGraph &world)=0;
	
	/// Get the result
	template<class T>
	TypedArg<T> GetResult()=0;
	///@}
	
protected:

	/////////////////////////////////////////////////////
	///@name Argument access for implementations
	///@{
	
	/// Get the value of an argument
	template<class T>
	const T &GetArg(const string &name, const T &def);

	/// See if an argument exists
	template<class T>
	bool ArgExists(const string &name);
	///@}

private:
	map<string, Arg *> m_Args;
};

template<class T>
const T &PrimitiveFunction::GetArg(const string &name, const T &def)
{
	// look for the argument
	map<string,Arg*>::iterator i=m_Args.find(name);
	if (i==m_Args.end())
	{
		return def;
	}
	
	// check it's the right type
	TypedArg<T> *ret = dynamic_cast<TypedArg<T>*>(i->second);
	if (ret)
	{
		return ret->m_Data;
	}
	
	return def;
}

template<class T>
bool PrimitiveFunction::ArgExists(const string &name)
{
	// look for the argument
	map<string,Arg*>::iterator i=m_Args.find(name);
	if (i==m_Args.end())
	{
		return false;
	}
	
	// check it's the right type
	TypedArg<T> *ret = dynamic_cast<TypedArg<T>*>(i->second);
	if (ret)
	{
		return true;
	}
	
	return false;
}

template<class T>
void PrimitiveFunction::SetArg(const string &name, const T &arg)
{
	m_Args[name]=new TypedArg<T>(arg);
}


}

#endif
