// Copyright (C) 2007 Dave Griffiths
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

#include <assert.h>
#include "SchemeHelper.h"
#include "Engine.h"
#include "PDataFunctions.h"
#include "Renderer.h"

using namespace PDataFunctions;
using namespace SchemeHelper;
using namespace fluxus;

Scheme_Object *pdata_get(int argc, Scheme_Object **argv)
{
	ArgCheck("pdata-get", "si", argc, argv);		
	
	Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();    
	if (Grabbed) 
	{
		char *name=StringFromScheme(argv[0]);
		unsigned int index=IntFromScheme(argv[1]);
		unsigned int size;
		char type;
		Scheme_Object *ret=FloatsToScheme(dVector().arr(),3);
		
		if (Grabbed->GetDataInfo(name,type,size))
		{
			if (index<size)
			{
				if (type=='v')	
				{
					ret=FloatsToScheme(Grabbed->GetData<dVector>(name,index).arr(),3); 
				}
				else if (type=='c')	
				{
					ret=FloatsToScheme(Grabbed->GetData<dColour>(name,index).arr(),4); 
				}
				else if (type=='m')	
				{
					ret=FloatsToScheme(Grabbed->GetData<dMatrix>(name,index).arr(),16); 
				}
			}
		}
		return ret;
	}
    return scheme_void;
}

Scheme_Object *pdata_set(int argc, Scheme_Object **argv)
{
	ArgCheck("pdata-set", "si?", argc, argv);		
    Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();    
	if (Grabbed) 
	{
		size_t ssize=0;
		char *name=StringFromScheme(argv[0]);
		unsigned int index=IntFromScheme(argv[1]);
		unsigned int size;
		char type;
		
		if (Grabbed->GetDataInfo(name,type,size))
		{
			if (index<size)
			{
				if (type=='f')	
				{
					if (SCHEME_NUMBERP(argv[2])) Grabbed->SetData<float>(name,index,FloatFromScheme(argv[2]));
					else cerr<<"expected number value in pdata-set"<<endl;
				}
				else if (type=='v')	
				{
					if (SCHEME_VECTORP(argv[2]) && SCHEME_VEC_SIZE(argv[2])==3) 
					{
						dVector v;
						FloatsFromScheme(argv[2],v.arr(),3);
						Grabbed->SetData<dVector>(name,index,v);
					}
					else cerr<<"expected vector (size 3) value in pdata-set"<<endl;
				}
				else if (type=='c')	
				{
					if (SCHEME_VECTORP(argv[2]) && SCHEME_VEC_SIZE(argv[2])>=3 && SCHEME_VEC_SIZE(argv[2])<=4)
					{
						dColour c;
						if (SCHEME_VEC_SIZE(argv[2])==3) FloatsFromScheme(argv[2],c.arr(),3);
						else FloatsFromScheme(argv[2],c.arr(),4);
						Grabbed->SetData<dColour>(name,index,c);
					}
					else cerr<<"expected colour vector (size 3 or 4) value in pdata-set"<<endl;
				}
				else if (type=='m')	
				{
					if (SCHEME_VECTORP(argv[2]) && SCHEME_VEC_SIZE(argv[2])==16)
					{
						dMatrix m;
						FloatsFromScheme(argv[2],m.arr(),16);
						Grabbed->SetData<dMatrix>(name,index,m);
					}
					else cerr<<"expected matrix vector (size 16) value in pdata-set"<<endl;
				}
			}
		}
	}
    return scheme_void;
}

Scheme_Object *pdata_add(int argc, Scheme_Object **argv)
{
	ArgCheck("pdata-add", "ss", argc, argv);			
    Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();    
	if (Grabbed) 
	{
		char *names=StringFromScheme(argv[0]);
		char *types=StringFromScheme(argv[1]);
		char type=0;
		unsigned int size=0;
		
		PData *ptr=NULL;
		Grabbed->GetDataInfo("p", type, size);
		
		switch (types[0])
		{
			case 'v': ptr = new TypedPData<dVector>; ((TypedPData<dVector>*)ptr)->m_Data.resize(size); break;
			case 'c': ptr = new TypedPData<dColour>; ((TypedPData<dColour>*)ptr)->m_Data.resize(size); break;
			case 'f': ptr = new TypedPData<float>; ((TypedPData<float>*)ptr)->m_Data.resize(size); break;
			case 'm': ptr = new TypedPData<dMatrix>; ((TypedPData<dMatrix>*)ptr)->m_Data.resize(size); break;
			default : cerr<<"pdata-new: unknown type "<<types[0]<<endl; break;
		}
		
		if (ptr)
		{
			Grabbed->AddData(names,ptr);
		}
	}
	
	return scheme_void;
}

Scheme_Object *pdata_op(int argc, Scheme_Object **argv)
{
	ArgCheck("pdata-op", "ss?", argc, argv);			
    PData *ret=NULL;
	
    Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();    
	if (Grabbed) 
	{
		char *op=StringFromScheme(argv[0]);
		char *pd=StringFromScheme(argv[1]);
		
		// find out what the inputs are, and call the corresponding function
		if (SCHEME_CHAR_STRINGP(argv[2]))
		{
			char *operand=StringFromScheme(argv[2]);
			
			PData* pd2 = Grabbed->GetDataRaw(operand);
			
			TypedPData<dVector> *data = dynamic_cast<TypedPData<dVector>*>(pd2);	
			if (data) ret = Grabbed->DataOp(op, pd, data);
			else
			{
				TypedPData<dColour> *data = dynamic_cast<TypedPData<dColour>*>(pd2);
				if (data) ret = Grabbed->DataOp(op, pd, data);
				else 
				{
					TypedPData<float> *data = dynamic_cast<TypedPData<float>*>(pd2);
					if (data) ret = Grabbed->DataOp(op, pd, data);
					else 
					{
						TypedPData<dMatrix> *data = dynamic_cast<TypedPData<dMatrix>*>(pd2);
						if (data) ret = Grabbed->DataOp(op, pd, data);
					}
				}
			}
		}
		else if (SCHEME_NUMBERP(argv[2]))
		{
			ret = Grabbed->DataOp(op, pd, (float)FloatFromScheme(argv[2]));
		}
		else if (SCHEME_VECTORP(argv[2]))
		{
			switch (SCHEME_VEC_SIZE(argv[2]))
			{
				case 3:
				{
					dVector v;
					FloatsFromScheme(argv[2],v.arr(),3);
					ret = Grabbed->DataOp(op, pd, v);
				}
				break;
				case 4:
				{
					dColour v;
					FloatsFromScheme(argv[2],v.arr(),4);
					ret = Grabbed->DataOp(op, pd, v);
				}
				break;
				case 16:
				{
					dMatrix v;
					FloatsFromScheme(argv[2],v.arr(),16);
					ret = Grabbed->DataOp(op, pd, v);
				}
				break;	
			}
		}
	}
		
	// convert the return data
	if (ret!=NULL)
	{
		TypedPData<dVector> *data = dynamic_cast<TypedPData<dVector>*>(ret);	
		if (data) 
		{
			dVector r = data->m_Data[0];
			delete ret;
			return FloatsToScheme(r.arr(),3);
		}
		else
		{
			TypedPData<dColour> *data = dynamic_cast<TypedPData<dColour>*>(ret);
			if (data) 
			{
				dColour r = data->m_Data[0];
				delete ret;
				return FloatsToScheme(r.arr(),4);
			}
			else 
			{
				TypedPData<float> *data = dynamic_cast<TypedPData<float>*>(ret);
				if (data) 
				{		
					float r = data->m_Data[0];
					delete ret;
					return scheme_make_double(r);
				}
				else 
				{
					TypedPData<dMatrix> *data = dynamic_cast<TypedPData<dMatrix>*>(ret);
					if (data) 
					{
						dMatrix r = data->m_Data[0];
						delete ret;
						return FloatsToScheme(r.arr(),16);
					}
				}
			}
		}
	}
	
	return scheme_void;
}

Scheme_Object *pdata_copy(int argc, Scheme_Object **argv)
{
 	ArgCheck("pdata-copy", "ss", argc, argv);			
  	
	Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();    
	if (Grabbed) 
	{
		char *source=StringFromScheme(argv[0]);
		char *dest=StringFromScheme(argv[1]);
		Grabbed->CopyData(source,dest);
	}
	
	return scheme_void;
}

Scheme_Object *pdata_size(int argc, Scheme_Object **argv)
{
    Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();    
	if (Grabbed) 
	{
		return scheme_make_integer_value(Grabbed->Size());
	}
    return scheme_void;
}

Scheme_Object *finalise(int argc, Scheme_Object **argv)
{
	return scheme_void;
}

Scheme_Object *recalc_normals(int argc, Scheme_Object **argv)
{
 	ArgCheck("recalc-normals", "i", argc, argv);			
	Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();    
	if (Grabbed) Grabbed->RecalculateNormals(IntFromScheme(argv[0]));
	return scheme_void;
}

void PDataFunctions::AddGlobals(Scheme_Env *env)
{	
	scheme_add_global("pdata-get", scheme_make_prim_w_arity(pdata_get, "pdata-get", 2, 2), env);
	scheme_add_global("pdata-set", scheme_make_prim_w_arity(pdata_set, "pdata-set", 3, 3), env);
	scheme_add_global("pdata-add", scheme_make_prim_w_arity(pdata_add, "pdata-add", 2, 2), env);
	scheme_add_global("pdata-op", scheme_make_prim_w_arity(pdata_op, "pdata-op", 3, 3), env);
	scheme_add_global("pdata-copy", scheme_make_prim_w_arity(pdata_copy, "pdata-copy", 2, 2), env);
	scheme_add_global("pdata-size", scheme_make_prim_w_arity(pdata_size, "pdata-size", 0, 0), env);
	scheme_add_global("finalise", scheme_make_prim_w_arity(finalise, "finalise", 0, 0), env);
	scheme_add_global("recalc-normals", scheme_make_prim_w_arity(recalc_normals, "recalc-normals", 1, 1), env);
}
