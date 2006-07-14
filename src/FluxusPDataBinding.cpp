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

#include <fstream>
#include <deque>
#include <libguile.h>
#include "FluxusPDataBinding.h"
#include "SearchPaths.h"
#include "Repl.h"
#include <LinePrimitive.h>
#include <ParticlePrimitive.h>
#include <PixelPrimitive.h>
#include <LocatorPrimitive.h>

void FluxusPDataBinding::RegisterProcs()
{
	SchemePrim::Init();
	
	// advanced prim editing
	scm_c_define_gsubr("pdata-set",3,0,0,(CALLBACK_CAST) pdata_set);
	scm_c_define_gsubr("pdata-get",2,0,0,(CALLBACK_CAST) pdata_get);
	scm_c_define_gsubr("pdata-size",0,0,0,(CALLBACK_CAST) pdata_size);
	scm_c_define_gsubr("pdata-add",2,0,0,(CALLBACK_CAST) pdata_add);
	scm_c_define_gsubr("pdata-copy",2,0,0,(CALLBACK_CAST) pdata_copy);
	scm_c_define_gsubr("pdata-op",3,0,0,(CALLBACK_CAST) pdata_op);   
	scm_c_define_gsubr("finalise",0,0,0,(CALLBACK_CAST) finalise);
	scm_c_define_gsubr("recalc-normals",1,0,0,(CALLBACK_CAST) recalc_normals);

}


SCM FluxusPDataBinding::pdata_get(SCM s_t, SCM s_i)
{
	SCM_ASSERT(scm_is_string(s_t), s_t, SCM_ARG1, "pdata-get");
    SCM_ASSERT(scm_is_number(s_i), s_i, SCM_ARG2, "pdata-get");
	
	Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();    
	if (Grabbed) 
	{
		char *name=scm_to_locale_string(s_t);
		unsigned int index=scm_to_int(s_i);
		unsigned int size;
		char type;
		SCM ret=flx_floats_to_scm(dVector().arr(),3);
		
		if (Grabbed->GetDataInfo(name,type,size))
		{
			if (index<size)
			{
				if (type=='v')	
				{
					ret=flx_floats_to_scm(Grabbed->GetData<dVector>(name,index).arr(),3); 
				}
				else if (type=='c')	
				{
					ret=flx_floats_to_scm(Grabbed->GetData<dColour>(name,index).arr(),4); 
				}
				else if (type=='m')	
				{
					ret=flx_floats_to_scm(Grabbed->GetData<dMatrix>(name,index).arr(),16); 
				}
			}
		}
		free(name); 
		return ret;
	}
    return SCM_UNSPECIFIED;
}

SCM FluxusPDataBinding::pdata_set(SCM s_t, SCM s_i, SCM s_v)
{
    SCM_ASSERT(scm_is_string(s_t), s_t, SCM_ARG1, "pdata-set");
    SCM_ASSERT(scm_is_number(s_i), s_i, SCM_ARG2, "pdata-set");
	//SCM_ASSERT(scm_is_generalized_vector(s_v), s_v, SCM_ARG3, "pdata-set");	
	//SCM_ASSERT(scm_c_generalized_vector_length(s_v)==3, s_v, SCM_ARG3, "pdata-set");
	
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();    
	if (Grabbed) 
	{
		size_t ssize=0;
		char *name=scm_to_locale_string(s_t);
		unsigned int index=scm_to_int(s_i);
		unsigned int size;
		char type;
		
		if (Grabbed->GetDataInfo(name,type,size))
		{
			if (index<size)
			{
				if (type=='f')	
				{
					Grabbed->SetData<float>(name,index,scm_to_double(s_v));
				}
				else if (type=='v')	
				{
					dVector v;
					flx_floats_from_scm(s_v,v.arr());
					Grabbed->SetData<dVector>(name,index,v);
				}
				else if (type=='c')	
				{
					dColour c;
					flx_floats_from_scm(s_v,c.arr());
					Grabbed->SetData<dColour>(name,index,c);
				}
				else if (type=='m')	
				{
					dMatrix m;
					flx_floats_from_scm(s_v,m.arr());
					Grabbed->SetData<dMatrix>(name,index,m);
				}
			}
		}
		free(name); 
	}
    return SCM_UNSPECIFIED;
}

SCM FluxusPDataBinding::pdata_add(SCM s_name, SCM s_type)
{
    SCM_ASSERT(scm_is_string(s_name), s_name, SCM_ARG1, "pdata-add");
    SCM_ASSERT(scm_is_string(s_type), s_type, SCM_ARG2, "pdata-add");
	
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();    
	if (Grabbed) 
	{
		char *names=scm_to_locale_string(s_name);
		char *types=scm_to_locale_string(s_type);
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
		
		free(names);
		free(types);
	}
	
	return SCM_UNSPECIFIED;
}

SCM FluxusPDataBinding::pdata_op(SCM s_op, SCM s_pd, SCM s_oper)
{
    SCM_ASSERT(scm_is_string(s_op), s_op, SCM_ARG1, "pdata-op");
    SCM_ASSERT(scm_is_string(s_pd), s_pd, SCM_ARG2, "pdata-op");
	PData *ret=NULL;
	
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();    
	if (Grabbed) 
	{
		char *op=scm_to_locale_string(s_op);
		char *pd=scm_to_locale_string(s_pd);
		
		// find out what the inputs are, and call the corresponding function
		if (scm_is_string(s_oper))
		{
			char *operand=scm_to_locale_string(s_oper);
			
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
			free(operand);
		}
		else if (scm_is_number(s_oper))
		{
			ret = Grabbed->DataOp(op, pd, (float)scm_to_double(s_oper));
		}
		else if (scm_is_generalized_vector(s_oper))
		{
			switch (scm_c_generalized_vector_length(s_oper))
			{
				case 3:
				{
					dVector v;
					flx_floats_from_scm(s_oper,v.arr());
					ret = Grabbed->DataOp(op, pd, v);
				}
				break;
				case 4:
				{
					dColour v;
					flx_floats_from_scm(s_oper,v.arr());
					ret = Grabbed->DataOp(op, pd, v);
				}
				break;
				case 16:
				{
					dMatrix v;
					flx_floats_from_scm(s_oper,v.arr());
					ret = Grabbed->DataOp(op, pd, v);
				}
				break;	
			}
		}
		
		free(op);
		free(pd);
	}
		
	// convert the return data
	if (ret!=NULL)
	{
		TypedPData<dVector> *data = dynamic_cast<TypedPData<dVector>*>(ret);	
		if (data) 
		{
			dVector r = data->m_Data[0];
			delete ret;
			return flx_floats_to_scm(r.arr(),3);
		}
		else
		{
			TypedPData<dColour> *data = dynamic_cast<TypedPData<dColour>*>(ret);
			if (data) 
			{
				dColour r = data->m_Data[0];
				delete ret;
				return flx_floats_to_scm(r.arr(),4);
			}
			else 
			{
				TypedPData<float> *data = dynamic_cast<TypedPData<float>*>(ret);
				if (data) 
				{		
					float r = data->m_Data[0];
					delete ret;
					return scm_from_double(r);
				}
				else 
				{
					TypedPData<dMatrix> *data = dynamic_cast<TypedPData<dMatrix>*>(ret);
					if (data) 
					{
						dMatrix r = data->m_Data[0];
						delete ret;
						return flx_floats_to_scm(r.arr(),16);
					}
				}
			}
		}
	}
	
	return SCM_UNSPECIFIED;
}

SCM FluxusPDataBinding::pdata_copy(SCM s_s, SCM s_d)
{
    SCM_ASSERT(scm_is_string(s_s), s_s, SCM_ARG1, "pdata-copy");
    SCM_ASSERT(scm_is_string(s_d), s_d, SCM_ARG2, "pdata-copy");
	
	Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();    
	if (Grabbed) 
	{
		char *source=scm_to_locale_string(s_s);
		char *dest=scm_to_locale_string(s_d);
		Grabbed->CopyData(source,dest);
		free(source);
		free(dest);
	}
	
	return SCM_UNSPECIFIED;
}

SCM FluxusPDataBinding::pdata_size()
{
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();    
	if (Grabbed) 
	{
		return scm_from_int(Grabbed->Size());
	}
    return SCM_UNSPECIFIED;
}

SCM FluxusPDataBinding::finalise()
{
	return SCM_UNSPECIFIED;
}

SCM FluxusPDataBinding::recalc_normals(SCM s_b)
{
    SCM_ASSERT(scm_is_number(s_b), s_b, SCM_ARG1, "recalc-normals");
	Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();    
	if (Grabbed) Grabbed->RecalculateNormals(scm_to_double(s_b));
	return SCM_UNSPECIFIED;
}
