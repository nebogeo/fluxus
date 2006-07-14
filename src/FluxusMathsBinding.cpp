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
#include "FluxusMathsBinding.h"
#include "SearchPaths.h"
#include "Repl.h"
#include <LinePrimitive.h>
#include <ParticlePrimitive.h>
#include <PixelPrimitive.h>
#include <LocatorPrimitive.h>

void FluxusMathsBinding::RegisterProcs()
{
	SchemePrim::Init();
	
	scm_c_define_gsubr("vmul",2,0,0,(CALLBACK_CAST) vmul);
	scm_c_define_gsubr("vadd",2,0,0,(CALLBACK_CAST) vadd);
	scm_c_define_gsubr("vsub",2,0,0,(CALLBACK_CAST) vsub);
	scm_c_define_gsubr("vdiv",2,0,0,(CALLBACK_CAST) vdiv);
	scm_c_define_gsubr("vtransform",2,0,0,(CALLBACK_CAST) vtransform);   
	scm_c_define_gsubr("vtransform-rot",2,0,0,(CALLBACK_CAST) vtransform_rot);   
	scm_c_define_gsubr("vnormalise",1,0,0,(CALLBACK_CAST) vnormalise);   
	scm_c_define_gsubr("vdot",2,0,0,(CALLBACK_CAST) vdot);   
	scm_c_define_gsubr("vmag",1,0,0,(CALLBACK_CAST) vmag);   
	scm_c_define_gsubr("vdist",2,0,0,(CALLBACK_CAST) vdist); 
	scm_c_define_gsubr("vcross",2,0,0,(CALLBACK_CAST) vcross);   
	scm_c_define_gsubr("mmul",2,0,0,(CALLBACK_CAST) mmul);
	scm_c_define_gsubr("madd",2,0,0,(CALLBACK_CAST) madd);
	scm_c_define_gsubr("msub",2,0,0,(CALLBACK_CAST) msub);
	scm_c_define_gsubr("mdiv",2,0,0,(CALLBACK_CAST) mdiv);
	scm_c_define_gsubr("mident",0,0,0,(CALLBACK_CAST)  mident);
	scm_c_define_gsubr("mtranslate",1,0,0,(CALLBACK_CAST) mtranslate);   
	scm_c_define_gsubr("mrotate",1,0,0,(CALLBACK_CAST) mrotate); 
	scm_c_define_gsubr("mscale",1,0,0,(CALLBACK_CAST) mscale);
	scm_c_define_gsubr("mtranspose",1,0,0,(CALLBACK_CAST) mtranspose);
	scm_c_define_gsubr("minverse",1,0,0,(CALLBACK_CAST) minverse);
	scm_c_define_gsubr("maim",2,0,0,(CALLBACK_CAST) maim);
	scm_c_define_gsubr("qaxisangle",2,0,0,(CALLBACK_CAST)qaxisangle);
	scm_c_define_gsubr("qmul",2,0,0,(CALLBACK_CAST) qmul);
	scm_c_define_gsubr("qnormalise",1,0,0,(CALLBACK_CAST)qnormalise);
	scm_c_define_gsubr("qtomatrix",1,0,0,(CALLBACK_CAST)qtomatrix);

}


SCM FluxusMathsBinding::vmul(SCM s_a, SCM s_b)
{
	SCM_ASSERT(scm_is_generalized_vector(s_a),  s_a,  SCM_ARG1, "vmul");
	SCM_ASSERT(scm_is_number(s_b),  s_b,  SCM_ARG2, "vmul");	
	SCM_ASSERT(scm_c_generalized_vector_length(s_a)==3,  s_a,  SCM_ARG1, "vmul");
	dVector a;
	flx_floats_from_scm(s_a,a.arr());
	return flx_floats_to_scm((a*scm_to_double(s_b)).arr(),3);
}

SCM FluxusMathsBinding::vadd(SCM s_a, SCM s_b)
{
	SCM_ASSERT(scm_is_generalized_vector(s_a),  s_a,  SCM_ARG1, "vadd");
	SCM_ASSERT(scm_c_generalized_vector_length(s_a)==3,  s_a,  SCM_ARG1, "vadd");
	SCM_ASSERT(scm_is_generalized_vector(s_b),  s_b,  SCM_ARG2, "vadd");
	SCM_ASSERT(scm_c_generalized_vector_length(s_b)==3,  s_b,  SCM_ARG2, "vadd");
	dVector a;
	flx_floats_from_scm(s_a,a.arr());
	dVector b;
	flx_floats_from_scm(s_b,b.arr());
	return flx_floats_to_scm((a+b).arr(),3);
}


SCM FluxusMathsBinding::vsub(SCM s_a, SCM s_b)
{
	SCM_ASSERT(scm_is_generalized_vector(s_a),  s_a,  SCM_ARG1, "vsub");
	SCM_ASSERT(scm_is_generalized_vector(s_b),  s_b,  SCM_ARG2, "vsub");
	SCM_ASSERT(scm_c_generalized_vector_length(s_a)==3,  s_a,  SCM_ARG1, "vsub");
	SCM_ASSERT(scm_c_generalized_vector_length(s_b)==3,  s_b,  SCM_ARG2, "vsub");	
	dVector a;
	flx_floats_from_scm(s_a,a.arr());
	dVector b;
	flx_floats_from_scm(s_b,b.arr());
	return flx_floats_to_scm((a-b).arr(),3);
}

SCM FluxusMathsBinding::vdiv(SCM s_a, SCM s_b)
{
	SCM_ASSERT(scm_is_generalized_vector(s_a),  s_a,  SCM_ARG1, "vdiv");
	SCM_ASSERT(scm_is_number(s_b),  s_b,  SCM_ARG2, "vdiv");
	SCM_ASSERT(scm_c_generalized_vector_length(s_a)==3,  s_a,  SCM_ARG1, "vdiv");
	dVector a;
	flx_floats_from_scm(s_a,a.arr());
	return flx_floats_to_scm((a/scm_to_double(s_b)).arr(),3);
}

SCM FluxusMathsBinding::vtransform(SCM s_v, SCM s_m)
{
	SCM_ASSERT(scm_is_generalized_vector(s_v),  s_v,  SCM_ARG1, "vtransform");
	SCM_ASSERT(scm_c_generalized_vector_length(s_v)==3,  s_v,  SCM_ARG1, "vtransform");
	SCM_ASSERT(scm_is_generalized_vector(s_m),  s_m,  SCM_ARG2, "vtransform");	
	SCM_ASSERT(scm_c_generalized_vector_length(s_m)==16,  s_m,  SCM_ARG2, "vtransform");
	dVector v;
	flx_floats_from_scm(s_v,v.arr());
	dMatrix m;
	flx_floats_from_scm(s_m,m.arr());
	return flx_floats_to_scm((m.transform(v)).arr(),3);
}

SCM FluxusMathsBinding::vtransform_rot(SCM s_v, SCM s_m)
{
	SCM_ASSERT(scm_is_generalized_vector(s_v),  s_v,  SCM_ARG1, "vtransform-rot");
	SCM_ASSERT(scm_c_generalized_vector_length(s_v)==3,  s_v,  SCM_ARG1, "vtransform-rot");
	SCM_ASSERT(scm_is_generalized_vector(s_m),  s_m,  SCM_ARG2, "vtransform-rot");	
	SCM_ASSERT(scm_c_generalized_vector_length(s_m)==16,  s_m,  SCM_ARG2, "vtransform-rot");
	dVector v;
	flx_floats_from_scm(s_v,v.arr());
	dMatrix m;
	flx_floats_from_scm(s_m,m.arr());
	return flx_floats_to_scm((m.transform_no_trans(v)).arr(),3);
}

SCM FluxusMathsBinding::vnormalise(SCM s_v)
{
	SCM_ASSERT(scm_is_generalized_vector(s_v),  s_v,  SCM_ARG1, "vnormalise");
	SCM_ASSERT(scm_c_generalized_vector_length(s_v)==3,  s_v,  SCM_ARG1, "vnormalise");
	dVector v;
	flx_floats_from_scm(s_v,v.arr());
	v.normalise();
	return flx_floats_to_scm(v.arr(),3);
	return SCM_UNSPECIFIED;
}

SCM FluxusMathsBinding::vdot(SCM s_a, SCM s_b)
{
	SCM_ASSERT(scm_is_generalized_vector(s_a),  s_a,  SCM_ARG1, "vdot");
	SCM_ASSERT(scm_c_generalized_vector_length(s_a)==3,  s_a,  SCM_ARG1, "vdot");
	SCM_ASSERT(scm_is_generalized_vector(s_b),  s_b,  SCM_ARG2, "vdot");
	SCM_ASSERT(scm_c_generalized_vector_length(s_b)==3,  s_a,  SCM_ARG2, "vdot");
	dVector a;
	flx_floats_from_scm(s_a,a.arr());
	dVector b;
	flx_floats_from_scm(s_b,b.arr());
	return scm_from_double(a.dot(b));
}

SCM FluxusMathsBinding::vmag(SCM s_a)
{
	SCM_ASSERT(scm_is_generalized_vector(s_a),  s_a,  SCM_ARG1, "vmag");
	SCM_ASSERT(scm_c_generalized_vector_length(s_a)==3,  s_a,  SCM_ARG1, "vmag");
	dVector a;
	flx_floats_from_scm(s_a,a.arr());
	return scm_from_double(a.mag());
}

SCM FluxusMathsBinding::vdist(SCM s_a, SCM s_b)
{
	SCM_ASSERT(scm_is_generalized_vector(s_a),  s_a,  SCM_ARG1, "vdist");
	SCM_ASSERT(scm_c_generalized_vector_length(s_a)==3,  s_a,  SCM_ARG1, "vdist");
	SCM_ASSERT(scm_is_generalized_vector(s_b),  s_b,  SCM_ARG2, "vdist");
	SCM_ASSERT(scm_c_generalized_vector_length(s_b)==3,  s_a,  SCM_ARG2, "vdist");
	dVector a;
	flx_floats_from_scm(s_a,a.arr());
	dVector b;
	flx_floats_from_scm(s_b,b.arr());
	return scm_from_double(a.dist(b));
}

SCM FluxusMathsBinding::vcross(SCM s_a, SCM s_b)
{
	SCM_ASSERT(scm_is_generalized_vector(s_a),  s_a,  SCM_ARG1, "vcross");
	SCM_ASSERT(scm_c_generalized_vector_length(s_a)==3,  s_a,  SCM_ARG1, "vcross");
	SCM_ASSERT(scm_is_generalized_vector(s_b),  s_b,  SCM_ARG2, "vcross");
	SCM_ASSERT(scm_c_generalized_vector_length(s_b)==3,  s_b,  SCM_ARG2, "vcross");	
	dVector a;
	flx_floats_from_scm(s_a,a.arr());
	dVector b;
	flx_floats_from_scm(s_b,b.arr());
	return flx_floats_to_scm((a.cross(b)).arr(),3);
}

SCM FluxusMathsBinding::mmul(SCM s_a, SCM s_b)
{
	SCM_ASSERT(scm_is_generalized_vector(s_a),  s_a,  SCM_ARG1, "mmul");
	SCM_ASSERT(scm_is_generalized_vector(s_b),  s_b,  SCM_ARG2, "mmul");
	SCM_ASSERT(scm_c_generalized_vector_length(s_b)==16,  s_b,  SCM_ARG2, "mmul");
	SCM_ASSERT(scm_c_generalized_vector_length(s_a)==16,  s_a,  SCM_ARG1, "mmul");		
	dMatrix a;
	flx_floats_from_scm(s_a,a.arr());
	dMatrix b;
	flx_floats_from_scm(s_b,b.arr());
	return flx_floats_to_scm((a*b).arr(),16);
}

SCM FluxusMathsBinding::madd(SCM s_a, SCM s_b)
{
	SCM_ASSERT(scm_is_generalized_vector(s_a),  s_a,  SCM_ARG1, "madd");
	SCM_ASSERT(scm_is_generalized_vector(s_b),  s_b,  SCM_ARG2, "madd");	
	SCM_ASSERT(scm_c_generalized_vector_length(s_b)==16,  s_b,  SCM_ARG2, "madd");
	SCM_ASSERT(scm_c_generalized_vector_length(s_a)==16,  s_a,  SCM_ARG1, "madd");		
	dMatrix a;
	flx_floats_from_scm(s_a,a.arr());
	dMatrix b;
	flx_floats_from_scm(s_b,b.arr());
	return flx_floats_to_scm((a+b).arr(),16);
}

SCM FluxusMathsBinding::msub(SCM s_a, SCM s_b)
{
	SCM_ASSERT(scm_is_generalized_vector(s_a),  s_a,  SCM_ARG1, "msub");
	SCM_ASSERT(scm_is_generalized_vector(s_b),  s_b,  SCM_ARG2, "msub");	
	SCM_ASSERT(scm_c_generalized_vector_length(s_b)==16,  s_b,  SCM_ARG2, "msub");
	SCM_ASSERT(scm_c_generalized_vector_length(s_a)==16,  s_a,  SCM_ARG1, "msub");		
	dMatrix a;
	flx_floats_from_scm(s_a,a.arr());
	dMatrix b;
	flx_floats_from_scm(s_b,b.arr());
	return flx_floats_to_scm((a-b).arr(),16);
}

SCM FluxusMathsBinding::mdiv(SCM s_a, SCM s_b)
{
	SCM_ASSERT(scm_is_generalized_vector(s_a),  s_a,  SCM_ARG1, "mdiv");
	SCM_ASSERT(scm_is_generalized_vector(s_b),  s_b,  SCM_ARG2, "mdiv");	
	SCM_ASSERT(scm_c_generalized_vector_length(s_b)==16,  s_b,  SCM_ARG2, "mdiv");
	SCM_ASSERT(scm_c_generalized_vector_length(s_a)==16,  s_a,  SCM_ARG1, "mdiv");
	dMatrix a;
	flx_floats_from_scm(s_a,a.arr());
	dMatrix b;
	flx_floats_from_scm(s_b,b.arr());
	return flx_floats_to_scm((a/b).arr(),16);
}

SCM FluxusMathsBinding::mident()
{
	dMatrix m;
	return flx_floats_to_scm(m.arr(),16);	
}

SCM FluxusMathsBinding::mtranslate(SCM s_v)
{
	SCM_ASSERT(scm_is_generalized_vector(s_v),  s_v,  SCM_ARG1, "mtranslate");
	SCM_ASSERT(scm_c_generalized_vector_length(s_v)==3,  s_v,  SCM_ARG1, "mtranslate");
	dVector a;
	flx_floats_from_scm(s_v,a.arr());
	dMatrix m;
	m.translate(a.x,a.y,a.z);
	return flx_floats_to_scm(m.arr(),16);	
}

SCM FluxusMathsBinding::mrotate(SCM s_v)
{
	SCM_ASSERT(scm_is_generalized_vector(s_v),  s_v,  SCM_ARG1, "mrotate");	
	
	if (scm_c_generalized_vector_length(s_v)==3)
	{
		// euler angles
		dVector a;
		flx_floats_from_scm(s_v,a.arr());
		dMatrix m;
		m.rotxyz(a.x,a.y,a.z);
		return flx_floats_to_scm(m.arr(),16);	
	}
	else if (scm_c_generalized_vector_length(s_v)==4)
	{
		// quaternion
		dQuat a;
		flx_floats_from_scm(s_v,a.arr());
		dMatrix m=a.toMatrix();
		return flx_floats_to_scm(m.arr(),16);	
	}
	
	cerr<<"mrotate - wrong number of elements in vector"<<endl;
	return SCM_UNSPECIFIED;
}

SCM FluxusMathsBinding::mscale(SCM s_v)
{
	SCM_ASSERT(scm_is_generalized_vector(s_v),  s_v,  SCM_ARG1, "mscale");
	SCM_ASSERT(scm_c_generalized_vector_length(s_v)==3,  s_v,  SCM_ARG1, "mscale");
	dVector a;
	flx_floats_from_scm(s_v,a.arr());
	dMatrix m;
	m.scale(a.x,a.y,a.z);
	return flx_floats_to_scm(m.arr(),16);	
}

SCM FluxusMathsBinding::mtranspose(SCM s_a)
{
	SCM_ASSERT(scm_is_generalized_vector(s_a),  s_a,  SCM_ARG1, "mtranspose");
	SCM_ASSERT(scm_c_generalized_vector_length(s_a)==16,  s_a,  SCM_ARG1, "mtranspose");
	dMatrix m;
	flx_floats_from_scm(s_a,m.arr());
	m.transpose();
	return flx_floats_to_scm(m.arr(),16);	
}

SCM FluxusMathsBinding::minverse(SCM s_a)
{
	SCM_ASSERT(scm_is_generalized_vector(s_a),  s_a,  SCM_ARG1, "minverse");
	SCM_ASSERT(scm_c_generalized_vector_length(s_a)==16,  s_a,  SCM_ARG1, "minverse");
	dMatrix m;
	flx_floats_from_scm(s_a,m.arr());
	m=m.inverse();
	return flx_floats_to_scm(m.arr(),16);	
}

SCM FluxusMathsBinding::maim(SCM s_a, SCM s_b)
{
	SCM_ASSERT(scm_is_generalized_vector(s_a),  s_a,  SCM_ARG1, "maim");
	SCM_ASSERT(scm_is_generalized_vector(s_b),  s_b,  SCM_ARG2, "maim");	
	SCM_ASSERT(scm_c_generalized_vector_length(s_a)==3,  s_a,  SCM_ARG1, "maim");
	SCM_ASSERT(scm_c_generalized_vector_length(s_b)==3,  s_b,  SCM_ARG2, "maim");
	dVector a;
	flx_floats_from_scm(s_a,a.arr());
	dVector b;
	flx_floats_from_scm(s_b,b.arr());
	dMatrix m;
	m.aim(a,b);
	return flx_floats_to_scm(m.arr(),16);
}
	
SCM FluxusMathsBinding::qaxisangle(SCM s_axis, SCM s_angle)
{
	SCM_ASSERT(scm_is_generalized_vector(s_axis), s_axis, SCM_ARG1, "qaxisangle");
	SCM_ASSERT(scm_c_generalized_vector_length(s_axis)==3, s_axis, SCM_ARG1, "qaxisangle");
	SCM_ASSERT(scm_is_number(s_angle),  s_angle,  SCM_ARG1, "qaxisangle");
	
	dVector ax;
	flx_floats_from_scm(s_axis,ax.arr());
	dQuat q;
	q.setaxisangle(ax,scm_to_double(s_angle));
	return flx_floats_to_scm(q.arr(),4);
}

SCM FluxusMathsBinding::qmul(SCM s_a, SCM s_b)
{
	SCM_ASSERT(scm_is_generalized_vector(s_a), s_a, SCM_ARG1, "qmul");
	SCM_ASSERT(scm_c_generalized_vector_length(s_a)==4, s_a, SCM_ARG1, "qmul");
	SCM_ASSERT(scm_is_generalized_vector(s_b), s_b, SCM_ARG2, "qmul");
	SCM_ASSERT(scm_c_generalized_vector_length(s_b)==4, s_b, SCM_ARG2, "qmul");
	
	dQuat a;
	flx_floats_from_scm(s_a,a.arr());
	dQuat b;
	flx_floats_from_scm(s_b,b.arr());
	dQuat c=a*b;
	return flx_floats_to_scm(c.arr(),4);
}

SCM FluxusMathsBinding::qnormalise(SCM s_a)
{
	SCM_ASSERT(scm_is_generalized_vector(s_a), s_a, SCM_ARG1, "qnormalise");
	SCM_ASSERT(scm_c_generalized_vector_length(s_a)==4, s_a, SCM_ARG1, "qnormalise");
	
	dQuat a;
	flx_floats_from_scm(s_a,a.arr());
	a.renorm();
	return flx_floats_to_scm(a.arr(),4);
}

SCM FluxusMathsBinding::qtomatrix(SCM s_a)
{
	SCM_ASSERT(scm_is_generalized_vector(s_a), s_a, SCM_ARG1, "qnormalise");
	SCM_ASSERT(scm_c_generalized_vector_length(s_a)==4, s_a, SCM_ARG1, "qnormalise");
	
	dQuat a;
	flx_floats_from_scm(s_a,a.arr());
	dMatrix m=a.toMatrix();
	return flx_floats_to_scm(m.arr(),16);
}
