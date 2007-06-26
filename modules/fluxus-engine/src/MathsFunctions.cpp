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
#include "MathsFunctions.h"
#include "dada.h"
#include "FluxusEngine.h"

using namespace MathsFunctions;
using namespace SchemeHelper;
using namespace Fluxus;

// todo: 
// 1)better examples, at least showing the most basic functionality of every vector function in an simple script,
// or rather have a simple example script showing this, 
// 2)could be good if we had browseable links in the scratch pad too,
// to travel alond the docs and find what you want, a basic menu could already solve this. Ive prototyped something down here.

// StartSectionDoc-en
// Maths
// These functions are optimised for 3D graphics, and the collision of computer science and maths is apparent here, so 
// vectors representing "vectors" are in this context taken to be 3 elements long, quaternions are vectors of length 4, 
// and matrices are vectors of 16 elements long.
// Example:
// vmul
// vadd
// vsub
// vdiv
// vtransform
// vtransform-rot
// vnormalise
// vdot
// vmag
// vdist
// vcross
// mmul
// madd
// msub
// mdiv
// mident
// mtranslate
// mrotate
// mscale
// mtranspose
// minverse
// maim
// qaxisangle
// qmul
// qnormalise
// qtomatrix
// qconjugate
// EndSectionDoc 

// StartSectionDoc-pt
// Matemática
// Estas funções sao optimizadas para gráficos em 3d, e a colisão entre ciência da computação e matemática é aparente 
// aqui, então vetores representando "vectors" são nesse contexto tidos como 3 elementos em tamanho, quaternions são vetores de 
// tamanho 4, e matrizes são vetores de 16 elementos.
// Exemplo:
// vmul
// vadd
// vsub
// vdiv
// vtransform
// vtransform-rot
// vnormalise
// vdot
// vmag
// vdist
// vcross
// mmul
// madd
// msub
// mdiv
// mident
// mtranslate
// mrotate
// mscale
// mtranspose
// minverse
// maim
// qaxisangle
// qmul
// qnormalise
// qtomatrix
// qconjugate
// EndSectionDoc

// StartFunctionDoc-en
// vmul vector number
// Returns: result-vector
// Description:
// Multiplies a vector by a number
// Example:
// (vmul (vector 1 2 3) 2)
// EndFunctionDoc

// StartFunctionDoc-pt
// vmul vetor número
// Retorna: vetor resultante
// Descrição:
// Multiplica um vetor por um número.
// Exemplo:
// (vmul (vector 1 2 3) 2)
// EndFunctionDoc

Scheme_Object *vmul(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("vmul", "vf", argc, argv);
	dVector ret = VectorFromScheme(argv[0])*scheme_real_to_double(argv[1]);
  	MZ_GC_UNREG();
	return FloatsToScheme(ret.arr(),3);
}

// StartFunctionDoc-en
// vadd vector vector
// Returns: result-vector
// Description:
// Adds two vectors together
// Example:
// (vadd (vector 1 2 3) (vector 1 2 3))
// EndFunctionDoc

// StartFunctionDoc-pt
// vadd vetor vetor
// Retorna: vetor resultante
// Descrição:
// Adiciona dois vetores, um ao outro.
// Exemplo:
// (vadd (vector 1 2 3) (vector 1 2 3))
// EndFunctionDoc

Scheme_Object *vadd(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("vadd", "vv", argc, argv);	
	dVector ret = VectorFromScheme(argv[0])+VectorFromScheme(argv[1]);
  	MZ_GC_UNREG();
	return FloatsToScheme(ret.arr(),3);
}

// StartFunctionDoc-en
// vsub vector vector
// Returns: result-vector
// Description:
// Subtracts a vector from another
// Example:
// (vsub (vector 1 2 3) (vector 1 2 3))
// EndFunctionDoc

// StartFunctionDoc-pt
// vsub vetor vetor
// Retorna: vetor resultante
// Descrição:
// Subtrai um vetor de outro.
// Exemplo:
// (vsub (vector 1 2 3) (vector 1 2 3))
// EndFunctionDoc

Scheme_Object *vsub(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("vsub", "vv", argc, argv);
 	dVector ret = VectorFromScheme(argv[0])-VectorFromScheme(argv[1]);
 	MZ_GC_UNREG();
	return FloatsToScheme(ret.arr(),3);
}

// StartFunctionDoc-en
// vdiv vector number
// Returns: result-vector
// Description:
// Divides a vector by a number
// Example:
// (vdiv (vector 1 2 3) 2)
// EndFunctionDoc

// StartFunctionDoc-pt
// vdiv vetor número
// Retorna: vetor resultante
// Descrição:
// Divide um vetor por um número
// Exemplo:
// (vdiv (vector 1 2 3) 2)
// EndFunctionDoc

Scheme_Object *vdiv(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("vdiv", "vf", argc, argv);
 	dVector ret = VectorFromScheme(argv[0])/scheme_real_to_double(argv[1]);
	MZ_GC_UNREG(); 
	return FloatsToScheme(ret.arr(),3);
}

// StartFunctionDoc-en
// vtransform vector matrix
// Returns: result-vector
// Description:
// Multiplies (transforms) a vector by a matrix
// Example:
// (vtransform (vector 0 1 0) (mrotate (vector 90 0 0)))
// EndFunctionDoc

// StartFunctionDoc-pt
// vtransform vetor matriz
// Retorna: vetor resultante
// Descrição:
// Multiplica (transforma) um vetor por uma matriz.
// Exemplo:
// (vtransform (vector 0 1 0) (mrotate (vector 90 0 0)))
// EndFunctionDoc

Scheme_Object *vtransform(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("vtransform", "vm", argc, argv);
	dVector ret = MatrixFromScheme(argv[1]).transform(VectorFromScheme(argv[0]));
	MZ_GC_UNREG(); 
	return FloatsToScheme(ret.arr(),3);
}

// StartFunctionDoc-en
// vtransform-rot vector matrix
// Returns: result-vector
// Description:
// Multiplies (transforms) a vector by a matrix, but leaves out the translation part. For operations 
// involving normals.
// Example:
// (vtransform-rot (vector 0 1 0) (mrotate (vector 90 0 0)))
// EndFunctionDoc

// StartFunctionDoc-pt
// vtransform-rot vetor matriz
// Retorna: vetor resultante
// Descrição:
// Multiplica (transforma) um vetor por uma matriz, mas deixa de fora a parte de translação. Para 
// ser usado em operações involvendo normais.
// Exemplo:
// (vtransform-rot (vector 0 1 0) (mrotate (vector 90 0 0)))
// EndFunctionDoc

Scheme_Object *vtransform_rot(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("vtransform-rot", "vm", argc, argv);
	dVector ret=MatrixFromScheme(argv[1]).transform_no_trans(VectorFromScheme(argv[0]));
	MZ_GC_UNREG(); 
	return FloatsToScheme(ret.arr(),3);
}

// StartFunctionDoc-en
// vnormalise vector
// Returns: result-vector
// Description:
// Returns the normalised form of the vector (length=1)
// Example:
// (vtransform-rot (vector 0 1 0) (mrotate (vector 90 0 0)))
// EndFunctionDoc

// StartFunctionDoc-pt
// vnormalise vetor
// Retorna: vetor resultante
// Descrição:
// Retorna a forma normalisada do vetor (length=1)
// Exemplo:
// (vtransform-rot (vector 0 1 0) (mrotate (vector 90 0 0)))
// EndFunctionDoc

Scheme_Object *vnormalise(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("vnormalise", "v", argc, argv);
	dVector v=VectorFromScheme(argv[0]);
	v.normalise();
	MZ_GC_UNREG(); 
	return FloatsToScheme(v.arr(),3);
}

// StartFunctionDoc-en
// vdot vector vector
// Returns: result-number
// Description:
// Returns the dot product of two vectors
// Example:
// (vdot (vector 0 1 0) (vector 1 0 0))
// EndFunctionDoc

// StartFunctionDoc-pt
// vdot vetor vetor
// Retorna: número resultante
// Descrição:
// Retorna o produto multiplicado de dois vetores.
// Exemplo:
// (vdot (vector 0 1 0) (vector 1 0 0))
// EndFunctionDoc

Scheme_Object *vdot(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("vdot", "vv", argc, argv);
	float ret=VectorFromScheme(argv[0]).dot(VectorFromScheme(argv[1]));
	MZ_GC_UNREG(); 
	return scheme_make_float(ret);
}

// StartFunctionDoc-en
// vmag vector
// Returns: result-number
// Description:
// Returns the magnitude, or length of the vector
// Example:
// (vmag (vector 0 1 1))
// EndFunctionDoc

// StartFunctionDoc-pt
// vmag vetor
// Retorna: número resultante
// Descrição:
// Retorna a magnitude, ou alcance do vetor
// Exemplo:
// (vmag (vector 0 1 1))
// EndFunctionDoc

Scheme_Object *vmag(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("vmag", "v", argc, argv);
	float ret=VectorFromScheme(argv[0]).mag();
	MZ_GC_UNREG(); 
	return scheme_make_float(ret);
}

// StartFunctionDoc-en
// vdist vector vector
// Returns: result-number
// Description:
// Treating the vectors as points, returns the distance between them.
// Example:
// (vdist (vector 100 100 0) (vector 0 0 100))
// EndFunctionDoc

// StartFunctionDoc-pt
// vdist vetor vetor
// Retorna: número resultante
// Descrição:
// Tratando os vetores como pontos, retorna a distancia entre eles.
// Exemplo:
// (vdist (vector 100 100 0) (vector 0 0 100))
// EndFunctionDoc

Scheme_Object *vdist(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("vdist", "vv", argc, argv);
	float ret=VectorFromScheme(argv[0]).dist(VectorFromScheme(argv[1]));
	MZ_GC_UNREG(); 
	return scheme_make_float(ret);
}

// StartFunctionDoc-en
// vcross vector vector
// Returns: result-vector
// Description:
// Returns the cross product of two vectors, resulting in a vector that is perpendicular to the 
// crossed ones.
// Example:
// (vcross (vector 100 100 0) (vector 0 0 100))
// EndFunctionDoc

// StartFunctionDoc-pt
// vcross vetor vetor
// Retorna: vetor resultante
// Descrição:
// Retorna o produto cruzado entre dois vetores, resultando em um vetor que é perpendicular aos 
// cruzados.
// Exemplo:
// (vcross (vector 100 100 0) (vector 0 0 100)) 
// EndFunctionDoc

Scheme_Object *vcross(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("vcross", "vv", argc, argv);
	dVector ret=VectorFromScheme(argv[0]).cross(VectorFromScheme(argv[1]));
	MZ_GC_UNREG(); 
	return FloatsToScheme(ret.arr(),3);
}

// StartFunctionDoc-en
// mmul matrix-vector matrix-vector
// Returns: matrix-vector
// Description:
// Multiplies two matrices together
// Example:
// (vmul (mtranslate (vector 1 0 0)) (mrotate (vector 0 90 0)))
// EndFunctionDoc
 
// StartFunctionDoc-pt
// mmul vetor-matriz vetor-matriz
// Retorna: vetor-matriz
// Descrição:
// Multiplica duas matrizes.
// Exemplo:
// (vmul (mtranslate (vector 1 0 0)) (mrotate (vector 0 90 0)))
// EndFunctionDoc
 
Scheme_Object *mmul(int argc, Scheme_Object **argv)
{
 	DECL_ARGV();
 	ArgCheck("mmul", "mm", argc, argv);
 	dMatrix ret=MatrixFromScheme(argv[0])*MatrixFromScheme(argv[1]);
 	MZ_GC_UNREG(); 
 	return FloatsToScheme(ret.arr(),16);
}
 
// StartFunctionDoc-en
// madd matrix-vector matrix-vector
// Returns: matrix-vector
// Description:
// Adds two matrices together
// Example:
// (vadd (mtranslate (vector 1 0 0)) (mrotate (vector 0 90 0)))
// EndFunctionDoc

// StartFunctionDoc-pt
// madd vetor-matriz vetor-matriz
// Retorna: vetor-matriz
// Descrição:
// Adiciona duas matrizes.
// Exemplo:
// (vadd (mtranslate (vector 1 0 0)) (mrotate (vector 0 90 0)))
// EndFunctionDoc

Scheme_Object *madd(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("madd", "mm", argc, argv);
	dMatrix ret=MatrixFromScheme(argv[0])+MatrixFromScheme(argv[1]);
	MZ_GC_UNREG(); 
	return FloatsToScheme(ret.arr(),16);
}

// StartFunctionDoc-en
// msub matrix-vector matrix-vector
// Returns: matrix-vector
// Description:
// Subtracts a matrix from another.
// Example:
// (vsub (mtranslate (vector 1 0 0)) (mrotate (vector 0 90 0)))
// EndFunctionDoc

// StartFunctionDoc-pt
// msub vetor-matriz vetor-matriz
// Retorna: vetor-matriz
// Descrição:
// Subtrai uma matriz de outra.
// Exemplo:
// (vsub (mtranslate (vector 1 0 0)) (mrotate (vector 0 90 0)))
// EndFunctionDoc

Scheme_Object *msub(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("madd", "mm", argc, argv);
	dMatrix ret=MatrixFromScheme(argv[0])-MatrixFromScheme(argv[1]);
	MZ_GC_UNREG(); 
	return FloatsToScheme(ret.arr(),16);	
}

// StartFunctionDoc-en
// mdiv matrix-vector matrix-vector
// Returns: matrix-vector
// Description:
// Divides a matrix by another
// Example:
// (vdiv (mtranslate (vector 1 0 0)) (mrotate (vector 0 90 0)))
// EndFunctionDoc

// StartFunctionDoc-pt
// mdiv vetor-matriz vetor-matriz
// Retorna: vetor-matriz
// Descrição:
// Divide uma matriz por outra.
// Exemplo:
// (vdiv (mtranslate (vector 1 0 0)) (mrotate (vector 0 90 0)))
// EndFunctionDoc

Scheme_Object *mdiv(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("mdiv", "mm", argc, argv);
	dMatrix ret=MatrixFromScheme(argv[0])/MatrixFromScheme(argv[1]);
	MZ_GC_UNREG(); 
	return FloatsToScheme(ret.arr(),16);	
}

// StartFunctionDoc-en
// mident 
// Returns: matrix-vector
// Description:
// Returns the identity matrix
// Example:
// (mident)
// EndFunctionDoc

// StartFunctionDoc-pt
// mident
// Retorna: vetor-matriz
// Descrição:
// Retorna a matriz identidade
// Exemplo:
// (mident)
// EndFunctionDoc

Scheme_Object *mident(int argc, Scheme_Object **argv)
{
	dMatrix m;
	return FloatsToScheme(m.arr(),16);	
}

// StartFunctionDoc-en
// mtranslate vector
// Returns: matrix-vector
// Description:
// Returns a matrix representing the specified transform
// Example:
// (mtransform (vector 100 0 0))
// EndFunctionDoc

// StartFunctionDoc-pt
// mtranslate vetor
// Retorna: vetor-matriz
// Descrição:
// Retorna uma matriz representando a tranformação(translação) especificada.
// Exemplo:
// (mtransform (vector 100 0 0))
// EndFunctionDoc

Scheme_Object *mtranslate(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("mtranslate", "v", argc, argv);
	dVector t = VectorFromScheme(argv[0]);
	dMatrix m;
	m.translate(t.x,t.y,t.z);
	MZ_GC_UNREG(); 
	return FloatsToScheme(m.arr(),16);	
}

// StartFunctionDoc-en
// mrotate vector
// Returns: matrix-vector
// Description:
// Returns a matrix representing the specified rotation. Accepts a vector of euler angles, or a quaternion.
// Example:
// (mrotate (vector 0 45 0))
// EndFunctionDoc

// StartFunctionDoc-pt
// mrotate vetor
// Retorna: vetor-matriz
// Descrição:
// Retorna uma matriz representando a rotação especificada. Aceita um vetor de angulos euler, ou um quatérnio.
// Exemplo:
// (mrotate (vector 0 45 0))
// EndFunctionDoc

Scheme_Object *mrotate(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	if (!SCHEME_VECTORP(argv[0])) scheme_wrong_type("mrotate", "vector", 0, argc, argv);
		
	if (SCHEME_VEC_SIZE(argv[0])==3)
	{
		// euler angles
		dVector a;
		FloatsFromScheme(argv[0],a.arr(),3);
		dMatrix m;
		m.rotxyz(a.x,a.y,a.z);
		MZ_GC_UNREG(); 
		return FloatsToScheme(m.arr(),16);	
	}
	else if (SCHEME_VEC_SIZE(argv[0])==4)
	{
		// quaternion
		dQuat a;
		FloatsFromScheme(argv[0],a.arr(),4);
		dMatrix m=a.toMatrix();
		MZ_GC_UNREG(); 
		return FloatsToScheme(m.arr(),16);	
	}
	
	cerr<<"mrotate - wrong number of elements in vector"<<endl;
	MZ_GC_UNREG(); 
	return scheme_void;
}

// StartFunctionDoc-en
// mscale vector
// Returns: matrix-vector
// Description:
// Returns a matrix representing the specified scaling.
// Example:
// (mscale (vector 0.5 2 0.5))
// EndFunctionDoc

// StartFunctionDoc-pt
// mscale vetor
// Retorna: vetor-matriz
// Descrição:
// Retorna uma matriz representando a escalagem especificada.
// Exemplo:
// (mscale (vector 0.5 2 0.5))
// EndFunctionDoc

Scheme_Object *mscale(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("mscale", "v", argc, argv);
	dVector t = VectorFromScheme(argv[0]);
	dMatrix m;
	m.scale(t.x,t.y,t.z);
	MZ_GC_UNREG(); 
	return FloatsToScheme(m.arr(),16);	
}

// StartFunctionDoc-en
// mtranspose matrix-vector
// Returns: matrix-vector
// Description:
// Returns the transpose of the input vector
// Example:
// (mtranspose (mident))
// EndFunctionDoc

// StartFunctionDoc-pt
// mtranspose vetor-matriz
// Retorna: vetor matriz
// Descrição:
// Retorna a transposta do vetor de entrada
// Exemplo:
// (mtranspose (mident))
// EndFunctionDoc

Scheme_Object *mtranspose(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("mtranspose", "m", argc, argv);
	dMatrix m;
	FloatsFromScheme(argv[0],m.arr(),16);
	m.transpose();
	MZ_GC_UNREG(); 
	return FloatsToScheme(m.arr(),16);	
}

// StartFunctionDoc-en
// minverse matrix-vector
// Returns: matrix-vector
// Description:
// Returns the inverse of the input vector.
// Example:
// (minverse (mscale (vector 0.5 2 0.5)))
// EndFunctionDoc

// StartFunctionDoc-pt
// minverse vetor-matriz
// Retorna: vetor-matriz
// Descrição:
// Retorna o inverso do vetor de entrada.
// Exemplo:
// (minverse (mscale (vector 0.5 2 0.5)))
// EndFunctionDoc

Scheme_Object *minverse(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("minverse", "m", argc, argv);
	dMatrix m;
	FloatsFromScheme(argv[0],m.arr(),16);
	m=m.inverse();
	MZ_GC_UNREG(); 
	return FloatsToScheme(m.arr(),16);	
}

// StartFunctionDoc-en
// maim aim-vector up-vector
// Returns: matrix-vector
// Description:
// Returns a matrix representing an aiming rotation so that the x axis points down the aim direction, and 
// the y axis points up the up vector. Probably suffers from gimbal lock.
// Example:
// (maim (vector 0 0 1) (vector 0 1 0))
// EndFunctionDoc

// StartFunctionDoc-pt
// maim vetor-mira vetor-acima
// Retorna: vetor-matriz
// Descrição:
// Retorna uma matriz representando uma rotação de mira de forma que o eixo X aponta pra baixo da direção
// de mira, e o eixo y aponta pra cima do vetor de cima. Provavelmente sofre do Gimbal Lock.
// Exemplo:
// (maim (vector 0 0 1) (vector 0 1 0))
// EndFunctionDoc

Scheme_Object *maim(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("maim", "vv", argc, argv);
	dMatrix m;
	m.aim(VectorFromScheme(argv[0]),VectorFromScheme(argv[1]));
	MZ_GC_UNREG(); 
	return FloatsToScheme(m.arr(),16);
}

// StartFunctionDoc-en
// qaxisangle axis-vector angle-number
// Returns: quaternion-vector
// Description:
// Returns the quaternion representing rotation of angle degrees about the specified axis. 
// Example:
// (qaxisangle (vector 0 1 0) 45)
// EndFunctionDoc
	
// StartFunctionDoc-pt
// qaxisangle vetor-eixo angulo
// Retorna: vetor-quaternion
// Descrição:
// Retorna o quatérnio representando o ângulo de rotação sobre o eixo especificado.
// Exemplo:
// (qaxisangle (vector 0 1 0) 45)
// EndFunctionDoc

Scheme_Object *qaxisangle(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("qaxisangle", "vf", argc, argv);
	dQuat q;
	q.setaxisangle(VectorFromScheme(argv[0]),scheme_real_to_double(argv[1]));
	MZ_GC_UNREG(); 
	return FloatsToScheme(q.arr(),4);
}

// StartFunctionDoc-en
// qmul quaternion-vector quaternion-vector
// Returns: quaternion-vector
// Description:
// Multiplies two quaternions together. 
// Example:
// (qmul (qaxisangle (vector 0 1 0) 45) (qaxisangle (vector 0 0 1) 180))
// EndFunctionDoc

// StartFunctionDoc-pt
// qmul vetor-quatérnio vetor-quatérnio
// Retorna: vetor-quatérnio
// Descrição:
// Multiplica um quatérnio por outro.
// Exemplo:
// (qmul (qaxisangle (vector 0 1 0) 45) (qaxisangle (vector 0 0 1) 180))
// EndFunctionDoc

Scheme_Object *qmul(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("qmul", "qq", argc, argv);
	dQuat ret=QuatFromScheme(argv[0])*QuatFromScheme(argv[1]);
	MZ_GC_UNREG(); 
	return FloatsToScheme(ret.arr(),4);
}

// StartFunctionDoc-en
// qnormalise quaternion-vector
// Returns: quaternion-vector
// Description:
// Normalises a quaternion. 
// Example
// (qnormalise (qaxisangle (vector 0 19 0) 45))
// EndFunctionDoc

// StartFunctionDoc-pt
// qnormalise vetor-quatérnio
// Retorna: vetor-quatérnio
// Descrição:
// Normalisa um quatérnio
// Exemplo:
// (qnormalise (qaxisangle (vector 0 19 0) 45))
// EndFunctionDoc

Scheme_Object *qnormalise(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("qnormalise", "q", argc, argv);
	dQuat a;
	FloatsFromScheme(argv[0],a.arr(),4);
	a.renorm();
	MZ_GC_UNREG(); 
	return FloatsToScheme(a.arr(),4);
}

// StartFunctionDoc-en
// qtomatrix quaternion-vector
// Returns: matrix-vector
// Description:
// Converts a quaternion into a rotation matrix. 
// Example
// (qtomatrix (qaxisangle (vector 0 1 0) 45))
// EndFunctionDoc

// StartFunctionDoc-pt
// qtomatrix vetor-quatérnio
// Retorna: vetor-matriz
// Descrição:
// Converte um quatérnio em uma matriz de rotação
// Exemplo:
// (qtomatrix (qaxisangle (vector 0 1 0) 45))
// EndFunctionDoc

Scheme_Object *qtomatrix(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("qtomatrix", "q", argc, argv);
	
	dQuat a;
	FloatsFromScheme(argv[0],a.arr(),4);
	dMatrix m=a.toMatrix();
	MZ_GC_UNREG(); 
	return FloatsToScheme(m.arr(),16);
}

// StartFunctionDoc-en
// qconjugate quaternion-vector
// Returns: quaternion-vector
// Description:
// Conjugatea a quaternion. 
// Example:
// (qconjugate (qaxisangle (vector 0 1 0) 45))
// EndFunctionDoc

// StartFunctionDoc-pt
// qconjugate vetor-quatérnio
// Retorna: vetor-quatérnio
// Descrição:
// Conjuga um quatérnio
// Exemplo:
// (qconjugate (qaxisangle (vector 0 1 0) 45))
// EndFunctionDoc

Scheme_Object *qconjugate(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("qconjugate", "q", argc, argv);
	dQuat ret=QuatFromScheme(argv[0]).conjugate();
	MZ_GC_UNREG(); 
	return FloatsToScheme(ret.arr(),4);
}

void MathsFunctions::AddGlobals(Scheme_Env *env)
{	
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, env);
	MZ_GC_REG();
	scheme_add_global("vmul", scheme_make_prim_w_arity(vmul, "vmul", 2, 2), env);
	scheme_add_global("vadd", scheme_make_prim_w_arity(vadd, "vadd", 2, 2), env);
	scheme_add_global("vsub", scheme_make_prim_w_arity(vsub, "vsub", 2, 2), env);
	scheme_add_global("vdiv", scheme_make_prim_w_arity(vdiv, "vdiv", 2, 2), env);
	scheme_add_global("vtransform", scheme_make_prim_w_arity(vtransform, "vtransform", 2, 2), env);
	scheme_add_global("vtransform-rot", scheme_make_prim_w_arity(vtransform_rot, "vtransform-rot", 2, 2), env);
	scheme_add_global("vnormalise", scheme_make_prim_w_arity(vnormalise, "vnormalise", 1, 1), env);
	scheme_add_global("vdot", scheme_make_prim_w_arity(vdot, "vdot", 2, 2), env);
	scheme_add_global("vdist", scheme_make_prim_w_arity(vdist, "vdist", 2, 2), env);
	scheme_add_global("vmag", scheme_make_prim_w_arity(vmag, "vmag", 1, 1), env);
	scheme_add_global("vcross", scheme_make_prim_w_arity(vcross, "vcross", 2, 2), env);
	scheme_add_global("mmul", scheme_make_prim_w_arity(mmul, "mmul", 2, 2), env);
	scheme_add_global("madd", scheme_make_prim_w_arity(madd, "madd", 2, 2), env);
	scheme_add_global("msub", scheme_make_prim_w_arity(msub, "msuv", 2, 2), env);
	scheme_add_global("mdiv", scheme_make_prim_w_arity(mdiv, "mdiv", 2, 2), env);
	scheme_add_global("mident", scheme_make_prim_w_arity(mident, "mident", 0, 0), env);
	scheme_add_global("mtranslate", scheme_make_prim_w_arity(mtranslate, "mtranslate", 1, 1), env);
	scheme_add_global("mrotate", scheme_make_prim_w_arity(mrotate, "mrotate", 1, 1), env);
	scheme_add_global("mscale", scheme_make_prim_w_arity(mscale, "mscale", 1, 1), env);
	scheme_add_global("mtranspose", scheme_make_prim_w_arity(mtranspose, "mtranspose", 1, 1), env);
	scheme_add_global("minverse", scheme_make_prim_w_arity(minverse, "minverse", 1, 1), env);
	scheme_add_global("maim", scheme_make_prim_w_arity(maim, "maim", 2, 2), env);
	scheme_add_global("qaxisangle", scheme_make_prim_w_arity(qaxisangle, "qaxisangle", 2, 2), env);
	scheme_add_global("qmul", scheme_make_prim_w_arity(qmul, "qmul", 2, 2), env);
	scheme_add_global("qnormalise", scheme_make_prim_w_arity(qnormalise, "qnormalise", 1, 1), env);
	scheme_add_global("qtomatrix", scheme_make_prim_w_arity(qtomatrix, "qtomatrix", 1, 1), env);	
	scheme_add_global("qconjugate", scheme_make_prim_w_arity(qconjugate, "qconjugate", 1, 1), env);	
 	MZ_GC_UNREG(); 
}
