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

#include "SchemeHelper.h"
#include "MathsFunctions.h"
#include "dada.h"
#include "Noise.h"
#include "SimplexNoise.h"
#include "FluxusEngine.h"

using namespace MathsFunctions;
using namespace SchemeHelper;
using namespace Fluxus;

// todo: 
// 1)better examples, at least showing the most basic functionality of every vector function in an simple script,
// or rather have a simple example script showing this, 

// nearly there... ;)
// :), indeed, i guess that the examples can be left for second plane if we have a good amount of tutorials perhaps?

// StartSectionDoc-en
// maths
// These functions are optimised for 3D graphics, and the collision of computer science and maths is apparent here, so 
// scheme vectors representing maths vectors are in this context taken to be 3 elements long, quaternions are vectors of length 4, 
// and matrices are vectors of 16 elements long.
// Example:
// EndSectionDoc 

// StartSectionDoc-pt
// matematica
// Estas funções sao optimizadas para gráficos em 3d, e a colisão entre ciência da computação e matemática é aparente 
// aqui, então vetores representando "vectors" são nesse contexto tidos como 3 elementos em tamanho, quaternions são vetores de 
// tamanho 4, e matrizes são vetores de 16 elementos.
// Exemplo:
// EndSectionDoc

// StartSectionDoc-fr
// maths
// Ces fonctions sont optimisées pour les graphiques 3D, et la collision entre science informatique et mathématique est ici évidente.
// Donc les vecteurs Scheme représentant les vecteurs informatiques contiennent dans ce contexte 3 éléments, quaternions sont des vecteurs
// de 4 éléments, et les matrices, des vecteurs de 16 éléments. 
// Exemple:
// EndSectionDoc

// StartFunctionDoc-en
// vmulc vector number
// Returns: result-vector
// Description:
// Multiplies a vector by a number.
// Deprecated C version, the Scheme version is faster and more flexible.
// Example:
// (vmulc (vector 1 2 3) 2)
// EndFunctionDoc

// StartFunctionDoc-pt
// vmulc vetor número
// Retorna: vetor resultante
// Descrição:
// Multiplica um vetor por um número.
// Exemplo:
// (vmulc (vector 1 2 3) 2)
// EndFunctionDoc

// StartFunctionDoc-fr
// vmulc vecteur nombre
// Retour: vecteur-resultant
// Description:
// Multiplie un vecteur par un nombre.
// Vesrion C dépréciée, la version Scheme est plus rapide et flexible.
// Exemple:
// (vmulc (vector 1 2 3) 2)
// EndFunctionDoc

Scheme_Object *vmulc(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("vmulc", "vf", argc, argv);
	dVector ret = VectorFromScheme(argv[0])*scheme_real_to_double(argv[1]);
	MZ_GC_UNREG();
	return FloatsToScheme(ret.arr(),3);
}

// StartFunctionDoc-en
// vaddc vector vector
// Returns: result-vector
// Description:
// Adds two vectors together.
// Deprecated C version, the Scheme version is faster and more flexible.
// Example:
// (vaddc (vector 1 2 3) (vector 1 2 3))
// EndFunctionDoc

// StartFunctionDoc-pt
// vaddc vetor vetor
// Retorna: vetor resultante
// Descrição:
// Adiciona dois vetores, um ao outro.
// Exemplo:
// (vaddc (vector 1 2 3) (vector 1 2 3))
// EndFunctionDoc

// StartFunctionDoc-fr
// vaddc vecteur vecteur
// Retour: vecteur-resultant
// Description:
// Additionne deux vecteurs ensemble.
// Vesrion C dépréciée, la version Scheme est plus rapide et flexible.
// Exemple:
// (vaddc (vector 1 2 3) (vector 1 2 3))
// EndFunctionDoc

Scheme_Object *vaddc(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("vaddc", "vv", argc, argv);
	dVector ret = VectorFromScheme(argv[0])+VectorFromScheme(argv[1]);
	MZ_GC_UNREG();
	return FloatsToScheme(ret.arr(),3);
}

// StartFunctionDoc-en
// vsubc vector vector
// Returns: result-vector
// Description:
// Subtracts a vector from another.
// Deprecated C version, the Scheme version is faster and more flexible.
// Example:
// (vsubc (vector 1 2 3) (vector 1 2 3))
// EndFunctionDoc

// StartFunctionDoc-pt
// vsubc vetor vetor
// Retorna: vetor resultante
// Descrição:
// Subtrai um vetor de outro.
// Exemplo:
// (vsubc (vector 1 2 3) (vector 1 2 3))
// EndFunctionDoc

// StartFunctionDoc-fr
// vsubc vecteur vecteur
// Retour: vecteur-resultant
// Description:
// Soustrait un vecteur par un autre.
// Vesrion C dépréciée, la version Scheme est plus rapide et flexible.
// Exemple:
// (vsubc (vector 1 2 3) (vector 1 2 3))
// EndFunctionDoc

Scheme_Object *vsubc(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("vsubc", "vv", argc, argv);
	dVector ret = VectorFromScheme(argv[0])-VectorFromScheme(argv[1]);
	MZ_GC_UNREG();
	return FloatsToScheme(ret.arr(),3);
}

// StartFunctionDoc-en
// vdivc vector number
// Returns: result-vector
// Description:
// Divides a vector by a number.
// Deprecated C version, the Scheme version is faster and more flexible.
// Example:
// (vdivc (vector 1 2 3) 2)
// EndFunctionDoc

// StartFunctionDoc-pt
// vdivc vetor número
// Retorna: vetor resultante
// Descrição:
// Divide um vetor por um número
// Exemplo:
// (vdivc (vector 1 2 3) 2)
// EndFunctionDoc

// StartFunctionDoc-fr
// vdivc vecteur nombre
// Retour: vecteur-resultant
// Description:
// Divise un vecteur par un nombre.
// Vesrion C dépréciée, la version Scheme est plus rapide et flexible.
// Exemple:
// (vdivc (vector 1 2 3) 2)
// EndFunctionDoc

Scheme_Object *vdivc(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("vdivc", "vf", argc, argv);
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

// StartFunctionDoc-fr
// vtransform vecteur matrice
// Retour: vecteur-resultant
// Description:
// Multiplie (transforme) un vecteur par un matrice.
// Exemple:
// (vtransform (vector 0 1 0) (mrotate (vector 90 0 0)))
// EndFunctionDoc

Scheme_Object *vtransform(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("vtransform", "vm", argc, argv);
	dVector ret = MatrixFromScheme(argv[1]).transform_persp(VectorFromScheme(argv[0]));
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

// StartFunctionDoc-fr
// vtransform-rot vecteur matrice
// Retour: vecteur-resultant
// Description:
// Multiplie (transform) un vecteur par un matrice, mais ignore la partie translation pour les opérations
// impliquant les normales.
// Exemple:
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
// (vnormalise (vector 3 4 5))
// EndFunctionDoc

// StartFunctionDoc-pt
// vnormalise vetor
// Retorna: vetor resultante
// Descrição:
// Retorna a forma normalisada do vetor (length=1)
// Exemplo:
// (vnormalise (vector 3 4 5))
// EndFunctionDoc

// StartFunctionDoc-fr
// vnormalise vecteur
// Retour: vecteur-resultant
// Description:
// Retourne un vecteur normalisé à partir du vecteur spécifié (longueur=1)
// Exemple:
// (vnormalise (vector 3 4 5))
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

// StartFunctionDoc-fr
// vdot vecteur vecteur
// Retour: nombre-resultant
// Description:
// Retourne le produit scalaire de deux vecteurs.
// Exemple:
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
// Returns the magnitude, or length of the vector.
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

// StartFunctionDoc-fr
// vmag vecteur
// Retour: nombre-resultant
// Description:
// Retourne la magnitude, ou longueur du vecteur.
// Example:
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
// vreflect vector vector
// Returns: result-vector
// Description:
// Returns the reflection of one vector against another.
// Example:
// (vreflect (vector 0 1 1) (vector 1 0 1))
// EndFunctionDoc

// StartFunctionDoc-pt
// vreflect vetor vetor
// Retorna: vetor-resultante
// Descrição:
// Retorna a reflexão de um vetor em relação ao outro.
// Exemplo:
// (vreflect (vector 0 1 1) (vector 1 0 1))
// EndFunctionDoc

// StartFunctionDoc-fr
// vreflect vecteur vecteur
// Retour: vecteur-resultant
// Description:
// Retourne la reflection d'un vecteur par un autre.
// Exemple:
// (vreflect (vector 0 1 1) (vector 1 0 1))
// EndFunctionDoc

Scheme_Object *vreflect(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("vreflect", "vv", argc, argv);
	dVector ret=VectorFromScheme(argv[0]).reflect(VectorFromScheme(argv[1]));
	MZ_GC_UNREG(); 
	return FloatsToScheme(ret.arr(),3);
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

// StartFunctionDoc-fr
// vdist vecteur vecteur
// Retour: nombre-resultant
// Description:
// Traite les vecteurs comme des points et retourne la distance entre les deux.
// Exemple:
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
// vdist-sq vector vector
// Returns: result-number
// Description:
// Treating the vectors as points, returns the squared distance between them. Faster than vdist.
// Example:
// (vdist-sq (vector 100 100 0) (vector 0 0 100))
// EndFunctionDoc

// StartFunctionDoc-pt
// vdist-sq vetor vetor
// Retorna: número resultante
// Descrição:
// Tratando os vetores como pontos, retorna a distancia entre eles.
// Exemplo:
// (vdist-sq (vector 100 100 0) (vector 0 0 100))
// EndFunctionDoc

// StartFunctionDoc-fr
// vdist-sq vecteur vecteur
// Retour: nombre-resultant
// Description:
// Traite les vecteurs comme des points et retourne la racine carrée de la distance entre les deux.
// Plus rapide que vdist.
// Exemple:
// (vdist-sq (vector 100 100 0) (vector 0 0 100))
// EndFunctionDoc

Scheme_Object *vdistsq(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("vdist-sq", "vv", argc, argv);
	float ret=VectorFromScheme(argv[0]).distsq(VectorFromScheme(argv[1]));
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

// StartFunctionDoc-fr
// vcross vecteur vecteur
// Retour: vecteur-resultant
// Description:
// Retourne le produit vectoriel (cross product) des deux vecteurs, résultant un vecteur perpendiculaire
// aux vecteurs croisés.
// Exemple:
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
// (mmul (mtranslate (vector 1 0 0)) (mrotate (vector 0 90 0)))
// EndFunctionDoc

// StartFunctionDoc-pt
// mmul vetor-matriz vetor-matriz
// Retorna: vetor-matriz
// Descrição:
// Multiplica duas matrizes.
// Exemplo:
// (mmul (mtranslate (vector 1 0 0)) (mrotate (vector 0 90 0)))
// EndFunctionDoc

// StartFunctionDoc-fr
// mmul matrice-vecteur matrice-vecteur
// Retour: matrice-vecteur
// Description:
// Multiplie deux matrices ensemble.
// Exemple:
// (mmul (mtranslate (vector 1 0 0)) (mrotate (vector 0 90 0)))
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
// (madd (mtranslate (vector 1 0 0)) (mrotate (vector 0 90 0)))
// EndFunctionDoc

// StartFunctionDoc-pt
// madd vetor-matriz vetor-matriz
// Retorna: vetor-matriz
// Descrição:
// Adiciona duas matrizes.
// Exemplo:
// (madd (mtranslate (vector 1 0 0)) (mrotate (vector 0 90 0)))
// EndFunctionDoc

// StartFunctionDoc-fr
// madd matrice-vecteur matrice-vecteur
// Retour: matrice-vecteur
// Description:
// Additionne deux matrices ensemble.
// Exemple:
// (madd (mtranslate (vector 1 0 0)) (mrotate (vector 0 90 0)))
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
// (msub (mtranslate (vector 1 0 0)) (mrotate (vector 0 90 0)))
// EndFunctionDoc

// StartFunctionDoc-pt
// msub vetor-matriz vetor-matriz
// Retorna: vetor-matriz
// Descrição:
// Subtrai uma matriz de outra.
// Exemplo:
// (msub (mtranslate (vector 1 0 0)) (mrotate (vector 0 90 0)))
// EndFunctionDoc

// StartFunctionDoc-fr
// msub matrice-vecteur matrice-vecteur
// Retour: matrice-vecteur
// Description:
// Soustrait une matrice par une autre.
// Exemple:
// (msub (mtranslate (vector 1 0 0)) (mrotate (vector 0 90 0)))
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
// (mdiv (mtranslate (vector 1 0 0)) (mrotate (vector 0 90 0)))
// EndFunctionDoc

// StartFunctionDoc-pt
// mdiv vetor-matriz vetor-matriz
// Retorna: vetor-matriz
// Descrição:
// Divide uma matriz por outra.
// Exemplo:
// (mdiv (mtranslate (vector 1 0 0)) (mrotate (vector 0 90 0)))
// EndFunctionDoc

// StartFunctionDoc-fr
// mdiv matrice-vecteur matrice-vecteur
// Retour: matrice-vecteur
// Description:
// Divise une matrice par une autre.
// Exemple:
// (mdiv (mtranslate (vector 1 0 0)) (mrotate (vector 0 90 0)))
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

// StartFunctionDoc-fr
// mident 
// Retour: matrice-vecteur
// Description:
// Retourne une matrice identité.
// Exemple:
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
// (mtranslate (vector 100 0 0))
// EndFunctionDoc

// StartFunctionDoc-pt
// mtranslate vetor
// Retorna: vetor-matriz
// Descrição:
// Retorna uma matriz representando a tranformação(translação) especificada.
// Exemplo:
// (mtranslate (vector 100 0 0))
// EndFunctionDoc

// StartFunctionDoc-fr
// mtranslate vecteur
// Retour: matrice-vecteur
// Description:
// Retourne une matrice représentant la transformation spécifiée.
// Exemple:
// (mtranslate (vector 100 0 0))
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

// StartFunctionDoc-fr
// mrotate vecteur
// Retour: matrice-vecteur
// Description:
// Retourne une matrice représentant la rotation spécifiée. Accepte un vecteur aux angles d'Euler,
// ou un quaternion.
// Exemple:
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
	
	Trace::Stream<<"mrotate - wrong number of elements in vector"<<endl;
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

// StartFunctionDoc-fr
// mscale vecteur
// Retour: matrice-vecteur
// Description:
// Retourne une matrice représentant l'échelle spécifiée.
// Exemple:
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

// StartFunctionDoc-fr
// mtranspose matrice-vecteur
// Retour: matrice-vecteur
// Description:
// Retourne la transposition du vecteur d'entrée.
// Exemple:
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

// StartFunctionDoc-fr
// minverse matrice-vecteur
// Retour: matrice-vecteur
// Description:
// Retourne l'inverse du vecteur d'entrée.
// Exemple:
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

// StartFunctionDoc-fr
// maim vecteur-cible vector-haut
// Retour: matrice-vecteur
// Description:
// Retourne une matrice représentant une rotation cible de facon à ce que l'axe X pointe en bas la direction ciblée,
// et l'axe Y pointe en haut du vecteur-haut. Souffre probablement de Gimbal 
// Example:
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
// matrix->euler matrix-vector
// Returns: vector
// Description:
// Returns the euler angles extracted from the matrix.
// Example:
// (matrix->euler (mrotate (vector 15 0 0)))
// EndFunctionDoc

// StartFunctionDoc-pt
// matrix->euler matriz-vetor
// Retorna: vetor
// Descrição:
// Retorna os angulos euler extraidos da matriz.
// Exemplo:
// (matrix->euler (mrotate (vector 15 0 0)))
// EndFunctionDoc

// StartFunctionDoc-fr
// matrix->euler vecteur-matrice
// Retour: vecteur
// Description:
// Retourne les angles d'Euler extraient de la matrice.
// Exemple:
// (matrix->euler (mrotate (vector 15 0 0)))
// EndFunctionDoc

Scheme_Object *matrix_to_euler(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("matrix->euler", "m", argc, argv);
	dMatrix m;
	FloatsFromScheme(argv[0], m.arr(), 16);
	dVector angles;
	m.extract_euler(angles.x, angles.y, angles.z);
	MZ_GC_UNREG();
	return FloatsToScheme(angles.arr(), 3);
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

// StartFunctionDoc-fr
// qaxisangle axe-vecteur angle-nombre
// Retour: quaternion-vecteur
// Description:
// Retour un quaternion représentant la rotation d'angle en degrés de l'axe spécifié.
// Exemple:
// (qaxisangle (vector 0 1 0) 45)
// EndFunctionDoc

Scheme_Object *qaxisangle(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("qaxisangle", "vf", argc, argv);
	dQuat q;
	q.setAxisAngle(VectorFromScheme(argv[0]),scheme_real_to_double(argv[1]));
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

// StartFunctionDoc-fr
// qmul quaternion-vecteur quaternion-vecteur
// Retour: quaternion-vecteur
// Description:
// Multiplie deux quaternions ensemble. 
// Exemple:
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
// Example:
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

// StartFunctionDoc-fr
// qnormalise quaternion-vecteur
// Retour: quaternion-vecteur
// Description:
// Normalise un quaternion. 
// Exemple:
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
// Example:
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

// StartFunctionDoc-fr
// qtomatrix quaternion-vecteur
// Retour: matrice-vecteur
// Description:
// Convertis un quaternion en un matrice de rotation.
// Exemple:
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
// Conjugate a quaternion. 
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

// StartFunctionDoc-fr
// qconjugate quaternion-vecteur
// Retour: quaternion-vecteur
// Description:
// Retourne le conjugué d'un quaternion.
// Exemple:
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

// StartFunctionDoc-en
// fmod numerator-number denominator-number
// Returns: real-number
// Description:
// Returns the floating-point remainder of numerator/denominator.
// Example:
// (fmod 14.4 10)
// EndFunctionDoc

// StartFunctionDoc-pt
// fmod numero-numerador numero-denominador
// Retorna: numero-real
// Descrição:
// Retorna o resto em ponto-flutuante de numerador/denominador.
// Exemplo:
// (fmod 14.4 10)
// EndFunctionDoc

// StartFunctionDoc-fr
// fmod numerateur-nombre denominateur-nombre
// Retour: réel-nombre
// Description:
// Retourne le reste en nombre flottant de numerateur/denominateur.
// Exemple:
// (fmod 14.4 10)
// EndFunctionDoc

Scheme_Object *fmod(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("fmod", "ff", argc, argv);
	float ret=fmod(scheme_real_to_double(argv[0]),scheme_real_to_double(argv[1]));
	MZ_GC_UNREG(); 
	return scheme_make_double(ret);
}

// StartFunctionDoc-en
// snoise real-number ...
// Returns: real-number
// Description:
// Returns 1D/2D/3D/4D Simplex Noise in the range -1->1 depending on the number of parameters.
// Example:
// (snoise 1.0 2.0) ; 2D noise
// (snoise 6.1 2.4 .5 1.3) ; 4D noise
// 
// ; example on a pixel prim
// (clear)
// (with-primitive (build-pixels 100 100)
//     (pdata-index-map!
//         (lambda (i c)
//             (snoise (* 0.1 (modulo i (pixels-width)))
//                     (* 0.1 (quotient i (pixels-height)))))
//         "c")
//     (pixels-upload))
// EndFunctionDoc

// StartFunctionDoc-pt
// snoise numero-real
// Retorna: numero-real
// Descrição:
// Retorna Simplex Noise 1D/2D/3D/4D no intervalo -1->1 dependendo do número de parâmetros.
// Exemplo:
// (snoise 1.0 2.0) ; 2D noise
// (snoise 6.1 2.4 .5 1.3) ; 4D noise
// 
// ; example on a pixel prim
// (clear)
// (with-primitive (build-pixels 100 100)
//     (pdata-index-map!
//         (lambda (i c)
//             (snoise (* 0.1 (modulo i (pixels-width)))
//                     (* 0.1 (quotient i (pixels-height)))))
//         "c")
//     (pixels-upload))
// EndFunctionDoc

// StartFunctionDoc-fr
// snoise réel-nombre ...
// Retour: réel-nombre
// Description:
// Retourne un 1D/2D/3D/4D Simplex Noise dans l'interval -1->1 en fonction du nombre de paramètres.
// Exemple:
// (snoise 1.0 2.0) ; bruit 2D
// (snoise 6.1 2.4 .5 1.3) ; bruit 4D
// 
// ; Exemple sur une primitive pixel
// (clear)
// (with-primitive (build-pixels 100 100)
//     (pdata-index-map!
//         (lambda (i c)
//             (snoise (* 0.1 (modulo i (pixels-width)))
//                     (* 0.1 (quotient i (pixels-height)))))
//         "c")
//     (pixels-upload))
// EndFunctionDoc

Scheme_Object *snoise(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	float ret=0;

	switch (argc)
	{
		case 1:
			ArgCheck("snoise", "f", argc, argv);
			ret=SimplexNoise::noise(scheme_real_to_double(argv[0]));
			break;
		case 2:
			ArgCheck("snoise", "ff", argc, argv);
			ret=SimplexNoise::noise(scheme_real_to_double(argv[0]),
									scheme_real_to_double(argv[1]));
			break;
		case 3:
			ArgCheck("snoise", "fff", argc, argv);
			ret=SimplexNoise::noise(scheme_real_to_double(argv[0]),
									scheme_real_to_double(argv[1]),
									scheme_real_to_double(argv[2]));
			break;
		case 4:
			ArgCheck("snoise", "ffff", argc, argv);
			ret=SimplexNoise::noise(scheme_real_to_double(argv[0]),
									scheme_real_to_double(argv[1]),
									scheme_real_to_double(argv[2]),
									scheme_real_to_double(argv[3]));
			break;
		default:
			Trace::Stream<<"snoise - wrong number of arguments"<<endl;
			MZ_GC_UNREG();
			return scheme_make_double(0);
			break;
	}
	MZ_GC_UNREG();
	return scheme_make_double(ret);
}

// StartFunctionDoc-en
// noise real-number ...
// Returns: real-number
// Description:
// Returns the Perlin Noise value at specified coordinates.
// Example:
// (noise 1.0 2.0) ; 2D noise
// (noise 6.1 2.4 .5) ; 3D noise
//
// ; example on a pixel prim
// (clear)
// (with-primitive (build-pixels 100 100)
//     (pdata-index-map!
//         (lambda (i c)
//             (noise (* 0.1 (modulo i (pixels-width)))
//                    (* 0.1 (quotient i (pixels-height)))))
//         "c")
//     (pixels-upload))
// EndFunctionDoc

// StartFunctionDoc-pt
// noise numero-real
// Retorna: numero-real
// Descrição:
// Retorna o valor do Noise Perlin nas coordenadas especificadas.
// Exemplo:
// (noise 1.0 2.0) ; 2D noise
// (noise 6.1 2.4 .5) ; 3D noise
//
// ; example on a pixel prim
// (clear)
// (with-primitive (build-pixels 100 100)
//     (pdata-index-map!
//         (lambda (i c)
//             (noise (* 0.1 (modulo i (pixels-width)))
//                    (* 0.1 (quotient i (pixels-height)))))
//         "c")
//     (pixels-upload))
// EndFunctionDoc

// StartFunctionDoc-fr
// noise réel-nombre ...
// Retour: réel-nombre
// Description:
// Retour une valeur Perlin Noise aux coordonnées spécifiées.
// Exemple:
// (noise 1.0 2.0) ; bruit 2D
// (noise 6.1 2.4 .5) ; bruit 3D
//
// ; Exemple sur une primitive pixel
// (clear)
// (with-primitive (build-pixels 100 100)
//     (pdata-index-map!
//         (lambda (i c)
//             (noise (* 0.1 (modulo i (pixels-width)))
//                    (* 0.1 (quotient i (pixels-height)))))
//         "c")
//     (pixels-upload))
// EndFunctionDoc

Scheme_Object *noise(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	float ret=0;

	switch (argc)
	{
		case 1:
			ArgCheck("noise", "f", argc, argv);
			ret=Noise::noise(scheme_real_to_double(argv[0]));
			break;
		case 2:
			ArgCheck("noise", "ff", argc, argv);
			ret=Noise::noise(scheme_real_to_double(argv[0]),
							 scheme_real_to_double(argv[1]));
			break;
		case 3:
			ArgCheck("noise", "fff", argc, argv);
			ret=Noise::noise(scheme_real_to_double(argv[0]),
							 scheme_real_to_double(argv[1]),
							 scheme_real_to_double(argv[2]));
			break;
		default:
			Trace::Stream<<"noise - wrong number of arguments"<<endl;
			MZ_GC_UNREG();
			return scheme_make_double(0);
			break;
	}
	MZ_GC_UNREG();
	return scheme_make_double(ret);
}

// StartFunctionDoc-en
// noise-seed unsigned-number
// Returns: void
// Description:
// Sets the seed value for noise.
// Example:
// (noise-seed 1)
// EndFunctionDoc

// StartFunctionDoc-pt
// noise-seed numero-unsigned
// Retorna: void
// Descrição:
// Ajusta o valor da semente para o noise.
// Exemplo:
// (noise-seed 1)
// EndFunctionDoc

// StartFunctionDoc-fr
// noise-seed nombre-non-signé
// Retour: vide
// Description:
// Fixe la valeur racine du bruit.
// Exemple:
// (noise-seed 1)
// EndFunctionDoc

Scheme_Object *noise_seed(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("noise-seed", "i", argc, argv);
	Noise::noise_seed((int)scheme_real_to_double(argv[0]));
	MZ_GC_UNREG();
	return scheme_void;
}

// StartFunctionDoc-en
// noise-detail octaves-number falloff-number
// Returns: void
// Description:
// Adjusts the character and level of detail produced by the Perlin noise function.
// Example:
// (noise-detail 4) ; noise with 4 octaves
// (noise-detail 4 .5) ; noise with 4 octaves and .5 falloff
// EndFunctionDoc

// StartFunctionDoc-pt
// noise-detail numeros-oitavos numeros-queda
// Retorna: void
// Descrição:
// Ajusta o caráter e nível de detalhe produzido pelo função noise Perlin.
// Exemplo:
// (noise-detail 4) ; noise with 4 octaves
// (noise-detail 4 .5) ; noise with 4 octaves and .5 falloff
// EndFunctionDoc

// StartFunctionDoc-fr
// noise-detail octaves-nombre atténuation-nombre
// Retour: vide
// Description:
// Ajuste le caractère et le niveau de détail produit par le bruit Perlin.
// Exemple:
// (noise-detail 4) ; bruit de 4 octaves
// (noise-detail 4 .5) ; bruit de 4 octaves et atténuation à .5
// EndFunctionDoc

Scheme_Object *noise_detail(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	switch (argc)
	{
		case 1:
			ArgCheck("noise-detail", "i", argc, argv);
			Noise::noise_detail((int)scheme_real_to_double(argv[0]));
			break;

		case 2:
			ArgCheck("noise-detail", "if", argc, argv);
			Noise::noise_detail((int)scheme_real_to_double(argv[0]),
					scheme_real_to_double(argv[1]));
			break;

		default:
			Trace::Stream<<"noise-detail - wrong number of arguments"<<endl;
			break;
	}
	MZ_GC_UNREG();
	return scheme_void;
}

void MathsFunctions::AddGlobals(Scheme_Env *env)
{
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, env);
	MZ_GC_REG();
	scheme_add_global("vmulc", scheme_make_prim_w_arity(vmulc, "vmulc", 2, 2), env);
	scheme_add_global("vaddc", scheme_make_prim_w_arity(vaddc, "vaddc", 2, 2), env);
	scheme_add_global("vsubc", scheme_make_prim_w_arity(vsubc, "vsubc", 2, 2), env);
	scheme_add_global("vdivc", scheme_make_prim_w_arity(vdivc, "vdivc", 2, 2), env);
	scheme_add_global("vtransform", scheme_make_prim_w_arity(vtransform, "vtransform", 2, 2), env);
	scheme_add_global("vtransform-rot", scheme_make_prim_w_arity(vtransform_rot, "vtransform-rot", 2, 2), env);
	scheme_add_global("vnormalise", scheme_make_prim_w_arity(vnormalise, "vnormalise", 1, 1), env);
	scheme_add_global("vdot", scheme_make_prim_w_arity(vdot, "vdot", 2, 2), env);
	scheme_add_global("vreflect", scheme_make_prim_w_arity(vreflect, "vreflect", 2, 2), env);
	scheme_add_global("vdist", scheme_make_prim_w_arity(vdist, "vdist", 2, 2), env);
	scheme_add_global("vdist-sq", scheme_make_prim_w_arity(vdistsq, "vdist-sq", 2, 2), env);
	scheme_add_global("vmag", scheme_make_prim_w_arity(vmag, "vmag", 1, 1), env);
	scheme_add_global("vcross", scheme_make_prim_w_arity(vcross, "vcross", 2, 2), env);
	scheme_add_global("mmul2", scheme_make_prim_w_arity(mmul, "mmul", 2, 2), env);
	scheme_add_global("madd2", scheme_make_prim_w_arity(madd, "madd", 2, 2), env);
	scheme_add_global("msub2", scheme_make_prim_w_arity(msub, "msuv", 2, 2), env);
	scheme_add_global("mdiv2", scheme_make_prim_w_arity(mdiv, "mdiv", 2, 2), env);
	scheme_add_global("mident", scheme_make_prim_w_arity(mident, "mident", 0, 0), env);
	scheme_add_global("mtranslate", scheme_make_prim_w_arity(mtranslate, "mtranslate", 1, 1), env);
	scheme_add_global("mrotate", scheme_make_prim_w_arity(mrotate, "mrotate", 1, 1), env);
	scheme_add_global("mscale", scheme_make_prim_w_arity(mscale, "mscale", 1, 1), env);
	scheme_add_global("mtranspose", scheme_make_prim_w_arity(mtranspose, "mtranspose", 1, 1), env);
	scheme_add_global("minverse", scheme_make_prim_w_arity(minverse, "minverse", 1, 1), env);
	scheme_add_global("maim", scheme_make_prim_w_arity(maim, "maim", 2, 2), env);
	scheme_add_global("matrix->euler", scheme_make_prim_w_arity(matrix_to_euler, "matrix->euler", 1, 1), env);
	scheme_add_global("qaxisangle", scheme_make_prim_w_arity(qaxisangle, "qaxisangle", 2, 2), env);
	scheme_add_global("qmul", scheme_make_prim_w_arity(qmul, "qmul", 2, 2), env);
	scheme_add_global("qnormalise", scheme_make_prim_w_arity(qnormalise, "qnormalise", 1, 1), env);
	scheme_add_global("qtomatrix", scheme_make_prim_w_arity(qtomatrix, "qtomatrix", 1, 1), env);
	scheme_add_global("qconjugate", scheme_make_prim_w_arity(qconjugate, "qconjugate", 1, 1), env);
	scheme_add_global("fmod", scheme_make_prim_w_arity(fmod, "fmod", 2, 2), env);
	scheme_add_global("snoise", scheme_make_prim_w_arity(snoise, "snoise", 1, 4), env);
	scheme_add_global("noise", scheme_make_prim_w_arity(noise, "noise", 1, 3), env);
	scheme_add_global("noise-seed", scheme_make_prim_w_arity(noise_seed, "noise-seed", 1, 1), env);
	scheme_add_global("noise-detail", scheme_make_prim_w_arity(noise_detail, "noise-detail", 1, 2), env);
	MZ_GC_UNREG();
}
