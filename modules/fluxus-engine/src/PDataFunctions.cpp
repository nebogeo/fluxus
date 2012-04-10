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
#include "FluxusEngine.h"

using namespace PDataFunctions;
using namespace SchemeHelper;
using namespace Fluxus;

// StartSectionDoc-en
// primitive-data
// Primitive data (pdata for short) is fluxus' name for data which comprises primitives. In polygon primitives this means 
// the vertex information, in particle primitives it corresponds to the particle information, in NURBS primitives it's 
// the control vertices. Access to pdata gives you the ability to use primitives which are otherwise not very 
// interesting, and deform and shape other primitives to give much more detailed models and animations. You can also add
// your own pdata, which is treated exactly like the built in types. Primitive data is named by type strings, the names of
// which depend on the sort of primitive. All pdata commands operate on the currently grabbed primitive.
// Example:
// ; a function to deform the points of an object
// (define (deform n)
//     (pdata-set! "p" n (vadd  (pdata-ref "p" n)                ; the original point, plus
//         (vmul (vector (flxrnd) (flxrnd) (flxrnd)) 0.1)))     ; a small random vector
//     (if (zero? n)
//         0
//         (deform (- n 1))))
//     
// (hint-unlit) ; set some render settings to
// (hint-wire)  ; make things easier to see
// (line-width 4)
// (define myobj (build-sphere 10 10)) ; make a sphere
// (grab myobj)
// (deform (pdata-size)) ; deform it
// (ungrab)
// EndSectionDoc 

// StartSectionDoc-pt
// dados-primitivos
// Dados primitivos (pdata para diminuir) é o nome fluxus' para dados
// que formam primitivas. Em primitivas poligonais isto significa
// informação dos vértices, em primitivas de partículas isto
// corresponde a informação da partícula, em primitivas NURBS são os
// vértices de controle. Acesso a pdata dá a você a habilidade de usar
// primitivas que de outra forma não seria tão interessante, e
// deformar e fazer outras primitivas resultando em modelos muito mais
// detalhados e animações. Você pode também adicionar sua própria
// pdata, que é tratado exatamente como os tipos já existentes. Dados
// primitivos são nomeados por strings de tipo, os nomes de qual
// depende a ordem da primitiva. Todos os comandos Pdata operam na
// primitiva atualmente pega [grabbed].
// Exemplo:
// ; uma função para deformar os pontos de um objeto
// (define (deform n)
//     (pdata-set! "p" n (vadd  (pdata-ref "p" n)                ; o ponto original, mais
//         (vmul (vector (flxrnd) (flxrnd) (flxrnd)) 0.1)))     ; um pequeno vetor randomico
//     (if (zero? n)
//         0
//         (deform (- n 1))))
//     
// (hint-unlit) ; ajustar algumas opções de render
// (hint-wire)  ; para fazer as coisas mais fáceis de ver
// (line-width 4)
// (define myobj (build-sphere 10 10)) ; fazer uma esfera
// (grab myobj)
// (deform (pdata-size)) ; deformá-la
// (ungrab)
// EndSectionDoc

// StartSectionDoc-fr
// primitive-data
// Les Données Primitives (raccourcient en "pdata") est l'appellation fluxus des données dont sont composées les primitives.
// Dans les primitives polygones, celà revient aux informations des vertex; dans les primtives particules, celà correspond
// aux informationx des particules; dans les primitives NURBS elles controlent les arrêtes. Accéder aux pdata donnent accès aux
// primitives, qui sont sinon pas très interresantes, et permet de déformer et façonner les autres primitives pour donner plus
// de détails aux modèles et animation. Vous pouvez également ajouter vos propres pdata, qui sont traitées comme celles inclusent.
// Les pdata sont nommées par chaînes de caractères, les noms dépendent du type de primitive. Toutes les opérations pdata opèrent
// sur la primitive actuellement accrochée.
// Exemple:
// ; une fonction qui déforme les points d'un objet
// (define (deform n)
//     (pdata-set! "p" n (vadd  (pdata-ref "p" n)                ; le point original,the original point, plus
//         (vmul (vector (flxrnd) (flxrnd) (flxrnd)) 0.1)))     ; plus un petit vecteur aléatoire
//     (if (zero? n)
//         0
//         (deform (- n 1))))
//     
// (hint-unlit) ; déclarer quelques options de rendu
// (hint-wire)  ; pour améliorer la visibilité
// (line-width 4)
// (define myobj (build-sphere 10 10)) ; construire une sphère
// (grab myobj)
// (deform (pdata-size)) ; la déformer
// (ungrab)
// EndSectionDoc 

// StartFunctionDoc-en
// pdata-ref type-string index-number
// Returns: value-vector/colour/matrix/number
// Description:
// Returns the corresponding pdata element.
// Example:
// (pdata-ref "p" 1)
// EndFunctionDoc

// StartFunctionDoc-pt
// pdata-ref string-tipo número-index
// Retorna: vetor-valor/cor/matriz/número
// Descrição:
// Retorna o elemento pdata correspondente.
// Exemplo:
// (pdata-ref "p" 1)
// EndFunctionDoc

// StartFunctionDoc-fr
// pdata-ref type-chaîne-de-caractères index-nombre
// Retour: valeur-vecteur/couleur/matrice/nombre
// Description:
// Retourne l'élément pdata correspondant.
// Exemple:
// (pdata-ref "p" 1)
// EndFunctionDoc

Scheme_Object *pdata_ref(int argc, Scheme_Object **argv)
{
	Scheme_Object *ret=NULL;
	
	MZ_GC_DECL_REG(2);
	MZ_GC_VAR_IN_REG(0, argv);
	MZ_GC_VAR_IN_REG(1, ret);
	MZ_GC_REG();	
	ArgCheck("pdata-ref", "si", argc, argv);		
	
	Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();    
	if (Grabbed) 
	{
		string name=StringFromScheme(argv[0]);
		unsigned int index=IntFromScheme(argv[1]);
		unsigned int size=0;
		char type;
		
		if (Grabbed->GetDataInfo(name,type,size))
		{
			if (type=='f')	
			{
				ret=scheme_make_double(Grabbed->GetData<float>(name,index%size)); 
			}
			else if (type=='v')	
			{
				ret=FloatsToScheme(Grabbed->GetData<dVector>(name,index%size).arr(),3); 
			}
			else if (type=='c')	
			{
				ret=FloatsToScheme(Grabbed->GetData<dColour>(name,index%size).arr(),4); 
			}
			else if (type=='m')	
			{
				ret=FloatsToScheme(Grabbed->GetData<dMatrix>(name,index%size).arr(),16); 
			}
			else
			{
				// this output causes fluxus to lock up with primitives 
				// with tens of thousands of pdata elements
				//Trace::Stream<<"unknown pdata type ["<<type<<"]"<<endl;
			}
		
		}
		
		if (ret==NULL)
		{
			// this output causes fluxus to lock up with primitives 
			// with tens of thousands of pdata elements
			//Trace::Stream<<"could not find pdata called ["<<name<<"]"<<endl;
  			MZ_GC_UNREG();
			return scheme_make_double(0);
		}
		
  		MZ_GC_UNREG();
		return ret;
	}			
	
	Trace::Stream<<"pdata-get called without an objected being grabbed"<<endl;
  	MZ_GC_UNREG();
    return scheme_void;
}

// StartFunctionDoc-en
// pdata-set! type-string index-number value-vector/colour/matrix/number
// Returns: void
// Description:
// Writes to the corresponding pdata element.
// Example:
// (pdata-set! "p" 1 (vector 0 100 0))
// EndFunctionDoc

// StartFunctionDoc-pt
// pdata-set! string-tipo número-index vetor-valor/cor/matriz/número
// Retorna: void
// Descrição:
// Escreve ao elemento pdata correspondente.
// Exemplo:
// (pdata-set! "p" 1 (vector 0 100 0))
// EndFunctionDoc

// StartFunctionDoc-fr
// pdata-set! type-chaîne-de-caractères index-nomvre valeur-vecteur/couleur/matrice/nombre
// Retour: vide
// Description:
// Ecris dans l'élément pdata correspondant.
// Exemple:
// (pdata-set! "p" 1 (vector 0 100 0))
// EndFunctionDoc

Scheme_Object *pdata_set(int argc, Scheme_Object **argv)
{
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, argv);
	MZ_GC_REG();
	ArgCheck("pdata-set!", "si?", argc, argv);
    Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();
	if (Grabbed)
	{
		size_t ssize=0;
		string name=StringFromScheme(argv[0]);
		unsigned int index=IntFromScheme(argv[1]);
		unsigned int size;
		char type;

		if (Grabbed->GetDataInfo(name,type,size))
		{
			if (type=='f')
			{
				if (SCHEME_NUMBERP(argv[2])) Grabbed->SetData<float>(name,index%size,FloatFromScheme(argv[2]));
				else Trace::Stream<<"expected number value in pdata-set"<<endl;
			}
			else if (type=='v')
			{
				if (SCHEME_VECTORP(argv[2]) && SCHEME_VEC_SIZE(argv[2])==3)
				{
					dVector v;
					FloatsFromScheme(argv[2],v.arr(),3);
					Grabbed->SetData<dVector>(name,index%size,v);
				}
				else if (name.compare("s")==0) // one value scale
				{
					if (SCHEME_NUMBERP(argv[2]))
					{
						float t=FloatFromScheme(argv[2]);
						dVector v(t,t,t);
						Grabbed->SetData<dVector>(name,index%size,v);
					}
					else Trace::Stream<<"expected number or vector (size 3) value in pdata-set"<<endl;
				}
				else Trace::Stream<<"expected vector (size 3) value in pdata-set"<<endl;
			}
			else if (type=='c')
			{
				ArgCheck("pdata-set!", "c", 1, &argv[2]);
				dColour c=ColourFromScheme(argv[2],Grabbed->GetState()->ColourMode);
				Grabbed->SetData<dColour>(name,index%size,c);
				/*
				if (SCHEME_VECTORP(argv[2]) && SCHEME_VEC_SIZE(argv[2])>=3 && SCHEME_VEC_SIZE(argv[2])<=4)
				{
					dColour c;
					if (SCHEME_VEC_SIZE(argv[2])==3) FloatsFromScheme(argv[2],c.arr(),3);
					else FloatsFromScheme(argv[2],c.arr(),4);
					if (Grabbed->GetState()->ColourMode==MODE_HSV)
					{
						c = c.HSVtoRGB();
					}

					Grabbed->SetData<dColour>(name,index%size,c);
				}
				else Trace::Stream<<"expected colour vector (size 3 or 4) value in pdata-set"<<endl;
				*/
			}
			else if (type=='m')
			{
				if (SCHEME_VECTORP(argv[2]) && SCHEME_VEC_SIZE(argv[2])==16)
				{
					dMatrix m;
					FloatsFromScheme(argv[2],m.arr(),16);
					Grabbed->SetData<dMatrix>(name,index%size,m);
				}
				else Trace::Stream<<"expected matrix vector (size 16) value in pdata-set"<<endl;
			}
		}
	}
	MZ_GC_UNREG();
    return scheme_void;
}

// StartFunctionDoc-en
// pdata-add name-string type-string
// Returns: void
// Description:
// Adds a new user pdata array. Type is one of "v":vector, "c":colour, "f":float or "m":matrix.
// Example:
// (pdata-add "mydata" "v")
// (pdata-set "mydata" 0 (vector 1 2 3))
// EndFunctionDoc

// StartFunctionDoc-pt
// pdata-add nome-string string-tipo
// Retorna: void
// Descrição:
// Adiciona uma nova array de pdata do usuario. Tipo é um dos
// sequintes "v":vector, "c":colour, "f":float ou "m":matrix.
// Exemplo:
// (pdata-add "mydata" "v")
// (pdata-set "mydata" 0 (vector 1 2 3))
// EndFunctionDoc

// StartFunctionDoc-fr
// pdata-add nom-chaîne-de-caractères type-chaîne-de-caractères
// Retour: vide
// Description:
// Ajoute un nouveau tableau de pdata utilisateur. Le type peut être "v":vecteur, "c":couleur, "f":flottant or "m":matrice.
// Exemple:
// (pdata-add "mydata" "v")
// (pdata-set "mydata" 0 (vector 1 2 3))
// EndFunctionDoc

Scheme_Object *pdata_add(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("pdata-add", "ss", argc, argv);			
    Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();    
	if (Grabbed) 
	{
		string names=StringFromScheme(argv[0]);
		string types=StringFromScheme(argv[1]);
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
			default : Trace::Stream<<"pdata-new: unknown type "<<types[0]<<endl; break;
		}
		
		if (ptr)
		{
			Grabbed->AddData(names,ptr);
		}
	}
	MZ_GC_UNREG(); 
	
	return scheme_void;
}

// StartFunctionDoc-en
// pdata-exists? name-string
// Returns: void
// Description:
// Returns true if the pdata array exists on the primitive
// Example:
// (with-primitive (build-cube)
//   (when (pdata-exists? "p") 
//     (display "we have positions!") (newline))
//   (pdata-add "myarray" "v")
//   (when (pdata-exists? "myarray") 
//     (display "we have myarray!") (newline)))
// EndFunctionDoc

// StartFunctionDoc-pt
// pdata-exists? string-nome
// Retorna: void
// Descrição:
// Retorna verdadeiro se a array pdata existe na primitiva.
// Exemplo:
// (with-primitive (build-cube)
//   (when (pdata-exists? "p") 
//     (display "we have positions!") (newline)))
//   (pdata-add "myarray" "v")
//   (when (pdata-exists? "myarray") 
//     (display "we have myarray!") (newline))))
// EndFunctionDoc

// StartFunctionDoc-fr
// pdata-exists? nom-chaîne-de-caractères
// Retour: vide
// Description:
// Retourne vrai, si le tableau de pdata existe sur la primitive.
// Exemple:
// (with-primitive (build-cube)
//   (when (pdata-exists? "p") 
//     (display "we have positions!") (newline))
//   (pdata-add "myarray" "v")
//   (when (pdata-exists? "myarray") 
//     (display "we have myarray!") (newline)))
// EndFunctionDoc

Scheme_Object *pdata_exists(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("pdata-exists?", "s", argc, argv);			
    Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();    
	if (Grabbed) 
	{
		string name=StringFromScheme(argv[0]);
		char type=0;
		unsigned int size=0;
		if (Grabbed->GetDataInfo(name, type, size))
		{
			MZ_GC_UNREG(); 
			return scheme_true;
		}
		
	}
	MZ_GC_UNREG(); 
	return scheme_false;
}

// TODO: add more type descriptions down here
// StartFunctionDoc-en
// pdata-names 
// Returns: void
// Description:
// Returns a list of the names of the pdata arrays for this primitive.
// t -> texture
// p -> position
// c -> color
// Example:
// (with-primitive (build-cube)
//     (display (pdata-names)) (newline))
// EndFunctionDoc

// StartFunctionDoc-pt
// pdata-names
// Retorna: void
// Descrição:
// Retorna uma lista com nomes das arrays de pdata para esta primitiva
// t -> texture
// p -> position
// c -> color
// Exemplo:
// (with-primitive (build-cube)
//     (display (pdata-names)) (newline))
// EndFunctionDoc

// StartFunctionDoc-fr
// pdata-names 
// Retour: vide
// Description:
// Retourne la liste des noms des tableaux pdata pour la primitive.
// t -> texture
// p -> position
// c -> couleur
// Exemple:
// (with-primitive (build-cube)
//     (display (pdata-names)) (newline))
// EndFunctionDoc

Scheme_Object *pdata_names(int argc, Scheme_Object **argv)
{
	Scheme_Object *l = NULL;
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, l);
	MZ_GC_REG();

	Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();
	if (Grabbed)
	{		
		vector<string> names;
		Grabbed->GetDataNames(names);	
		l = scheme_null;
		
		for(vector<string>::iterator i=names.begin(); i!=names.end(); ++i)
		{
			l=scheme_make_pair(scheme_make_utf8_string(i->c_str()),l);
		}
		
		MZ_GC_UNREG();
		return l;
	}

	MZ_GC_UNREG();
    return scheme_void;
}



// StartFunctionDoc-en
// pdata-op funcname-string pdataname-string operator
// Returns: void
// Description:
// This is an experimental feature allowing you to do operations on pdata very quickly,
// for instance adding element for element one array of pdata to another. You can 
// implement this in Scheme as a loop over each element, but this is slow as the 
// interpreter is doing all the work. It's much faster if you can use a pdata-op as
// the same operation will only be one Scheme call.
// Example:
// (clear)
// (define t (build-torus 1 4 10 10))
// 
// (with-primitive t
//     (pdata-op "+" "p" (vector 1 0 0))  ; add a vector to all the pdata vectors
//     (pdata-op "+" "p" "n")  ; add two pdata vectors element for element
//     (pdata-op "*" "n" (vector -1 -1 -1)) ;  multiply a vector to all the pdata vectors
//     (pdata-op "*" "n" "p")  ; multiply two pdata vectors element for element
//     (let ((pos (pdata-op "closest" "p" (vector 100 0 0)))) ;  returns position of the closest vertex to this point
//         (with-state ; draw a sphere there
//             (translate pos)
//             (scale (vector 0.1 0.1 0.1))
//             (build-sphere 5 5)))
//     ; can't think of a good example for these...
//     ;(pdata-op "sin" "mydata" "myotherdata")  ; sine of one float pdata to another
//     ;(pdata-op "cos" "mydata" "myotherdata")  ; cosine of one float pdata to another
//     )
// 
// ; most common example of pdata op is for particles
// (define p (with-state
//     (hint-points)
//     (point-width 10)
//     (build-particles 100)))
// 
// (with-primitive p
//     (pdata-add "vel" "v") ; add a velocity vector
//     (pdata-map!
//         (lambda (vel)
//             (srndvec)) ; set random velocities
//         "vel")
//     (pdata-map!
//         (lambda (c)
//             (rndvec)) ; set random colours
//         "c"))
// 
// (every-frame (with-primitive p
//     (pdata-op "+" "p" "vel"))) 
// EndFunctionDoc

// StartFunctionDoc-pt
// pdata-op string-nomefunc string-nomepdata operador
// Retorna: void
// Descrição:
// Esta é uma função experimental que permite a você fazer operações
// na pdata muito rapidamente, para constar adicionar elemento por
// elemento de uma array de pdata para outra. Você pode implementar
// isto em scheme como um loop sobre cada elemento, mas isto é devagar
// já que o interpretador está fazendo todo o trabalho. É muito mais
// rápido se você puder usar um pdata-op já que a mesma operação vai
// ser apenas uma chamada à scheme.
// Exemplo:
// (clear)
// (define t (build-torus 1 4 10 10))
// 
// (with-primitive t
//     (pdata-op "+" "p" (vector 1 0 0))  ; add a vector to all the pdata vectors
//     (pdata-op "+" "p" "n")  ; add two pdata vectors element for element
//     (pdata-op "*" "n" (vector -1 -1 -1)) ;  multiply a vector to all the pdata vectors
//     (pdata-op "*" "n" "p")  ; multiply two pdata vectors element for element
//     (let ((pos (pdata-op "closest" "p" (vector 100 0 0)))) ;  returns position of the closest vertex to this point
//         (with-state ; draw a sphere there
//             (translate pos)
//             (scale (vector 0.1 0.1 0.1))
//             (build-sphere 5 5)))
//     ; can't think of a good example for these...
//     ;(pdata-op "sin" "mydata" "myotherdata")  ; sine of one float pdata to another
//     ;(pdata-op "cos" "mydata" "myotherdata")  ; cosine of one float pdata to another
//     )
// 
// ; most common example of pdata op is for particles
// (define p (with-state
//     (hint-points)
//     (point-width 10)
//     (build-particles 100)))
// 
// (with-primitive p
//     (pdata-add "vel" "v") ; add a velocity vector
//     (pdata-map!
//         (lambda (vel)
//             (srndvec)) ; set random velocities
//         "vel")
//     (pdata-map!
//         (lambda (c)
//             (rndvec)) ; set random colours
//         "c"))
// 
// (every-frame (with-primitive p
//     (pdata-op "+" "p" "vel")))
// EndFunctionDoc

// StartFunctionDoc-fr
// pdata-op nomfonction-chaîne-de-caractères nompdata-chaîne-de-caractères operateur
// Retour: vide
// Description:
// Ceci est une fonctionnalité expérimentale vous permettant des opérations sur les pdata très rapidement.
// Par exemple, ajouter un élément d'un tableau à un autre. Vous pouvez l'implément
// en Scheme en boucle sur chaque élément, mais cette méthode est lente puisque l'interpréteur
// fait tout le travail. Il est bien plus rapide d'utiliser les pdata-op puisque la même
// opération est faite en un unique appel Scheme.
// Exemple:
// (clear)
// (define t (build-torus 1 4 10 10))
// 
// (with-primitive t
//     (pdata-op "+" "p" (vector 1 0 0))  ; ajoute un vecteur à chaque pdata vecteur
//     (pdata-op "+" "p" "n")  ; ajoute deux pdata vecteurs à chaque éléments
//     (pdata-op "*" "n" (vector -1 -1 -1)) ;  multiplie un vecteur à tous les vecteurs pdata
//     (pdata-op "*" "n" "p")  ; multiplie deux pdata vecteurs aux éléments
//     (let ((pos (pdata-op "closest" "p" (vector 100 0 0)))) ;  retourne la position du vertex le proche de ce point
//         (with-state ; dessine une sphère
//             (translate pos)
//             (scale (vector 0.1 0.1 0.1))
//             (build-sphere 5 5)))
//     ;(pdata-op "sin" "mydata" "myotherdata")  ; sinus d'un flottant pdata dans un autre
//     ;(pdata-op "cos" "mydata" "myotherdata")  ; cosinus d'un flottant pdata dans un autre
//     )
// 
// Exemple le plus commun d'opération pdata sur des particules
// (define p (with-state
//     (hint-points)
//     (point-width 10)
//     (build-particles 100)))
// 
// (with-primitive p
//     (pdata-add "vel" "v") ; ajoute un vecteur de vélocité
//     (pdata-map!
//         (lambda (vel)
//             (srndvec)) ; détermine des vélocités aléatoires
//         "vel")
//     (pdata-map!
//         (lambda (c)
//             (rndvec)) ; détermine des couleurs alétoires
//         "c"))
// 
// (every-frame (with-primitive p
//     (pdata-op "+" "p" "vel"))) 
// EndFunctionDoc

Scheme_Object *pdata_op(int argc, Scheme_Object **argv)
{
	DECL_ARGV(); 
	ArgCheck("pdata-op", "ss?", argc, argv);			
    PData *ret=NULL;
	
    Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();    
	if (Grabbed) 
	{
		string op=StringFromScheme(argv[0]);
		string pd=StringFromScheme(argv[1]);
		
		// find out what the inputs are, and call the corresponding function
		if (SCHEME_CHAR_STRINGP(argv[2]))
		{
			string operand=StringFromScheme(argv[2]);
			
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
			MZ_GC_UNREG();
			return FloatsToScheme(r.arr(),3);
		}
		else
		{
			TypedPData<dColour> *data = dynamic_cast<TypedPData<dColour>*>(ret);
			if (data) 
			{
				dColour r = data->m_Data[0];
				delete ret;
				MZ_GC_UNREG();
				return FloatsToScheme(r.arr(),4);
			}
			else 
			{
				TypedPData<float> *data = dynamic_cast<TypedPData<float>*>(ret);
				if (data) 
				{		
					float r = data->m_Data[0];
					delete ret;
					MZ_GC_UNREG();
					return scheme_make_double(r);
				}
				else 
				{
					TypedPData<dMatrix> *data = dynamic_cast<TypedPData<dMatrix>*>(ret);
					if (data) 
					{
						dMatrix r = data->m_Data[0];
						delete ret;
						MZ_GC_UNREG();
						return FloatsToScheme(r.arr(),16);
					}
				}
			}
		}
	}
	MZ_GC_UNREG();
	
	return scheme_void;
}

// StartFunctionDoc-en
// pdata-copy pdatafrom-string pdatato-string
// Returns: void
// Description:
// Copies the contents of one pdata array to another. Arrays must match types.
// Example:
// (pdata-copy "p" "mydata") ; copy the vertex positions to a user array
// EndFunctionDoc

// StartFunctionDoc-pt
// pdata-copy string-pdata-de string-pdata-para
// Retorna: void
// Descrição:
// Copia o conteúdo de uma array pdata para outra. As arrays tem que
// ser do mesmo tipo.
// Exemplo:
// (pdata-copy "p" "mydata") ; copia as posições de vértices para uma array do usuário
// EndFunctionDoc


// StartFunctionDoc-fr
// pdata-copy originepdata-chaîne-de-caractères ciblepdata-chaîne-de-caractères
// Retour: vide
// Description:
// Copie le contenu d'un tableau pdata vers un autre. Les tableaux doivent être de même types.
// Exemple:
// (pdata-copy "p" "mydata") ; copie les positions des vertex vers un tableau utilisateur
// EndFunctionDoc

Scheme_Object *pdata_copy(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
 	ArgCheck("pdata-copy", "ss", argc, argv);			
  	
	Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();    
	if (Grabbed) 
	{
		string source=StringFromScheme(argv[0]);
		string dest=StringFromScheme(argv[1]);
		Grabbed->CopyData(source,dest);
	}
	MZ_GC_UNREG(); 	
	return scheme_void;
}

// StartFunctionDoc-en
// pdata-size 
// Returns: count-number
// Description:
// Returns the size of the pdata arrays (they must all be the same). This is mainly 
// used for iterating over the arrays.
// Example:
// (define (mashup n)
//     (pdata-set "p" n (vector (flxrnd) (flxrnd) (flxrnd))) ; randomise the vertex position
//     (if (zero? n)
//         0
//         (mashup (- n 1)))) ; loops till n is 0
//
// (define shape (build-sphere 10 10))
// (grab shape)
// (mashup (pdata-size)) ; randomise verts on currently grabbed primitive
// (ungrab)
// EndFunctionDoc

// StartFunctionDoc-pt
// pdata-size
// Retorna: número-contador
// Descrição:
// Retorna o tamanho das arrays pdata (elas precisam ser todas a
// mesma). Isto é principalmente para iterar através das arrays.
// Exemplo:
// (define (mashup n)
//     (pdata-set "p" n (vector (flxrnd) (flxrnd) (flxrnd))) ; randomise the vertex position
//     (if (zero? n)
//         0
//         (mashup (- n 1)))) ; loops till n is 0
//
// (define shape (build-sphere 10 10))
// (grab shape)
// (mashup (pdata-size)) ; randomise verts on currently grabbed primitive
// (ungrab)
// EndFunctionDoc

// StartFunctionDoc-fr
// pdata-size 
// Retour: compte-nombre
// Description:
// Retourne la taille du tableau pdata (ils doivent normalement être tous égaux).
// Ceci est principalement utilisé pour le parcours des tableaux.
// Exemple:
// (define (mashup n)
//     (pdata-set "p" n (vector (flxrnd) (flxrnd) (flxrnd))) ; Position aléatoire des vertex
//     (if (zero? n)
//         0
//         (mashup (- n 1)))) ; loops till n is 0
//
// (define shape (build-sphere 10 10))
// (grab shape)
// (mashup (pdata-size)) ; disperse aléatoirement les vertex de la primitive actuellement attachée
// (ungrab)
// EndFunctionDoc

Scheme_Object *pdata_size(int argc, Scheme_Object **argv)
{
    Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();    
	if (Grabbed) 
	{
		return scheme_make_integer_value(Grabbed->Size());
	}
    return scheme_void;
}

// StartFunctionDoc-en
// recalc-normals smoothornot-number
// Returns: void
// Description:
// For polygon primitives only. Looks at the vertex positions and calculates the lighting normals for you 
// automatically. Call with "1" for smooth normals, "0" for faceted normals.
// Example:
// (define shape (build-sphere 10 10)) ; build a sphere (which is smooth by default)
// (grab shape)
// (recalc-normals 0) ; make the sphere faceted
// (ungrab)
// EndFunctionDoc

// StartFunctionDoc-pt
// recalc-normals número-macio-ou-não
// Retorna: void
// Descrição:
// Para primitivas poligonais apenas. Olha a posição dos vértices e
// cálcula as normais da luz pra você automaticamente. Chame com "1"
// para normais macias, "0" para normais facetadas.
// Exemplo:
// (define shape (build-sphere 10 10)) ; build a sphere (which is smooth by default)
// (grab shape)
// (recalc-normals 0) ; make the sphere faceted
// (ungrab)
// EndFunctionDoc

// StartFunctionDoc-fr
// recalc-normals lissageounon-nombre
// Retour: vide
// Description:
// Pour les primitives polygones uniquement. Etudie la position des vertex et calcule les normales d'éclairage
// automatiquement. Appellé avec "1" pour les normales lissées, "0" pour des normales en facettes.
// Exemple:
// (define shape (build-sphere 10 10)) ; construit une sphère (lissée par défaut)
// (grab shape)
// (recalc-normals 0) ; rend la sphère en facettes
// (ungrab)
// EndFunctionDoc

Scheme_Object *recalc_normals(int argc, Scheme_Object **argv)
{
 	DECL_ARGV();
	ArgCheck("recalc-normals", "i", argc, argv);			
	Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();    
	if (Grabbed) Grabbed->RecalculateNormals(IntFromScheme(argv[0]));
	MZ_GC_UNREG(); 
	return scheme_void;
}

void PDataFunctions::AddGlobals(Scheme_Env *env)
{	
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, env);
	MZ_GC_REG();
	scheme_add_global("pdata-ref", scheme_make_prim_w_arity(pdata_ref, "pdata-ref", 2, 2), env);
	scheme_add_global("pdata-set!", scheme_make_prim_w_arity(pdata_set, "pdata-set!", 3, 3), env);
	scheme_add_global("pdata-add", scheme_make_prim_w_arity(pdata_add, "pdata-add", 2, 2), env);
	scheme_add_global("pdata-exists?", scheme_make_prim_w_arity(pdata_exists, "pdata-exists?", 1, 1), env);
	scheme_add_global("pdata-names", scheme_make_prim_w_arity(pdata_names, "pdata-names", 0, 0), env);
	scheme_add_global("pdata-op", scheme_make_prim_w_arity(pdata_op, "pdata-op", 3, 3), env);
	scheme_add_global("pdata-copy", scheme_make_prim_w_arity(pdata_copy, "pdata-copy", 2, 2), env);
	scheme_add_global("pdata-size", scheme_make_prim_w_arity(pdata_size, "pdata-size", 0, 0), env);
	scheme_add_global("recalc-normals", scheme_make_prim_w_arity(recalc_normals, "recalc-normals", 1, 1), env);
 	MZ_GC_UNREG(); 
}
