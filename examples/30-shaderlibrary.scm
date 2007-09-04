; The Fluxus GLSL Shader Library
; ------------------------------
; 
; This collection is a basic set of standalone shaders for you to use 
; or modify to suit your needs. These shaders don't have any fluxus
; specific code, so they should be suitable for use in any application 
; supporting GLSL.
; 
; They are currently very much inspired by illumination models used for
; general renderman shaders. There is no texturing support yet, this 
; will be added later, probably along with some more interesting NPR 
; rendering shaders.
; 
; These shaders are kept in the materials directory - installed as
; part of fluxus which will be somewhere like:
; /usr/local/lib/plt/collects/fluxus-0.13/materials/shaders

(clear)
(clear-colour (vector 0.6 0.2 0.1))
(fluxus-init)
(show-fps 0)
(show-axis 0)

; set up a light (using fixed and GLSL lights)
(define lp (vector -50 50 50))
(define l (make-light 'point 'free))
(light-diffuse l (vector 1 1 1))
(light-position l lp)
(shadow-light l)

; takes a shader pair and a bunch of params and 
; renders a torus with the specified shader attached
(define (test-shader vert frag params)
    (push)
    (hint-cast-shadow)
    (shader vert frag)
    (scale (vector 1 1 1))
    (rotate (vector 90 0 0))
    (let ((b (build-torus 1 2 20 20)))
    (pop)
    (translate (vector 0 0 6))
    (grab b)
    (shader-set! params)
    (shader-set! (list "LightPos" 
        (vtransform lp (minverse (get-transform)))))
    (ungrab)))

(push)
; a big list of shaders and example settings

; Anisotropic Specular Reflection Shader
; This shader is useful for depicting surfaces
; such as velvet or brushed metal, as it allows
; you to stretch the highlight along the 
; SpecDirection vector (in object space)
; (ps. I think this might be buggy)
(test-shader "aniso.vert.glsl" "aniso.frag.glsl"
    (list "LightPos" lp
          "AmbientColour" (vector 0.1 0.1 0.1)
          "DiffuseColour" (vector 1.0 0.5 0.1)
          "SpecularColour" (vector 1 1 1)
          "Roughness" 0.1
          "AnisoRoughness" 0.1
          "AmbientIntensity" 0.0    
          "DiffuseIntensity" 1.0    
          "SpecularIntensity" 1.0    
          "SpecDirection" (vector 0 1 0)))

; Blinn/Phong Shader
; This is the standard per-fragment lighting
; shading model
(test-shader "blinn.vert.glsl" "blinn.frag.glsl"
    (list 
          "AmbientColour" (vector 0.1 0.1 0.1)
          "DiffuseColour" (vector 1 0 1)
          "SpecularColour" (vector 1 1 1)
          "Roughness" 0.01
          "AmbientIntensity" 1.0    
          "DiffuseIntensity" 1.0    
          "SpecularIntensity" 1.0))

; Toon (Cel) NPR Shader
; Standard toon model with configurable
; colours for the different shading areas
(test-shader "toon.vert.glsl" "toon.frag.glsl"
    (list "LightPos" lp
          "HighlightColour" (vector 0.6 0.9 0.6 1)
          "MidColour" (vector 0 0 1 1)
          "ShadowColour" (vector 0 0 0.4 1)
          "HighlightSize" 0.1
          "ShadowSize" 0.2
          "OutlineWidth" 0.4))

; BadPrint NPR Shader
; This shader tries to emulate the effect
; of a bad printing process. Can be controlled
; with different settings for RGB
(test-shader "badprint.vert.glsl" "badprint.frag.glsl"
    (list "LightPos" lp
          "Size"     (vector 200 200 200)
          "Scale"    (vector 0.3 0.3 0.3)
          "Offset"   (vector 0.5 0.5 0.5)
          "Register" (vector 0 0 0)))

; Facing Ratio Shader
; Blends from inner to outer colour depending
; on the facing ratio of the fragment. Useful for
; getting the scanning electron microscope look.    
(test-shader "facingratio.vert.glsl" "facingratio.frag.glsl"
    (list "InnerColour" (vector 0 0.2 0 1)
          "OuterColour" (vector 0 0.5 1 1)))

(translate (vector 6 0 -30))

; Gooch NPR Shading Model
; Orginally for technical drawing style 
; rendering, uses warm and cool colours
; to depict shading to keep detail in the
; shadowed areas
(test-shader "gooch.vert.glsl" "gooch.frag.glsl"
     (list "LightPos" lp
           "DiffuseColour" (vector 1 1 1)
           "WarmColour" (vector 0.6 0.6 0)
           "CoolColour" (vector 0 0 0.6)
           "OutlineWidth" 0.4))
           
; Lambertian Shading Model
; Simple diffuse only shader
(test-shader "lambert.vert.glsl" "lambert.frag.glsl"
     (list "LightPos" lp
           "Tint" (vector 0 1 1)))
 
; Wood Shader
; Cheezy procedural wood grain shader         
(test-shader "wood.vert.glsl" "wood.frag.glsl"
    (list "LightPos" lp
          "AmbientColour" (vector 0.1 0.1 0.1)
          "DiffuseColour1" (vector 0.8 0.4 0.1)
          "DiffuseColour2" (vector 0.5 0.1 0.1)
          "SpecularColour" (vector 1 1 1)
          "Roughness" 0.06
          "AmbientIntensity" 1.0    
          "DiffuseIntensity" 1.0    
          "SpecularIntensity1" 0.5  
          "SpecularIntensity2" 1.0  
          "GrainMult" 50.0
          "GrainCentre" (vector 0 2 3)))
    
; Wrapped Diffuse Shader
; A diffuse lighting model which allows you 
; to change the shading to approximate (very
; approximately) global illumination.
(test-shader "wrapped.vert.glsl" "wrapped.frag.glsl"
    (list "LightPos" lp
          "Tint" (vector 0 1 1)
          "WrapAngle" 3.0))
      
; Glossy Specular Reflection Shader
; A more controllable version of blinn shading,
; Useful for ceramic or fluids - from Advanced 
; Renderman, thanks to Larry Gritz
(test-shader "glossy.vert.glsl" "glossy.frag.glsl"
    (list "LightPos" (vector -30 40 50)
          "AmbientColour" (vector 0.1 0.1 0.1)
          "DiffuseColour" (vector 1 0 0)
          "SpecularColour" (vector 1 1 1)
          "Roughness" 0.05
          "AmbientIntensity" 1.0    
          "DiffuseIntensity" 1.0    
          "SpecularIntensity" 1.0    
          "Sharpness" 0.6))
(pop)

(push)
(colour (vector 0.4 0.4 0.9))
(translate (vector 0 -1 0))
(rotate (vector 90 0 0))
(scale (vector 100 100 100))
(build-plane)
(pop)
