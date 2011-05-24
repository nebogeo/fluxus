; Copyright (C) 2007 Dave Griffiths
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

; this is the startup script for the fluxus scratchpad
; this script loads all the modules and sets things up so the
; fluxus application works without having to worry about setup


; setup where to find the library module collections

(current-library-collection-paths
	(path-list-string->path-list
         (string-append
		   (or (getenv "PLTCOLLECTS") plt-collects-location)
		   (if (eq? fluxus-platform 'win32) ";" ":")
		   fluxus-collects-location)
         (current-library-collection-paths)))

(define fluxus-name (string-append "fluxus-" fluxus-version))

; now require everything
(require scheme/pretty)
(require fluxus-018/fluxus)

; load the helpmap
(init-help (string-append fluxus-collects-location "/" fluxus-name "/helpmap.scm"))

; set the font for the scratchpad
(define fluxus-scratchpad-font
  (string-append fluxus-data-location "/material/fonts/DejaVuSansMono.ttf"))

; the scratchpad autofocus settings
(define fluxus-scratchpad-do-autofocus 1)
(define fluxus-scratchpad-debug-autofocus 0)
(define fluxus-scratchpad-autofocus-width 70000)
(define fluxus-scratchpad-autofocus-height 50000)
(define fluxus-scratchpad-autofocus-error 5000)
(define fluxus-scratchpad-autofocus-drift 1.0)
(define fluxus-scratchpad-autofocus-scale-drift 1.0)
(define fluxus-scratchpad-autofocus-min-scale 0.4)
(define fluxus-scratchpad-autofocus-max-scale 5.0)
(define fluxus-scratchpad-visible-lines 40)
(define fluxus-scratchpad-visible-columns 80)
(define fluxus-scratchpad-x-pos 0)
(define fluxus-scratchpad-y-pos 85000)
(define fluxus-scratchpad-hide-script #f)
(define fluxus-scratchpad-cursor-colour (vector 1 1 0 .5))

; initial parameterso of scratchpad effects
(define fluxus-scratchpad-effect-jiggle-size 0)

(define fluxus-scratchpad-effect-wave-wavelength 1.0)
(define fluxus-scratchpad-effect-wave-size 0)
(define fluxus-scratchpad-effect-wave-speed 1.0)

(define fluxus-scratchpad-effect-ripple-size 0)
(define fluxus-scratchpad-effect-ripple-center-x 0)
(define fluxus-scratchpad-effect-ripple-center-y 0)
(define fluxus-scratchpad-effect-ripple-wavelength 1.0)
(define fluxus-scratchpad-effect-ripple-speed 1.0)

(define fluxus-scratchpad-effect-swirl-size 0)
(define fluxus-scratchpad-effect-swirl-center-x 0)
(define fluxus-scratchpad-effect-swirl-center-y 0)
(define fluxus-scratchpad-effect-swirl-rotation 1.0)

; setup the standard searchpaths
(set-searchpaths (list
                  "./"
                  (string-append fluxus-data-location "/material/textures/")
                  (string-append fluxus-data-location "/material/shaders/")
                  (string-append fluxus-data-location "/material/meshes/")
				  (string-append fluxus-data-location "/material/fonts/")
				  (string-append fluxus-data-location "/material/samples/")
				  (string-append fluxus-data-location "/plugins/")))

; this part is needed when resetting the interpreter - we need to feed back
; the current screen size from the application into the scheme code
(let ((screen-size (get-screen-size)))
  (fluxus-reshape-callback (inexact->exact (round (vector-ref screen-size 0)))
                           (inexact->exact (round (vector-ref screen-size 1)))))

;-------------------------------------------------
; osx app specific section
; add collects and search path on osx
(when (eq? (system-type) 'macosx)
	(let* ([home-dir (path->string (find-system-path 'home-dir))]
		[fluxus-user-dir (string-append home-dir "Documents/Fluxus/")]
		[fluxus-user-materials (string-append fluxus-user-dir "material/")]
		[fluxus-user-collects (string-append fluxus-user-dir "collects/")])

	(set-searchpaths
		(cons fluxus-user-materials
			(get-searchpaths)))

	(current-library-collection-paths
		(cons
			fluxus-user-collects
			(current-library-collection-paths)))))

;-------------------------------------------------
; here is the hacking section
; todo: remove all below at some point

; override the built in time function for pre 0.12 compatibility
(define time flxtime)

; for compatibility pre 0.13
(define pdata-set pdata-set!)
(define pdata-get pdata-ref)

; for compatibility pre 0.15
(define build-line build-ribbon)

;-------------------------------------------------
; execute the user config script, if it exists

(when (getenv "HOME")
	(let ((user-script (string-append (getenv "HOME") "/.fluxus.scm")))
		(when (file-exists? user-script)
	  		(load user-script))))

