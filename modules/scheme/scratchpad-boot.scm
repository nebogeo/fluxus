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
		(or (getenv "PLTCOLLECTS") fluxus-collects-location)
		(current-library-collection-paths)))

(define fluxus-name (string-append "fluxus-" fluxus-version))

; the path to load extensions from
(define fluxus-extension-path 
	(build-path (path->string (car (current-library-collection-paths))) 
		fluxus-name "extensions"))
			   
; now require everything we want
(require "fluxus-engine.ss")
(require "fluxus-audio.ss")
(require "fluxus-osc.ss")

; todo: find out how to get rid of the burnt in version no
(require (lib "scratchpad.ss" "fluxus-0.14"))
(require (lib "scratchpad-input.ss" "fluxus-0.14"))
(require (lib "scratchpad-camera.ss" "fluxus-0.14"))
(require (lib "scratchpad-help.ss" "fluxus-0.14"))
(require (lib "fluxus-obj-import.ss" "fluxus-0.14"))
(require (lib "fluxus-building-blocks.ss" "fluxus-0.14"))

; load the helpmap
(init-help (string-append (path->string (car (current-library-collection-paths)))
	fluxus-name "/helpmap.scm"))

; set the font for the scratchpad
(define fluxus-scratchpad-font (string-append (path->string (car (current-library-collection-paths)))
	fluxus-name "/material/fonts/Bitstream-Vera-Sans-Mono.ttf"))

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

; setup the standard searchpaths
(set-searchpaths (list
	"./"
	(string-append (path->string (car (current-library-collection-paths)))
		fluxus-name "/material/textures/")
	(string-append (path->string (car (current-library-collection-paths)))
		fluxus-name "/material/shaders/")
	(string-append (path->string (car (current-library-collection-paths)))
		fluxus-name "/material/meshes/")))
	
;-------------------------------------------------
; here is the hacking section
; todo: remove all below at some point

; need some things to get us a guile like environment for script compatibility...
(require (all-except (lib "misc.ss" "swindle") identity regexp-quote concat))

; override the built in time function for pre 0.12 compatibility
(define time flxtime)

; for compatibility pre 0.13
(define pdata-set pdata-set!)
(define pdata-get pdata-ref)

;-------------------------------------------------
; execute the user config script, if it exists

(define user-script (string-append (getenv "HOME") "/.fluxus.scm"))
(if (file-exists? user-script)
	(load user-script))
