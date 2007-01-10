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

; this is the startup script for the fluxus canvas
; this script loads all the modules and sets things up so the 
; fluxus application works without having to worry about setup

; setup where to find the library module collections
(current-library-collection-paths 
	(path-list-string->path-list fluxus-collects-location 
	(current-library-collection-paths)))

; the path to load extensions from
(define fluxus-extension-path 
	(build-path (path->string (car (current-library-collection-paths))) "fluxus-0.12"
               "compiled" "native" (system-library-subpath #f)))
			   
; load the binary module extensions
(load-extension (build-path fluxus-extension-path "fluxus-engine.so"))
(load-extension (build-path fluxus-extension-path "fluxus-audio.so"))
(load-extension (build-path fluxus-extension-path "fluxus-osc.so"))

; now require everything we want
(require fluxus-engine)
(require fluxus-audio)
(require fluxus-osc)

(require (lib "fluxus-canvas.ss" "fluxus-0.12"))
(require (lib "fluxus-input.ss" "fluxus-0.12"))
(require (lib "fluxus-camera.ss" "fluxus-0.12"))
(require (lib "fluxus-obj-import.ss" "fluxus-0.12"))

(clear)

;-------------------------------------------------
; here is the hacking section
; todo: remove all below at some point

; need some things to get us a guile like environment for script compatibility...
(require (all-except (lib "misc.ss" "swindle") identity regexp-quote concat))

; override the built in time function for pre 0.12 compatibility
(define time flxtime)
