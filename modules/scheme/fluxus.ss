; Copyright (C) 2008 Dave Griffiths
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

#lang scheme/base

(require "fluxus-engine.ss"
         "fluxus-audio.ss"
         "fluxus-osc.ss"
			; todo: get rid of burnt in versions
		 fluxus-015/input
		 fluxus-015/help
		 fluxus-015/camera
		 fluxus-015/building-blocks
		 fluxus-015/collada-import
		 fluxus-015/randomness
;		 fluxus-015/fluxa ; removed until big pause in liblo startup is fixed
		 fluxus-015/scratchpad
		 (only-in srfi/13 string-pad))
		 
(provide 
 (all-from-out "fluxus-engine.ss")
 (all-from-out "fluxus-audio.ss")
 (all-from-out "fluxus-osc.ss")
 (all-from-out fluxus-015/input)
 (all-from-out fluxus-015/help)
 (all-from-out fluxus-015/camera)
 (all-from-out fluxus-015/building-blocks)
 (all-from-out fluxus-015/collada-import)
 (all-from-out fluxus-015/randomness)  
 ;(all-from-out fluxus-015/fluxa)
 (all-from-out fluxus-015/scratchpad))
