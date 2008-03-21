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

(module fluxus mzscheme
  (require "fluxus-engine.ss")
  (require "fluxus-audio.ss")
  (require "fluxus-osc.ss")
  ; todo: get rid of burnt in versions
  (require (lib "input.ss" "fluxus-0.15"))
  (require (lib "help.ss" "fluxus-0.15"))
  (require (lib "camera.ss" "fluxus-0.15"))
  (require (lib "building-blocks.ss" "fluxus-0.15"))
  (require (lib "collada-import.ss" "fluxus-0.15"))
  (require (lib "obj-import.ss" "fluxus-0.15"))
  (require (lib "obj-export.ss" "fluxus-0.15"))
  (require (lib "randomness.ss" "fluxus-0.15"))
  (require (lib "scratchpad.ss" "fluxus-0.15"))
 (require (only (lib "13.ss" "srfi") string-pad))
  (provide 
   (all-from "fluxus-engine.ss")
   (all-from "fluxus-audio.ss")
   (all-from "fluxus-osc.ss")
   (all-from (lib "input.ss" "fluxus-0.15"))
   (all-from (lib "help.ss" "fluxus-0.15"))
   (all-from (lib "camera.ss" "fluxus-0.15"))
   (all-from (lib "building-blocks.ss" "fluxus-0.15"))
   (all-from (lib "collada-import.ss" "fluxus-0.15"))
   (all-from (lib "obj-import.ss" "fluxus-0.15"))
   (all-from (lib "obj-export.ss" "fluxus-0.15"))
   (all-from (lib "randomness.ss" "fluxus-0.15"))  
   (all-from (lib "scratchpad.ss" "fluxus-0.15"))))
