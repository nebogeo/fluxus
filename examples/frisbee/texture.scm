(require (lib "frisbee.ss" "fluxus-0.15"))

(scene
 (list
  (object #:texture "test.png"
          #:rotate (vmul (vec3 0 clock 0) 0.01))))
