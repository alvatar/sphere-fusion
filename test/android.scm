;;; Copyright (c) 2013 by Álvaro Castro Castilla
;;; Test for 2d and texturing with OpenGL 2.1

;; Press space for playing music
;; Click any mouse button for playing sound

(##import-include core: base-macros)
(##import-include core: assert-macros)

;(shell-command "sfusion new -t opengl2d -p android tmp")
(delete-file "tmp")

