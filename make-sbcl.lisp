;;; To dump an executable with SBCL:
;;; (sb-ext:save-lisp-and-die filename :toplevel fn :executable t)
;;; :save-runtime-options may also be relevant
;;; Ensure that cffi::*foreign-library-directories* contains something with 
;;; sdl.dll in it

(eval-when (:compile-toplevel)
  (ql:quickload :cffi))

(defun %make (toplevel &optional (name (read)))
  #+sbcl
  (sb-ext:save-lisp-and-die name :toplevel toplevel :executable t))

(defun add-library-path (fn)
  "Add #P\".\" to cffi's search path, then call fn."
  (lambda ()
    (push #P"." cffi::*foreign-library-directories*)
    (funcall fn)))

(defun make (&optional (name "slugmage.exe"))
  (compile-file "game.lisp")
  (load "game.fasl")
  (%make (add-library-path #'main) name))
