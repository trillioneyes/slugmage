(push :slugmage-buildscript *features*)

#+sbcl
(progn 
  (compile-file "make-sbcl.lisp")
  (load "make-sbcl.fasl")
  (make (or (cadr *posix-argv*) "slugmage.exe")))

#-(or sbcl)
(format t "Sorry, only sbcl is supported right now.")
