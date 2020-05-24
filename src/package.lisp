;;;; package.lisp

(cl:defpackage #:launchpad
  (:use #:cl #:cl-arrows #:3d-matrices)
  (:local-nicknames (#:a #:alexandria)))

(in-package #:launchpad)

(defun interleave (list &rest lists)
  "Return a list whose elements are taken from LIST and each of LISTS like this:
   1st of list, 1st of 1st of lists,..., 1st of last of lists, 2nd of list,..."
  (apply #'mapcan (lambda (&rest els)
                    els)
         list lists))
