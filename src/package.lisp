;;;; package.lisp

(cl:defpackage #:launchpad
  (:use #:cl #:cl-arrows)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:raw-command #:command
   ;;
   #:reset
   #:flash
   #:all-low #:all-hig #:all-med
   #:change-layout
   #:get-keys
   ;;
   #:color
   #:xy
   #:key
   ;;
   #:button-scene
   #:button-automap-on #:button-automap-off
   #:button-on
   #:button-off))

(in-package #:launchpad)

(defun interleave (list &rest lists)
  "Return a list whose elements are taken from LIST and each of LISTS like this:
   1st of list, 1st of 1st of lists,..., 1st of last of lists, 2nd of list,..."
  (apply #'mapcan (lambda (&rest els)
                    els)
         list lists))
