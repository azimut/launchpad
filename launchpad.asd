;;;; launchpad.asd

(asdf:defsystem #:launchpad
  :description "Describe launchpad here"
  :author "azimut <azimut.github@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-rtmidi
               #:cl-arrows
               #:3d-matrices)
  :components ((:file "package")
               (:file "launchpad")))
