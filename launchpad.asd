;;;; launchpad.asd

(asdf:defsystem #:launchpad
  :description "Describe launchpad here"
  :author "azimut <azimut.github@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :pathname "src"
  :depends-on (#:cl-rtmidi
               #:cl-arrows)
  :components ((:file "package")
               (:file "launchpad")))

(asdf:defsystem #:launchpad/examples
  :description "Describe launchpad here"
  :author "azimut <azimut.github@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :pathname "src"
  :depends-on (#:launchpad
               #:3d-matrices
               #:scheduler)
  :components ((:file "examples")))
