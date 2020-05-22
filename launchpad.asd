;;;; launchpad.asd

(asdf:defsystem #:launchpad
  :description "Describe launchpad here"
  :author "azimut <azimut.github@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-rtmidi)
  :components ((:file "package")
               (:file "launchpad")))
