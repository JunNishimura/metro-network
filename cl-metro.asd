;; cl-metro.asd
(in-package :cl-user)
(defpackage :cl-metro-asd
  (:use :cl :asdf))
(in-package :cl-metro-asd)

(defsystem :cl-metro
  :class :package-inferred-system
  :description "all functionalities implemented with Common Lisp"
  :version "0.1"
  :author "nishimura"
  :license "Public Domain"
  :depends-on ("cl-metro/main") )
