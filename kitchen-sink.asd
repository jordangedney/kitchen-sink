;;;; kitchen-sink.asd

(asdf:defsystem #:kitchen-sink
  :description "and everything else"
  :author "jordangedney"
  :license  "a good one"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "kitchen-sink")))
