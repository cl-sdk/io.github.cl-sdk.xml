(defsystem "cl-xml"
  :description "A Common Lisp XML reader, writer, and custom parser."
  :version "0.1.0"
  :license "MIT"
  :components ((:file "package")
               (:file "cl-xml" :depends-on ("package"))))
