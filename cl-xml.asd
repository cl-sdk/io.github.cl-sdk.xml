(defsystem "cl-xml"
  :description "A Common Lisp XML reader, writer, and custom parser."
  :version "0.1.0"
  :license "MIT"
  :depends-on ("trivial-gray-streams")
  :components ((:file "package")
               (:file "cl-xml" :depends-on ("package"))))
