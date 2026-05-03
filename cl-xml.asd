(defsystem "cl-xml"
  :description "A Common Lisp XML reader, writer, and custom parser."
  :version "0.1.0"
  :license "MIT"
  :depends-on ("trivial-gray-streams")
  :components ((:file "package")
               (:file "structures"   :depends-on ("package"))
               (:file "chars"        :depends-on ("package"))
               (:file "tokeniser"    :depends-on ("chars" "structures"))
               (:file "events"       :depends-on ("tokeniser" "structures"))
               (:file "sax"          :depends-on ("package"))
               (:file "dom-builder"  :depends-on ("sax" "structures" "chars"))
               (:file "namespace"    :depends-on ("structures"))
               (:file "api"          :depends-on ("events" "sax" "dom-builder"))
               ))
