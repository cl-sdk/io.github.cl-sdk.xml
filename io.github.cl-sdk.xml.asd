(defsystem #:io.github.cl-sdk.xml
  :description "A Common Lisp XML reader, writer, and custom parser."
  :long-description #.(uiop:read-file-string
                       (uiop:subpathname *load-pathname* "README.md"))
  :version "0.1.0"
  :author "Bruno Dias <dias.h.bruno@gmail.com>"
  :maintainer "Bruno Dias <dias.h.bruno@gmail.com>"
  :license "Unlicense"
  :homepage "https://github.com/cl-sdk/cl-xml"
  :source-control (:git "https://github.com/cl-sdk/cl-xml")
  :bug-tracker "https://github.com/cl-sdk/cl-xml/issues"
  :depends-on (#:trivial-gray-streams)
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
