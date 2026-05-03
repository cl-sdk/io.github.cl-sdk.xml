(defsystem #:cl-xml.test
  :description "Tests for cl-xml."
  :license "MIT"
  :depends-on ("cl-xml" "cl-xml.xsd" "cl-xml.soap" "cl-xml.wsdl" "fiveam" "trivial-gray-streams")
  :components ((:module "t"
                :components
                ((:file "package")
                 (:file "cl-xml-tests")))))
