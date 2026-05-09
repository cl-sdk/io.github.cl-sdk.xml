(defsystem #:io.github.cl-sdk.wsdl.test
  :description "Tests for io.github.cl-sdk.xml.wsdl."
  :version "0.1.0"
  :author "Bruno Dias <dias.h.bruno@gmail.com>"
  :license "Unlicense"
  :homepage "https://github.com/cl-sdk/cl-xml"
  :source-control (:git "https://github.com/cl-sdk/cl-xml")
  :bug-tracker "https://github.com/cl-sdk/cl-xml/issues"
  :depends-on (#:io.github.cl-sdk.xml #:io.github.cl-sdk.wsdl #:fiveam)
  :components ((:module "t"
                :components
                ((:file "cl-wsdl-package")
                 (:file "cl-wsdl-tests" :depends-on ("cl-wsdl-package"))))))
