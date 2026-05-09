(defsystem #:io.github.cl-sdk.xsd
  :description "XSD (XML Schema Definition) support for io.github.cl-sdk.xml."
  :version "0.1.0"
  :author "Bruno Dias <dias.h.bruno@gmail.com>"
  :maintainer "Bruno Dias <dias.h.bruno@gmail.com>"
  :license "Unlicense"
  :homepage "https://github.com/cl-sdk/cl-xml"
  :source-control (:git "https://github.com/cl-sdk/cl-xml")
  :bug-tracker "https://github.com/cl-sdk/cl-xml/issues"
  :depends-on (#:io.github.cl-sdk.xml)
  :components ((:file "xsd-package")
               (:file "xsd" :depends-on ("xsd-package"))))
