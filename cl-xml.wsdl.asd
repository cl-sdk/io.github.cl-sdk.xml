(defsystem "cl-xml.wsdl"
  :description "WSDL 2.0 support for cl-xml."
  :version "0.1.0"
  :license "MIT"
  :depends-on ("cl-xml")
  :components ((:file "wsdl-package")
               (:file "wsdl" :depends-on ("wsdl-package"))))
