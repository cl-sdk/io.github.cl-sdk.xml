(defsystem "cl-xml.xsd"
  :description "XSD (XML Schema Definition) support for cl-xml."
  :version "0.1.0"
  :license "MIT"
  :depends-on ("cl-xml")
  :components ((:file "xsd-package")
               (:file "xsd" :depends-on ("xsd-package"))))
