(defsystem "cl-xml.soap"
  :description "SOAP 1.1/1.2 support for cl-xml."
  :version "0.1.0"
  :license "MIT"
  :depends-on ("cl-xml")
  :components ((:file "soap-package")
               (:file "soap" :depends-on ("soap-package"))))
