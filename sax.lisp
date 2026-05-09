(in-package #:io.github.cl-sdk.xml)

;;; SAX handler protocol — event-driven parsing interface

(defclass sax-handler ()
  ()
  (:documentation
   "Base class for SAX event handlers.
Specialize the generic functions below to process the events you care about;
default methods are no-ops (except END-DOCUMENT, which returns NIL)."))

(defgeneric start-document (handler)
  (:documentation "Called once before any other event.")
  (:method ((handler sax-handler)) nil))

(defgeneric end-document (handler)
  (:documentation
   "Called once after all events have been fired.
The return value becomes the return value of PARSE-XML.")
  (:method ((handler sax-handler)) nil))

(defgeneric start-element (handler tag attributes)
  (:documentation
   "Called when an opening (or self-closing) tag is encountered.
TAG is a string; ATTRIBUTES is an alist of (name . value) string pairs.")
  (:method ((handler sax-handler) tag attributes)
    (declare (ignore tag attributes))
    nil))

(defgeneric end-element (handler tag)
  (:documentation
   "Called when a closing (or self-closing) tag has been processed.
TAG is a string.")
  (:method ((handler sax-handler) tag)
    (declare (ignore tag))
    nil))

(defgeneric characters (handler text)
  (:documentation
   "Called for a run of character data content.
TEXT is a string with entity and character references already expanded.")
  (:method ((handler sax-handler) text)
    (declare (ignore text))
    nil))

(defgeneric comment (handler data)
  (:documentation
   "Called when a comment is encountered.
DATA is the raw comment body (between <!-- and -->).")
  (:method ((handler sax-handler) data)
    (declare (ignore data))
    nil))

(defgeneric processing-instruction (handler target data)
  (:documentation
   "Called when a processing instruction is encountered.
TARGET and DATA are both strings.")
  (:method ((handler sax-handler) target data)
    (declare (ignore target data))
    nil))

(defgeneric cdata-section (handler data)
  (:documentation
   "Called when a CDATA section is encountered.
DATA is the raw content string (between <![CDATA[ and ]]>).")
  (:method ((handler sax-handler) data)
    (declare (ignore data))
    nil))

(defgeneric doctype-declaration (handler doctype)
  (:documentation
   "Called when a DOCTYPE declaration is parsed.
DOCTYPE is an xml-doctype struct containing the parsed name, optional external
identifiers, and a list of xml-dtd-element structs from the internal subset.")
  (:method ((handler sax-handler) doctype)
    (declare (ignore doctype))
    nil))
