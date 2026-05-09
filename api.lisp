(in-package #:io.github.cl-sdk.xml)

;;; Event-to-handler bridge

(defun reduce-events (events handler)
  "Replay a list of XML event structs produced by PARSE-XML-EVENTS onto HANDLER,
calling the appropriate SAX-HANDLER generic function for each event.
Returns no useful value; call END-DOCUMENT on the handler separately."
  (dolist (event events)
    (typecase event
      (xml-event-start-element
       (start-element handler
                      (xml-event-start-element-tag event)
                      (xml-event-start-element-attributes event)))
      (xml-event-end-element
       (end-element handler (xml-event-end-element-tag event)))
      (xml-event-characters
       (characters handler (xml-event-characters-text event)))
      (xml-event-comment
       (comment handler (xml-event-comment-data event)))
      (xml-event-pi
       (processing-instruction handler
                               (xml-event-pi-target event)
                               (xml-event-pi-data event)))
      (xml-event-cdata
       (cdata-section handler (xml-event-cdata-data event)))
      (xml-event-doctype
       (doctype-declaration handler (xml-event-doctype-doctype event))))))

;;; Public API

(defun parse-xml (input &key (handler (make-instance 'dom-builder)))
  "Parse INPUT (a string, standard character stream, or trivial-gray-streams
character stream) as an XML document using a SAX-style event handler.

When called without a HANDLER keyword argument, uses the built-in DOM-BUILDER
handler and returns an XML-DOCUMENT node (backward-compatible behaviour).

When a custom SAX-HANDLER subclass instance is supplied, the parser fires the
following generic functions on it as it walks the input:
  START-DOCUMENT, START-ELEMENT, END-ELEMENT, CHARACTERS, COMMENT,
  PROCESSING-INSTRUCTION, CDATA-SECTION, END-DOCUMENT.
The return value of END-DOCUMENT on the handler becomes the return value of
PARSE-XML.

Entity references (&amp; &lt; &gt; &quot; &apos; &#N; &#xN;) are expanded
before CHARACTERS and attribute values are reported."
  (let ((events (parse-xml-events input)))
    (start-document handler)
    (reduce-events events handler)
    (end-document handler)))
