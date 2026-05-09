(in-package #:io.github.cl-sdk.xml)

;;; XML event types — the intermediate representation between the tokeniser
;;; and any downstream processor (SAX handler, validator, etc.).

(defstruct xml-event-start-element
  "Event fired when an opening (or self-closing) tag is encountered.
TAG is a string; ATTRIBUTES is an alist of (name . value) string pairs."
  tag
  attributes)

(defstruct xml-event-end-element
  "Event fired when a closing (or self-closing) tag has been fully processed.
TAG is a string."
  tag)

(defstruct xml-event-characters
  "Event fired for a run of character data content.
TEXT is a string with entity and character references already expanded."
  text)

(defstruct xml-event-comment
  "Event fired when a comment is encountered.
DATA is the raw comment body (between <!-- and -->)."
  data)

(defstruct xml-event-pi
  "Event fired when a processing instruction is encountered.
TARGET and DATA are both strings."
  target
  data)

(defstruct xml-event-cdata
  "Event fired when a CDATA section is encountered.
DATA is the raw content string (between <![CDATA[ and ]]>)."
  data)

(defstruct xml-event-doctype
  "Event fired when a DOCTYPE declaration is parsed.
DOCTYPE is an xml-doctype struct."
  doctype)

;;; Stream normalisation helper

(defun %normalize-input (input)
  "Coerce INPUT (string, standard stream, or Gray stream) to a character
input stream suitable for the tokeniser."
  (etypecase input
    (string (make-string-input-stream input))
    (fundamental-character-input-stream input)
    (stream input)))

;;; Event collection — adapted from the former SAX-driving parsing functions.
;;; These push event structs onto a mutable list rather than calling a handler.

(defun %collect-element-events (stream events)
  "Parse an XML element whose opening '<' has already been consumed.
STREAM is positioned at the first character of the tag name.
Pushes xml-event-start-element, child events, and xml-event-end-element onto
EVENTS (a fill-pointer vector or adjustable list via vector-push-extend).
Returns no useful value."
  (let* ((tag        (parse-name stream))
         (attributes (parse-attributes stream)))
    (skip-whitespace stream)
    (let ((ch (peek-char nil stream nil nil)))
      (cond
        ;; Self-closing tag: />
        ((eql ch #\/)
         (read-char stream)             ; consume '/'
         (unless (eql (read-char stream nil nil) #\>)
           (error "Expected '>' to close self-closing tag '~a'" tag))
         (vector-push-extend (make-xml-event-start-element :tag tag :attributes attributes) events)
         (vector-push-extend (make-xml-event-end-element :tag tag) events))
        ;; Opening tag: >  …content…  </tag>
        ((eql ch #\>)
         (read-char stream)             ; consume '>'
         (vector-push-extend (make-xml-event-start-element :tag tag :attributes attributes) events)
         (%collect-children-events stream tag events)
         (vector-push-extend (make-xml-event-end-element :tag tag) events))
        (t
         (error "Expected '>' or '/>' after attributes of '~a'" tag))))))

(defun %collect-children-events (stream parent-tag events)
  "Parse the content of an element, pushing events onto EVENTS, until the
matching closing tag is consumed."
  (loop
    (let ((ch (peek-char nil stream nil nil)))
      (unless ch
        (error "Unexpected end of input while parsing children of <~a>" parent-tag))
      (cond
        ;; Character data (possibly containing entity references)
        ((char/= ch #\<)
         (vector-push-extend
          (make-xml-event-characters :text (parse-content-text stream))
          events))
        ;; Markup starting with '<'
        (t
         (read-char stream)             ; consume '<'
         (let ((next (peek-char nil stream nil nil)))
           (unless next
             (error "Unexpected end of input after '<'"))
           (cond
             ;; Closing tag: </parent-tag>
             ((char= next #\/)
              (read-char stream)        ; consume '/'
              (let ((tag (parse-name stream)))
                (unless (string= tag parent-tag)
                  (error "Mismatched closing tag: expected </~a>, got </~a>"
                         parent-tag tag))
                (skip-whitespace stream)
                (unless (eql (read-char stream nil nil) #\>)
                  (error "Expected '>' to close </~a>" tag))
                (return)))
             ;; Nodes beginning with '<!'
             ((char= next #\!)
              (read-char stream)        ; consume '!'
              (let ((after-bang (peek-char nil stream nil nil)))
                (cond
                  ;; Comment: <!--
                  ((eql after-bang #\-)
                   (read-char stream)   ; consume first '-'
                   (unless (eql (peek-char nil stream nil nil) #\-)
                     (error "Expected second '-' in comment opening '<!--'"))
                   (read-char stream)   ; consume second '-'
                   (vector-push-extend
                    (make-xml-event-comment :data (xml-comment-data (parse-comment stream)))
                    events))
                  ;; CDATA section: <![CDATA[
                  ((eql after-bang #\[)
                   (read-char stream)   ; consume '['
                   (loop for expected across "CDATA["
                         do (let ((c (read-char stream nil nil)))
                              (unless (and c (char= c expected))
                                (error "Invalid CDATA section start"))))
                   (vector-push-extend
                    (make-xml-event-cdata :data (parse-cdata-section stream))
                    events))
                  (t
                   (error "Unexpected '<!' sequence")))))
             ;; Processing instruction: <?
             ((char= next #\?)
              (read-char stream)        ; consume '?'
              (let ((pi-node (parse-pi stream)))
                (vector-push-extend
                 (make-xml-event-pi :target (xml-pi-target pi-node)
                                    :data   (xml-pi-data pi-node))
                 events)))
             ;; Child element
             (t
              (%collect-element-events stream events)))))))))

(defun %collect-prolog-events (stream events)
  "Parse the XML document prolog, pushing events for comments, processing
instructions, and DOCTYPE declarations onto EVENTS.
Leaves STREAM positioned at the '<' of the root element."
  (loop
    (skip-whitespace stream)
    (unless (eql (peek-char nil stream nil nil) #\<)
      (return))
    (read-char stream)                  ; consume '<'
    (let ((next (peek-char nil stream nil nil)))
      (cond
        ;; Processing instruction or XML declaration: <?
        ((eql next #\?)
         (read-char stream)             ; consume '?'
         (let ((pi-node (parse-pi stream)))
           (vector-push-extend
            (make-xml-event-pi :target (xml-pi-target pi-node)
                               :data   (xml-pi-data pi-node))
            events)))
        ;; Comment or DOCTYPE: <!
        ((eql next #\!)
         (read-char stream)             ; consume '!'
         (let ((after-bang (peek-char nil stream nil nil)))
           (cond
             ;; Comment: <!--
             ((eql after-bang #\-)
              (read-char stream)        ; consume first '-'
              (unless (eql (peek-char nil stream nil nil) #\-)
                (error "Expected second '-' in comment opening '<!--'"))
              (read-char stream)        ; consume second '-'
              (vector-push-extend
               (make-xml-event-comment :data (xml-comment-data (parse-comment stream)))
               events))
             ;; DOCTYPE: <!DOCTYPE
             ((eql after-bang #\D)
              (loop for expected across "DOCTYPE"
                    do (let ((c (read-char stream nil nil)))
                         (unless (and c (char= c expected))
                           (error "Invalid DOCTYPE declaration"))))
              (vector-push-extend
               (make-xml-event-doctype :doctype (parse-doctype stream))
               events))
             (t
              (error "Unexpected '<!~c' in prolog" after-bang)))))
        ;; Anything else is the root element: unread '<' and stop
        (t
         (unread-char #\< stream)
         (return))))))

;;; Public entry point — pure parser

(defun parse-xml-events (input)
  "Parse INPUT (a string, standard character stream, or trivial-gray-streams
character stream) as an XML document and return a list of XML event structs
in document order.

The returned list contains zero or more of:
  xml-event-pi, xml-event-comment, xml-event-doctype  (from the prolog)
  xml-event-start-element, xml-event-end-element,
  xml-event-characters, xml-event-comment, xml-event-pi,
  xml-event-cdata                                      (from the document body)

This function is the pure parsing layer: it does not invoke any handler and
has no dependency on the SAX protocol.  Use PARSE-XML for the full pipeline,
or feed the returned list to REDUCE-EVENTS with a custom SAX-HANDLER."
  (let ((stream (%normalize-input input))
        (events (make-array 32 :adjustable t :fill-pointer 0)))
    (%collect-prolog-events stream events)
    (unless (eql (peek-char nil stream nil nil) #\<)
      (error "Expected root element"))
    (read-char stream)                  ; consume '<'
    (%collect-element-events stream events)
    (coerce events 'list)))
