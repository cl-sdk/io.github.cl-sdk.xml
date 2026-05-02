(in-package #:cl-xml)

;;; Data structures

(defstruct xml-node
  "Represents an XML element node with a tag name, an alist of attributes,
and a list of children. Each child may be an xml-node (element), xml-comment,
xml-pi, xml-cdata, or a string (character data)."
  tag
  attributes
  children)

(defstruct xml-comment
  "Represents an XML comment <!-- … -->."
  data)

(defstruct xml-pi
  "Represents an XML processing instruction <?target data?>."
  target
  data)

(defstruct xml-cdata
  "Represents an XML CDATA section <![CDATA[…]]>."
  data)

(defstruct xml-document
  "Represents a parsed XML document.
PROLOG is a list of xml-comment and xml-pi nodes that precede the root element.
DOCTYPE declarations are recognized but not included in PROLOG.
ROOT is the root xml-node."
  prolog
  root)

;;; Character classification — XML 1.0 §2.3

(defun name-start-char-p (ch)
  "True if CH is a valid XML 1.0 NameStartChar."
  (let ((c (char-code ch)))
    (or (char= ch #\:)
        (char= ch #\_)
        (and (char>= ch #\A) (char<= ch #\Z))
        (and (char>= ch #\a) (char<= ch #\z))
        (and (>= c #xC0)    (<= c #xD6))
        (and (>= c #xD8)    (<= c #xF6))
        (and (>= c #xF8)    (<= c #x2FF))
        (and (>= c #x370)   (<= c #x37D))
        (and (>= c #x37F)   (<= c #x1FFF))
        (and (>= c #x200C)  (<= c #x200D))
        (and (>= c #x2070)  (<= c #x218F))
        (and (>= c #x2C00)  (<= c #x2FEF))
        (and (>= c #x3001)  (<= c #xD7FF))
        (and (>= c #xF900)  (<= c #xFDCF))
        (and (>= c #xFDF0)  (<= c #xFFFD))
        (and (>= c #x10000) (<= c #xEFFFF)))))

(defun name-char-p (ch)
  "True if CH is a valid XML 1.0 NameChar."
  (let ((c (char-code ch)))
    (or (name-start-char-p ch)
        (char= ch #\-)
        (char= ch #\.)
        (and (char>= ch #\0) (char<= ch #\9))
        (= c #xB7)
        (and (>= c #x0300) (<= c #x036F))
        (and (>= c #x203F) (<= c #x2040)))))

(defun xml-whitespace-p (ch)
  "True if CH is an XML 1.0 whitespace character (S production)."
  (member ch '(#\Space #\Tab #\Newline #\Return)))

;;; Low-level cursor helpers

(defun skip-whitespace (str pos)
  "Advance POS past XML whitespace characters in STR."
  (loop while (and (< pos (length str))
                   (xml-whitespace-p (char str pos)))
        do (incf pos))
  pos)

(defun parse-name (str pos)
  "Parse an XML Name (tag or attribute name) starting at POS.
Validates NameStartChar for the first character and NameChar for the rest.
Returns a cons (name . new-pos)."
  (when (or (>= pos (length str))
            (not (name-start-char-p (char str pos))))
    (error "Invalid XML name start character at position ~a" pos))
  (let ((start pos))
    (incf pos)
    (loop while (and (< pos (length str))
                     (name-char-p (char str pos)))
          do (incf pos))
    (cons (subseq str start pos) pos)))

;;; Entity and character reference expansion — XML 1.0 §4.1, §4.6

(defun expand-char-ref (str pos)
  "Expand a character reference starting just after '&#' at POS.
Returns a cons (string . new-pos)."
  (let ((hex-p (and (< pos (length str)) (char= (char str pos) #\x))))
    (when hex-p (incf pos))
    (let ((start pos))
      (loop while (and (< pos (length str))
                       (digit-char-p (char str pos) (if hex-p 16 10)))
            do (incf pos))
      (when (= start pos)
        (error "Empty character reference at position ~a" pos))
      (unless (and (< pos (length str)) (char= (char str pos) #\;))
        (error "Unterminated character reference at position ~a" pos))
      (let* ((num-str (subseq str start pos))
             (code    (parse-integer num-str :radix (if hex-p 16 10))))
        (cons (string (code-char code)) (1+ pos))))))

(defun expand-entity-ref (str pos)
  "Parse and expand a predefined entity or character reference starting just
after '&' at POS.  Supports &amp; &lt; &gt; &quot; &apos; and &#N; / &#xN;.
Returns a cons (string . new-pos)."
  (if (and (< pos (length str)) (char= (char str pos) #\#))
      (expand-char-ref str (1+ pos))
      (destructuring-bind (name . after-name) (parse-name str pos)
        (unless (and (< after-name (length str))
                     (char= (char str after-name) #\;))
          (error "Unterminated entity reference '&~a' at position ~a" name pos))
        (let ((expansion (cdr (assoc name
                                     '(("amp"  . "&")
                                       ("lt"   . "<")
                                       ("gt"   . ">")
                                       ("quot" . "\"")
                                       ("apos" . "'"))
                                     :test #'string=))))
          (unless expansion
            (error "Unknown entity reference '&~a;'" name))
          (cons expansion (1+ after-name))))))

;;; Attribute value parsing — XML 1.0 §2.3, §3.3.3

(defun parse-attribute-value (str pos)
  "Parse a single- or double-quoted attribute value starting at POS,
expanding entity and character references and forbidding bare '<'.
Returns a cons (value . new-pos)."
  (let ((quote (char str pos)))
    (unless (member quote '(#\" #\'))
      (error "Expected a quote character at position ~a" pos))
    (incf pos)
    (let ((buf (make-array 0 :element-type 'character
                             :adjustable t :fill-pointer 0)))
      (loop
        (when (>= pos (length str))
          (error "Unterminated attribute value"))
        (let ((ch (char str pos)))
          (cond
            ((char= ch quote)
             (incf pos)
             (return))
            ((char= ch #\<)
             (error "Illegal '<' in attribute value at position ~a" pos))
            ((char= ch #\&)
             (destructuring-bind (expansion . new-pos)
                 (expand-entity-ref str (1+ pos))
               (loop for c across expansion do (vector-push-extend c buf))
               (setf pos new-pos)))
            (t
             (vector-push-extend ch buf)
             (incf pos)))))
      (cons (copy-seq buf) pos))))

;;; Attribute list parsing — XML 1.0 §3.1

(defun parse-attributes (str pos)
  "Parse zero or more attributes starting at POS, stopping at '>' or '/>'.
Detects duplicate attribute names.
Returns a cons (attributes . new-pos) where attributes is an alist of
\(name . value) pairs."
  (let ((attributes '()))
    (loop
      (setf pos (skip-whitespace str pos))
      (when (or (>= pos (length str))
                (member (char str pos) '(#\> #\/)))
        (return))
      (destructuring-bind (name . after-name) (parse-name str pos)
        (when (assoc name attributes :test #'string=)
          (error "Duplicate attribute '~a'" name))
        (setf pos (skip-whitespace str after-name))
        (unless (and (< pos (length str)) (char= (char str pos) #\=))
          (error "Expected '=' after attribute name '~a' at position ~a" name pos))
        (setf pos (skip-whitespace str (1+ pos)))
        (destructuring-bind (value . after-value) (parse-attribute-value str pos)
          (push (cons name value) attributes)
          (setf pos after-value))))
    (cons (nreverse attributes) pos)))

;;; Comment parsing — XML 1.0 §2.5

(defun parse-comment (str pos)
  "Parse a comment body and closing '-->'.  POS must be just past '<!--'.
Returns a cons (xml-comment-node . new-pos)."
  (let ((start pos))
    (loop
      (when (>= pos (length str))
        (error "Unterminated comment"))
      (when (and (<= (+ pos 3) (length str))
                 (string= str "-->" :start1 pos :end1 (+ pos 3)))
        (return (cons (make-xml-comment :data (subseq str start pos))
                      (+ pos 3))))
      ;; '--' must not appear inside a comment except as part of -->
      (when (and (< (1+ pos) (length str))
                 (char= (char str pos) #\-)
                 (char= (char str (1+ pos)) #\-))
        (error "Illegal '--' inside comment at position ~a" pos))
      (incf pos))))

;;; Processing instruction parsing — XML 1.0 §2.6

(defun parse-pi (str pos)
  "Parse a processing instruction body and closing '?>'.
POS must be just past the opening '<?'.
Returns a cons (xml-pi-node . new-pos)."
  (destructuring-bind (target . after-target) (parse-name str pos)
    (let* ((pos (skip-whitespace str after-target))
           (data-start pos))
      (loop
        (when (>= pos (length str))
          (error "Unterminated processing instruction"))
        (when (and (< (1+ pos) (length str))
                   (char= (char str pos) #\?)
                   (char= (char str (1+ pos)) #\>))
          (return (cons (make-xml-pi :target target
                                     :data (subseq str data-start pos))
                        (+ pos 2))))
        (incf pos)))))

;;; DOCTYPE skipping — XML 1.0 §2.8

(defun skip-doctype (str pos)
  "Skip a DOCTYPE declaration.  POS must be just past '<!DOCTYPE'.
Handles a nested internal subset enclosed in '[' … ']'.
Returns new-pos after the closing '>'."
  (let ((depth 0))
    (loop
      (when (>= pos (length str))
        (error "Unterminated DOCTYPE declaration"))
      (let ((ch (char str pos)))
        (cond
          ((char= ch #\[) (incf depth) (incf pos))
          ((char= ch #\]) (decf depth) (incf pos))
          ((and (= depth 0) (char= ch #\>))
           (return (1+ pos)))
          (t (incf pos)))))))

;;; Character data parsing — XML 1.0 §2.4

(defun parse-content-text (str pos)
  "Parse character data and entity/character references up to the next '<'.
Returns a cons (string . new-pos)."
  (let ((buf (make-array 0 :element-type 'character
                           :adjustable t :fill-pointer 0)))
    (loop
      (when (or (>= pos (length str))
                (char= (char str pos) #\<))
        (return))
      (let ((ch (char str pos)))
        (cond
          ((char= ch #\&)
           (destructuring-bind (expansion . new-pos)
               (expand-entity-ref str (1+ pos))
             (loop for c across expansion do (vector-push-extend c buf))
             (setf pos new-pos)))
          (t
           (vector-push-extend ch buf)
           (incf pos)))))
    (cons (copy-seq buf) pos)))

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

;;; Default DOM-building SAX handler

(defclass dom-builder (sax-handler)
  ((%prolog :initform '())
   (%stack  :initform '())
   (%root   :initform nil))
  (:documentation
   "SAX handler that builds an XML-DOCUMENT structure — the default behaviour
of PARSE-XML when no custom handler is supplied.
Each stack frame is a list (tag attributes children-accumulator)."))

(defmethod start-element ((handler dom-builder) tag attributes)
  (push (list tag attributes '()) (slot-value handler '%stack)))

(defmethod end-element ((handler dom-builder) tag)
  (declare (ignore tag))
  (let* ((frame (pop (slot-value handler '%stack)))
         (node  (make-xml-node :tag        (first frame)
                               :attributes (second frame)
                               :children   (nreverse (third frame)))))
    (if (slot-value handler '%stack)
        (push node (third (first (slot-value handler '%stack))))
        (setf (slot-value handler '%root) node))))

(defmethod characters ((handler dom-builder) text)
  ;; Whitespace-only runs between elements are discarded, matching the
  ;; original DOM parser behaviour.
  (when (and (slot-value handler '%stack)
             (not (every #'xml-whitespace-p text)))
    (push text (third (first (slot-value handler '%stack))))))

(defmethod comment ((handler dom-builder) data)
  (let ((node (make-xml-comment :data data)))
    (if (slot-value handler '%stack)
        (push node (third (first (slot-value handler '%stack))))
        (push node (slot-value handler '%prolog)))))

(defmethod processing-instruction ((handler dom-builder) target data)
  (let ((node (make-xml-pi :target target :data data)))
    (if (slot-value handler '%stack)
        (push node (third (first (slot-value handler '%stack))))
        (push node (slot-value handler '%prolog)))))

(defmethod cdata-section ((handler dom-builder) data)
  (when (slot-value handler '%stack)
    (push (make-xml-cdata :data data)
          (third (first (slot-value handler '%stack))))))

(defmethod end-document ((handler dom-builder))
  (make-xml-document :prolog (nreverse (slot-value handler '%prolog))
                     :root   (slot-value handler '%root)))

;;; SAX-based element parsing — XML 1.0 §3.1

(defun parse-element-sax (str pos handler)
  "Parse an XML element whose opening '<' has already been consumed.
POS points to the first character of the tag name.
Fires START-ELEMENT, child events, and END-ELEMENT on HANDLER.
Returns new-pos after the element."
  (destructuring-bind (tag . after-tag) (parse-name str pos)
    (destructuring-bind (attributes . after-attrs) (parse-attributes str after-tag)
      (let ((pos (skip-whitespace str after-attrs)))
        (cond
          ;; Self-closing tag: />
          ((and (< (1+ pos) (length str))
                (char= (char str pos) #\/)
                (char= (char str (1+ pos)) #\>))
           (start-element handler tag attributes)
           (end-element handler tag)
           (+ pos 2))
          ;; Opening tag: >  …content…  </tag>
          ((and (< pos (length str))
                (char= (char str pos) #\>))
           (start-element handler tag attributes)
           (let ((end-pos (parse-children-sax str (1+ pos) tag handler)))
             (end-element handler tag)
             end-pos))
          (t
           (error "Expected '>' or '/>' after attributes of '~a' at position ~a"
                  tag pos)))))))

(defun parse-children-sax (str pos parent-tag handler)
  "Parse the content of an element, firing SAX events on HANDLER, until the
matching closing tag is consumed.  Returns new-pos after the closing '>'."
  (loop
    (when (>= pos (length str))
      (error "Unexpected end of input while parsing children of <~a>" parent-tag))
    (let ((ch (char str pos)))
      (cond
        ;; Character data (possibly containing entity references)
        ((char/= ch #\<)
         (destructuring-bind (text . new-pos) (parse-content-text str pos)
           (characters handler text)
           (setf pos new-pos)))
        ;; Markup starting with '<'
        (t
         (incf pos)                     ; consume '<'
         (when (>= pos (length str))
           (error "Unexpected end of input after '<'"))
         (let ((next (char str pos)))
           (cond
             ;; Closing tag: </parent-tag>
             ((char= next #\/)
              (incf pos)                ; consume '/'
              (destructuring-bind (tag . after-tag) (parse-name str pos)
                (unless (string= tag parent-tag)
                  (error "Mismatched closing tag: expected </~a>, got </~a>"
                         parent-tag tag))
                (let ((after-ws (skip-whitespace str after-tag)))
                  (unless (and (< after-ws (length str))
                               (char= (char str after-ws) #\>))
                    (error "Expected '>' to close </~a> at position ~a"
                           tag after-ws))
                  (return (1+ after-ws)))))
             ;; Nodes beginning with '<!'
             ((char= next #\!)
              (incf pos)                ; consume '!'
              (cond
                ;; Comment: <!--
                ((and (< (1+ pos) (length str))
                      (char= (char str pos) #\-)
                      (char= (char str (1+ pos)) #\-))
                 (destructuring-bind (comment-node . new-pos)
                     (parse-comment str (+ pos 2))
                   (comment handler (xml-comment-data comment-node))
                   (setf pos new-pos)))
                ;; CDATA section: <![CDATA[
                ((and (<= (+ pos 7) (length str))
                      (string= str "[CDATA[" :start1 pos :end1 (+ pos 7)))
                 (let* ((cdata-start (+ pos 7))
                        (end (search "]]>" str :start2 cdata-start)))
                   (unless end
                     (error "Unterminated CDATA section"))
                   (cdata-section handler (subseq str cdata-start end))
                   (setf pos (+ end 3))))
                (t
                 (error "Unexpected '<!' at position ~a" (- pos 2)))))
             ;; Processing instruction: <?
             ((char= next #\?)
              (destructuring-bind (pi-node . new-pos)
                  (parse-pi str (1+ pos))
                (processing-instruction handler
                                        (xml-pi-target pi-node)
                                        (xml-pi-data pi-node))
                (setf pos new-pos)))
             ;; Child element
             (t
              (setf pos (parse-element-sax str pos handler))))))))))

;;; SAX-based document prolog — XML 1.0 §2.8

(defun parse-prolog-sax (str pos handler)
  "Parse the XML document prolog, firing SAX events for comments and
processing instructions on HANDLER.  DOCTYPE declarations are silently
skipped.  Returns new-pos pointing to the '<' of the root element."
  (loop
    (setf pos (skip-whitespace str pos))
    (unless (and (< pos (length str)) (char= (char str pos) #\<))
      (return pos))
    (let ((peek (and (< (1+ pos) (length str)) (char str (1+ pos)))))
      (cond
        ;; Comment: <!--
        ((and (eql peek #\!)
              (< (+ pos 3) (length str))
              (char= (char str (+ pos 2)) #\-)
              (char= (char str (+ pos 3)) #\-))
         (destructuring-bind (comment-node . new-pos)
             (parse-comment str (+ pos 4))
           (comment handler (xml-comment-data comment-node))
           (setf pos new-pos)))
        ;; Processing instruction (includes XML declaration): <?
        ((eql peek #\?)
         (destructuring-bind (pi-node . new-pos)
             (parse-pi str (+ pos 2))
           (processing-instruction handler
                                   (xml-pi-target pi-node)
                                   (xml-pi-data pi-node))
           (setf pos new-pos)))
        ;; DOCTYPE: <!DOCTYPE
        ((and (eql peek #\!)
              (<= (+ pos 9) (length str))
              (string= str "<!DOCTYPE" :start1 pos :end1 (+ pos 9)))
         (setf pos (skip-doctype str (+ pos 9))))
        ;; Anything else is the root element
        (t (return pos))))))

;;; Public API

(defun parse-xml (str &key (handler (make-instance 'dom-builder)))
  "Parse STR as an XML document using a SAX-style event handler.

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
  (start-document handler)
  (let ((pos (parse-prolog-sax str 0 handler)))
    (unless (and (< pos (length str)) (char= (char str pos) #\<))
      (error "Expected root element at position ~a" pos))
    (parse-element-sax str (1+ pos) handler))
  (end-document handler))
