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

;;; BOM stripping — XML 1.0 Appendix F

(defun strip-bom (str)
  "Strip a leading Unicode BOM (U+FEFF) from STR if present, per XML Appendix F.
Returns a new string with the BOM removed, or STR unchanged if no BOM is present.
A new string is allocated intentionally: the parser uses positions into the string,
so removing the BOM by adjusting an offset throughout would require plumbing changes
across every parsing function."
  (if (and (> (length str) 0) (char= (char str 0) #\uFEFF))
      (subseq str 1)
      str))

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

;;; Element parsing — XML 1.0 §3.1

(defun parse-element (str pos)
  "Parse an XML element whose opening '<' has already been consumed.
POS points to the first character of the tag name.
Returns a cons (node . new-pos)."
  (destructuring-bind (tag . after-tag) (parse-name str pos)
    (destructuring-bind (attributes . after-attrs) (parse-attributes str after-tag)
      (let ((pos (skip-whitespace str after-attrs)))
        (cond
          ;; Self-closing tag: />
          ((and (< (1+ pos) (length str))
                (char= (char str pos) #\/)
                (char= (char str (1+ pos)) #\>))
           (cons (make-xml-node :tag tag :attributes attributes :children '())
                 (+ pos 2)))
          ;; Opening tag: >  …content…  </tag>
          ((and (< pos (length str))
                (char= (char str pos) #\>))
           (multiple-value-bind (children end-pos)
               (parse-children str (1+ pos) tag)
             (cons (make-xml-node :tag tag :attributes attributes :children children)
                   end-pos)))
          (t
           (error "Expected '>' or '/>' after attributes of '~a' at position ~a"
                  tag pos)))))))

(defun parse-children (str pos parent-tag)
  "Parse the content of an element until the matching closing tag.
Each child node is preserved as its appropriate type:
  - xml-node for child elements
  - xml-comment for <!-- … --> comments
  - xml-pi for <?target data?> processing instructions
  - xml-cdata for <![CDATA[…]]> sections
  - string for character data (whitespace-only runs are discarded)
Returns two values: (children new-pos)."
  (let ((children '()))
    (loop
      (when (>= pos (length str))
        (error "Unexpected end of input while parsing children of <~a>" parent-tag))
      (let ((ch (char str pos)))
        (cond
          ;; Character data (possibly containing entity references)
          ((char/= ch #\<)
           (destructuring-bind (text . new-pos) (parse-content-text str pos)
             (unless (every #'xml-whitespace-p text)
               (push text children))
             (setf pos new-pos)))
          ;; Markup starting with '<'
          (t
           (incf pos)                   ; consume '<'
           (when (>= pos (length str))
             (error "Unexpected end of input after '<'"))
           (let ((next (char str pos)))
             (cond
               ;; Closing tag: </parent-tag>
               ((char= next #\/)
                (incf pos)              ; consume '/'
                (destructuring-bind (tag . after-tag) (parse-name str pos)
                  (unless (string= tag parent-tag)
                    (error "Mismatched closing tag: expected </~a>, got </~a>"
                           parent-tag tag))
                  (let ((after-ws (skip-whitespace str after-tag)))
                    (unless (and (< after-ws (length str))
                                 (char= (char str after-ws) #\>))
                      (error "Expected '>' to close </~a> at position ~a"
                             tag after-ws))
                    (return (values (nreverse children) (1+ after-ws))))))
               ;; Nodes beginning with '<!'
               ((char= next #\!)
                (incf pos)              ; consume '!'
                (cond
                  ;; Comment: <!--
                  ((and (< (1+ pos) (length str))
                        (char= (char str pos) #\-)
                        (char= (char str (1+ pos)) #\-))
                   (destructuring-bind (comment-node . new-pos)
                       (parse-comment str (+ pos 2))
                     (push comment-node children)
                     (setf pos new-pos)))
                  ;; CDATA section: <![CDATA[
                  ((and (<= (+ pos 7) (length str))
                        (string= str "[CDATA[" :start1 pos :end1 (+ pos 7)))
                   (let* ((cdata-start (+ pos 7))
                          (end (search "]]>" str :start2 cdata-start)))
                     (unless end
                       (error "Unterminated CDATA section"))
                     (push (make-xml-cdata :data (subseq str cdata-start end))
                           children)
                     (setf pos (+ end 3))))
                  (t
                   (error "Unexpected '<!' at position ~a" (- pos 2)))))
               ;; Processing instruction: <?
               ((char= next #\?)
                (destructuring-bind (pi-node . new-pos)
                    (parse-pi str (1+ pos))
                  (push pi-node children)
                  (setf pos new-pos)))
               ;; Child element
               (t
                (destructuring-bind (child . new-pos) (parse-element str pos)
                  (push child children)
                  (setf pos new-pos)))))))))))

;;; Document prolog — XML 1.0 §2.8

(defun parse-prolog (str pos)
  "Parse the XML document prolog, collecting xml-comment and xml-pi nodes.
DOCTYPE declarations are recognized and skipped (not included in the output).
Returns two values: (prolog-nodes new-pos) where new-pos points to the '<'
of the root element."
  (let ((nodes '()))
    (loop
      (setf pos (skip-whitespace str pos))
      (unless (and (< pos (length str)) (char= (char str pos) #\<))
        (return))
      (let ((peek (and (< (1+ pos) (length str)) (char str (1+ pos)))))
        (cond
          ;; Comment: <!--
          ((and (eql peek #\!)
                (< (+ pos 3) (length str))
                (char= (char str (+ pos 2)) #\-)
                (char= (char str (+ pos 3)) #\-))
           (destructuring-bind (comment-node . new-pos)
               (parse-comment str (+ pos 4))
             (push comment-node nodes)
             (setf pos new-pos)))
          ;; Processing instruction (includes XML declaration): <?
          ((eql peek #\?)
           (destructuring-bind (pi-node . new-pos)
               (parse-pi str (+ pos 2))
             (push pi-node nodes)
             (setf pos new-pos)))
          ;; DOCTYPE: <!DOCTYPE
          ((and (eql peek #\!)
                (<= (+ pos 9) (length str))
                (string= str "<!DOCTYPE" :start1 pos :end1 (+ pos 9)))
           (setf pos (skip-doctype str (+ pos 9))))
          ;; Anything else is the root element
          (t (return)))))
    (values (nreverse nodes) pos)))

;;; Encoding resolution — XML 1.0 §4.3.3, Appendix F

(defun parse-xml-declaration-attrs (data)
  "Parse the pseudo-attributes from an XML declaration PI data string DATA.
Appends a '/>' sentinel so that parse-attributes sees a valid attribute-list
terminator (it stops when it encounters '>' or '/').  This sentinel must stay
in sync with parse-attributes' termination check.
Returns an alist of (name . value) pairs."
  (car (parse-attributes (concatenate 'string data "/>") 0)))

(defun resolve-encoding (prolog)
  "Inspect the XML declaration in PROLOG (if any) and validate the declared
encoding.  UTF-8 (case-insensitive) and the absence of any encoding declaration
are both accepted as the default.  Any other declared encoding signals an error."
  (let* ((decl (find-if (lambda (node)
                          (and (xml-pi-p node)
                               (string= "xml" (xml-pi-target node))))
                        prolog))
         (encoding (when decl
                     (cdr (assoc "encoding"
                                 (parse-xml-declaration-attrs (xml-pi-data decl))
                                 :test #'string=)))))
    (when (and encoding (not (string-equal encoding "UTF-8")))
      (error "Unsupported encoding '~a': only UTF-8 is supported. ~
              Transcode the document to UTF-8 before passing it to parse-xml, ~
              or omit the encoding declaration to use the default (UTF-8)."
             encoding))))

;;; Public API

(defun parse-xml (str)
  "Parse STR as an XML document and return an xml-document node.
The prolog field contains xml-comment and xml-pi nodes from before the root
element; DOCTYPE declarations are skipped.
The root field contains the root xml-node.
Inside elements, comments become xml-comment nodes, processing instructions
become xml-pi nodes, CDATA sections become xml-cdata nodes, and character
data is returned as strings.
Entity references (&amp; &lt; &gt; &quot; &apos; &#N; &#xN;) are expanded."
  (let ((str (strip-bom str)))
    (multiple-value-bind (prolog-nodes pos) (parse-prolog str 0)
      (resolve-encoding prolog-nodes)
      (unless (and (< pos (length str)) (char= (char str pos) #\<))
        (error "Expected root element at position ~a" pos))
      (let ((result (parse-element str (1+ pos))))
        (make-xml-document :prolog prolog-nodes :root (car result))))))
