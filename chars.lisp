(in-package #:io.github.cl-sdk.xml)

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

;;; Low-level stream helpers

(defun skip-whitespace (stream)
  "Advance past XML whitespace characters in STREAM."
  (loop while (let ((ch (peek-char nil stream nil nil)))
                (and ch (xml-whitespace-p ch)))
        do (read-char stream)))

(defun parse-name (stream)
  "Parse an XML Name (tag or attribute name).
Validates NameStartChar for the first character and NameChar for the rest.
Returns the name as a string."
  (let ((first-ch (peek-char nil stream nil nil)))
    (unless (and first-ch (name-start-char-p first-ch))
      (error "Invalid XML name start character"))
    (let ((buf (make-array 8 :element-type 'character :adjustable t :fill-pointer 0)))
      (vector-push-extend (read-char stream) buf)
      (loop while (let ((ch (peek-char nil stream nil nil)))
                    (and ch (name-char-p ch)))
            do (vector-push-extend (read-char stream) buf))
      (copy-seq buf))))

;;; Entity and character reference expansion — XML 1.0 §4.1, §4.6

(defun expand-char-ref (stream)
  "Expand a character reference starting just after '&#'.
Returns the expanded string."
  (let ((hex-p (eql (peek-char nil stream nil nil) #\x)))
    (when hex-p (read-char stream))
    (let ((buf (make-array 4 :element-type 'character :adjustable t :fill-pointer 0)))
      (loop while (let ((ch (peek-char nil stream nil nil)))
                    (and ch (digit-char-p ch (if hex-p 16 10))))
            do (vector-push-extend (read-char stream) buf))
      (when (zerop (fill-pointer buf))
        (error "Empty character reference"))
      (unless (eql (peek-char nil stream nil nil) #\;)
        (error "Unterminated character reference"))
      (read-char stream)                ; consume ';'
      (string (code-char (parse-integer (copy-seq buf) :radix (if hex-p 16 10)))))))

(defun expand-entity-ref (stream)
  "Parse and expand a predefined entity or character reference starting just
after '&'.  Supports &amp; &lt; &gt; &quot; &apos; and &#N; / &#xN;.
Returns the expanded string."
  (if (eql (peek-char nil stream nil nil) #\#)
      (progn
        (read-char stream)              ; consume '#'
        (expand-char-ref stream))
      (let ((name (parse-name stream)))
        (unless (eql (peek-char nil stream nil nil) #\;)
          (error "Unterminated entity reference '&~a'" name))
        (read-char stream)              ; consume ';'
        (let ((expansion (cdr (assoc name
                                     '(("amp"  . "&")
                                       ("lt"   . "<")
                                       ("gt"   . ">")
                                       ("quot" . "\"")
                                       ("apos" . "'"))
                                     :test #'string=))))
          (unless expansion
            (error "Unknown entity reference '&~a;'" name))
          expansion))))

;;; Attribute value parsing — XML 1.0 §2.3, §3.3.3

(defun parse-attribute-value (stream)
  "Parse a single- or double-quoted attribute value from STREAM,
expanding entity and character references and forbidding bare '<'.
Returns the value string."
  (let ((quote (read-char stream nil nil)))
    (unless (member quote '(#\" #\'))
      (error "Expected a quote character"))
    (let ((buf (make-array 0 :element-type 'character
                             :adjustable t :fill-pointer 0)))
      (loop
        (let ((ch (peek-char nil stream nil nil)))
          (unless ch (error "Unterminated attribute value"))
          (cond
            ((char= ch quote)
             (read-char stream)
             (return))
            ((char= ch #\<)
             (error "Illegal '<' in attribute value"))
            ((char= ch #\&)
             (read-char stream)         ; consume '&'
             (let ((expansion (expand-entity-ref stream)))
               (loop for c across expansion do (vector-push-extend c buf))))
            (t
             (vector-push-extend (read-char stream) buf)))))
      (copy-seq buf))))

;;; Attribute list parsing — XML 1.0 §3.1

(defun parse-attributes (stream)
  "Parse zero or more attributes from STREAM, stopping at '>' or '/>'.
Detects duplicate attribute names.
Returns an alist of (name . value) pairs."
  (let ((attributes '()))
    (loop
      (skip-whitespace stream)
      (let ((ch (peek-char nil stream nil nil)))
        (when (or (null ch) (member ch '(#\> #\/)))
          (return)))
      (let ((name (parse-name stream)))
        (when (assoc name attributes :test #'string=)
          (error "Duplicate attribute '~a'" name))
        (skip-whitespace stream)
        (unless (eql (peek-char nil stream nil nil) #\=)
          (error "Expected '=' after attribute name '~a'" name))
        (read-char stream)              ; consume '='
        (skip-whitespace stream)
        (let ((value (parse-attribute-value stream)))
          (push (cons name value) attributes))))
    (nreverse attributes)))
