(in-package #:io.github.cl-sdk.xml)

;;; Comment parsing — XML 1.0 §2.5

(defun parse-comment (stream)
  "Parse a comment body and closing '-->'.  STREAM must be just past '<!--'.
Returns an xml-comment node."
  (let ((buf (make-array 0 :element-type 'character
                           :adjustable t :fill-pointer 0)))
    (loop
      (let ((ch (read-char stream nil nil)))
        (unless ch (error "Unterminated comment"))
        (cond
          ((char= ch #\-)
           (let ((ch2 (peek-char nil stream nil nil)))
             (if (eql ch2 #\-)
                 (progn
                   (read-char stream)   ; consume second '-'
                   (let ((ch3 (peek-char nil stream nil nil)))
                     (if (eql ch3 #\>)
                         (progn
                           (read-char stream) ; consume '>'
                           (return (make-xml-comment :data (copy-seq buf))))
                         (error "Illegal '--' inside comment"))))
                 (vector-push-extend ch buf))))
          (t
           (vector-push-extend ch buf)))))))

;;; Processing instruction parsing — XML 1.0 §2.6

(defun parse-pi (stream)
  "Parse a processing instruction body and closing '?>'.
STREAM must be just past the opening '<?'.
Returns an xml-pi node."
  (let ((target (parse-name stream)))
    (skip-whitespace stream)
    (let ((buf (make-array 0 :element-type 'character
                             :adjustable t :fill-pointer 0)))
      (loop
        (let ((ch (read-char stream nil nil)))
          (unless ch (error "Unterminated processing instruction"))
          (cond
            ((char= ch #\?)
             (let ((ch2 (peek-char nil stream nil nil)))
               (if (eql ch2 #\>)
                   (progn
                     (read-char stream) ; consume '>'
                     (return (make-xml-pi :target target :data (copy-seq buf))))
                   (vector-push-extend ch buf))))
            (t
             (vector-push-extend ch buf))))))))

;;; DTD Nmtoken — XML 1.0 §2.3

(defun %parse-dtd-nmtoken (stream)
  "Parse an XML Nmtoken — one or more NameChar characters.
Unlike parse-name, the first character need not be a NameStartChar.
Returns the token as a string."
  (let ((buf (make-array 4 :element-type 'character :adjustable t :fill-pointer 0)))
    (let ((first-ch (peek-char nil stream nil nil)))
      (unless (and first-ch (name-char-p first-ch))
        (error "Invalid Nmtoken: expected at least one NameChar")))
    (loop while (let ((ch (peek-char nil stream nil nil)))
                  (and ch (name-char-p ch)))
          do (vector-push-extend (read-char stream) buf))
    (copy-seq buf)))

;;; DTD AttType parsing — XML 1.0 §3.3.1

(defun %parse-dtd-att-type (stream)
  "Parse an AttType (attribute type) from STREAM.
Returns one of:
  :cdata :id :idref :idrefs :entity :entities :nmtoken :nmtokens — keyword types
  (:notation name+)      — NOTATION enumeration
  (:enumeration token+)  — Nmtoken enumeration"
  (skip-whitespace stream)
  (let ((ch (peek-char nil stream nil nil)))
    (if (eql ch #\()
        ;; Enumeration: '(' S? Nmtoken (S? '|' S? Nmtoken)* S? ')'
        (progn
          (read-char stream)              ; consume '('
          (skip-whitespace stream)
          (let ((tokens (list (%parse-dtd-nmtoken stream))))
            (skip-whitespace stream)
            (loop while (eql (peek-char nil stream nil nil) #\|)
                  do (read-char stream)   ; consume '|'
                  do (skip-whitespace stream)
                  do (push (%parse-dtd-nmtoken stream) tokens)
                  do (skip-whitespace stream))
            (unless (eql (peek-char nil stream nil nil) #\))
              (error "Expected ')' in enumerated attribute type"))
            (read-char stream)            ; consume ')'
            (cons :enumeration (nreverse tokens))))
        ;; Named type keyword or NOTATION
        (let ((kw (parse-name stream)))
          (cond
            ((string= kw "CDATA")    :cdata)
            ((string= kw "ID")       :id)
            ((string= kw "IDREF")    :idref)
            ((string= kw "IDREFS")   :idrefs)
            ((string= kw "ENTITY")   :entity)
            ((string= kw "ENTITIES") :entities)
            ((string= kw "NMTOKEN")  :nmtoken)
            ((string= kw "NMTOKENS") :nmtokens)
            ((string= kw "NOTATION")
             ;; NotationType: NOTATION S '(' S? Name (S? '|' S? Name)* S? ')'
             (skip-whitespace stream)
             (unless (eql (peek-char nil stream nil nil) #\()
               (error "Expected '(' after NOTATION in attribute type"))
             (read-char stream)           ; consume '('
             (skip-whitespace stream)
             (let ((names (list (parse-name stream))))
               (skip-whitespace stream)
               (loop while (eql (peek-char nil stream nil nil) #\|)
                     do (read-char stream) ; consume '|'
                     do (skip-whitespace stream)
                     do (push (parse-name stream) names)
                     do (skip-whitespace stream))
               (unless (eql (peek-char nil stream nil nil) #\))
                 (error "Expected ')' in NOTATION attribute type"))
               (read-char stream)         ; consume ')'
               (cons :notation (nreverse names))))
            (t
             (error "Unknown DTD attribute type '~a'" kw)))))))

;;; DTD DefaultDecl parsing — XML 1.0 §3.3.2

(defun %parse-dtd-att-default (stream)
  "Parse a DefaultDecl from STREAM.
Returns :required, :implied, (:fixed value), or (:default value)."
  (let ((ch (peek-char nil stream nil nil)))
    (if (eql ch #\#)
        (progn
          (read-char stream)              ; consume '#'
          (let ((kw (parse-name stream)))
            (cond
              ((string= kw "REQUIRED") :required)
              ((string= kw "IMPLIED")  :implied)
              ((string= kw "FIXED")
               (skip-whitespace stream)
               (list :fixed (parse-attribute-value stream)))
              (t
               (error "Expected REQUIRED, IMPLIED, or FIXED after '#' in DTD default, got '#~a'" kw)))))
        ;; Bare AttValue (no '#' keyword) — an implied default
        (list :default (parse-attribute-value stream)))))

;;; DTD ATTLIST declaration — XML 1.0 §3.3

(defun %parse-dtd-attlist-decl (stream)
  "Parse a DTD ATTLIST declaration.
STREAM must be positioned just after 'ATTLIST' (whitespace not yet consumed).
Consumes through and including the closing '>'.
Returns an xml-dtd-attlist struct."
  (skip-whitespace stream)
  (let ((element-name (parse-name stream))
        (definitions '()))
    (loop
      (skip-whitespace stream)
      (let ((ch (peek-char nil stream nil nil)))
        (cond
          ((null ch)    (error "Unterminated DTD ATTLIST declaration"))
          ((eql ch #\>) (read-char stream) (return))   ; consume '>' and exit
          (t
           (let ((att-name (parse-name stream)))
             (skip-whitespace stream)
             (let ((att-type (%parse-dtd-att-type stream)))
               (skip-whitespace stream)
               (push (make-xml-dtd-att-def :name    att-name
                                           :type    att-type
                                           :default (%parse-dtd-att-default stream))
                     definitions)))))))
    (make-xml-dtd-attlist :element-name element-name
                          :definitions  (nreverse definitions))))

;;; DTD ENTITY declaration — XML 1.0 §4.2

(defun %parse-dtd-entity-decl (stream)
  "Parse a DTD ENTITY declaration (general or parameter, internal or external).
STREAM must be positioned just after 'ENTITY' (whitespace not yet consumed).
Consumes through and including the closing '>'.
Returns an xml-dtd-entity struct.

DEFINITION field of the returned struct:
  string                    — internal entity; the raw replacement text
  (:external pub sys)       — external parsed entity
  (:unparsed pub sys ndata) — external unparsed entity (general entities with NDATA)"
  (skip-whitespace stream)
  ;; Optional '%' marks a parameter entity
  (let ((parameter-p (eql (peek-char nil stream nil nil) #\%)))
    (when parameter-p
      (read-char stream)                ; consume '%'
      (skip-whitespace stream))         ; required S between '%' and Name
    (let ((name (parse-name stream)))
      (skip-whitespace stream)
      (let* ((next (peek-char nil stream nil nil))
             (definition
               (cond
                 ;; Internal entity: EntityValue (single- or double-quoted)
                 ((member next '(#\" #\') :test #'eql)
                  (%parse-dtd-quoted-string stream))
                 ;; External entity: SYSTEM or PUBLIC keyword
                 ((and next (name-start-char-p next))
                  (let ((keyword (parse-name stream)))
                    (cond
                      ((string= keyword "SYSTEM")
                       (skip-whitespace stream)
                       (list :external nil (%parse-dtd-quoted-string stream)))
                      ((string= keyword "PUBLIC")
                       (skip-whitespace stream)
                       (let ((pub (%parse-dtd-quoted-string stream)))
                         (skip-whitespace stream)
                         (list :external pub (%parse-dtd-quoted-string stream))))
                      (t
                       (error "Expected SYSTEM, PUBLIC, or quoted value in entity declaration, got '~a'"
                              keyword)))))
                 (t
                  (error "Expected entity value or external identifier in entity declaration")))))
        ;; For external general entities, check for optional NDATA clause
        (skip-whitespace stream)
        (when (and (not parameter-p)
                   (consp definition)
                   (eq (car definition) :external))
          (let ((ch (peek-char nil stream nil nil)))
            (when (and ch (name-start-char-p ch))
              (let ((kw (parse-name stream)))
                (unless (string= kw "NDATA")
                  (error "Expected 'NDATA' or '>' in external entity declaration, got '~a'" kw))
                (skip-whitespace stream)
                (let ((ndata-name (parse-name stream)))
                  (setf definition
                        (list :unparsed (second definition) (third definition) ndata-name))
                  (skip-whitespace stream))))))
        (unless (eql (read-char stream nil nil) #\>)
          (error "Expected '>' to close DTD ENTITY declaration for '~a'" name))
        (make-xml-dtd-entity :name        name
                              :parameter-p parameter-p
                              :definition  definition)))))

;;; DTD NOTATION declaration — XML 1.0 §4.7

(defun %parse-dtd-notation-decl (stream)
  "Parse a DTD NOTATION declaration.
STREAM must be positioned just after 'NOTATION' (whitespace not yet consumed).
Consumes through and including the closing '>'.
Returns an xml-dtd-notation struct."
  (skip-whitespace stream)
  (let ((name (parse-name stream)))
    (skip-whitespace stream)
    (let ((keyword (parse-name stream)))
      (multiple-value-bind (public-id system-id)
          (cond
            ((string= keyword "SYSTEM")
             (skip-whitespace stream)
             (values nil (%parse-dtd-quoted-string stream)))
            ((string= keyword "PUBLIC")
             (skip-whitespace stream)
             (let ((pub (%parse-dtd-quoted-string stream)))
               ;; System literal is optional in NOTATION (PublicID context)
               (skip-whitespace stream)
               (let ((sys (when (member (peek-char nil stream nil nil) '(#\" #\'))
                            (%parse-dtd-quoted-string stream))))
                 (values pub sys))))
            (t
             (error "Expected SYSTEM or PUBLIC in NOTATION declaration, got '~a'" keyword)))
        (skip-whitespace stream)
        (unless (eql (read-char stream nil nil) #\>)
          (error "Expected '>' to close DTD NOTATION declaration for '~a'" name))
        (make-xml-dtd-notation :name      name
                                :public-id public-id
                                :system-id system-id)))))

(defun %parse-dtd-quoted-string (stream)
  "Parse a quoted string (single- or double-quoted) for DOCTYPE external identifiers.
Returns the string contents."
  (let ((quote (read-char stream nil nil)))
    (unless (member quote '(#\" #\') :test #'char=)
      (error "Expected a quote character in DOCTYPE external identifier"))
    (let ((buf (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
      (loop
        (let ((ch (read-char stream nil nil)))
          (unless ch (error "Unterminated external identifier in DOCTYPE"))
          (if (char= ch quote)
              (return (copy-seq buf))
              (vector-push-extend ch buf)))))))

(defun %skip-dtd-to-close-angle (stream)
  "Skip characters until the first unquoted '>' is consumed.
Used to skip DTD markup declarations that are not parsed (ATTLIST, ENTITY, etc.)."
  (loop
    (let ((ch (read-char stream nil nil)))
      (unless ch (error "Unterminated markup declaration in DOCTYPE internal subset"))
      (cond
        ((char= ch #\>) (return))
        ;; Skip over quoted strings so embedded '>' characters don't end the decl.
        ((member ch '(#\" #\') :test #'char=)
         (loop
           (let ((qch (read-char stream nil nil)))
             (unless qch (error "Unterminated quoted string in markup declaration"))
             (when (char= qch ch) (return)))))))))

(defun %parse-dtd-cp (stream)
  "Parse a content particle (cp): a Name or a group, with an optional quantifier.
Returns a string (Name), or (:? c), (:* c), (:+ c), (:seq cp*), or (:choice cp*)."
  (skip-whitespace stream)
  (let ((ch (peek-char nil stream nil nil)))
    (when (null ch) (error "Unexpected end of input in DTD content particle"))
    (let ((content (if (char= ch #\()
                       (%parse-dtd-group stream)
                       (parse-name stream))))
      ;; Quantifier (no whitespace allowed between content and quantifier per spec)
      (let ((q (peek-char nil stream nil nil)))
        (cond
          ((eql q #\?) (read-char stream) (list :? content))
          ((eql q #\*) (read-char stream) (list :* content))
          ((eql q #\+) (read-char stream) (list :+ content))
          (t content))))))

(defun %parse-dtd-mixed (stream)
  "Parse mixed content model starting at '#' (opening '(' already consumed).
Consumes '#PCDATA', optional '| Name' pairs, the closing ')', and optional '*'.
Returns (:mixed name*)."
  (loop for expected across "#PCDATA"
        do (let ((c (read-char stream nil nil)))
             (unless (and c (char= c expected))
               (error "Expected #PCDATA in DTD mixed content model"))))
  (skip-whitespace stream)
  (let ((names '()))
    (loop
      (let ((ch (peek-char nil stream nil nil)))
        (cond
          ((null ch) (error "Unterminated DTD mixed content model"))
          ((char= ch #\))
           (read-char stream)                   ; consume ')'
           (when (eql (peek-char nil stream nil nil) #\*)
             (read-char stream))                ; consume optional '*'
           (return (list* :mixed (nreverse names))))
          ((char= ch #\|)
           (read-char stream)                   ; consume '|'
           (skip-whitespace stream)
           (push (parse-name stream) names)
           (skip-whitespace stream))
          (t
           (error "Expected ')' or '|' in DTD mixed content model, got '~c'" ch)))))))

(defun %parse-dtd-seq-or-choice (stream)
  "Parse a sequence or choice element-content group (opening '(' already consumed).
Consumes content particles and the closing ')'.
Returns (:seq cp*) or (:choice cp*)."
  (let ((first-cp (%parse-dtd-cp stream)))
    (skip-whitespace stream)
    (let ((sep (peek-char nil stream nil nil)))
      (cond
        ((null sep) (error "Unexpected end of input in DTD content group"))
        ;; Single cp wrapped in a group — treat as a one-element sequence.
        ((char= sep #\))
         (read-char stream)              ; consume ')'
         (list :seq first-cp))
        ;; Sequence: (cp , cp , ...)
        ((char= sep #\,)
         (let ((children (list first-cp)))
           (loop while (progn (skip-whitespace stream)
                              (eql (peek-char nil stream nil nil) #\,))
                 do (read-char stream)   ; consume ','
                 do (push (%parse-dtd-cp stream) children))
           (skip-whitespace stream)
           (unless (eql (peek-char nil stream nil nil) #\))
             (error "Expected ')' at end of DTD sequence group"))
           (read-char stream)
           (cons :seq (nreverse children))))
        ;; Choice: (cp | cp | ...)
        ((char= sep #\|)
         (let ((children (list first-cp)))
           (loop while (progn (skip-whitespace stream)
                              (eql (peek-char nil stream nil nil) #\|))
                 do (read-char stream)   ; consume '|'
                 do (push (%parse-dtd-cp stream) children))
           (skip-whitespace stream)
           (unless (eql (peek-char nil stream nil nil) #\))
             (error "Expected ')' at end of DTD choice group"))
           (read-char stream)
           (cons :choice (nreverse children))))
        (t
         (error "Expected ',', '|', or ')' in DTD content group, got '~c'" sep))))))

(defun %parse-dtd-group (stream)
  "Parse a content group '(' body ')'.
Consumes the opening '(' and closing ')'.
Does NOT consume a trailing quantifier (except mixed content which absorbs its '*').
Returns (:seq cp*), (:choice cp*), or (:mixed name*)."
  (read-char stream)                    ; consume '('
  (skip-whitespace stream)
  (let ((next (peek-char nil stream nil nil)))
    (cond
      ((null next) (error "Unexpected end of input in DTD content group"))
      ((char= next #\#) (%parse-dtd-mixed stream))
      (t              (%parse-dtd-seq-or-choice stream)))))

(defun parse-dtd-content-model (stream)
  "Parse a DTD element content model specification from STREAM.
Returns one of:
  :empty               — EMPTY
  :any                 — ANY
  (:mixed name*)       — mixed content (#PCDATA possibly with element names)
  content-particle     — element content group with optional top-level quantifier"
  (skip-whitespace stream)
  (let ((ch (peek-char nil stream nil nil)))
    (cond
      ((null ch) (error "Unexpected end of input in DTD content model"))
      ((char= ch #\()
       (let ((group (%parse-dtd-group stream)))
         ;; Mixed content already absorbed its quantifier inside %parse-dtd-mixed.
         ;; Element content groups may carry a top-level quantifier.
         (if (and (consp group) (eq (car group) :mixed))
             group
             (let ((q (peek-char nil stream nil nil)))
               (cond
                 ((eql q #\?) (read-char stream) (list :? group))
                 ((eql q #\*) (read-char stream) (list :* group))
                 ((eql q #\+) (read-char stream) (list :+ group))
                 (t group))))))
      (t
       (let ((kw (parse-name stream)))
         (cond
           ((string= kw "EMPTY") :empty)
           ((string= kw "ANY")   :any)
           (t (error "Expected EMPTY, ANY, or '(' in DTD content model, got '~a'" kw))))))))

;;; DTD internal subset parsing

(defun %parse-dtd-element-decl (stream)
  "Parse a DTD ELEMENT declaration.
STREAM must be positioned just after 'ELEMENT' (whitespace not yet consumed).
Consumes through and including the closing '>'.
Returns an xml-dtd-element struct."
  (skip-whitespace stream)
  (let ((name (parse-name stream)))
    (skip-whitespace stream)
    (let ((content-model (parse-dtd-content-model stream)))
      (skip-whitespace stream)
      (unless (eql (read-char stream nil nil) #\>)
        (error "Expected '>' to close DTD ELEMENT declaration for '~a'" name))
      (make-xml-dtd-element :name name :content-model content-model))))

(defun %parse-dtd-internal-subset (stream)
  "Parse the internal subset of a DOCTYPE declaration.
STREAM is positioned just after '['.
Consumes through and including the closing ']'.
Returns (values elements attlists entities notations)."
  (let (elements attlists entities notations)
    (loop
      (skip-whitespace stream)
      (let ((ch (peek-char nil stream nil nil)))
        (unless ch (error "Unterminated DOCTYPE internal subset"))
        (cond
          ;; End of internal subset
          ((char= ch #\])
           (read-char stream)            ; consume ']'
           (return (values (nreverse elements)
                           (nreverse attlists)
                           (nreverse entities)
                           (nreverse notations))))
          ;; Parameter entity reference %name; — skip (PE refs between declarations
          ;; are legal in the internal subset but we do not expand them)
          ((char= ch #\%)
           (read-char stream)            ; consume '%'
           (parse-name stream)           ; consume name
           (unless (eql (read-char stream nil nil) #\;)
             (error "Expected ';' after parameter entity name in DTD")))
          ;; Markup declaration or comment starting with '<'
          ((char= ch #\<)
           (read-char stream)            ; consume '<'
           (let ((next (peek-char nil stream nil nil)))
             (unless next (error "Unexpected end of input in DOCTYPE internal subset"))
             (cond
               ;; Processing instruction: <? — parse and discard
               ((char= next #\?)
                (read-char stream)       ; consume '?'
                (parse-pi stream))
               ;; Markup declaration: <!
               ((char= next #\!)
                (read-char stream)       ; consume '!'
                (let ((after-bang (peek-char nil stream nil nil)))
                  (cond
                    ;; Comment: <!-- — parse and discard
                    ((eql after-bang #\-)
                     (read-char stream)  ; consume first '-'
                     (unless (eql (peek-char nil stream nil nil) #\-)
                       (error "Expected second '-' in comment in DOCTYPE internal subset"))
                     (read-char stream)  ; consume second '-'
                     (parse-comment stream))
                    ;; Named declaration: ELEMENT, ATTLIST, ENTITY, NOTATION
                    ((and after-bang (name-start-char-p after-bang))
                     (let ((decl-type (parse-name stream)))
                       (cond
                         ((string= decl-type "ELEMENT")
                          (push (%parse-dtd-element-decl stream) elements))
                         ((string= decl-type "ATTLIST")
                          (push (%parse-dtd-attlist-decl stream) attlists))
                         ((string= decl-type "ENTITY")
                          (push (%parse-dtd-entity-decl stream) entities))
                         ((string= decl-type "NOTATION")
                          (push (%parse-dtd-notation-decl stream) notations))
                         (t
                          (error "Unknown markup declaration type '~a' in DOCTYPE internal subset"
                                 decl-type)))))
                    (t
                     (error "Unexpected '<!~a' in DOCTYPE internal subset"
                            (or after-bang ""))))))
               (t
                (error "Unexpected '<~c' in DOCTYPE internal subset" next)))))
          (t
           (error "Unexpected character '~c' in DOCTYPE internal subset" ch)))))))

;;; DOCTYPE declaration parsing — XML 1.0 §2.8

(defun parse-doctype (stream)
  "Parse a DOCTYPE declaration.
STREAM must be positioned just after '<!DOCTYPE' (whitespace not yet consumed).
Consumes through and including the closing '>'.
Returns an xml-doctype struct."
  (skip-whitespace stream)
  (let ((name (parse-name stream))
        public-id system-id elements attlists entities notations)
    (skip-whitespace stream)
    ;; Optional external identifier: SYSTEM or PUBLIC
    (let ((ch (peek-char nil stream nil nil)))
      (when (and ch (name-start-char-p ch))
        (let ((keyword (parse-name stream)))
          (cond
            ((string= keyword "SYSTEM")
             (skip-whitespace stream)
             (setf system-id (%parse-dtd-quoted-string stream)))
            ((string= keyword "PUBLIC")
             (skip-whitespace stream)
             (setf public-id (%parse-dtd-quoted-string stream))
             (skip-whitespace stream)
             ;; System identifier is optional after PUBLIC in DOCTYPE declarations.
             (when (member (peek-char nil stream nil nil) '(#\" #\'))
               (setf system-id (%parse-dtd-quoted-string stream))))
            (t
             (error "Expected SYSTEM or PUBLIC in DOCTYPE declaration, got '~a'"
                    keyword))))))
    (skip-whitespace stream)
    ;; Optional internal subset
    (when (eql (peek-char nil stream nil nil) #\[)
      (read-char stream)                ; consume '['
      (multiple-value-setq (elements attlists entities notations)
        (%parse-dtd-internal-subset stream)))
    (skip-whitespace stream)
    (unless (eql (read-char stream nil nil) #\>)
      (error "Expected '>' to close DOCTYPE declaration"))
    (make-xml-doctype :name      name
                      :public-id public-id
                      :system-id system-id
                      :elements  elements
                      :attlists  attlists
                      :entities  entities
                      :notations notations)))

;;; Character data parsing — XML 1.0 §2.4

(defun parse-content-text (stream)
  "Parse character data and entity/character references from STREAM up to
the next '<'.  Returns the text string."
  (let ((buf (make-array 0 :element-type 'character
                           :adjustable t :fill-pointer 0)))
    (loop
      (let ((ch (peek-char nil stream nil nil)))
        (when (or (null ch) (char= ch #\<))
          (return)))
      (let ((ch (read-char stream)))
        (cond
          ((char= ch #\&)
           (let ((expansion (expand-entity-ref stream)))
             (loop for c across expansion do (vector-push-extend c buf))))
          (t
           (vector-push-extend ch buf)))))
    (copy-seq buf)))

;;; CDATA section parsing — XML 1.0 §2.7

(defun parse-cdata-section (stream)
  "Parse a CDATA section.  STREAM must be just past '<![CDATA['.
Returns the raw content string."
  (let ((buf (make-array 0 :element-type 'character
                           :adjustable t :fill-pointer 0)))
    (loop
      (let ((ch (read-char stream nil nil)))
        (unless ch (error "Unterminated CDATA section"))
        (cond
          ((char= ch #\])
           (let ((ch2 (peek-char nil stream nil nil)))
             (cond
               ((eql ch2 #\])
                (read-char stream)      ; consume second ']'
                (let ((ch3 (peek-char nil stream nil nil)))
                  (cond
                    ((eql ch3 #\>)
                     (read-char stream) ; consume '>'
                     (return (copy-seq buf)))
                    ;; ']]' not followed by '>' — emit first ']', unread second ']' for reprocessing
                    (t
                     (vector-push-extend ch buf)
                     (unread-char #\] stream)))))
               (t
                (vector-push-extend ch buf)))))
          (t
           (vector-push-extend ch buf)))))))
