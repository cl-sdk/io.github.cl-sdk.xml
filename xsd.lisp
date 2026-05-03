(in-package #:cl-xml)

;;;; XSD (XML Schema Definition) — subset implementation
;;;;
;;;; Supported subset (XSD 1.0):
;;;;   xs:schema with targetNamespace
;;;;   xs:element  — name, type, minOccurs, maxOccurs, ref, inline type
;;;;   xs:attribute — name, type, use, default, fixed, inline simpleType
;;;;   xs:complexType — sequence / all / choice compositors, attributes, mixed
;;;;   xs:simpleType  — xs:restriction with enumeration, pattern(*),
;;;;                    minLength, maxLength, length,
;;;;                    minInclusive, maxInclusive, minExclusive, maxExclusive,
;;;;                    totalDigits, fractionDigits
;;;;   All XSD 1.0 built-in primitive and derived types
;;;;
;;;; (*) Pattern facets are stored but not enforced (no regex dependency).

;;; ─── Namespace URI ────────────────────────────────────────────────────────

(defparameter +xsd-namespace-uri+
  "http://www.w3.org/2001/XMLSchema"
  "The XML Schema Definition namespace URI (XSD 1.0).")

;;; ─── Structures ───────────────────────────────────────────────────────────

(defstruct xsd-schema
  "A parsed XML Schema Definition document.
TARGET-NAMESPACE is the target namespace URI string, or NIL.
ELEMENTS is an alist of (name . xsd-element) for top-level element declarations.
TYPES is an alist of (name . type) where each value is an xsd-complex-type
or xsd-simple-type struct."
  target-namespace
  elements
  types)

(defstruct xsd-element
  "An xs:element declaration.
NAME is the element name string, or NIL when REF is set.
TYPE is one of:
  a keyword        — built-in type (e.g. :STRING, :INTEGER, :BOOLEAN)
  a string         — name of a schema-defined type to be resolved lazily
  xsd-complex-type — anonymous inline complex type
  xsd-simple-type  — anonymous inline simple type
  NIL              — xs:anyType (unconstrained)
MIN-OCCURS is a non-negative integer (default 1).
MAX-OCCURS is a non-negative integer or :UNBOUNDED (default 1).
REF is the name string of a referenced top-level element declaration, or NIL."
  name
  type
  (min-occurs 1)
  (max-occurs 1)
  ref)

(defstruct xsd-attribute
  "An xs:attribute declaration.
NAME is the attribute name string.
TYPE follows the same conventions as xsd-element type.
USE is :OPTIONAL (default), :REQUIRED, or :PROHIBITED.
DEFAULT is the default value string, or NIL.
FIXED is the fixed value string, or NIL."
  name
  type
  (use :optional)
  default
  fixed)

(defstruct xsd-complex-type
  "An xs:complexType definition.
NAME is the type name string, or NIL for anonymous inline types.
COMPOSITOR is :SEQUENCE, :ALL, :CHOICE, or NIL (empty or text-only content).
ELEMENTS is a list of xsd-element structs — the compositor's particle children.
ATTRIBUTES is a list of xsd-attribute structs.
MIXED is T if the type allows interspersed character content."
  name
  compositor
  (elements '())
  (attributes '())
  (mixed nil))

(defstruct xsd-simple-type
  "An xs:simpleType definition.
NAME is the type name string, or NIL for anonymous inline types.
BASE is the base type: a keyword (built-in) or a string (schema-defined type name).
FACETS is a plist of restriction facets:
  :enumeration   — list of allowed value strings
  :pattern       — regex pattern string (stored but not enforced)
  :min-length    — minimum string length (integer)
  :max-length    — maximum string length (integer)
  :length        — exact string length (integer)
  :min-inclusive — inclusive lower bound (string)
  :max-inclusive — inclusive upper bound (string)
  :min-exclusive — exclusive lower bound (string)
  :max-exclusive — exclusive upper bound (string)
  :total-digits    — maximum total significant digits (integer)
  :fraction-digits — maximum fraction digits (integer)"
  name
  base
  (facets '()))

;;; ─── Validation condition ─────────────────────────────────────────────────

(define-condition xsd-validation-error (error)
  ((message :initarg :message :reader xsd-validation-error-message)
   (path    :initarg :path    :reader xsd-validation-error-path
            :initform nil))
  (:report (lambda (c s)
             (if (xsd-validation-error-path c)
                 (format s "XSD validation error at ~a: ~a"
                         (xsd-validation-error-path c)
                         (xsd-validation-error-message c))
                 (format s "XSD validation error: ~a"
                         (xsd-validation-error-message c)))))
  (:documentation
   "Condition signaled when an XML document fails XSD schema validation.
MESSAGE is a string describing the failure; PATH is the slash-delimited element
location string, or NIL when there is no specific location."))

;;; ─── Built-in type registry ───────────────────────────────────────────────

(defparameter +xsd-builtin-types+
  '("string" "normalizedString" "token"
    "boolean"
    "decimal" "float" "double"
    "duration" "dateTime" "time" "date"
    "gYearMonth" "gYear" "gMonthDay" "gDay" "gMonth"
    "hexBinary" "base64Binary"
    "anyURI" "QName" "NOTATION"
    "integer"
    "nonPositiveInteger" "negativeInteger"
    "long" "int" "short" "byte"
    "nonNegativeInteger" "positiveInteger"
    "unsignedLong" "unsignedInt" "unsignedShort" "unsignedByte"
    "NMTOKEN" "NMTOKENS" "Name" "NCName"
    "ID" "IDREF" "IDREFS" "ENTITY" "ENTITIES"
    "anyType" "anySimpleType")
  "Names of all XSD 1.0 built-in types (without namespace prefix).")

;;; ─── Parsing helpers (internal) ──────────────────────────────────────────

(defun %xsd-local-name (name-str)
  "Strip any namespace prefix from NAME-STR (e.g. \"xs:element\" → \"element\")."
  (let ((colon (position #\: name-str)))
    (if colon (subseq name-str (1+ colon)) name-str)))

(defun %node-local (node)
  "Return the local (unprefixed) name of xml-node NODE's tag."
  (let ((tag (xml-node-tag node)))
    (if (xml-qname-p tag)
        (xml-qname-local-name tag)
        (%xsd-local-name tag))))

(defun %node-xsd-p (node local-name)
  "Return T if NODE's tag has the given XSD local name LOCAL-NAME."
  (string= (%node-local node) local-name))

(defun %xsd-attr (node attr-name)
  "Return the value of attribute ATTR-NAME from xml-node NODE, or NIL if absent."
  (cdr (find attr-name (xml-node-attributes node)
             :key (lambda (a)
                    (let ((k (car a)))
                      (if (xml-qname-p k)
                          (xml-qname-local-name k)
                          (%xsd-local-name k))))
             :test #'string=)))

(defun %element-children (node)
  "Return only the xml-node children of NODE (excludes strings, comments, etc.)."
  (remove-if-not #'xml-node-p (xml-node-children node)))

(defun %parse-occurs (str default)
  "Parse a minOccurs/maxOccurs string. Returns DEFAULT when STR is NIL."
  (cond ((null str) default)
        ((string= str "unbounded") :unbounded)
        (t (parse-integer str :junk-allowed nil))))

(defun %builtin-type-p (name)
  "Return T if NAME (without any prefix) is an XSD built-in type name."
  (and (member name +xsd-builtin-types+ :test #'string=) t))

(defun %parse-type-ref (type-str)
  "Convert a type attribute value string to a keyword (built-in) or string (user-defined).
Returns NIL when TYPE-STR is NIL."
  (when type-str
    (let ((local (%xsd-local-name type-str)))
      (if (%builtin-type-p local)
          (intern (string-upcase local) :keyword)
          type-str))))

;;; ─── XSD document parsing (internal) ─────────────────────────────────────

(defun %parse-xsd-restriction (node)
  "Parse an xs:restriction element. Returns (values base-type facets-plist)."
  (let ((base   (%parse-type-ref (%xsd-attr node "base")))
        (facets '()))
    (dolist (child (%element-children node))
      (let ((local (%node-local child))
            (value (%xsd-attr child "value")))
        (cond
          ((string= local "enumeration")
           (setf (getf facets :enumeration)
                 (append (getf facets :enumeration) (list value))))
          ((string= local "pattern")
           (setf (getf facets :pattern) value))
          ((string= local "minLength")
           (setf (getf facets :min-length) (parse-integer value)))
          ((string= local "maxLength")
           (setf (getf facets :max-length) (parse-integer value)))
          ((string= local "length")
           (setf (getf facets :length) (parse-integer value)))
          ((string= local "minInclusive")
           (setf (getf facets :min-inclusive) value))
          ((string= local "maxInclusive")
           (setf (getf facets :max-inclusive) value))
          ((string= local "minExclusive")
           (setf (getf facets :min-exclusive) value))
          ((string= local "maxExclusive")
           (setf (getf facets :max-exclusive) value))
          ((string= local "totalDigits")
           (setf (getf facets :total-digits) (parse-integer value)))
          ((string= local "fractionDigits")
           (setf (getf facets :fraction-digits) (parse-integer value)))
          ;; whiteSpace and assertions are accepted but not enforced
          )))
    (values base facets)))

(defun %parse-xsd-simple-type (node)
  "Parse an xs:simpleType element into an xsd-simple-type struct."
  (let* ((name        (%xsd-attr node "name"))
         (restriction (find-if (lambda (c) (%node-xsd-p c "restriction"))
                               (%element-children node))))
    (if restriction
        (multiple-value-bind (base facets) (%parse-xsd-restriction restriction)
          (make-xsd-simple-type :name name :base base :facets facets))
        ;; xs:list and xs:union: not supported — fall back to xs:string
        (make-xsd-simple-type :name name :base :string :facets '()))))

(defun %parse-xsd-attribute-decl (node)
  "Parse an xs:attribute element into an xsd-attribute struct."
  (let* ((name      (%xsd-attr node "name"))
         (type-str  (%xsd-attr node "type"))
         (use-str   (%xsd-attr node "use"))
         (default   (%xsd-attr node "default"))
         (fixed     (%xsd-attr node "fixed"))
         (st-child  (find-if (lambda (c) (%node-xsd-p c "simpleType"))
                             (%element-children node)))
         (eff-type  (cond (st-child  (%parse-xsd-simple-type st-child))
                          (type-str  (%parse-type-ref type-str))
                          (t         :string)))
         (use       (cond ((string= use-str "required")   :required)
                          ((string= use-str "prohibited") :prohibited)
                          (t                              :optional))))
    (make-xsd-attribute :name name :type eff-type
                        :use use :default default :fixed fixed)))

(defun %parse-xsd-compositor-children (compositor-node)
  "Parse xs:element declarations inside an xs:sequence/xs:all/xs:choice node."
  (let (result)
    (dolist (child (%element-children compositor-node))
      (let ((local (%node-local child)))
        (cond
          ((string= local "element")
           (push (%parse-xsd-element child) result))
          ;; Nested compositor: inline the children (simplified flattening)
          ((member local '("sequence" "all" "choice") :test #'string=)
           (dolist (inner (%parse-xsd-compositor-children child))
             (push inner result)))
          ;; xs:group and xs:any: not supported, silently skipped
          )))
    (nreverse result)))

(defun %parse-xsd-complex-type (node)
  "Parse an xs:complexType element into an xsd-complex-type struct."
  (let ((name   (%xsd-attr node "name"))
        (mixed  (string= (%xsd-attr node "mixed") "true"))
        compositor
        (elements '())
        (attrs    '()))
    (dolist (child (%element-children node))
      (let ((local (%node-local child)))
        (cond
          ((member local '("sequence" "all" "choice") :test #'string=)
           (setf compositor (intern (string-upcase local) :keyword))
           (setf elements   (%parse-xsd-compositor-children child)))
          ((string= local "attribute")
           (push (%parse-xsd-attribute-decl child) attrs))
          ((string= local "anyAttribute")
           nil)
          ((string= local "simpleContent")
           ;; <xs:simpleContent><xs:extension base="..."><xs:attribute .../></xs:extension>
           (let ((inner (find-if (lambda (c)
                                   (member (%node-local c)
                                           '("extension" "restriction")
                                           :test #'string=))
                                 (%element-children child))))
             (when inner
               (dolist (a (%element-children inner))
                 (when (%node-xsd-p a "attribute")
                   (push (%parse-xsd-attribute-decl a) attrs))))))
          ((string= local "complexContent")
           (let ((inner (find-if (lambda (c)
                                   (member (%node-local c)
                                           '("extension" "restriction")
                                           :test #'string=))
                                 (%element-children child))))
             (when inner
               (dolist (ic (%element-children inner))
                 (let ((il (%node-local ic)))
                   (cond
                     ((member il '("sequence" "all" "choice") :test #'string=)
                      (setf compositor (intern (string-upcase il) :keyword))
                      (setf elements   (%parse-xsd-compositor-children ic)))
                     ((string= il "attribute")
                      (push (%parse-xsd-attribute-decl ic) attrs))))))))
          ((string= local "annotation")
           nil))))
    (make-xsd-complex-type :name       name
                            :compositor compositor
                            :elements   elements
                            :attributes (nreverse attrs)
                            :mixed      mixed)))

(defun %parse-xsd-element (node)
  "Parse an xs:element declaration into an xsd-element struct."
  (let* ((name     (%xsd-attr node "name"))
         (type-str (%xsd-attr node "type"))
         (ref      (%xsd-attr node "ref"))
         (min      (%parse-occurs (%xsd-attr node "minOccurs") 1))
         (max      (%parse-occurs (%xsd-attr node "maxOccurs") 1))
         (ct-child (find-if (lambda (c) (%node-xsd-p c "complexType"))
                            (%element-children node)))
         (st-child (find-if (lambda (c) (%node-xsd-p c "simpleType"))
                            (%element-children node)))
         (eff-type (cond (ct-child  (%parse-xsd-complex-type ct-child))
                         (st-child  (%parse-xsd-simple-type  st-child))
                         (type-str  (%parse-type-ref type-str))
                         (t         nil))))
    (make-xsd-element :name      name
                      :type      eff-type
                      :min-occurs min
                      :max-occurs max
                      :ref       ref)))

(defun %parse-xsd-schema (root)
  "Parse the xs:schema root element into an xsd-schema struct."
  (let ((target-ns (%xsd-attr root "targetNamespace"))
        (elements  '())
        (types     '()))
    (dolist (child (%element-children root))
      (let ((local (%node-local child)))
        (cond
          ((string= local "element")
           (let ((e (%parse-xsd-element child)))
             (when (xsd-element-name e)
               (push (cons (xsd-element-name e) e) elements))))
          ((string= local "complexType")
           (let ((ct (%parse-xsd-complex-type child)))
             (when (xsd-complex-type-name ct)
               (push (cons (xsd-complex-type-name ct) ct) types))))
          ((string= local "simpleType")
           (let ((st (%parse-xsd-simple-type child)))
             (when (xsd-simple-type-name st)
               (push (cons (xsd-simple-type-name st) st) types))))
          ;; xs:annotation, xs:include, xs:import, xs:redefine: silently skipped
          )))
    (make-xsd-schema :target-namespace target-ns
                     :elements (nreverse elements)
                     :types    (nreverse types))))

;;; ─── Public: load-xsd ─────────────────────────────────────────────────────

(defun load-xsd (input)
  "Parse INPUT as an XSD schema document and return an xsd-schema struct.

INPUT may be a string or a character input stream.  The document must have an
xs:schema root element; any namespace prefix (xs:, xsd:) or an unqualified
'schema' tag name are all accepted.

Signals an error if INPUT is not well-formed XML or does not have a schema root."
  (let* ((doc  (parse-xml input))
         (root (xml-document-root doc)))
    (unless (%node-xsd-p root "schema")
      (let ((tag (xml-node-tag root)))
        (error "Expected xs:schema root element, found '~a'"
               (if (xml-qname-p tag) (xml-qname-local-name tag) tag))))
    (%parse-xsd-schema root)))

;;; ─── Validation internals ─────────────────────────────────────────────────

(defun %validation-fail (path fmt &rest args)
  "Signal an xsd-validation-error at PATH with the formatted message."
  (error 'xsd-validation-error
         :path    path
         :message (apply #'format nil fmt args)))

(defun %child-path (parent-path child-tag)
  "Build an element path string by appending CHILD-TAG to PARENT-PATH."
  (let ((local (if (xml-qname-p child-tag)
                   (xml-qname-local-name child-tag)
                   (%xsd-local-name child-tag))))
    (if parent-path
        (concatenate 'string parent-path "/" local)
        local)))

(defun %resolve-type (type-ref schema)
  "Resolve TYPE-REF against SCHEMA.
TYPE-REF may be a keyword (built-in), an xsd-complex-type or xsd-simple-type
struct (already parsed inline), a string (named type), or NIL (anyType).
Returns the resolved keyword or struct, or NIL for anyType."
  (cond
    ((null type-ref)  nil)
    ((keywordp type-ref) type-ref)
    ((or (xsd-complex-type-p type-ref)
         (xsd-simple-type-p type-ref)) type-ref)
    ((stringp type-ref)
     (let ((local (%xsd-local-name type-ref)))
       (or (cdr (assoc local (xsd-schema-types schema) :test #'string=))
           (error "Undefined type '~a' referenced in schema" type-ref))))
    (t (error "Invalid type reference: ~s" type-ref))))

(defun %resolve-element-ref (ref schema)
  "Look up element declaration REF in the schema's top-level elements.
Signals an error if REF is not found."
  (let ((local (%xsd-local-name ref)))
    (or (cdr (assoc local (xsd-schema-elements schema) :test #'string=))
        (error "Undefined element reference '~a' in schema" ref))))

(defun %element-text-content (node)
  "Return the concatenated text content of xml-node NODE."
  (with-output-to-string (out)
    (dolist (child (xml-node-children node))
      (typecase child
        (string     (write-string child out))
        (xml-cdata  (write-string (xml-cdata-data child) out))))))

;;; ─── Built-in type value validators ──────────────────────────────────────

(defun %valid-integer-p (str)
  "Return T if STR is a valid xs:integer value (optional sign, one or more digits)."
  (and (> (length str) 0)
       (let ((start (if (member (char str 0) '(#\+ #\-)) 1 0)))
         (and (< start (length str))
              (every #'digit-char-p (subseq str start))))))

(defun %valid-decimal-p (str)
  "Return T if STR is a valid xs:decimal value."
  (and (> (length str) 0)
       (let* ((start (if (member (char str 0) '(#\+ #\-)) 1 0))
              (body  (subseq str start))
              (dots  (count #\. body)))
         (and (< start (length str))
              (<= dots 1)
              (> (length body) 0)
              (every (lambda (c) (or (digit-char-p c) (char= c #\.))) body)
              (some #'digit-char-p body)))))

(defun %valid-float-p (str)
  "Return T if STR is a valid xs:float or xs:double (includes INF, -INF, NaN)."
  (or (member str '("INF" "-INF" "NaN") :test #'string=)
      (and (> (length str) 0)
           (let* ((s     (if (member (char str 0) '(#\+ #\-)) (subseq str 1) str))
                  (e-pos (or (position #\e s) (position #\E s))))
             (if e-pos
                 (and (%valid-decimal-p (subseq s 0 e-pos))
                      (let ((exp (subseq s (1+ e-pos))))
                        (and (> (length exp) 0)
                             (%valid-integer-p
                              (if (member (char exp 0) '(#\+ #\-))
                                  exp
                                  (concatenate 'string "+" exp))))))
                 (%valid-decimal-p s))))))

(defun %valid-boolean-p (str)
  "Return T if STR is a valid xs:boolean value."
  (member str '("true" "false" "1" "0") :test #'string=))

(defun %valid-date-p (str)
  "Return T if STR has the xs:date format YYYY-MM-DD (timezone suffix allowed)."
  (and (>= (length str) 10)
       (char= (char str 4) #\-)
       (char= (char str 7) #\-)
       (every #'digit-char-p
              (list (char str 0) (char str 1) (char str 2) (char str 3)
                    (char str 5) (char str 6)
                    (char str 8) (char str 9)))))

(defun %valid-datetime-p (str)
  "Return T if STR has the xs:dateTime format YYYY-MM-DDTHH:MM:SS (timezone suffix allowed)."
  (and (>= (length str) 19)
       (%valid-date-p str)
       (char= (char str 10) #\T)
       (char= (char str 13) #\:)
       (char= (char str 16) #\:)
       (every #'digit-char-p
              (list (char str 11) (char str 12)
                    (char str 14) (char str 15)
                    (char str 17) (char str 18)))))

(defun %valid-time-p (str)
  "Return T if STR has the xs:time format HH:MM:SS (timezone suffix allowed)."
  (and (>= (length str) 8)
       (char= (char str 2) #\:)
       (char= (char str 5) #\:)
       (every #'digit-char-p
              (list (char str 0) (char str 1)
                    (char str 3) (char str 4)
                    (char str 6) (char str 7)))))

(defun %valid-hexbinary-p (str)
  "Return T if STR is a valid xs:hexBinary value (even number of hex digits)."
  (and (evenp (length str))
       (every (lambda (c)
                (or (digit-char-p c)
                    (member (char-upcase c) '(#\A #\B #\C #\D #\E #\F))))
              str)))

(defun %decimal-to-rational (str)
  "Convert a valid decimal string STR to a rational number."
  (let* ((sign  (if (and (> (length str) 0) (char= (char str 0) #\-)) -1 1))
         (s     (if (member (char str 0) '(#\+ #\-)) (subseq str 1) str))
         (dot   (position #\. s))
         (int-s (if dot (subseq s 0 dot) s))
         (frac-s (if dot (subseq s (1+ dot)) ""))
         (int-v  (if (string= int-s "") 0 (parse-integer int-s)))
         (frac-v (if (string= frac-s "") 0 (parse-integer frac-s)))
         (denom  (expt 10 (length frac-s))))
    (* sign (+ int-v (/ frac-v denom)))))

(defun %validate-builtin-type (value type-keyword path)
  "Validate that VALUE (a string) conforms to built-in TYPE-KEYWORD.
Signals xsd-validation-error when validation fails."
  (case type-keyword
    ;; Boolean
    (:boolean
     (unless (%valid-boolean-p value)
       (%validation-fail path
         "Expected boolean (true/false/1/0), got '~a'" value)))
    ;; Integer types
    ((:integer :nonpositiveinteger :negativeinteger :nonnegativeinteger
      :positiveinteger :long :int :short :byte
      :unsignedlong :unsignedint :unsignedshort :unsignedbyte)
     (unless (%valid-integer-p value)
       (%validation-fail path "Expected integer, got '~a'" value))
     (let ((n (parse-integer value)))
       (case type-keyword
         (:positiveinteger
          (when (<= n 0)
            (%validation-fail path
              "Expected positive integer (> 0), got ~a" n)))
         (:nonnegativeinteger
          (when (< n 0)
            (%validation-fail path
              "Expected non-negative integer (>= 0), got ~a" n)))
         (:negativeinteger
          (when (>= n 0)
            (%validation-fail path
              "Expected negative integer (< 0), got ~a" n)))
         (:nonpositiveinteger
          (when (> n 0)
            (%validation-fail path
              "Expected non-positive integer (<= 0), got ~a" n)))
         (:byte
          (unless (<= -128 n 127)
            (%validation-fail path "xs:byte value ~a out of range [-128, 127]" n)))
         (:short
          (unless (<= -32768 n 32767)
            (%validation-fail path "xs:short value ~a out of range [-32768, 32767]" n)))
         (:int
          (unless (<= -2147483648 n 2147483647)
            (%validation-fail path "xs:int value ~a out of range [-2^31, 2^31-1]" n)))
         (:unsignedbyte
          (unless (<= 0 n 255)
            (%validation-fail path "xs:unsignedByte value ~a out of range [0, 255]" n)))
         (:unsignedshort
          (unless (<= 0 n 65535)
            (%validation-fail path "xs:unsignedShort value ~a out of range [0, 65535]" n)))
         (:unsignedint
          (unless (<= 0 n 4294967295)
            (%validation-fail path
              "xs:unsignedInt value ~a out of range [0, 4294967295]" n))))))
    ;; Decimal
    (:decimal
     (unless (%valid-decimal-p value)
       (%validation-fail path "Expected xs:decimal, got '~a'" value)))
    ;; Float / double
    ((:float :double)
     (unless (%valid-float-p value)
       (%validation-fail path "Expected xs:float/double, got '~a'" value)))
    ;; Date and time
    (:date
     (unless (%valid-date-p value)
       (%validation-fail path "Expected xs:date (YYYY-MM-DD), got '~a'" value)))
    (:datetime
     (unless (%valid-datetime-p value)
       (%validation-fail path
         "Expected xs:dateTime (YYYY-MM-DDTHH:MM:SS), got '~a'" value)))
    (:time
     (unless (%valid-time-p value)
       (%validation-fail path "Expected xs:time (HH:MM:SS), got '~a'" value)))
    ;; Binary encodings
    (:hexbinary
     (unless (%valid-hexbinary-p value)
       (%validation-fail path
         "Expected xs:hexBinary (even number of hex digits), got '~a'" value)))
    ;; String-family and unconstrained types: no structural check
    ((:string :normalizedstring :token
      :anyuri :anytype :anysimpletype
      :name :ncname :id :idref :idrefs :entity :entities
      :nmtoken :nmtokens :qname :notation
      :duration :base64binary
      :gyearmonth :gyear :gmonthday :gday :gmonth)
     nil)
    ;; Unknown keywords fall through without error
    (t nil)))

;;; ─── Facet validation ─────────────────────────────────────────────────────

(defun %validate-facets (value facets path)
  "Validate VALUE (a string) against FACETS (a plist of restriction facets)."
  ;; Enumeration
  (let ((enums (getf facets :enumeration)))
    (when enums
      (unless (member value enums :test #'string=)
        (%validation-fail path
          "Value '~a' not in enumeration ~s" value enums))))
  ;; Length facets
  (let ((len (length value)))
    (let ((exact (getf facets :length)))
      (when exact
        (unless (= len exact)
          (%validation-fail path
            "String length ~a does not match required length ~a" len exact))))
    (let ((min-len (getf facets :min-length)))
      (when min-len
        (when (< len min-len)
          (%validation-fail path
            "String length ~a is less than minLength ~a" len min-len))))
    (let ((max-len (getf facets :max-length)))
      (when max-len
        (when (> len max-len)
          (%validation-fail path
            "String length ~a exceeds maxLength ~a" len max-len)))))
  ;; Numeric range facets — only checked when value is a valid decimal
  (when (or (getf facets :min-inclusive) (getf facets :max-inclusive)
            (getf facets :min-exclusive) (getf facets :max-exclusive))
    (when (%valid-decimal-p value)
      (let ((n (%decimal-to-rational value)))
        (let ((min-inc (getf facets :min-inclusive)))
          (when (and min-inc (%valid-decimal-p min-inc))
            (when (< n (%decimal-to-rational min-inc))
              (%validation-fail path
                "Value ~a is less than minInclusive ~a" value min-inc))))
        (let ((max-inc (getf facets :max-inclusive)))
          (when (and max-inc (%valid-decimal-p max-inc))
            (when (> n (%decimal-to-rational max-inc))
              (%validation-fail path
                "Value ~a exceeds maxInclusive ~a" value max-inc))))
        (let ((min-exc (getf facets :min-exclusive)))
          (when (and min-exc (%valid-decimal-p min-exc))
            (when (<= n (%decimal-to-rational min-exc))
              (%validation-fail path
                "Value ~a is not greater than minExclusive ~a" value min-exc))))
        (let ((max-exc (getf facets :max-exclusive)))
          (when (and max-exc (%valid-decimal-p max-exc))
            (when (>= n (%decimal-to-rational max-exc))
              (%validation-fail path
                "Value ~a is not less than maxExclusive ~a" value max-exc)))))))
  ;; totalDigits
  (let ((total-digits (getf facets :total-digits)))
    (when total-digits
      (when (%valid-decimal-p value)
        (let* ((s     (if (member (char value 0) '(#\+ #\-)) (subseq value 1) value))
               (dot   (position #\. s))
               (int-s (if dot (subseq s 0 dot) s))
               (frac-s (if dot (subseq s (1+ dot)) ""))
               ;; When the integer part is all zeros (e.g. "0"), treat it as
               ;; 1 significant digit rather than 0 to correctly count "0.0".
               (int-trimmed (string-trim "0" int-s))
               (int-digits  (if (and (string= int-trimmed "") (> (length int-s) 0))
                                1
                                (length int-trimmed)))
               (digits (+ int-digits
                          (length (string-right-trim "0" frac-s)))))
          (when (> digits total-digits)
            (%validation-fail path
              "Value '~a' has ~a significant digits, exceeds totalDigits ~a"
              value digits total-digits))))))
  ;; fractionDigits
  (let ((frac-digits (getf facets :fraction-digits)))
    (when frac-digits
      (when (%valid-decimal-p value)
        (let* ((s     (if (member (char value 0) '(#\+ #\-)) (subseq value 1) value))
               (dot   (position #\. s))
               (frac  (if dot (length (string-right-trim "0" (subseq s (1+ dot)))) 0)))
          (when (> frac frac-digits)
            (%validation-fail path
              "Value '~a' has ~a fraction digits, exceeds fractionDigits ~a"
              value frac frac-digits)))))))

;;; ─── Type-aware value validation ──────────────────────────────────────────

(defun %validate-value (value type-ref schema path)
  "Validate VALUE (a string) against TYPE-REF resolved using SCHEMA."
  (let ((resolved (%resolve-type type-ref schema)))
    (cond
      ((null resolved)
       nil)  ; xs:anyType — always valid
      ((keywordp resolved)
       (%validate-builtin-type value resolved path))
      ((xsd-simple-type-p resolved)
       ;; First validate against the base type
       (%validate-value value (xsd-simple-type-base resolved) schema path)
       ;; Then check restriction facets
       (%validate-facets value (xsd-simple-type-facets resolved) path))
      ((xsd-complex-type-p resolved)
       nil)  ; complex types are not used as attribute/text value types
      (t nil))))

;;; ─── Attribute validation ─────────────────────────────────────────────────

(defun %xml-attr-value (xml-node attr-name)
  "Return the value of attribute ATTR-NAME from xml-node XML-NODE, or NIL."
  (cdr (find attr-name (xml-node-attributes xml-node)
             :key (lambda (a)
                    (let ((k (car a)))
                      (if (xml-qname-p k)
                          (xml-qname-local-name k)
                          (%xsd-local-name k))))
             :test #'string=)))

(defun %validate-attributes (xml-node xsd-attrs schema path)
  "Validate attributes of XML-NODE against the list of xsd-attribute XSD-ATTRS."
  (dolist (attr-decl xsd-attrs)
    (let* ((attr-name  (xsd-attribute-name attr-decl))
           (attr-use   (xsd-attribute-use attr-decl))
           (attr-fixed (xsd-attribute-fixed attr-decl))
           (raw-value  (%xml-attr-value xml-node attr-name)))
      (cond
        ;; Required attribute is absent
        ((and (eq attr-use :required) (null raw-value))
         (%validation-fail path
           "Required attribute '~a' is missing" attr-name))
        ;; Prohibited attribute is present
        ((and (eq attr-use :prohibited) raw-value)
         (%validation-fail path
           "Prohibited attribute '~a' must not appear" attr-name))
        ;; Attribute present: validate type and fixed value
        (raw-value
         (when attr-fixed
           (unless (string= raw-value attr-fixed)
             (%validation-fail path
               "Attribute '~a' must have fixed value '~a', got '~a'"
               attr-name attr-fixed raw-value)))
         (%validate-value raw-value (xsd-attribute-type attr-decl) schema
                          (concatenate 'string path "[@" attr-name "]")))))))

;;; ─── Element content validation ───────────────────────────────────────────

(defun %element-name-matches-decl (xml-node xsd-elem schema)
  "Return T if XML-NODE's tag matches the declaration XSD-ELEM."
  (let ((xml-local (if (xml-qname-p (xml-node-tag xml-node))
                       (xml-qname-local-name (xml-node-tag xml-node))
                       (%xsd-local-name (xml-node-tag xml-node)))))
    (cond
      ;; Element ref: compare with top-level element name
      ((xsd-element-ref xsd-elem)
       (let ((top (%resolve-element-ref (xsd-element-ref xsd-elem) schema)))
         (string= xml-local (xsd-element-name top))))
      ;; Direct name
      ((xsd-element-name xsd-elem)
       (string= xml-local (xsd-element-name xsd-elem)))
      (t nil))))

(defun %effective-element-decl (xsd-elem schema)
  "Return the effective xsd-element for XSD-ELEM: resolves ref to top-level decl."
  (if (xsd-element-ref xsd-elem)
      (%resolve-element-ref (xsd-element-ref xsd-elem) schema)
      xsd-elem))

(defun %validate-sequence (xml-children xsd-elements schema path)
  "Validate XML-CHILDREN against an xs:sequence XSD-ELEMENTS."
  (let ((xml-pos 0)
        (xml-len (length xml-children)))
    (dolist (xsd-elem xsd-elements)
      (let ((min (xsd-element-min-occurs xsd-elem))
            (max (xsd-element-max-occurs xsd-elem))
            (count 0))
        (loop
          (when (>= xml-pos xml-len) (return))
          (let ((xml-node (nth xml-pos xml-children)))
            (unless (%element-name-matches-decl xml-node xsd-elem schema)
              (return))
            ;; Check max-occurs
            (when (and (not (eq max :unbounded)) (>= count max))
              (return))
            (incf count)
            (incf xml-pos)
            ;; Recursively validate the matched child
            (%validate-element xml-node
                               (%effective-element-decl xsd-elem schema)
                               schema
                               (%child-path path (xml-node-tag xml-node)))))
        (when (< count min)
          (let ((decl-name (or (xsd-element-name xsd-elem)
                               (xsd-element-ref xsd-elem)
                               "?")))
            (%validation-fail path
              "Element '~a' required at least ~a time(s), found ~a"
              decl-name min count)))))
    ;; Unexpected trailing children
    (when (< xml-pos xml-len)
      (let ((extra (nth xml-pos xml-children)))
        (%validation-fail path
          "Unexpected element '~a'"
          (if (xml-qname-p (xml-node-tag extra))
              (xml-qname-local-name (xml-node-tag extra))
              (xml-node-tag extra)))))))

(defun %validate-all (xml-children xsd-elements schema path)
  "Validate XML-CHILDREN against an xs:all XSD-ELEMENTS."
  ;; xs:all: each declared element occurs 0 or 1 times, in any order
  (let ((seen '()))
    (dolist (xml-node xml-children)
      (let* ((xml-local (if (xml-qname-p (xml-node-tag xml-node))
                            (xml-qname-local-name (xml-node-tag xml-node))
                            (%xsd-local-name (xml-node-tag xml-node))))
             (xsd-elem  (find-if (lambda (e)
                                   (%element-name-matches-decl xml-node e schema))
                                 xsd-elements)))
        (if xsd-elem
            (progn
              (when (member xml-local seen :test #'string=)
                (%validation-fail path
                  "Element '~a' appears more than once in xs:all content"
                  xml-local))
              (push xml-local seen)
              (%validate-element xml-node
                                 (%effective-element-decl xsd-elem schema)
                                 schema
                                 (%child-path path (xml-node-tag xml-node))))
            (%validation-fail path
              "Unexpected element '~a' in xs:all content" xml-local))))
    ;; Check all required (minOccurs=1) declarations were satisfied
    (dolist (xsd-elem xsd-elements)
      (when (= (xsd-element-min-occurs xsd-elem) 1)
        (let ((decl-name (or (xsd-element-name xsd-elem)
                             (xsd-element-ref xsd-elem))))
          (unless (member decl-name seen :test #'string=)
            (%validation-fail path
              "Required element '~a' missing from xs:all content"
              decl-name)))))))

(defun %validate-choice (xml-children xsd-elements schema path)
  "Validate XML-CHILDREN against an xs:choice XSD-ELEMENTS."
  (dolist (xml-node xml-children)
    (let ((xsd-elem (find-if (lambda (e)
                               (%element-name-matches-decl xml-node e schema))
                             xsd-elements)))
      (if xsd-elem
          (%validate-element xml-node
                             (%effective-element-decl xsd-elem schema)
                             schema
                             (%child-path path (xml-node-tag xml-node)))
          (let ((xml-local (if (xml-qname-p (xml-node-tag xml-node))
                               (xml-qname-local-name (xml-node-tag xml-node))
                               (xml-node-tag xml-node))))
            (%validation-fail path
              "Element '~a' does not match any xs:choice branch" xml-local))))))

(defun %validate-complex (xml-node ctype schema path)
  "Validate XML-NODE against an xsd-complex-type CTYPE."
  ;; Validate attributes
  (%validate-attributes xml-node (xsd-complex-type-attributes ctype) schema path)
  ;; Gather element children; optionally check for unexpected text content
  (let ((xml-elem-children (remove-if-not #'xml-node-p (xml-node-children xml-node)))
        (text-content      (%element-text-content xml-node)))
    (unless (xsd-complex-type-mixed ctype)
      (when (and (xsd-complex-type-compositor ctype)
                 (> (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                         text-content))
                    0))
        (%validation-fail path
          "Non-whitespace character content not allowed in non-mixed complex type")))
    ;; Validate element children against compositor
    (case (xsd-complex-type-compositor ctype)
      (:sequence
       (%validate-sequence xml-elem-children
                           (xsd-complex-type-elements ctype) schema path))
      (:all
       (%validate-all xml-elem-children
                      (xsd-complex-type-elements ctype) schema path))
      (:choice
       (%validate-choice xml-elem-children
                         (xsd-complex-type-elements ctype) schema path))
      ((nil)
       ;; Empty content model: no element children expected
       (when xml-elem-children
         (%validation-fail path
           "Element children not allowed in empty-content complex type"))))))

(defun %validate-element (xml-node xsd-elem schema path)
  "Recursively validate XML-NODE against xsd-element XSD-ELEM using SCHEMA."
  (let* ((raw-type  (xsd-element-type xsd-elem))
         (resolved  (%resolve-type raw-type schema)))
    (cond
      ;; No type constraint (xs:anyType): always valid
      ((null resolved)
       nil)
      ;; Simple type: validate text content
      ((or (keywordp resolved) (xsd-simple-type-p resolved))
       (%validate-value (%element-text-content xml-node) resolved schema path))
      ;; Complex type: validate structure
      ((xsd-complex-type-p resolved)
       (%validate-complex xml-node resolved schema path))
      (t nil))))

;;; ─── Public: validate-xml ─────────────────────────────────────────────────

(defun validate-xml (document schema)
  "Validate DOCUMENT (an xml-document struct) against SCHEMA (an xsd-schema struct).

Validation checks:
  - The root element name matches a top-level xs:element declaration.
  - Element and attribute names and occurrence constraints are satisfied.
  - Attribute use (required / optional / prohibited) is respected.
  - Element text content conforms to the declared simple type.
  - Restriction facets (enumeration, length, numeric ranges, etc.) are enforced.

Returns T when validation succeeds.
Signals xsd-validation-error on the first validation failure."
  (let* ((root      (xml-document-root document))
         (root-tag  (xml-node-tag root))
         (root-local (if (xml-qname-p root-tag)
                         (xml-qname-local-name root-tag)
                         (%xsd-local-name root-tag)))
         (root-decl  (cdr (assoc root-local
                                 (xsd-schema-elements schema)
                                 :test #'string=))))
    (unless root-decl
      (%validation-fail nil
        "Root element '~a' has no declaration in the schema" root-local))
    (%validate-element root root-decl schema root-local)
    t))
