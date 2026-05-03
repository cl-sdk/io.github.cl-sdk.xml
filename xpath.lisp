(in-package #:cl-xml)

;;; ─────────────────────────────────────────────────────────────────────────
;;; XPath 1.0 evaluator for cl-xml
;;;
;;; Supported features:
;;;   Abbreviated syntax: / // . .. @
;;;   Axes: child parent self attribute descendant descendant-or-self
;;;         ancestor ancestor-or-self following-sibling preceding-sibling
;;;   Node tests: name * text() comment() processing-instruction() node()
;;;   Predicates: [n]  [@attr]  [@attr="v"]  [child-name]  [expr]
;;;   Union: expr | expr
;;;   Arithmetic: + - * div mod  (unary -)
;;;   Comparisons: = != < <= > >=
;;;   Boolean: and or
;;;   Functions: count string number boolean normalize-space string-length
;;;              contains starts-with not last position name local-name
;;;              true false concat substring substring-before substring-after
;;;              translate sum
;;; ─────────────────────────────────────────────────────────────────────────

;;;; ── 1. Tokenizer ─────────────────────────────────────────────────────────
;;;
;;; Token types
;;;   Keywords : :SLASH :DSLASH :DOT :DDOT :AT
;;;              :LBRACKET :RBRACKET :LPAREN :RPAREN :COMMA :PIPE
;;;              :PLUS :MINUS :STAR :EQ :NE :LT :LE :GT :GE
;;;   Tagged   : (:NAME   . "string")   – XML name or operator word
;;;              (:AXIS   . "string")   – name followed by ::
;;;              (:STRING . "value")    – string literal
;;;              (:NUMBER . number)     – numeric literal

(defun %xpath-tokenize (expr)
  "Tokenize XPath expression string EXPR. Returns a list of tokens."
  (let ((i 0) (n (length expr)) (toks '()))
    (flet ((at (j) (when (< j n) (char expr j))))
      (loop
        (loop while (and (< i n) (xml-whitespace-p (char expr i))) do (incf i))
        (when (>= i n) (return))
        (let ((c (char expr i)))
          (cond
            ;; Two-character operators
            ((and (char= c #\/) (eql (at (1+ i)) #\/))
             (push :dslash toks) (incf i 2))
            ((and (char= c #\.) (eql (at (1+ i)) #\.))
             (push :ddot toks) (incf i 2))
            ((and (char= c #\!) (eql (at (1+ i)) #\=))
             (push :ne toks) (incf i 2))
            ((and (char= c #\<) (eql (at (1+ i)) #\=))
             (push :le toks) (incf i 2))
            ((and (char= c #\>) (eql (at (1+ i)) #\=))
             (push :ge toks) (incf i 2))
            ;; Single-character tokens
            ((char= c #\/) (push :slash toks) (incf i))
            ((char= c #\.) (push :dot   toks) (incf i))
            ((char= c #\@) (push :at    toks) (incf i))
            ((char= c #\[) (push :lbracket toks) (incf i))
            ((char= c #\]) (push :rbracket toks) (incf i))
            ((char= c #\() (push :lparen   toks) (incf i))
            ((char= c #\)) (push :rparen   toks) (incf i))
            ((char= c #\,) (push :comma    toks) (incf i))
            ((char= c #\|) (push :pipe     toks) (incf i))
            ((char= c #\+) (push :plus     toks) (incf i))
            ((char= c #\-) (push :minus    toks) (incf i))
            ((char= c #\*) (push :star     toks) (incf i))
            ((char= c #\=) (push :eq       toks) (incf i))
            ((char= c #\<) (push :lt       toks) (incf i))
            ((char= c #\>) (push :gt       toks) (incf i))
            ;; String literals
            ((or (char= c #\') (char= c #\"))
             (incf i)
             (let ((j i))
               (loop (when (>= i n)
                       (error "Unterminated string literal in XPath expression"))
                     (when (char= (char expr i) c) (return))
                     (incf i))
               (push (cons :string (subseq expr j i)) toks)
               (incf i)))
            ;; Numbers: digits, optional '.' and more digits; or '.' followed by digits
            ((digit-char-p c)
             (let ((j i))
               (loop while (and (< i n) (digit-char-p (char expr i))) do (incf i))
               (when (and (< i n) (char= (char expr i) #\.)
                          ;; exclude ".." — already handled above, but guard anyway
                          (or (>= (1+ i) n) (not (char= (char expr (1+ i)) #\.))))
                 (incf i)
                 (loop while (and (< i n) (digit-char-p (char expr i))) do (incf i)))
               (push (cons :number (read-from-string (subseq expr j i))) toks)))
            ((and (char= c #\.) (< (1+ i) n) (digit-char-p (at (1+ i))))
             (let ((j i))
               (incf i)
               (loop while (and (< i n) (digit-char-p (char expr i))) do (incf i))
               (push (cons :number (read-from-string (subseq expr j i))) toks)))
            ;; Names: may be followed by :: making them axis specifiers.
            ;; Stop reading the name at ':' since XPath uses ':' as a
            ;; namespace separator and '::' as an axis separator, and
            ;; XML NameChar includes ':' which we need to treat specially.
            ((name-start-char-p c)
             (let ((j i))
               (loop while (and (< i n)
                                (name-char-p (char expr i))
                                (not (char= (char expr i) #\:)))
                     do (incf i))
               (let ((name (subseq expr j i))
                     (k    i))
                 ;; skip whitespace before potential ::
                 (loop while (and (< k n) (xml-whitespace-p (char expr k))) do (incf k))
                 (cond
                   ((and (< k n) (char= (char expr k) #\:)
                         (< (1+ k) n) (char= (char expr (1+ k)) #\:))
                    (setf i (+ k 2))
                    (push (cons :axis name) toks))
                   ;; Namespace-prefixed name: prefix:localname – treat as plain name
                   ((and (< k n) (char= (char expr k) #\:)
                         (< (1+ k) n) (name-start-char-p (char expr (1+ k))))
                    (incf k)
                    (let ((lj k))
                      (loop while (and (< k n)
                                       (name-char-p (char expr k))
                                       (not (char= (char expr k) #\:)))
                            do (incf k))
                      (setf i k)
                      (push (cons :name
                                  (concatenate 'string name ":" (subseq expr lj k)))
                            toks)))
                   (t
                    (push (cons :name name) toks))))))
            (t
             (error "Unexpected character ~S at position ~D in XPath ~S" c i expr))))))
    (nreverse toks)))


;;;; ── 2. Token stream ──────────────────────────────────────────────────────

(defstruct (%xpath-ts (:constructor %make-xpath-ts (tokens)))
  (tokens '() :type list))

(defun %xts-peek  (ts) (car  (%xpath-ts-tokens ts)))
(defun %xts-peek2 (ts) (cadr (%xpath-ts-tokens ts)))
(defun %xts-end-p (ts) (null (%xpath-ts-tokens ts)))

(defun %xts-consume (ts)
  (prog1 (car (%xpath-ts-tokens ts))
    (setf (%xpath-ts-tokens ts) (cdr (%xpath-ts-tokens ts)))))

(defun %xts-expect (ts tok)
  (let ((got (%xts-consume ts)))
    (unless (equal got tok)
      (error "XPath parse error: expected ~S, got ~S" tok got))
    got))


;;;; ── 3. Parser ───────────────────────────────────────────────────────────
;;;
;;; AST node format (all are lists starting with a keyword):
;;;   (:abs steps)               – absolute location path; steps may be NIL (root only)
;;;   (:rel steps)               – relative location path
;;;   (:step axis node-test preds) – a single step
;;;     axis     : :child :parent :self :attribute
;;;                :descendant :descendant-or-self
;;;                :ancestor :ancestor-or-self
;;;                :following-sibling :preceding-sibling
;;;     node-test: (:name "n") (:wild) (:text) (:comment) (:pi) (:pi "tgt") (:node)
;;;     preds    : list of expressions
;;;   (:num n)          – number literal
;;;   (:str s)          – string literal
;;;   (:filter e ps)    – filter expression with predicates
;;;   (:fpath  e steps) – filter/primary followed by step list
;;;   (:union  e1 e2)   – e1 | e2
;;;   (:or     e1 e2)
;;;   (:and    e1 e2)
;;;   (:=  e1 e2) (:!= e1 e2)
;;;   (:<  e1 e2) (:>  e1 e2) (:<=  e1 e2) (:>=  e1 e2)
;;;   (:+  e1 e2) (:- e1 e2) (:* e1 e2) (:div e1 e2) (:mod e1 e2)
;;;   (:neg e)
;;;   (:call "name" (arg ...))

(defun %xpath-node-type-p (name)
  "True when NAME is an XPath node-type keyword (text comment node processing-instruction)."
  (member name '("text" "comment" "node" "processing-instruction") :test #'string=))

;;; Predicate / grammar helpers

(defun %xpath-can-start-step-p (ts)
  "True when the lookahead can begin a location step."
  (let ((tok (%xts-peek ts)))
    (or (eq tok :dot)
        (eq tok :ddot)
        (eq tok :at)
        (eq tok :star)
        (and (consp tok) (eq (car tok) :axis))
        (and (consp tok) (eq (car tok) :name)))))

;;; Recursive-descent parser entry

(defun %xpath-parse-expr (ts) (%xpath-parse-or ts))

(defun %xpath-parse-or (ts)
  (let ((e (%xpath-parse-and ts)))
    (loop while (equal (%xts-peek ts) '(:name . "or"))
          do (%xts-consume ts)
             (setf e `(:or ,e ,(%xpath-parse-and ts))))
    e))

(defun %xpath-parse-and (ts)
  (let ((e (%xpath-parse-equality ts)))
    (loop while (equal (%xts-peek ts) '(:name . "and"))
          do (%xts-consume ts)
             (setf e `(:and ,e ,(%xpath-parse-equality ts))))
    e))

(defun %xpath-parse-equality (ts)
  (let ((e (%xpath-parse-relational ts)))
    (loop (let ((op (case (%xts-peek ts) (:eq :=) (:ne :!=) (t nil))))
            (unless op (return))
            (%xts-consume ts)
            (setf e `(,op ,e ,(%xpath-parse-relational ts)))))
    e))

(defun %xpath-parse-relational (ts)
  (let ((e (%xpath-parse-additive ts)))
    (loop (let ((op (case (%xts-peek ts)
                      (:lt :<) (:gt :>) (:le :<=) (:ge :>=) (t nil))))
            (unless op (return))
            (%xts-consume ts)
            (setf e `(,op ,e ,(%xpath-parse-additive ts)))))
    e))

(defun %xpath-parse-additive (ts)
  (let ((e (%xpath-parse-multiplicative ts)))
    (loop (let ((op (case (%xts-peek ts) (:plus :+) (:minus :-) (t nil))))
            (unless op (return))
            (%xts-consume ts)
            (setf e `(,op ,e ,(%xpath-parse-multiplicative ts)))))
    e))

(defun %xpath-parse-multiplicative (ts)
  (let ((e (%xpath-parse-unary ts)))
    (loop (let ((op (let ((tok (%xts-peek ts)))
                      (cond ((eq  tok :star)              :*)
                            ((equal tok '(:name . "div")) :div)
                            ((equal tok '(:name . "mod")) :mod)
                            (t nil)))))
            (unless op (return))
            (%xts-consume ts)
            (setf e `(,op ,e ,(%xpath-parse-unary ts)))))
    e))

(defun %xpath-parse-unary (ts)
  (if (eq (%xts-peek ts) :minus)
      (progn (%xts-consume ts) `(:neg ,(%xpath-parse-unary ts)))
      (%xpath-parse-union ts)))

(defun %xpath-parse-union (ts)
  (let ((e (%xpath-parse-path ts)))
    (loop while (eq (%xts-peek ts) :pipe)
          do (%xts-consume ts)
             (setf e `(:union ,e ,(%xpath-parse-path ts))))
    e))

(defun %xpath-parse-path (ts)
  "Parse a PathExpr: LocationPath | FilterExpr optionally followed by / or // and more steps."
  (cond
    ;; Absolute path starting with /
    ((eq (%xts-peek ts) :slash)
     (%xts-consume ts)
     (if (%xpath-can-start-step-p ts)
         `(:abs ,(%xpath-parse-rel-steps ts))
         '(:abs nil)))
    ;; Absolute path starting with // (= /descendant-or-self::node()/)
    ((eq (%xts-peek ts) :dslash)
     (%xts-consume ts)
     `(:abs ,(list* '(:step :descendant-or-self (:node) nil)
                    (%xpath-parse-rel-steps ts))))
    ;; Primary/filter expression (parenthesized, literal, or function call)
    ((or (eq (%xts-peek ts) :lparen)
         (and (consp (%xts-peek ts)) (eq (car (%xts-peek ts)) :number))
         (and (consp (%xts-peek ts)) (eq (car (%xts-peek ts)) :string))
         ;; name '(' where name is NOT a node-type test → function call
         (and (consp (%xts-peek ts)) (eq (car (%xts-peek ts)) :name)
              (eq (%xts-peek2 ts) :lparen)
              (not (%xpath-node-type-p (cdr (%xts-peek ts))))))
     (let* ((primary (%xpath-parse-primary ts))
            (preds   (%xpath-parse-predicates ts))
            (expr    (if preds `(:filter ,primary ,preds) primary)))
       (cond
         ((eq (%xts-peek ts) :slash)
          (%xts-consume ts)
          `(:fpath ,expr ,(%xpath-parse-rel-steps ts)))
         ((eq (%xts-peek ts) :dslash)
          (%xts-consume ts)
          `(:fpath ,expr ,(list* '(:step :descendant-or-self (:node) nil)
                                 (%xpath-parse-rel-steps ts))))
         (t expr))))
    ;; Relative location path
    (t
     `(:rel ,(%xpath-parse-rel-steps ts)))))

(defun %xpath-parse-rel-steps (ts)
  "Parse RelativeLocationPath; return a list of step ASTs."
  (let ((steps (list (%xpath-parse-step ts))))
    (loop
      (cond
        ((eq (%xts-peek ts) :slash)
         (%xts-consume ts)
         (push (%xpath-parse-step ts) steps))
        ((eq (%xts-peek ts) :dslash)
         (%xts-consume ts)
         (push '(:step :descendant-or-self (:node) nil) steps)
         (push (%xpath-parse-step ts) steps))
        (t (return))))
    (nreverse steps)))

(defun %xpath-parse-step (ts)
  "Parse a single Step."
  (let ((tok (%xts-peek ts)))
    (cond
      ;; Abbreviated: .  ..
      ((eq tok :dot)  (%xts-consume ts) '(:step :self   (:node) nil))
      ((eq tok :ddot) (%xts-consume ts) '(:step :parent (:node) nil))
      ;; Explicit axis: name::
      ((and (consp tok) (eq (car tok) :axis))
       (%xts-consume ts)
       (let ((axis (%xpath-parse-axis-name (cdr tok))))
         `(:step ,axis
                 ,(%xpath-parse-node-test ts)
                 ,(%xpath-parse-predicates ts))))
      ;; Abbreviated attribute axis: @
      ((eq tok :at)
       (%xts-consume ts)
       `(:step :attribute
               ,(%xpath-parse-node-test ts)
               ,(%xpath-parse-predicates ts)))
      ;; Wildcard *
      ((eq tok :star)
       (%xts-consume ts)
       `(:step :child (:wild) ,(%xpath-parse-predicates ts)))
      ;; Name – could be a node-type test (text() etc.) or an element name
      ((and (consp tok) (eq (car tok) :name))
       (cond
         ;; Node-type test: name followed by '('
         ((and (eq (%xts-peek2 ts) :lparen) (%xpath-node-type-p (cdr tok)))
          (let ((nt (%xpath-parse-type-test ts)))
            `(:step :child ,nt ,(%xpath-parse-predicates ts))))
         ;; Plain element name
         (t
          (let ((name (cdr (%xts-consume ts))))
            `(:step :child (:name ,name) ,(%xpath-parse-predicates ts))))))
      (t
       (error "XPath parse error: expected a step, got ~S" tok)))))

(defun %xpath-parse-type-test (ts)
  "Parse a node-type test like text(), comment(), node(), processing-instruction().
The name token must already be the current peek and must satisfy %xpath-node-type-p."
  (let ((type-name (cdr (%xts-consume ts))))
    (%xts-expect ts :lparen)
    (let ((nt (cond
                ((string= type-name "text")    '(:text))
                ((string= type-name "comment") '(:comment))
                ((string= type-name "node")    '(:node))
                ;; processing-instruction("target"?)
                (t
                 (if (and (consp (%xts-peek ts))
                          (eq (car (%xts-peek ts)) :string))
                     (list :pi (cdr (%xts-consume ts)))
                     '(:pi))))))
      (%xts-expect ts :rparen)
      nt)))

(defun %xpath-parse-axis-name (name)
  "Map an axis name string to a keyword."
  (let ((mapping '(("child"              . :child)
                   ("parent"             . :parent)
                   ("self"               . :self)
                   ("attribute"          . :attribute)
                   ("descendant"         . :descendant)
                   ("descendant-or-self" . :descendant-or-self)
                   ("ancestor"           . :ancestor)
                   ("ancestor-or-self"   . :ancestor-or-self)
                   ("following-sibling"  . :following-sibling)
                   ("preceding-sibling"  . :preceding-sibling)
                   ("following"          . :following)
                   ("preceding"          . :preceding)
                   ("namespace"          . :namespace))))
    (or (cdr (assoc name mapping :test #'string=))
        (error "Unknown XPath axis: ~S" name))))

(defun %xpath-parse-node-test (ts)
  "Parse a node test in step context."
  (let ((tok (%xts-peek ts)))
    (cond
      ((eq tok :star)
       (%xts-consume ts) '(:wild))
      ((and (consp tok) (eq (car tok) :name))
       (cond
         ((and (eq (%xts-peek2 ts) :lparen) (%xpath-node-type-p (cdr tok)))
          (%xpath-parse-type-test ts))
         (t
          (list :name (cdr (%xts-consume ts))))))
      (t
       (error "XPath parse error: expected a node test, got ~S" tok)))))

(defun %xpath-parse-predicates (ts)
  "Parse zero or more predicates; return a list of expression ASTs."
  (let (preds)
    (loop while (eq (%xts-peek ts) :lbracket)
          do (%xts-consume ts)
             (push (%xpath-parse-expr ts) preds)
             (%xts-expect ts :rbracket))
    (nreverse preds)))

(defun %xpath-parse-primary (ts)
  "Parse a PrimaryExpr."
  (let ((tok (%xts-peek ts)))
    (cond
      ;; Parenthesized expression
      ((eq tok :lparen)
       (%xts-consume ts)
       (let ((e (%xpath-parse-expr ts)))
         (%xts-expect ts :rparen)
         e))
      ;; Number literal
      ((and (consp tok) (eq (car tok) :number))
       (%xts-consume ts) `(:num ,(cdr tok)))
      ;; String literal
      ((and (consp tok) (eq (car tok) :string))
       (%xts-consume ts) `(:str ,(cdr tok)))
      ;; Function call
      ((and (consp tok) (eq (car tok) :name))
       (let ((name (cdr (%xts-consume ts))))
         (%xts-expect ts :lparen)
         (let (args)
           (unless (eq (%xts-peek ts) :rparen)
             (push (%xpath-parse-expr ts) args)
             (loop while (eq (%xts-peek ts) :comma)
                   do (%xts-consume ts)
                      (push (%xpath-parse-expr ts) args)))
           (%xts-expect ts :rparen)
           `(:call ,name ,(nreverse args)))))
      (t
       (error "XPath parse error: expected primary expression, got ~S" tok)))))

;;; Top-level compile function

(defun xpath-compile (path-string)
  "Compile PATH-STRING as an XPath 1.0 expression.
Returns an opaque compiled XPath object for use with XPATH-SELECT, XPATH-FIRST,
XPATH-STRING, XPATH-NUMBER, and XPATH-BOOLEAN."
  (let ((ts (%make-xpath-ts (%xpath-tokenize path-string))))
    (let ((ast (%xpath-parse-expr ts)))
      (unless (%xts-end-p ts)
        (error "Unexpected token at end of XPath expression: ~S" (%xts-peek ts)))
      ast)))


;;;; ── 4. Evaluation environment and context ──────────────────────────────

(defstruct (%xpath-env (:constructor %make-xpath-env (document)))
  document    ; xml-document used as root for absolute paths
  parent-map) ; eq hash-table: node → parent, built lazily

(defstruct (%xpath-ctx (:constructor %make-xpath-ctx (node position size env)))
  node
  position
  size
  env)

(defun %xpath-env-for (context-node document)
  "Build an %xpath-env, taking the document from CONTEXT-NODE when possible."
  (let ((doc (cond (document document)
                   ((xml-document-p context-node) context-node)
                   (t nil))))
    (%make-xpath-env doc)))

(defun %xpath-ensure-parent-map (env)
  "Return (and build on first call) the parent-map of ENV."
  (or (%xpath-env-parent-map env)
      (let ((map (make-hash-table :test #'eq)))
        (labels ((walk (node parent)
                   (setf (gethash node map) parent)
                   (cond
                     ((xml-document-p node)
                      (walk (xml-document-root node) node))
                     ((xml-node-p node)
                      (dolist (child (xml-node-children node))
                        (walk child node))))))
          (let ((doc (%xpath-env-document env)))
            (when doc (walk doc nil))))
        (setf (%xpath-env-parent-map env) map)
        map)))

(defun %xpath-parent (node env)
  (gethash node (%xpath-ensure-parent-map env)))


;;;; ── 5. String value of a node ──────────────────────────────────────────

(defun %xpath-string-value (node)
  "Return the XPath 1.0 string value of NODE."
  (cond
    ((xml-document-p node) (%xpath-string-value (xml-document-root node)))
    ((xml-node-p node)
     (with-output-to-string (s)
       (labels ((collect (n)
                  (cond
                    ((xml-node-p n)
                     (dolist (c (xml-node-children n)) (collect c)))
                    ((stringp n)    (write-string n s))
                    ((xml-cdata-p n) (write-string (xml-cdata-data n) s)))))
         (collect node))))
    ((stringp node)    node)
    ((xml-cdata-p node)    (xml-cdata-data node))
    ((xml-comment-p node)  (xml-comment-data node))
    ((xml-pi-p node)       (xml-pi-data node))
    ;; Attribute node: (name . value)
    ((and (consp node) (stringp (car node))) (cdr node))
    (t "")))


;;;; ── 6. Node name utilities ─────────────────────────────────────────────

(defun %xpath-node-name (node)
  "Return the qualified name of NODE as a string."
  (cond
    ((xml-node-p node)
     (let ((tag (xml-node-tag node)))
       (if (xml-qname-p tag)
           (let ((prefix (xml-qname-prefix tag))
                 (local  (xml-qname-local-name tag)))
             (if prefix (format nil "~a:~a" prefix local) local))
           tag)))
    ((and (consp node) (stringp (car node))) (car node))
    (t "")))

(defun %xpath-local-name (node)
  "Return the local (unprefixed) name of NODE."
  (cond
    ((xml-node-p node)
     (let ((tag (xml-node-tag node)))
       (if (xml-qname-p tag)
           (xml-qname-local-name tag)
           tag)))
    ((and (consp node) (stringp (car node)))
     ;; Attribute: strip prefix if present
     (let* ((raw   (car node))
            (colon (position #\: raw)))
       (if colon (subseq raw (1+ colon)) raw)))
    (t "")))


;;;; ── 7. Axis navigation ─────────────────────────────────────────────────

(defun %xpath-axis-nodes (axis node env)
  "Return nodes in AXIS relative to NODE."
  (case axis
    (:child
     (cond
       ((xml-document-p node) (list (xml-document-root node)))
       ((xml-node-p node)     (xml-node-children node))
       (t nil)))
    (:attribute
     (when (xml-node-p node) (xml-node-attributes node)))
    (:self
     (list node))
    (:parent
     (let ((p (%xpath-parent node env)))
       (when p (list p))))
    (:descendant
     (%xpath-descendant-nodes node))
    (:descendant-or-self
     (cons node (%xpath-descendant-nodes node)))
    (:ancestor
     (%xpath-ancestor-nodes node env))
    (:ancestor-or-self
     (append (%xpath-ancestor-nodes node env) (list node)))
    (:following-sibling
     (%xpath-following-siblings node env))
    (:preceding-sibling
     (%xpath-preceding-siblings node env))
    (t nil)))

(defun %xpath-descendant-nodes (node)
  "All descendant nodes of NODE in document order."
  (let (result)
    (labels ((walk (n)
               (let ((children (cond
                                 ((xml-document-p n) (list (xml-document-root n)))
                                 ((xml-node-p n)      (xml-node-children n))
                                 (t nil))))
                 (dolist (c children)
                   (push c result)
                   (walk c)))))
      (walk node))
    (nreverse result)))

(defun %xpath-ancestor-nodes (node env)
  "Return ancestor nodes from root down to (but not including) NODE."
  (let (result)
    (loop for p = (%xpath-parent node env) then (%xpath-parent p env)
          while p
          do (push p result))
    result))

(defun %xpath-following-siblings (node env)
  (let ((parent (%xpath-parent node env)))
    (when (xml-node-p parent)
      (cdr (member node (xml-node-children parent) :test #'eq)))))

(defun %xpath-preceding-siblings (node env)
  (let ((parent (%xpath-parent node env)))
    (when (xml-node-p parent)
      (nreverse
       (cdr (member node (reverse (xml-node-children parent)) :test #'eq))))))


;;;; ── 8. Node test matching ──────────────────────────────────────────────

(defun %xpath-matches-test (node test)
  "True when NODE satisfies the node-test TEST."
  (ecase (car test)
    (:name
     (let ((name (cadr test)))
       (cond
         ;; Attribute node: (name . value)
         ((and (consp node) (stringp (car node)))
          (string= name (car node)))
         ((xml-node-p node)
          (string= name (%xpath-local-name node)))
         (t nil))))
    (:wild
     (or (xml-node-p node)
         (and (consp node) (stringp (car node)))))
    (:text
     (or (stringp node) (xml-cdata-p node)))
    (:comment
     (xml-comment-p node))
    (:pi
     (and (xml-pi-p node)
          (or (null (cdr test))
              (string= (cadr test) (xml-pi-target node)))))
    (:node
     t)))


;;;; ── 9. XPath value type coercions ──────────────────────────────────────

;;; We represent XPath types as:
;;;   node-set  → list (possibly empty)
;;;   string    → string
;;;   number    → real number
;;;   boolean   → T or NIL

(defun %xpath-truthy-p (val)
  "XPath boolean value of VAL."
  (cond
    ((null val)    nil)
    ((eq  val t)   t)
    ((numberp val) (not (zerop val)))
    ((stringp val) (plusp (length val)))
    ((listp val)   (not (null val)))
    (t             t)))

(defun %xpath-to-string (val)
  "Coerce VAL to an XPath string."
  (cond
    ((stringp val) val)
    ((null val)    "")
    ((eq val t)    "true")
    ((numberp val)
     (if (and (floatp val) (zerop (nth-value 1 (floor val))))
         (format nil "~D" (round val))
         (let ((s (format nil "~A" val)))
           ;; Remove trailing 'd0'/'d+0' SBCL double-float suffix
           (if (find #\d s)
               (let ((d (position #\d s)))
                 (subseq s 0 d))
               s))))
    ((listp val)
     (if val (%xpath-string-value (first val)) ""))
    (t (%xpath-string-value val))))

(defun %xpath-to-number (val)
  "Coerce VAL to an XPath number."
  (cond
    ((numberp val) val)
    ((stringp val)
     (let ((s (string-trim '(#\Space #\Tab #\Newline #\Return) val)))
       (or (ignore-errors (parse-integer s))
           (ignore-errors
             (let ((*read-default-float-format* 'double-float))
               (let ((n (read-from-string s nil nil)))
                 (when (numberp n) n))))
           0)))
    ((null val)    0)
    ((eq val t)    1)
    ((listp val)   (if val (%xpath-to-number (%xpath-string-value (first val))) 0))
    (t             0)))


;;;; ── 10. Predicate evaluation ────────────────────────────────────────────

(defun %xpath-apply-predicates (nodes preds env)
  "Filter NODES by PREDS. Returns the filtered list in original order."
  (dolist (pred preds nodes)
    (let ((size (length nodes))
          (pos  0)
          filtered)
      (dolist (node nodes)
        (incf pos)
        (let* ((ctx (%make-xpath-ctx node pos size env))
               (val (%xpath-eval pred ctx)))
          (when (if (numberp val)
                    (= val pos)
                    (%xpath-truthy-p val))
            (push node filtered))))
      (setf nodes (nreverse filtered)))))


;;;; ── 11. Main evaluator ──────────────────────────────────────────────────

(defun %xpath-eval (ast ctx)
  "Evaluate XPath AST in evaluation context CTX. Returns an XPath value."
  (let ((env (%xpath-ctx-env ctx)))
    (ecase (car ast)

      (:num  (second ast))
      (:str  (second ast))

      ;; Absolute location path
      (:abs
       (let* ((doc   (%xpath-env-document env))
              (steps (cadr ast)))
         (unless doc
           (error "Absolute XPath path requires a document context"))
         (if (null steps)
             (list doc)
             (%xpath-eval-steps steps (list doc) env))))

      ;; Relative location path
      (:rel
       (%xpath-eval-steps (cadr ast) (list (%xpath-ctx-node ctx)) env))

      ;; Filter expression: primary + predicates
      (:filter
       (let* ((base  (%xpath-eval (second ast) ctx))
              (nodes (if (listp base) base (list base))))
         (%xpath-apply-predicates nodes (third ast) env)))

      ;; Filter/primary followed by path steps
      (:fpath
       (let* ((base  (%xpath-eval (second ast) ctx))
              (nodes (if (listp base) base (list base))))
         (%xpath-eval-steps (third ast) nodes env)))

      ;; Union
      (:union
       (let ((left  (%xpath-eval (second ast) ctx))
             (right (%xpath-eval (third  ast) ctx)))
         (let ((l (if (listp left)  left  (list left)))
               (r (if (listp right) right (list right))))
           ;; Preserve document order; remove duplicates by identity
           (remove-duplicates (append l r) :test #'eq :from-end t))))

      ;; Boolean operators
      (:or
       (or (%xpath-truthy-p (%xpath-eval (second ast) ctx))
           (%xpath-truthy-p (%xpath-eval (third  ast) ctx))))
      (:and
       (and (%xpath-truthy-p (%xpath-eval (second ast) ctx))
            (%xpath-truthy-p (%xpath-eval (third  ast) ctx))))

      ;; Equality / inequality
      (:=
       (%xpath-compare-eq (%xpath-eval (second ast) ctx)
                          (%xpath-eval (third  ast) ctx)))
      (:!=
       (not (%xpath-compare-eq (%xpath-eval (second ast) ctx)
                               (%xpath-eval (third  ast) ctx))))

      ;; Relational comparisons
      (:<  (%xpath-compare-rel #'<  (%xpath-eval (second ast) ctx)
                                    (%xpath-eval (third  ast) ctx)))
      (:>  (%xpath-compare-rel #'>  (%xpath-eval (second ast) ctx)
                                    (%xpath-eval (third  ast) ctx)))
      (:<= (%xpath-compare-rel #'<= (%xpath-eval (second ast) ctx)
                                    (%xpath-eval (third  ast) ctx)))
      (:>= (%xpath-compare-rel #'>= (%xpath-eval (second ast) ctx)
                                    (%xpath-eval (third  ast) ctx)))

      ;; Arithmetic
      (:+   (+ (%xpath-to-number (%xpath-eval (second ast) ctx))
               (%xpath-to-number (%xpath-eval (third  ast) ctx))))
      (:-   (- (%xpath-to-number (%xpath-eval (second ast) ctx))
               (%xpath-to-number (%xpath-eval (third  ast) ctx))))
      (:*   (* (%xpath-to-number (%xpath-eval (second ast) ctx))
               (%xpath-to-number (%xpath-eval (third  ast) ctx))))
      (:div (let ((divisor (%xpath-to-number (%xpath-eval (third ast) ctx))))
              (if (zerop divisor)
                  (error "XPath: division by zero")
                  (/ (%xpath-to-number (%xpath-eval (second ast) ctx))
                     divisor))))
      (:mod (let ((divisor (%xpath-to-number (%xpath-eval (third ast) ctx))))
              (if (zerop divisor)
                  (error "XPath: mod by zero")
                  (mod (%xpath-to-number (%xpath-eval (second ast) ctx))
                       divisor))))
      (:neg (- (%xpath-to-number (%xpath-eval (second ast) ctx))))

      ;; Function call
      (:call
       (%xpath-call-function (second ast) (third ast) ctx)))))

(defun %xpath-eval-steps (steps source-nodes env)
  "Evaluate a list of STEPS against SOURCE-NODES, threading the result forward."
  (dolist (step steps source-nodes)
    (destructuring-bind (kw axis node-test preds) step
      (declare (ignore kw))
      (let* (;; Collect axis nodes from each source node (in order, unique)
             (candidates
               (let (seen result)
                 (dolist (src source-nodes)
                   (dolist (n (%xpath-axis-nodes axis src env))
                     (unless (member n seen :test #'eq)
                       (push n seen)
                       (push n result))))
                 (nreverse result)))
             ;; Apply node test
             (tested (remove-if-not
                      (lambda (n) (%xpath-matches-test n node-test))
                      candidates)))
        ;; Apply predicates
        (setf source-nodes (%xpath-apply-predicates tested preds env))))))

;;; Comparison helpers

(defun %xpath-compare-eq (left right)
  "XPath = comparison."
  (cond
    ;; Both node-sets: true if any pair of string values are equal
    ((and (listp left) (listp right))
     (some (lambda (l) (some (lambda (r)
                               (string= (%xpath-string-value l)
                                        (%xpath-string-value r)))
                             right))
           left))
    ;; One node-set: compare string value to the other (coerced to string)
    ((listp left)
     (some (lambda (n) (string= (%xpath-string-value n) (%xpath-to-string right)))
           left))
    ((listp right)
     (some (lambda (n) (string= (%xpath-to-string left) (%xpath-string-value n)))
           right))
    ;; Both booleans
    ((and (typep left 'boolean) (typep right 'boolean))
     (eq left right))
    ;; Both numbers
    ((and (numberp left) (numberp right))
     (= left right))
    ;; Otherwise compare as strings
    (t
     (string= (%xpath-to-string left) (%xpath-to-string right)))))

(defun %xpath-compare-rel (op left right)
  "XPath relational comparison using numeric ordering."
  (cond
    ((and (listp left) (listp right))
     (some (lambda (l) (some (lambda (r)
                               (funcall op
                                        (%xpath-to-number (%xpath-string-value l))
                                        (%xpath-to-number (%xpath-string-value r))))
                             right))
           left))
    ((listp left)
     (some (lambda (n)
             (funcall op (%xpath-to-number (%xpath-string-value n))
                      (%xpath-to-number right)))
           left))
    ((listp right)
     (some (lambda (n)
             (funcall op (%xpath-to-number left)
                      (%xpath-to-number (%xpath-string-value n))))
           right))
    (t
     (funcall op (%xpath-to-number left) (%xpath-to-number right)))))


;;;; ── 12. Built-in functions ─────────────────────────────────────────────

(defun %xpath-call-function (name args ctx)
  "Dispatch a built-in XPath function call."
  (flet ((arg (n) (%xpath-eval (nth n args) ctx))
         (arg-str (n)
           (let ((v (%xpath-eval (nth n args) ctx)))
             (if (listp v)
                 (if v (%xpath-string-value (first v)) "")
                 (%xpath-to-string v)))))
    (cond
      ((string= name "true")
       t)
      ((string= name "false")
       nil)
      ((string= name "not")
       (not (%xpath-truthy-p (arg 0))))
      ((string= name "boolean")
       (%xpath-truthy-p (arg 0)))
      ((string= name "string")
       (if args (arg-str 0) (%xpath-string-value (%xpath-ctx-node ctx))))
      ((string= name "number")
       (%xpath-to-number (if args (arg 0) (%xpath-ctx-node ctx))))
      ((string= name "count")
       (let ((v (arg 0)))
         (if (listp v) (length v) (error "count() requires a node-set"))))
      ((string= name "sum")
       (let ((v (arg 0)))
         (if (listp v)
             (reduce #'+ v :initial-value 0
                         :key (lambda (n) (%xpath-to-number (%xpath-string-value n))))
             (error "sum() requires a node-set"))))
      ((string= name "last")
       (%xpath-ctx-size ctx))
      ((string= name "position")
       (%xpath-ctx-position ctx))
      ((string= name "name")
       (if args
           (let ((v (arg 0)))
             (%xpath-node-name (if (listp v) (first v) v)))
           (%xpath-node-name (%xpath-ctx-node ctx))))
      ((string= name "local-name")
       (if args
           (let ((v (arg 0)))
             (%xpath-local-name (if (listp v) (first v) v)))
           (%xpath-local-name (%xpath-ctx-node ctx))))
      ((string= name "normalize-space")
       (let ((s (if args (arg-str 0)
                    (%xpath-string-value (%xpath-ctx-node ctx)))))
         (%xpath-normalize-space s)))
      ((string= name "string-length")
       (length (if args (arg-str 0)
                   (%xpath-string-value (%xpath-ctx-node ctx)))))
      ((string= name "contains")
       (let ((hay (arg-str 0)) (needle (arg-str 1)))
         (not (null (search needle hay :test #'char=)))))
      ((string= name "starts-with")
       (let ((s (arg-str 0)) (prefix (arg-str 1)))
         (and (>= (length s) (length prefix))
              (string= s prefix :end1 (length prefix)))))
      ((string= name "concat")
       (apply #'concatenate 'string (mapcar (lambda (a)
                                              (%xpath-to-string (%xpath-eval a ctx)))
                                            args)))
      ((string= name "substring")
       (let* ((s     (arg-str 0))
              (start (round (%xpath-to-number (arg 1))))
              (len   (length s))
              (from  (max 0 (1- start)))  ; convert 1-based XPath to 0-based CL
              (to    (if (>= (length args) 3)
                         (min len (+ from (round (%xpath-to-number (arg 2)))))
                         len)))
         (if (>= from to) "" (subseq s from to))))
      ((string= name "substring-before")
       (let* ((s     (arg-str 0))
              (delim (arg-str 1))
              (pos   (search delim s :test #'char=)))
         (if pos (subseq s 0 pos) "")))
      ((string= name "substring-after")
       (let* ((s     (arg-str 0))
              (delim (arg-str 1))
              (pos   (search delim s :test #'char=)))
         (if pos (subseq s (+ pos (length delim))) "")))
      ((string= name "translate")
       (let ((s    (arg-str 0))
             (from (arg-str 1))
             (to   (arg-str 2)))
         (with-output-to-string (out)
           (loop for ch across s
                 for idx = (position ch from)
                 do (cond
                      ((null idx) (write-char ch out))
                      ((< idx (length to)) (write-char (char to idx) out))
                      ;; character appears in from but not in to: delete it
                      )))))
      (t
       (error "Unknown XPath function: ~S" name)))))

;;; Simple split helper used by normalize-space (avoids external dependency)

(defun %xpath-split-whitespace (str)
  "Split STR on runs of XML whitespace. Returns list of non-empty substrings."
  (let (words)
    (let ((i 0) (n (length str)))
      (loop
        (loop while (and (< i n) (xml-whitespace-p (char str i))) do (incf i))
        (when (>= i n) (return))
        (let ((j i))
          (loop while (and (< i n) (not (xml-whitespace-p (char str i)))) do (incf i))
          (push (subseq str j i) words))))
    (nreverse words)))

;;; Replace the cl:split-sequence-if call in normalize-space with the local helper

(defun %xpath-normalize-space (s)
  (let ((words (%xpath-split-whitespace s)))
    (if words
        (reduce (lambda (a b) (concatenate 'string a " " b)) words)
        "")))


;;;; ── 13. Public API ──────────────────────────────────────────────────────

(defun %xpath-ensure-ast (path)
  "Accept a string or pre-compiled AST; return an AST."
  (if (stringp path)
      (xpath-compile path)
      path))

(defun %xpath-root-ctx (context-node document)
  "Build the initial %xpath-ctx for a top-level query."
  (let ((env (%xpath-env-for context-node document)))
    (%make-xpath-ctx context-node 1 1 env)))

(defun xpath-select (path context-node &key document)
  "Evaluate XPath PATH against CONTEXT-NODE and return a list of matching nodes.

PATH may be a string or a compiled XPath object from XPATH-COMPILE.
CONTEXT-NODE may be an XML-DOCUMENT or XML-NODE.
DOCUMENT is the root XML-DOCUMENT; needed for absolute paths and for the
parent/ancestor axes when CONTEXT-NODE is not itself an XML-DOCUMENT.

Returns a (possibly empty) list of matching nodes.  For expressions that
produce a scalar value (number, string, boolean) the result is wrapped in
a one-element list."
  (let* ((ast (if (stringp path) (xpath-compile path) path))
         (ctx (%xpath-root-ctx context-node document))
         (val (%xpath-eval ast ctx)))
    (if (listp val) val (list val))))

(defun xpath-first (path context-node &key document)
  "Like XPATH-SELECT but returns only the first matching node, or NIL."
  (first (xpath-select path context-node :document document)))

(defun xpath-string (path context-node &key document)
  "Evaluate XPath PATH and return the string value of the first match.
Returns NIL when there are no matches."
  (let ((results (xpath-select path context-node :document document)))
    (when results
      (let ((first-result (first results)))
        (if (stringp first-result)
            first-result
            (%xpath-string-value first-result))))))

(defun xpath-number (path context-node &key document)
  "Evaluate XPath PATH and return its numeric value."
  (let* ((ast (if (stringp path) (xpath-compile path) path))
         (ctx (%xpath-root-ctx context-node document))
         (val (%xpath-eval ast ctx)))
    (%xpath-to-number val)))

(defun xpath-boolean (path context-node &key document)
  "Evaluate XPath PATH and return its boolean value (T or NIL)."
  (let* ((ast (if (stringp path) (xpath-compile path) path))
         (ctx (%xpath-root-ctx context-node document))
         (val (%xpath-eval ast ctx)))
    (%xpath-truthy-p val)))
