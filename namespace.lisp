(in-package #:io.github.cl-sdk.xml)

;;; Namespace resolution — Namespaces in XML 1.0

(defparameter +xml-namespace-uri+
  "http://www.w3.org/XML/1998/namespace"
  "Pre-declared namespace URI for the 'xml' prefix (Namespaces in XML 1.0 §3).")

(defparameter +xmlns-namespace-uri+
  "http://www.w3.org/2000/xmlns/"
  "Pre-declared namespace URI for the 'xmlns' prefix (Namespaces in XML 1.0 §3).")

(defun %split-qname (name)
  "Split a raw NAME string at the first colon.
Returns (values prefix local-name) where PREFIX is NIL when there is no colon."
  (let ((colon (position #\: name)))
    (if colon
        (values (subseq name 0 colon) (subseq name (1+ colon)))
        (values nil name))))

(defun %resolve-attr-qname (name ns-bindings)
  "Resolve a raw attribute NAME string into an XML-QNAME using NS-BINDINGS.
Attributes without a prefix are in no namespace (the default namespace does
not apply to attributes per Namespaces in XML 1.0 §6.2)."
  (multiple-value-bind (prefix local-name) (%split-qname name)
    (let ((uri (cond
                 ((null prefix) nil)
                 ((string= prefix "xml")   +xml-namespace-uri+)
                 ((string= prefix "xmlns") +xmlns-namespace-uri+)
                 (t
                  (let ((binding (assoc prefix ns-bindings :test #'string=)))
                    (unless binding
                      (error "Undeclared namespace prefix '~a'" prefix))
                    (cdr binding))))))
      (make-xml-qname :prefix        prefix
                      :local-name    local-name
                      :namespace-uri uri))))

(defun %resolve-elem-qname (name ns-bindings default-ns)
  "Resolve a raw element NAME string into an XML-QNAME.
NS-BINDINGS is an alist of (prefix . uri) pairs; DEFAULT-NS is the current
default namespace URI (NIL when there is no default namespace)."
  (multiple-value-bind (prefix local-name) (%split-qname name)
    (let ((uri (cond
                 ((null prefix) default-ns)
                 ((string= prefix "xml")   +xml-namespace-uri+)
                 ((string= prefix "xmlns") +xmlns-namespace-uri+)
                 (t
                  (let ((binding (assoc prefix ns-bindings :test #'string=)))
                    (unless binding
                      (error "Undeclared namespace prefix '~a'" prefix))
                    (cdr binding))))))
      (make-xml-qname :prefix        prefix
                      :local-name    local-name
                      :namespace-uri uri))))

(defun %collect-ns-decls (attributes)
  "Partition ATTRIBUTES (an alist of raw-name . value pairs) into namespace
declarations and ordinary attributes.
Returns (values ns-decls remaining) where NS-DECLS is an alist of (prefix . uri)
pairs with NIL as the prefix for a default-namespace declaration (xmlns=\"...\")
and REMAINING is the list of non-declaration attributes."
  (let (ns-decls remaining)
    (dolist (attr attributes)
      (let ((k (car attr)) (v (cdr attr)))
        (cond
          ((string= k "xmlns")
           (push (cons nil v) ns-decls))
          ((and (> (length k) 6) (string= k "xmlns:" :end1 6))
           (push (cons (subseq k 6) v) ns-decls))
          (t
           (push attr remaining)))))
    (values ns-decls (nreverse remaining))))

(defun %resolve-ns-node (node ns-bindings default-ns)
  "Return a fresh XML-NODE tree with namespace-resolved tag and attribute keys.
NS-BINDINGS is the inherited alist of (prefix . uri) pairs; DEFAULT-NS is the
inherited default namespace URI."
  (multiple-value-bind (ns-decls remaining-attrs)
      (%collect-ns-decls (xml-node-attributes node))
    (let* (;; Update default namespace: xmlns="" resets it to NIL
           (default-ns-entry (assoc nil ns-decls))
           (new-default-ns   (if default-ns-entry
                                 (let ((v (cdr default-ns-entry)))
                                   (if (string= v "") nil v))
                                 default-ns))
           ;; Prepend new prefix bindings so they shadow outer ones
           (new-bindings     (append (remove nil ns-decls :key #'car)
                                     ns-bindings))
           (qname            (%resolve-elem-qname (xml-node-tag node)
                                                  new-bindings
                                                  new-default-ns))
           (resolved-attrs   (mapcar (lambda (attr)
                                       (cons (%resolve-attr-qname (car attr)
                                                                   new-bindings)
                                             (cdr attr)))
                                     remaining-attrs))
           (resolved-children
            (mapcar (lambda (child)
                      (typecase child
                        (xml-node (%resolve-ns-node child
                                                    new-bindings
                                                    new-default-ns))
                        (t child)))
                    (xml-node-children node))))
      (make-xml-node :tag        qname
                     :attributes resolved-attrs
                     :children   resolved-children))))

(defun resolve-namespaces (document)
  "Return a copy of DOCUMENT with all element and attribute names resolved to
XML-QNAME instances according to Namespaces in XML 1.0.

Each XML-NODE's TAG field becomes an XML-QNAME struct, and each attribute key
in the attribute alist also becomes an XML-QNAME.  Namespace declaration
attributes (xmlns and xmlns:prefix) are consumed and do not appear in the
resolved attribute alist.

The 'xml' and 'xmlns' prefixes are pre-bound per the specification and require
no explicit xmlns declaration.

Signals an error if a namespace prefix is used but has not been declared.
An xmlns=\"\" declaration resets the default namespace to NIL (no namespace)."
  (let ((ns-bindings (list (cons "xml"   +xml-namespace-uri+)
                           (cons "xmlns" +xmlns-namespace-uri+))))
    (make-xml-document
     :prolog  (xml-document-prolog document)
     :doctype (xml-document-doctype document)
     :root    (%resolve-ns-node (xml-document-root document)
                                ns-bindings
                                nil))))
