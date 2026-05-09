(in-package #:io.github.cl-sdk.xml)

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
DOCTYPE is an xml-doctype struct if a DOCTYPE declaration was present, or NIL.
ROOT is the root xml-node."
  prolog
  doctype
  root)

(defstruct xml-dtd-element
  "Represents a DTD <!ELEMENT> declaration.
NAME is the element type name string.
CONTENT-MODEL is the parsed content model:
  :empty               — EMPTY keyword
  :any                 — ANY keyword
  (:mixed name*)       — mixed content (#PCDATA possibly with element names)
  content-particle     — element content (see below)

A content-particle is one of:
  \"name\"              — element name, occurs exactly once
  (:? content)         — content occurs 0 or 1 times (optional)
  (:* content)         — content occurs 0 or more times
  (:+ content)         — content occurs 1 or more times
  (:seq  cp*)          — sequence of content particles, exactly once
  (:choice cp*)        — choice among content particles, exactly once"
  name
  content-model)

(defstruct xml-dtd-att-def
  "Represents a single attribute definition within a DTD <!ATTLIST> declaration.
NAME is the attribute name string.
TYPE is the attribute type:
  :cdata              — string type (CDATA)
  :id                 — tokenized type (ID)
  :idref              — tokenized type (IDREF)
  :idrefs             — tokenized type (IDREFS)
  :entity             — tokenized type (ENTITY)
  :entities           — tokenized type (ENTITIES)
  :nmtoken            — tokenized type (NMTOKEN)
  :nmtokens           — tokenized type (NMTOKENS)
  (:notation n+)      — NOTATION enumeration: list of name strings
  (:enumeration tok+) — enumeration of Nmtoken strings
DEFAULT is the default declaration:
  :required           — #REQUIRED
  :implied            — #IMPLIED
  (:fixed value)      — #FIXED AttValue
  (:default value)    — bare AttValue (no keyword prefix)"
  name
  type
  default)

(defstruct xml-dtd-attlist
  "Represents a DTD <!ATTLIST> declaration.
ELEMENT-NAME is the element type name string this declaration applies to.
DEFINITIONS is a list of xml-dtd-att-def structs in document order."
  element-name
  definitions)

(defstruct xml-dtd-entity
  "Represents a DTD <!ENTITY> declaration.
NAME is the entity name string.
PARAMETER-P is T for a parameter entity (<!ENTITY % name …>), NIL otherwise.
DEFINITION is the entity definition:
  string                    — internal entity; the replacement text (raw, unexpanded)
  (:external pub sys)       — external parsed entity; pub is the public identifier
                               string or NIL, sys is the system identifier string
  (:unparsed pub sys ndata) — external unparsed entity; ndata is the NDATA name"
  name
  parameter-p
  definition)

(defstruct xml-dtd-notation
  "Represents a DTD <!NOTATION> declaration (XML 1.0 §4.7).
NAME is the notation name string.
PUBLIC-ID is the public identifier string, or NIL.
SYSTEM-ID is the system identifier string, or NIL.
At least one of PUBLIC-ID or SYSTEM-ID is non-NIL."
  name
  public-id
  system-id)

(defstruct xml-doctype
  "Represents a parsed DOCTYPE declaration.
NAME is the root element type name string.
PUBLIC-ID is the public identifier string, or NIL.
SYSTEM-ID is the system identifier string, or NIL.
ELEMENTS is a list of xml-dtd-element structs from the internal subset.
ATTLISTS is a list of xml-dtd-attlist structs from the internal subset.
ENTITIES is a list of xml-dtd-entity structs from the internal subset.
NOTATIONS is a list of xml-dtd-notation structs from the internal subset."
  name
  public-id
  system-id
  elements
  attlists
  entities
  notations)

(defstruct xml-qname
  "Represents a namespace-qualified XML name (Namespaces in XML 1.0 §2.1).
PREFIX is the namespace prefix string, or NIL when there is no prefix.
LOCAL-NAME is the local part of the name (the portion after ':'), or the full
unqualified name when PREFIX is NIL.
NAMESPACE-URI is the namespace URI string that PREFIX is bound to, or NIL when
the name is in no namespace."
  prefix
  local-name
  namespace-uri)
