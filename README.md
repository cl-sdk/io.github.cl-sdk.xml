# cl-xml

> **Note:** cl-xml is a temporary name as it is already taken on Quicklisp.

A Common Lisp XML reader, writer, and custom parser.

## Installation

```lisp
(ql:quickload "cl-xml")
```

## Parsing

`parse-xml` accepts a string and an optional `:handler` keyword argument.

* **Default behaviour** — when no handler is given, `parse-xml` returns an
  `xml-document` built by the built-in `dom-builder` handler (fully
  backward-compatible).
* **SAX behaviour** — when a custom handler is supplied, the parser fires
  events on it and returns whatever `end-document` returns.

```lisp
(defvar *doc*
  (cl-xml:parse-xml "<?xml version=\"1.0\"?>
<!-- preamble -->
<root>
  <item id=\"1\">hello &amp; world</item>
  <!-- note -->
  <![CDATA[literal <text>]]>
  <?app instruction?>
</root>"))
```

### SAX parsing

Provide a subclass of `sax-handler` and pass an instance as `:handler` to
`parse-xml`.  Specialize only the event methods you care about; unspecialized
methods are no-ops.

```lisp
(defclass my-handler (cl-xml:sax-handler) ())

(defmethod cl-xml:start-element ((h my-handler) tag attributes)
  (format t "open  ~a ~a~%" tag attributes))

(defmethod cl-xml:end-element ((h my-handler) tag)
  (format t "close ~a~%" tag))

(defmethod cl-xml:end-document ((h my-handler))
  :done)

(cl-xml:parse-xml "<root><child /></root>" :handler (make-instance 'my-handler))
;; open  root nil
;; open  child nil
;; close child
;; close root
;; => :done
```

#### SAX handler generic functions

| Generic function | When called |
|---|---|
| `(start-document handler)` | once, before any other event |
| `(end-document handler)` | once, after all events; return value is `parse-xml`'s result |
| `(start-element handler tag attributes)` | opening / self-closing tag |
| `(end-element handler tag)` | closing / self-closing tag |
| `(characters handler text)` | character data (entity refs already expanded) |
| `(comment handler data)` | `<!-- … -->` comment |
| `(processing-instruction handler target data)` | `<?target data?>` PI |
| `(cdata-section handler data)` | `<![CDATA[…]]>` section |
```

### xml-document

The top-level result of `parse-xml`.

| Accessor | Returns |
|---|---|
| `xml-document-prolog` | list of `xml-comment` / `xml-pi` nodes before the root element |
| `xml-document-root`   | the root `xml-node` |

```lisp
(cl-xml:xml-document-prolog *doc*)
;; => (#<xml-pi "xml" …> #<xml-comment " preamble ">)

(cl-xml:xml-node-tag (cl-xml:xml-document-root *doc*))
;; => "root"
```

### xml-node (element)

| Accessor | Returns |
|---|---|
| `xml-node-tag`        | element name as a string |
| `xml-node-attributes` | alist of `(name . value)` string pairs |
| `xml-node-children`   | list of child nodes (see node types below) |

```lisp
(let* ((root (cl-xml:xml-document-root *doc*))
       (item (first (cl-xml:xml-node-children root))))
  (cl-xml:xml-node-tag item)                      ; => "item"
  (cl-xml:xml-node-attributes item)               ; => (("id" . "1"))
  (cl-xml:xml-node-children item))                ; => ("hello & world")
```

### xml-comment

Represents a `<!-- … -->` comment.

| Accessor | Returns |
|---|---|
| `xml-comment-data` | comment body as a string |

### xml-pi (processing instruction)

Represents a `<?target data?>` processing instruction.

| Accessor | Returns |
|---|---|
| `xml-pi-target` | target name as a string |
| `xml-pi-data`   | data string (may be empty) |

### xml-cdata

Represents a `<![CDATA[…]]>` section.

| Accessor | Returns |
|---|---|
| `xml-cdata-data` | literal content as a string |

### Node types inside xml-node-children

Each child of an `xml-node` is one of:

| Type | Produced by |
|---|---|
| `xml-node`    | `<child …>` / `<child />` |
| `xml-comment` | `<!-- … -->` |
| `xml-pi`      | `<?target data?>` |
| `xml-cdata`   | `<![CDATA[…]]>` |
| `string`      | character data / entity references |

Whitespace-only character data between elements is discarded.

## XML 1.0 conformance

- **§2.3 Names** — `NameStartChar` / `NameChar` Unicode ranges enforced
- **§2.3 / §3.3.3 Attribute values** — bare `<` is an error; entity/character references expanded
- **§2.5 Comments** — `--` inside a comment body is an error
- **§2.7 CDATA sections** — content is literal (markup characters not interpreted)
- **§2.8 Prolog** — XML declaration and DOCTYPE handled; prolog comments/PIs preserved
- **§3.1 Attributes** — duplicate attribute names are an error
- **§4.6 References** — `&amp;` `&lt;` `&gt;` `&quot;` `&apos;` `&#N;` `&#xN;` expanded

## References

cl-xml is a hand-written recursive-descent parser implemented in Common Lisp.
It targets the specifications listed below.

- [Extensible Markup Language (XML) 1.0](https://www.w3.org/TR/xml/) — the core grammar and well-formedness rules that govern parsing, character data, entity references, comments, CDATA sections, processing instructions, and the document prolog.
- [XML Schema Part 1: Structures](https://www.w3.org/TR/xmlschema-1/) — the schema-definition language used as a reference for element and attribute declarations, content models, and type hierarchies.

## License

Unlicense
