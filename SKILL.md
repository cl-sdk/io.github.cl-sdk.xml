# cl-xml Skill

cl-xml is a Common Lisp library for parsing XML documents. It supports both a
DOM-style interface (returning a tree of structs) and a SAX-style interface
(firing events on a user-supplied handler).

> **Note:** `cl-xml` is a temporary name; the package is not yet on Quicklisp
> under this name.

---

## Capabilities

| Capability | Details |
|---|---|
| DOM parsing | `parse-xml` returns an `xml-document` struct containing the full node tree |
| SAX parsing | Pass a `sax-handler` subclass to `parse-xml` to process events incrementally |
| Entity expansion | Built-in entity references (`&amp;` `&lt;` `&gt;` `&quot;` `&apos;`) and character references (`&#N;` `&#xN;`) are expanded automatically |
| Comment nodes | `<!-- … -->` comments are preserved in the tree and surfaced as SAX events |
| Processing instructions | `<?target data?>` nodes are preserved and surfaced as SAX events |
| CDATA sections | `<![CDATA[…]]>` content is parsed literally; markup characters inside are not interpreted |
| Prolog handling | XML declaration and DOCTYPE declarations are recognized; leading comments and PIs are collected in `xml-document-prolog` |
| XML 1.0 Names | `NameStartChar` / `NameChar` Unicode ranges are enforced |
| Attribute validation | Bare `<` in attribute values and duplicate attribute names are detected as errors |

---

## Public API

### Entry point

```lisp
(cl-xml:parse-xml string &key handler)
```

* With no `:handler` — returns an `xml-document` (DOM mode).
* With a `:handler` instance — fires SAX events and returns whatever
  `end-document` returns (SAX mode).

---

### DOM structures

#### `xml-document`

The top-level result of `parse-xml`.

| Accessor | Returns |
|---|---|
| `xml-document-prolog` | list of `xml-comment` / `xml-pi` nodes that precede the root element |
| `xml-document-root`   | the root `xml-node` |

#### `xml-node` (element)

| Accessor | Returns |
|---|---|
| `xml-node-tag`        | element name as a string |
| `xml-node-attributes` | alist of `(name . value)` string pairs |
| `xml-node-children`   | list of child nodes (see types below) |

#### `xml-comment`

| Accessor | Returns |
|---|---|
| `xml-comment-data` | comment body as a string |

#### `xml-pi` (processing instruction)

| Accessor | Returns |
|---|---|
| `xml-pi-target` | target name as a string |
| `xml-pi-data`   | data string (may be empty) |

#### `xml-cdata`

| Accessor | Returns |
|---|---|
| `xml-cdata-data` | literal CDATA content as a string |

**Child node types** — each item in `xml-node-children` is one of:
`xml-node`, `xml-comment`, `xml-pi`, `xml-cdata`, or a plain `string`
(character data with entity references already expanded).
Whitespace-only text between elements is discarded.

---

### SAX handler protocol

Subclass `cl-xml:sax-handler` and specialize only the methods you need;
the rest are no-ops by default.

| Generic function | When called |
|---|---|
| `(start-document handler)` | once, before any other event |
| `(end-document handler)` | once, after all events; return value becomes `parse-xml`'s result |
| `(start-element handler tag attributes)` | opening or self-closing tag |
| `(end-element handler tag)` | closing or self-closing tag |
| `(characters handler text)` | character data (entity references already expanded) |
| `(comment handler data)` | `<!-- … -->` comment |
| `(processing-instruction handler target data)` | `<?target data?>` PI |
| `(cdata-section handler data)` | `<![CDATA[…]]>` section |

---

## Usage examples

### DOM parsing

```lisp
(defvar *doc*
  (cl-xml:parse-xml "<?xml version=\"1.0\"?>
<!-- intro -->
<root>
  <item id=\"1\">hello &amp; world</item>
  <![CDATA[literal <text>]]>
  <?app instruction?>
</root>"))

(cl-xml:xml-node-tag (cl-xml:xml-document-root *doc*))
;; => "root"

(let* ((root (cl-xml:xml-document-root *doc*))
       (item (first (cl-xml:xml-node-children root))))
  (cl-xml:xml-node-attributes item))   ; => (("id" . "1"))
```

### SAX parsing

```lisp
(defclass tag-collector (cl-xml:sax-handler)
  ((tags :initform '() :accessor tags)))

(defmethod cl-xml:start-element ((h tag-collector) tag attributes)
  (push tag (tags h)))

(defmethod cl-xml:end-document ((h tag-collector))
  (nreverse (tags h)))

(cl-xml:parse-xml "<a><b/><c/></a>" :handler (make-instance 'tag-collector))
;; => ("a" "b" "c")
```

---

## Loading

```lisp
;; Via ASDF (local checkout):
(asdf:load-system "cl-xml")

;; Via Quicklisp (once available):
(ql:quickload "cl-xml")
```

## License

MIT
