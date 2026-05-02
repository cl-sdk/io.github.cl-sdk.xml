(in-package #:cl-xml)

;;; Default DOM-building SAX handler

(defclass dom-builder (sax-handler)
  ((%prolog  :initform '())
   (%doctype :initform nil)
   (%stack   :initform '())
   (%root    :initform nil))
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

(defmethod doctype-declaration ((handler dom-builder) doctype)
  (setf (slot-value handler '%doctype) doctype))

(defmethod end-document ((handler dom-builder))
  (make-xml-document :prolog  (nreverse (slot-value handler '%prolog))
                     :doctype (slot-value handler '%doctype)
                     :root    (slot-value handler '%root)))
