(in-package :cl-faiss)

(defun make-vector-type (type n k)
  (sb-kernel:make-array-type
   (list n k)
   :complexp                 nil
   :element-type             type
   :specialized-element-type type))

(sb-c:defknown index-search (index (simple-array single-float (* *)) (unsigned-byte 63))
    (values (simple-array single-float       (* *))
            (simple-array (unsigned-byte 64) (* *)))
    (sb-c:any)
  :overwrite-fndb-silently t)

(sb-c:defoptimizer (index-search sb-c:derive-type) ((index vs k))
  (declare (ignore index))
  (let* ((vs-type (sb-c::lvar-type vs))
         (vs-dims (sb-kernel:array-type-dimensions vs-type))
         (n (if (atom vs-dims) '* (first vs-dims)))
         (k (if (sb-c:constant-lvar-p k) (sb-c:lvar-value k) '*)))
    (sb-kernel:make-values-type
     (list
      (make-vector-type (sb-kernel:specifier-type 'single-float)       n k)
      (make-vector-type (sb-kernel:specifier-type '(unsigned-byte 64)) n k)))))
