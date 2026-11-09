(in-package :cl-faiss)

(sb-c:defknown index-search (index (simple-array single-float (* *)) (unsigned-byte 63))
    (values (simple-array single-float       (*))
            (simple-array (unsigned-byte 64) (*)))
    (sb-c:any)
  :overwrite-fndb-silently t)

(sb-c:defoptimizer (index-search sb-c:derive-type) ((index vs k))
  (declare (ignore index vs))
  (when (sb-c:constant-lvar-p k)
    (let ((k (sb-c:lvar-value k)))
      (sb-kernel:make-values-type
       (list
        (make-vector-type (sb-kernel:specifier-type 'single-float)       k)
        (make-vector-type (sb-kernel:specifier-type '(unsigned-byte 64)) k))))))
