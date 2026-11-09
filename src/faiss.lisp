(in-package :cl-faiss)

(defmacro with-pointers-to-vector-data (bindings &body body)
  (reduce
   (lambda (binding acc)
     (destructuring-bind (ptr vector) binding
       `(with-pointer-to-vector-data (,ptr ,vector)
          ,acc)))
   bindings
   :from-end t
   :initial-value `(progn ,@body)))

(define-foreign-library faiss
  (:unix  (:or "libfaiss_c.so"))
  (t (:default "libfaiss_c")))
(use-foreign-library faiss)

(define-condition faiss-error (simple-error)
  ()
  (:documentation "Error signaled by faiss"))

(defcfun ("faiss_get_last_error" get-last-error) :string)

(deftype metric ()
  "Metric type for query indices"
  `(member :inner-product :l2 :l1 :linf
           :canberra :bray-curtis :jensen-shannon))

(serapeum:-> metric-to-foreign (metric)
             (values (unsigned-byte 31) &optional))
(defun metric-to-foreign (metric)
  (declare (optimize (speed 3)))
  (case metric
    (:inner-product   0)
    (:l2              1)
    (:l1              2)
    (:linf            3)
    (:canberra       20)
    (:bray-curtis    21)
    (:jensen-shannon 22)))

(serapeum:-> metric-from-foreign ((unsigned-byte 31))
             (values metric &optional))
(defun metric-from-foreign (metric)
  (declare (optimize (speed 3)))
  (ecase metric
    (0  :inner-product)
    (1  :l2)
    (2  :l1)
    (3  :linf)
    (20 :canberra)
    (21 :bray-curtis)
    (22 :jensen-shannon)))

;; Constructor / Destructor
(defcfun ("faiss_index_factory" %index-factory) :int
  (index  :pointer)
  (d      :int)
  (descr  :string)
  (metric :int))

(defcfun ("faiss_Index_free" %index-free) :void
  (index :pointer))

(serapeum:defconstructor index
  (ptr t))

(serapeum:-> index-factory ((unsigned-byte 31) string metric)
             (values index &optional))
(defun index-factory (vector-length description metric)
  (with-foreign-object (ptr :pointer)
    (unless (zerop (%index-factory
                    ptr vector-length description
                    (metric-to-foreign metric)))
      (error 'faiss-error
             :format-control "Cannot create an index: ~a"
             :format-arguments (list (get-last-error))))
    (index (mem-ref ptr :pointer))))

(serapeum:-> index-free (index)
             (values &optional))
(defun index-free (index)
  (%index-free (index-ptr index))
  (values))

(defmacro with-index ((index vector-length description metric) &body body)
  "Create an index (as with faiss_index_factory) and destroy in when
control leaves BODY."
  `(let ((,index (index-factory ,vector-length ,description ,metric)))
     (unwind-protect
          (progn ,@body)
       (index-free ,index))))

;; Getters
(defcfun ("faiss_Index_d" %index-vector-length) :int
  (index :pointer))

(serapeum:-> index-vector-length (index)
             (values (unsigned-byte 31) &optional))
(defun index-vector-length (index)
  "Get d (vector length parameter) from an index."
  (%index-vector-length
   (index-ptr index)))

(defcfun ("faiss_Index_is_trained" %index-trained-p) :boolean
  (index :pointer))

(serapeum:-> index-trained-p (index)
             (values boolean &optional))
(defun index-trained-p (index)
  "Return if an index is trained. Some indices do not need to be
trained. In this case T is returned."
  (%index-trained-p
   (index-ptr index)))

(defcfun ("faiss_Index_ntotal" %index-ntotal) :int64
  "Return total number of vectors in the index."
  (index :pointer))

(serapeum:-> index-ntotal (index)
             (values (unsigned-byte 63) &optional))
(defun index-ntotal (index)
  (%index-ntotal
   (index-ptr index)))

(defcfun ("faiss_Index_metric_type" %index-metric) :int
  (index :pointer))

(serapeum:-> index-metric (index)
             (values metric &optional))
(defun index-metric (index)
  "Return metric type with which an index was created."
  (metric-from-foreign
   (%index-metric
    (index-ptr index))))

(declaim (inline check-vector-length))
(defun check-vector-length (index vs)
  (unless (= (array-dimension vs 1)
             (index-vector-length index))
    (error 'faiss-error
           :format-control
           "Index vector length and array dimensions mismatch")))

;; Training / Addition
(macrolet ((frob (c-name lisp-name &key documentation)
             (let ((low-level-name (intern (format nil "%~a" (symbol-name lisp-name)))))
               `(progn
                  (defcfun (,c-name ,low-level-name) :int
                    (index :pointer)
                    (n     :int64)
                    (x     (:pointer :float)))

                  (serapeum:-> ,lisp-name (index (simple-array single-float (* *)))
                               (values &optional))
                  (defun ,lisp-name (index vs)
                    ,@(if documentation (list documentation))
                    (declare (optimize (speed 3)))
                    (check-vector-length index vs)
                    (with-pointer-to-vector-data (vs-ptr (sb-ext:array-storage-vector vs))
                      (unless (zerop (,low-level-name
                                      (index-ptr index)
                                      (array-dimension vs 0)
                                      vs-ptr))
                        (error 'faiss-error
                               :format-control "Add/Train error: ~a"
                               :format-arguments (list (get-last-error)))))
                    (values))))))
  (frob "faiss_Index_train" index-train
        :documentation "Perform training on a representative set of vectors.

Vectors are passed as an array of dimensionsionality NxD where N is
the number of vectors and D was specified when the index was
created.")
  (frob "faiss_Index_add"   index-add
        :documentation "Add vectors to an index.

Vectors are passed as an array of dimensionsionality NxD where N is
the number of vectors and D was specified when the index was
created."))

;; Searching
(defcfun ("faiss_Index_search" %index-search) :int
  (index     :pointer)
  (n         :int64)
  (x         (:pointer :float))
  (k         :int64)
  (distances (:pointer :float))
  (labels    (:pointer :int64)))

#+nil
(serapeum:-> index-search (index (simple-array single-float (* *)) (unsigned-byte 63))
             (values (simple-array single-float       (*))
                     (simple-array (unsigned-byte 64) (*))
                     &optional))
(sb-c:defknown index-search (index (simple-array single-float (* *)) (unsigned-byte 63))
    (values (simple-array single-float       (*))
            (simple-array (unsigned-byte 64) (*)))
    (sb-c:any)
  :overwrite-fndb-silently t)

(defun index-search (index vs k)
  "Search for k nearest neighbors to vectors VS in the index. VS is in
the same form as in INDEX-TRAIN / INDEX-ADD. Two vectors with
distances and labels are returned."
  (declare (optimize (speed 3)))
  (check-vector-length index vs)
  (let ((distances (make-array k :element-type 'single-float))
        (labels    (make-array k :element-type '(unsigned-byte 64))))
    (with-pointers-to-vector-data ((dptr  distances)
                                   (lptr  labels)
                                   (vsptr (sb-ext:array-storage-vector vs)))
      (unless (zerop (%index-search
                      (index-ptr index)
                      (array-dimension vs 0)
                      vsptr k dptr lptr))
        (error 'faiss-error
               :format-control "Search error: ~a"
               :format-arguments (list (get-last-error)))))
    (values distances labels)))

(defun make-vector-type (type n)
  (sb-kernel:make-array-type
   (list n)
   :complexp                 nil
   :element-type             type
   :specialized-element-type type))

(sb-c:defoptimizer (index-search sb-c:derive-type) ((index vs k))
  (declare (ignore index vs))
  (when (sb-c:constant-lvar-p k)
    (let ((k (sb-c:lvar-value k)))
      (sb-kernel:make-values-type
       (list
        (make-vector-type (sb-kernel:specifier-type 'single-float)       k)
        (make-vector-type (sb-kernel:specifier-type '(unsigned-byte 64)) k))))))
