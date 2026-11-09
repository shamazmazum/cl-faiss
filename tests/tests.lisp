(in-package :cl-faiss/tests)

(def-suite fp-index :description "FP indinces")

(defun run-tests ()
  (every #'identity
         (mapcar (lambda (suite)
                   (let ((status (run suite)))
                     (explain! status)
                     (results-status status)))
                 '(fp-index))))

(in-suite fp-index)

(serapeum:-> l2-metric ((simple-array single-float (*))
                        (simple-array single-float (*)))
             (values single-float &optional))
(defun l2-metric (v1 v2)
  (declare (optimize (speed 3)))
  (assert (= (length v1)
             (length v2)))
  (loop for x across v1
        for y across v2
        sum (expt (- x y) 2) single-float))

(serapeum:-> l1-metric ((simple-array single-float (*))
                        (simple-array single-float (*)))
             (values single-float &optional))
(defun l1-metric (v1 v2)
  (declare (optimize (speed 3)))
  (assert (= (length v1)
             (length v2)))
  (loop for x across v1
        for y across v2
        sum (abs (- x y)) single-float))

(serapeum:-> linf-metric ((simple-array single-float (*))
                          (simple-array single-float (*)))
             (values single-float &optional))
(defun linf-metric (v1 v2)
  (declare (optimize (speed 3)))
  (assert (= (length v1)
             (length v2)))
  (loop for x across v1
        for y across v2
        maximize (abs (- x y)) single-float))

(serapeum:-> row ((simple-array single-float (* *))
                  (unsigned-byte 32))
             (values (simple-array single-float (*)) &optional))
(defun row (vs k)
  (declare (optimize (speed 3)))
  (let ((row (make-array (array-dimension vs 1)
                         :element-type 'single-float))
        (idx (array-row-major-index vs k 0)))
    (loop for i below (length row) do
      (setf (aref row i)
            (row-major-aref vs (+ idx i))))
    row))

(serapeum:-> find-k-closest ((simple-array single-float (*))
                             (simple-array single-float (* *))
                             (function ((simple-array single-float (*))
                                        (simple-array single-float (*)))
                                       (values single-float &optional))
                             (integer 1))
             (values (simple-array (unsigned-byte 64) (*))
                     (simple-array single-float       (*))
                     &optional))
(defun find-k-closest (v vs f k)
  (declare (optimize (speed 3)))
  (assert (= (array-dimension vs 1)
             (length v)))
  (let ((queue (make-queue (1+ k))))
    (loop for i below (array-dimension vs 0)
          for d = (funcall f (row vs i) v)
          do (enqueue-limited! queue i (- d) k))
    (let ((indices (map '(vector (unsigned-byte 64))
                        #'identity
                        (to-sorted-list! queue))))
      (values indices
              (map '(vector single-float)
                   (lambda (i)
                     (funcall f (row vs i) v))
                   indices)))))

(serapeum:-> find-k-closest-faiss ((simple-array single-float (*))
                                   (simple-array single-float (* *))
                                   faiss:metric (integer 1))
             (values (simple-array (unsigned-byte 64) (*))
                     (simple-array single-float       (*))
                     &optional))
(defun find-k-closest-faiss (v vs metric k)
  (declare (optimize (speed 3)))
  (let* ((length (length v))
         (query (make-array (list 1 length) :element-type 'single-float)))
    (loop for i below length do
      (setf (row-major-aref query i)
            (aref v i)))
    (multiple-value-bind (dists labels)
        (faiss:with-index (index (length v) "Flat" metric)
          (faiss:index-add index vs)
          (faiss:index-search index query k))
      (values labels dists))))

(defun gen-vector (&key dim)
  (gen-buffer :length        (gen-integer :min dim :max dim)
              :element-type 'single-float
              :elements      (gen-float :bound 1.0)))

(defun gen-vectors (&key n dim)
  (lambda ()
    (let ((array (make-array (list n dim) :element-type 'single-float)))
      (loop for i below (array-total-size array) do
        (setf (row-major-aref array i)
              (- (random 2.0) 1.0)))
      array)))

(test flat-index
  (for-all* ((dim (gen-integer :min   3 :max   200))
             (ndb (gen-integer :min 300 :max 30000))
             (k   (gen-integer :min   1 :max    10))
             (vs  (gen-vectors :dim dim :n ndb))
             (v   (gen-vector  :dim dim)))
    (loop for metric-t in '(:l1 :l2 :linf)
          for metric-f in `(,#'l1-metric ,#'l2-metric ,#'linf-metric) do
            (serapeum:mvlet ((ind1 dist1 (find-k-closest-faiss v vs metric-t k))
                             (ind2 dist2 (find-k-closest v vs metric-f k)))
              (is-true (every #'approx:approxp dist1 dist2))
              (is-true (every #'= ind1 ind2))))))

