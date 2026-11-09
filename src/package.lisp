(defpackage cl-faiss
  (:use #:cl #:cffi)
  (:export #:metric
           #:index
           #:index-vector-length
           #:index-trained-p
           #:index-ntotal
           #:index-metric
           #:index-train
           #:index-add
           #:index-search
           #:with-index))
