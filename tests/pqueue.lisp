;; Written by Michał "phoe" Herda, MIT licensed
;; https://github.com/phoe/damn-fast-priority-queue

(in-package #:cl-faiss/tests)

(deftype data-type () 't)
(deftype data-vector-type () '(simple-array data-type (*)))
(deftype prio-type () 'single-float)
(deftype prio-vector-type () '(simple-array prio-type (*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Structure definition

(defstruct (queue (:conc-name #:%) (:constructor %make)
                  (:predicate nil) (:copier nil))
  (data-vector (make-array 256 :element-type 'data-type) :type data-vector-type)
  (prio-vector (make-array 256 :element-type 'prio-type) :type prio-vector-type)
  (size 0 :type a:array-length)
  (locked nil :type boolean))

(serapeum:-> make-queue (a:array-index)
             (values queue &optional))
(defun make-queue (storage-size)
  "Make a priority queue. Priority is a real number. An element which
has the lowest priority dequeues first."
  (%make :data-vector (make-array storage-size
                                  :element-type 'data-type)
         :prio-vector (make-array storage-size
                                  :element-type 'prio-type)))

(defmethod print-object ((object queue) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(~D)" (%size object))))

(serapeum:-> copy-queue (queue) (values queue &optional))
(defun copy-queue (queue)
  (%make :size (%size queue)
         :data-vector (copy-seq (%data-vector queue))
         :prio-vector (copy-seq (%prio-vector queue))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Dequeueing

(serapeum:-> heapify-downwards! (data-vector-type prio-vector-type a:array-index)
             (values &optional))
(declaim (inline heapify-downwards!))
(defun heapify-downwards! (data-vector prio-vector size)
  (let ((parent-index 0))
    (loop
      (let* ((left-index (+ (* parent-index 2) 1))
             (left-index-validp (< left-index size))
             (right-index (+ (* parent-index 2) 2))
             (right-index-validp (< right-index size)))
        (flet ((swap-left ()
                 (rotatef (aref prio-vector parent-index)
                          (aref prio-vector left-index))
                 (rotatef (aref data-vector parent-index)
                          (aref data-vector left-index))
                 (setf parent-index left-index))
               (swap-right ()
                 (rotatef (aref prio-vector parent-index)
                          (aref prio-vector right-index))
                 (rotatef (aref data-vector parent-index)
                          (aref data-vector right-index))
                 (setf parent-index right-index)))
          (declare (inline swap-left swap-right))
          (when (and (not left-index-validp)
                     (not right-index-validp))
            (return))
          (when (and left-index-validp
                     (< (aref prio-vector parent-index)
                        (aref prio-vector left-index))
                     (or (not right-index-validp)
                         (< (aref prio-vector parent-index)
                            (aref prio-vector right-index))))
            (return))
          (if (and right-index-validp
                   (<= (aref prio-vector right-index)
                       (aref prio-vector left-index)))
              (swap-right)
              (swap-left))))))
  (values))

(serapeum:-> dequeue! (queue) (values t boolean &optional))
(defun dequeue! (queue)
  "Remove an element with the lowest priority from the queue and return it."
  (declare (optimize (speed 3)))
  (if (zerop (%size queue))
      (values nil nil)
      (let ((data-vector (%data-vector queue))
            (prio-vector (%prio-vector queue)))
        (multiple-value-prog1 (values (aref data-vector 0) t)
          (decf (%size queue))
          (let ((old-data (aref data-vector (%size queue)))
                (old-prio (aref prio-vector (%size queue))))
            (setf (aref data-vector 0) old-data
                  (aref prio-vector 0) old-prio))
          (heapify-downwards! data-vector prio-vector (%size queue))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Enqueueing

(serapeum:-> heapify-upwards! (data-vector-type prio-vector-type a:array-length)
             (values &optional))
(declaim (inline heapify-upwards!))
(defun heapify-upwards! (data-vector prio-vector index)
  (do ((child-index index parent-index)
       (parent-index (ash (1- index) -1) (ash (1- parent-index) -1)))
      ((zerop child-index))
    (let ((child-priority (aref prio-vector child-index))
          (parent-priority (aref prio-vector parent-index)))
      (cond ((< child-priority parent-priority)
             (rotatef (aref prio-vector parent-index)
                      (aref prio-vector child-index))
             (rotatef (aref data-vector parent-index)
                      (aref data-vector child-index)))
            (t (return)))))
  (values))

(serapeum:-> enqueue! (queue t prio-type) (values &optional))
(defun enqueue! (queue object priority)
  "Insert an object to the queue."
  (declare (optimize (speed 3)))
  (symbol-macrolet ((data-vector (%data-vector queue))
                    (prio-vector (%prio-vector queue)))
    (let ((size (%size queue))
          (length (array-total-size data-vector)))
      (when (>= size length)
        (error 'queue-size-limit-reached :queue queue :element object))
      (setf (aref data-vector size) object
            (aref prio-vector size) priority)
      (heapify-upwards! data-vector prio-vector (%size queue))
      (incf (%size queue))
      (values))))

(serapeum:-> enqueue-limited! (queue t prio-type a:array-length)
             (values boolean &optional))
(defun enqueue-limited! (queue object priority limit)
  "Insert an object to the queue. An element with the lowest priority
can be removed, so that total amount of elements in the queue is no
more than @c(LIMIT)."
  (declare (optimize (speed 3)))
  (cond
    ((< (%size queue) limit)
     (enqueue! queue object priority)
     t)
    ((> priority (aref (%prio-vector queue) 0))
     (enqueue! queue object priority)
     (dequeue! queue)
     t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Introspection and maintenance

(defmacro do-queue ((object queue &optional result) &body body)
  "Loop for each element of the queue."
  (multiple-value-bind (forms declarations) (a:parse-body body)
    (a:with-gensyms (i)
      (a:once-only (queue)
        `(dotimes (,i (%size ,queue) ,result)
           (let ((,object (aref (%data-vector ,queue) ,i)))
             ,@declarations
             (tagbody ,@forms)))))))

(serapeum:-> peek (queue) (values t boolean &optional))
(declaim (inline peek))
(defun peek (queue)
  "Return @c((values e t)) where @c(e) is an element with the lowest
priority without removing it if the queue is non-empty, otherwise
return @c((values nil nil))."
  (if (zerop (%size queue))
      (values nil nil)
      (values (aref (%data-vector queue) 0) t)))

(serapeum:-> size (queue) (values a:array-length &optional))
(declaim (inline size))
(defun size (queue)
  "Get the number of elements in the queue."
  (%size queue))

(serapeum:-> in-queue-p (queue t &key (:key function))
             (values t &optional))
(declaim (inline in-queue-p))
(defun in-queue-p (queue object &key (key #'identity))
  "Check if an object is in the queue, O(n)."
  (declare (optimize (speed 3) (space 0)))
  (find object (%data-vector queue) :test #'eq :key key :end (%size queue)))

(serapeum:-> to-sorted-list! (queue &optional (function (t) (values t &optional)))
             (values list &optional))
(defun to-sorted-list! (q &optional (key #'identity))
  "Return a list of elements in the queue, sorted by priority. The
element with the highest priority comes first. The queue is
destructively modified in the process."
  (declare (optimize (speed 3)))
  (let (acc)
    (loop for obj = (dequeue! q)
          while obj
          do (push (funcall key obj) acc))
    acc))

(serapeum:-> map-into! (queue (function (t) (values t &optional)))
             (values &optional))
(declaim (inline map-into!))
(defun map-into! (q f)
  "Replace elements \\(e_0, e_1, \\dots, e_n\\) in the queue with
\\(f(e_0), f(e_1), \\dots, f(e_n)\\)."
  (let ((vector (%data-vector q)))
    (dotimes (i (%size q))
      (setf (aref vector i)
            (funcall f (aref vector i)))))
  (values))

(defun acquire-lock (queue)
  (loop
    (unless (sb-ext:compare-and-swap (%locked queue) nil t)
      (return t))))

(defun release-lock (lock)
  (setf (%locked lock) nil))

(defmacro with-queue-lock ((queue) &body body)
  "Run @c(body) in a critical section guarded by a spinlock associated
with the queue."
  (let ((sym (gensym)))
    `(let ((,sym ,queue))
       (acquire-lock ,sym)
       (unwind-protect
            (progn ,@body)
         (release-lock ,sym)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Conditions

(define-condition queue-size-limit-reached (error)
  ((%queue :reader queue-size-limit-reached-queue :initarg :queue)
   (%object :reader queue-size-limit-reached-object :initarg :element))
  (:default-initargs :queue (a:required-argument :queue)
                     :element (a:required-argument :element))
  (:report
   (lambda (condition stream)
     (let ((queue (queue-size-limit-reached-queue condition))
           (element (queue-size-limit-reached-object condition)))
       (format stream "Size limit (~D) reached for non-extensible ~
                    queue ~S while trying to enqueue element ~S onto it."
               (length (%data-vector queue)) queue element)))))
