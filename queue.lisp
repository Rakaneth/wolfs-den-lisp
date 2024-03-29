(in-package #:wolfs-den-lisp)

(defclass queue ()
  ((vec :initform (make-array 10 :adjustable t :fill-pointer 0)
        :accessor queue/vec)))

(defclass pri-queue (queue)
  ((pred :initform #'< :accessor pri-queue/pred :initarg :pred)))

(defclass pri-node ()
  ((weight :initform 0 :accessor pri-node/weight :initarg :weight)
   (data :accessor pri-node/data :initarg :data)))

(defmethod print-object ((n pri-node) stream)
  (print-unreadable-object (n stream)
    (format stream "Key ~d Data ~A" (pri-node/weight n) (pri-node/data n))))

(defun create-queue (&rest items)
  (let ((q (make-instance 'queue)))
    (dolist (item items q)
      (enqueue item q))))

(defun create-pri-queue (&key initial-items pred)
  (let ((q (make-instance 'pri-queue :pred (or pred #'<))))
     (loop :for (key . item) in initial-items
           ;;:do (format t "(~a, ~a)~%" key item)
           :do (enqueue (make-instance 'pri-node :weight key :data item) q)
           :finally (return q))))

(defgeneric enqueue (item q))

(defgeneric dequeue (q))

(defgeneric peek (q))

(defgeneric queue/length (q))

(defgeneric queue-empty-p (q))

(defmethod queue-empty-p ((q queue))
  (zerop (length (queue/vec q))))

(defun bheap-parent (n)
  (max (floor (1- n) 2) 0))

(defmethod swap! (index-a index-b (q queue))
  (with-accessors ((vec queue/vec)) q
    (rotatef (elt vec index-a) (elt vec index-b))
    q))

(defun bheap-left (n)
  (1+ (* 2 n)))

(defun bheap-right (n)
  (+ 2 (* 2 n)))

(defmacro get-node (idx q)
  `(elt (queue/vec ,q) ,idx))

(defmethod get-key (idx (q pri-queue))
  (and (< idx (queue/length q)) 
       (pri-node/weight (get-node idx q))))

(defmethod get-data (idx (q pri-queue))
  (and (< idx (queue/length q)) 
       (pri-node/data (get-node idx q))))

(defmethod heapify! (idx (q pri-queue))
  (loop :with pred = (pri-queue/pred q)
        :with i = idx
        :for l = (bheap-left i)
        :for r = (bheap-right i)
        :for lkey = (get-key l q)
        :for rkey = (get-key r q)
        :for ikey = (get-key i q)
        ;; :do (format t "i: ~a l: ~a r: ~a s: ~a~%" i l r smallest)
        :for smallkey = (compare-by (list lkey rkey ikey) :test pred)
        :for smallest = (cond
                          ((and lkey (= smallkey lkey)) l)
                          ((and rkey (= smallkey rkey)) r)
                          (t i))
        :if (= smallest i)
          :do (return q)
        :end
        :do (swap! smallest i q)
        :do (setf i smallest)))


(defmethod decrease-key (idx val (q pri-queue))
  (setf (pri-node/weight (get-node idx q)) val)
  (percup idx q)
  q)

(defmethod get-idx (data (q pri-queue))
  (let ((node (find-if #'(lambda (el) 
                           (equal (pri-node/data el) data))
                       (queue/vec q))))
    (if node (search (list node) (queue/vec q)))))

(defmethod check-heap (idx (q pri-queue))
  (let* ((l (bheap-left idx))
         (r (bheap-right idx))
         (lkey (get-key l q))
         (rkey (get-key r q))
         (ikey (get-key idx q))
         (pred (pri-queue/pred q)))
    (cond
      ((not (or lkey rkey)) t)
      ((not lkey) (funcall pred ikey rkey))
      ((not rkey) (funcall pred ikey lkey))
      (t (and (funcall pred ikey lkey) 
              (funcall pred ikey rkey)
              (check-heap l q)
              (check-heap r q))))))


(defmethod enqueue (item (q queue))
  (vector-push-extend item (queue/vec q)))

(defmethod percup (idx (q pri-queue))
  (loop :with pred = (pri-queue/pred q)
        :with i = idx
        :for p = (bheap-parent i)
        :for pkey = (get-key p q)
        :for ikey = (get-key i q)
        :until (or (zerop i) (funcall pred pkey ikey))
        ;; :do (format t "swapping ~a and ~a~%" (get-node i q) (get-node p q))
        :do (swap! i p q)
        :do (setf i p)))

(defmethod enqueue (item (q pri-queue))
  (vector-push-extend item (queue/vec q))
  (percup (1- (queue/length q)) q))

(defun priority-enqueue (weight item pq)
  (enqueue (make-instance 'pri-node :weight weight :data item) pq)
  pq)

(defmethod dequeue ((q queue))
  (with-accessors ((vec queue/vec)) q
    (let ((fst (elt vec 0)))
      (delete fst vec)
      fst)))

(defmethod dequeue ((q pri-queue))
  (with-accessors ((vec queue/vec)) q
    (let* ((result (elt vec 0)))
      (swap! 0 (1- (queue/length q)) q)
      (vector-pop vec)
      (unless (queue-empty-p q)  
        (heapify! 0 q))
      result)))

(defmethod peek ((q queue))
  (elt (queue/vec q) 0))

(defmethod queue/length ((q queue))
  (length (queue/vec q)))

(defmethod get-keys ((q pri-queue))
  (loop :for node :across (queue/vec q)
        :collect (pri-node/weight node)))

(defmethod print-object ((q queue) stream)
  (print-unreadable-object (q stream :type t)
    (format stream "Length: ~d ~a" (queue/length q) (queue/vec q))))


