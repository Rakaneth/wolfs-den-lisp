(in-package :wolfs-den-lisp)

(defun clamp (val low high)
  (cond
    ((< val low) low)
    ((> val high) high)
    (t val)))

(defun between-p (val low high)
  (eql (clamp val low high) val))

(defun decorate (text color-string)
  (format nil "[color=~A]~A[/color]" color-string text))

(defun compare-by (lst &key (key #'identity) test)
  (reduce #'(lambda (fst snd)
              (cond
                ((and snd (not fst)) snd)
                ((and fst (not snd)) fst)
                ((and (not fst) (not snd)) fst)
                (t (if (funcall test (funcall key fst) (funcall key snd))
                       fst
                       snd))))
          lst))

(defun max-by (lst &key key)
  (compare-by lst :key (or key #'identity) :test #'>))

(defun min-by (lst &key key)
  (compare-by lst :key (or key #'identity) :test #'<))
