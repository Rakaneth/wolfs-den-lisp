(in-package #:wolfs-den-lisp)

(defvar *creature-templates* (make-hash-table))
(defvar *map-templates* (make-hash-table))
(defvar *item-templates* (make-hash-table))
(defvar *ego-templates* (make-hash-table))

(defmacro define-template (repo-sym item-key plist-template)
  (let ((repo (ecase repo-sym 
                (:creature '*creature-templates*)
                (:map '*map-templates*)
                (:item '*item-templates*)
                (:ego '*ego-templates*))))
    `(setf (gethash ,item-key ,repo) ,plist-template)))

(defmacro define-map (item-key plist-template)
  `(define-template :map ,item-key ,plist-template))

(defmacro define-creature (item-key plist-template)
  `(define-template :creature ,item-key ,plist-template))

(defmacro define-item (item-key plist-template)
  `(define-template :item ,item-key ,plist-template))

(defmacro define-ego (item-key plist-template)
  `(define-template :ego ,item-key ,plist-template))

(defmacro with-repo (repo-key repo-sym &body body)
  `(let* ((,repo-sym (ecase ,repo-key
                       (:map *map-templates*)
                       (:creature *creature-templates*)
                       (:item *item-templates*)
                       (:ego *ego-templates*))))
     ,@body))

(defmacro with-template (repo-sym template-id template-sym &body body)
  (let ((repo (ecase repo-sym
                (:map '*map-templates*)
                (:creature '*creature-templates*)
                (:item '*item-templates*)
                (:ego '*ego-templates*))))
    `(let ((,template-sym (gethash ,template-id ,repo)))
       (unless ,template-sym
         (error (format nil "No template ~a in ~a templates" ,template-id ,repo-sym)))
       ,@body)))



(defun create-map (template-id)
  (with-template :map template-id template
    (let ((base-map (make-instance 'game-map
                                   :id (getf template :id)
                                   :name (getf template :name)
                                   :width (getf template :width)
                                   :height (getf template :height)))
          (map-type (or (getf template :type) :cell)))
      (debug-print "MAP" "Creating map ~a" template-id)
      (case map-type
        (:cell 
         (random-walls base-map)
         (dotimes (_ 6) (iterate-dungeon base-map))
         (wall-border base-map)
         (find-regions base-map)
         (fill-caves base-map)
         (connect-regions base-map))
        (t (error (format nil "~A algo not implemented yet." map-type)))))))



(defun apply-ego! (entity ego-key)
  (with-template :ego ego-key template
    (with-accessors ((name entity/name)
                     (tags entity/tags)
                     (color entity/color)
                     (stats entity/stats)) 
        entity
      (let* ((-name (getf template :name))
             (-color (getf template :color))
             (-suffix (getf template :suffix))
             (-hardness (getf template :hardness))
             (-staff (getf template :staff))
             (-hammer (getf template :hammer))
             (-sword (getf template :sword))
             (-axe (getf template :axe))
             (-armor (getf template :armor))
             (-light (getf template :light))
             (-lite (getf template :lite))
             (-stats (getf template :stats))
             (-tags (getf template :tags)))
        (add-tag entity ego-key)
        (when -name
          (if -suffix
              (setf name (format nil "~a of ~a" name -name))
              (setf name (format nil "~a ~a" -name name))))
        (when -color
          (setf color -color))
        (when -hardness
          (set-stat! entity :hardness -hardness))
        (when (and -staff (has-tag entity :staff))
          (mod-stats! entity -staff))
        (when (and -axe (has-tag entity :axe))
          (mod-stats! entity -axe))
        (when (and -sword (has-tag entity :sword))
          (mod-stats! entity -sword))
        (when (and -hammer (has-tag entity :hammer))
          (mod-stats! entity -hammer))
        (when (and -armor (has-tag entity :armor))
          (mod-stats! entity -armor))
        (when (and -light (has-tag entity :light))
          (mod-stats! entity -light))
        (when (and -lite (has-tag entity :lite))
          (mod-stats! entity -lite))
        (when -tags
          (dolist (tag -tags) (add-tag entity tag)))
        (when -stats
          (mod-stats! entity -stats))
        entity))))

(defun create-creature (template-id &key (pos '(0 . 0)) player name)
  (with-template :creature template-id template
    (let* ((foetus (make-instance 'entity
                                  :id (getf template :id)
                                  :name (or name (getf template :name))
                                  :color (or (getf template :color) "white")
                                  :char (or (getf template :glyph) #\@)
                                  :x (car pos)
                                  :y (cdr pos)
                                  :player player
                                  :e-type :creature
                                  :tags (getf template :tags)
                                  :layer (if player 4 3)))
           (t-stat (getf template :stats)))
      (loop :for -stat in '(:str :stam :spd :skl :sag :smt)
            :for t-stat-v = (getf t-stat -stat)
            :do (set-stat! foetus -stat (or t-stat-v 10)))
      (loop :for -sec in '(:atp :dfp :res :tou :wil :pwr :vis :dmg :hardness)
            :for s-stat-v = (getf t-stat -sec)
            :do (set-stat! foetus -sec (or s-stat-v 0))
            :finally (if (zerop (get-stat foetus :vis))
                         (set-stat! foetus :vis 6))
            :finally (return foetus)))))

;;; Untiered entities appear in all tier-based searches
(defun search-repo (repo-sym &key tier require-all require-any (tier-test #'=))
  (with-repo repo-sym map-repo
    (loop :for k being each hash-key of map-repo :using (hash-value v)
          :for v-tier = (getf v :tier)
          :for v-tags = (getf v :tags)
          :when (and (or (not tier)
                         (not v-tier)
                         (funcall tier-test (or v-tier 6) tier))
                     (subsetp require-all v-tags)
                     (or (not require-any) 
                         (intersection require-any v-tags)))
            :collect k 
            :and :collect v
          :end)))

(defun prob-table (repo-sym &key tier require-all require-any (tier-test #'=))
  (let ((results (search-repo repo-sym 
                              :tier tier
                              :require-all require-all
                              :require-any require-any
                              :tier-test tier-test)))
    (loop :for (k v) :on results :by #'cddr
          :for weight = (getf v :freq)
          :when weight
            :collect (cons k weight)
          :end)))


(defun random-creature (&key (pos '(0 . 0)) name require-all require-any tier (tier-test #'=))
  (let* ((cands (prob-table :creature
                            :tier tier
                            :tier-test tier-test
                            :require-all require-all
                            :require-any require-any))
         (choice (get-weighted cands)))
    (create-creature choice 
                     :pos pos
                     :name name)))
