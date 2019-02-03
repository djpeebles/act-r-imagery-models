;;;; =========================================================
;;;;
;;;;
;;;; =========================================================

;Dist RT   Pixels
; 2.0 1.08 76
; 3.4 1.06 129
; 4.0 1.16 151
; 4.6 1.09 174
; 5.0 1.18 189
; 6.5 1.28 246
; 7.0 1.32 265
; 7.5 1.33 283
; 8.0 1.39 302
; 9.0 1.46 340
; 9.5 1.45 359
;10.0 1.50 378
;10.5 1.40 397
;11.7 1.47 442
;12.7 1.52 480
;14.4 1.61 544
;14.9 1.66 563
;16.0 1.62 605
;16.5 1.81 624
;18.3 1.69 692
;19.0 1.88 718

;; Directory containing model
(defparameter *dir* (translate-logical-pathname "ACT-R:models;imagery-models;"))

(defparameter *human-data* '(1.08 1.06 1.16 1.09 1.18 1.28 1.32
                               1.33 1.39 1.46 1.45 1.50 1.40 1.47
                               1.52 1.61 1.66 1.62 1.81 1.69 1.88))

;; For the output file

(defparameter *distances* '(2.0 3.4 4.0 4.6 5.0 6.5 7.0
                            7.5 8.0 9.0 9.5 10.0 10.5 11.7
                            12.7 14.4 14.9 16.0 16.5 18.3 19.0))

;; load file containing statistics functions
(actr-load "ACT-R:models;imagery-models;package.fasl")
(actr-load "ACT-R:models;imagery-models;lhstats.fasl")
(actr-load "ACT-R:models;imagery-models;shared-code.lisp")
(actr-load "ACT-R:models;imagery-models;imagery-functions.lisp")

;; ==========================================================================
;; Remember - as the value of *step-size* increases, the fewer steps will be
;; necessary across all lengths.  As *length-proportion* increases, the fewer
;; steps will be necessary, with fewer steps related to greater lengths.
;; ==========================================================================
;; best values so far:
;; *step-size* 21
;; *length-proportion* 9
;; *proximity-threshold* 8.0
;; *imaginal-delay-time* 0.2
;; *perceptual-noise* 20
;; ==========================================================================

(defparameter *increment-x* nil "Number of x-axis pixels moved each step.")
(defparameter *increment-y* nil "Number of y-axis pixels moved each step.")
(defparameter *proximity-threshold* 10.0 "How close you have to be to the target to stop.")
(defparameter *noise-scale* 3.0)
(defparameter *step-size-scale* 18.0)
(defvar *imaginal-delay-time* 0.1)


(defparameter *starting-point* '(isa (polygon-loc polygon-obj)
                                 shape square
                                 points (((1 99)))
                                 screen-x (1 nil)
                                 screen-y (100 nil)))

(defparameter *current-target* nil)

(defparameter *target-points* '((a . ((76 100))) (b . ((129 100))) (c . ((151 100)))
                                (d . ((174 100))) (e . ((189 100))) (f . ((246 100)))
                                (g . ((265 100))) (h . ((283 100))) (i . ((302 100)))
                                (j . ((340 100))) (k . ((359 100))) (l . ((378 100)))
                                (m . ((397 100))) (n . ((442 100))) (o . ((480 100)))
                                (p . ((544 100))) (q . ((563 100))) (r . ((605 100)))
                                (s . ((624 100))) (t . ((692 100))) (u . ((718 100)))))

(defparameter *start* (first (first (sixth *starting-point*))))

;; ===========================================================
;; ===========================================================

(defun compare-points (current-image target-image)
  "Compare objects (two lists of x-y coordinate point lists)"
  (let ((distances nil))
    (setf distances (compute-distance (first current-image) target-image))
    distances))

(defun less-than-threshold (val)
  (<= val *proximity-threshold*))

(defun near-enough? (distance)
  (cond ((less-than-threshold distance) 'stop)
        (t 'translate)))

;; ===========================================================
;; ===========================================================

(defun translate (orig-pts)
  (let ((tmatrix `((1 0 ,*increment-x*)
                   (0 1 ,*increment-y*)
                   (0 0 1)))
        (tm nil)
        (result-list nil)
        (final-result-list nil))
    (loop for coords in (add-ones orig-pts)
          do (setf tm tmatrix)
             (push (multiply-coords-and-matrix coords tm) result-list))
    (setf final-result-list (reverse (remove-final-zeros result-list)))
    final-result-list))

;; ===========================================================
;; ===========================================================

;; This function should take the location points of the starting image and the
;; target image and compute the difference between the x and y values for each
;; point (which should be the same for each x and each y point if the shape is
;; the same and has just been translated).  From these x and y differences a
;; suitable increment value should be computed for each (i.e., by dividing both
;; by 10) to give a value similar to that used in the rotation function.  These
;; (signed) x-increment and y-increment values will then be used in the
;; subsequent translation operations.

(defun subtract-lists (lst1 lst2)
  (let ((frst (- (first lst1) (first lst2)))
        (scnd (- (second lst1) (second lst2))))
    (list frst scnd)))

;; ===========================================================
;; ===========================================================

(defun num-steps (x y)
  (let* ((true-distance (sqrt (+ (* x x) (* y y))))
         (noise (* *noise-scale* (act-r-noise (log true-distance))))
         (noisy-distance (+ true-distance noise))
         (step-size (* *step-size-scale* (log noisy-distance)))
         (nsteps (round (/ noisy-distance step-size))))
    (cond ((= nsteps 0) (setf nsteps 1)))
    (format t "True: ~,1f Noise: ~A Noisy Distance: ~,1f Step size: ~,1f Nsteps: ~A~%" true-distance noise noisy-distance step-size nsteps)
    nsteps))

;; ===========================================================
;; ===========================================================

(defun calculate-step-size (start-point target-point)
  (let* ((diffs (subtract-lists target-point start-point))
         (xdiff (first diffs))
         (ydiff (second diffs))
         (nsteps (num-steps xdiff ydiff)))
    (setf *increment-x* (/ xdiff nsteps))
    (setf *increment-y* (/ ydiff nsteps))))
;    (format t "xdif ~A ydif ~A xincr ~,2f yincr ~A nsteps ~A " xdiff ydiff *increment-x* *increment-y* nsteps)))

;; ===========================================================
;; ===========================================================

(defun transform-image (slot-to-change)
  (let ((delay (randomize-time *imaginal-delay-time*)))
    (schedule-mod-buffer-chunk 'imaginal
                               (list slot-to-change
                                     (translate
                                      (chunk-slot-value-fct
                                       (buffer-read 'imaginal)
                                       slot-to-change)))
                               delay)
    (schedule-event-relative delay 'set-imaginal-free)))

;; ===========================================================
;; ===========================================================

(defun runsim (nsubjects)
  (let ((final-averages nil))
    (add-act-r-command "record-time" 'record-time "Save the current mp-time.")
    (monitor-act-r-command "output-key" "record-time")
    (start-hand-at-key 'right "j")
    (setf *all-model-data* nil)
    (dotimes (subj nsubjects)
      (format t "Subject: ~A~%" subj)
      (setf *model-data* nil)
      (reset)
      (loop for tgt in '(a b c d e f g h i j k l m n o p q r s t u)
            do (setf *current-target* (first (rest (assoc tgt *target-points*))))
               (reset)
               (install-device '("motor" "keyboard"))
               (delete-all-visicon-features)
               (add-visicon-features *starting-point*)
               (new-word-sound (string tgt))
               (run 200))
      (setf *model-data* (reverse *model-data*))
      (push *model-data* *all-model-data*))
    (remove-act-r-command-monitor "output-key" "record-time")
    (remove-act-r-command "record-time")
    (format t "Human mean RTs:~A~%" *human-data*)
    (format t "Model RTs:~A~%" *all-model-data*)
    (setf final-averages (average *all-model-data*))
    (format t "Model RTs averaged over ~A subjects:~%~A~%" nsubjects final-averages)
    (format t "95% Confidence Intervals:~%~A~%" (map 'list #'caclulate-ci (rotate *all-model-data*)))
    (correlation *human-data* final-averages)
    (mean-deviation *human-data* final-averages)
;    (format t "Rotated:~%~A~%" (rotate *all-model-data*))
;    (format t "Means:~%~A~%" (map 'list #'stats:mean (rotate *all-model-data*)))
    (output-data-to-file "scanning-output.csv")))

;    (sb-ext:run-program
;     "/usr/bin/Rscript"
;     (list "/home/david/Dropbox/actr/actr7-12-10/models/imagery-models/scanning-plots.R") :input t)))

;; ===========================================================
;; ===========================================================

(define-model mental-translation

  (sgp :v nil
       :trace-detail medium
       :esc t
       :lf .75
       :er t
       :randomize-time t
       :motor-feature-prep-time 0.01
       ;; :motor-initiation-time 0.0
       ;; :peck-fitts-coeff 0.0
       :motor-burst-time 0.01
       ;; :min-fitts-time 0.0
       )

  (chunk-type maploc name coords)
  (chunk-type translation-goal state)
  (chunk-type polygon-features shape points)
  (chunk-type (polygon-loc (:include visual-location) (:include polygon-features)))
  (chunk-type (polygon-obj (:include visual-object) (:include polygon-features)))
  (chunk-type imagery-chunk current-image target-image)

  (define-chunks (polygon-obj isa chunk) (arrow isa chunk) (compare isa chunk)
    (translate isa chunk) (test isa chunk) (switch isa chunk) (done isa chunk)
    (g1 isa translation-goal state encode) (stop isa chunk) (encode isa chunk)
    (square isa chunk) (get-distance isa chunk))

  (add-dm
   (loc-a isa maploc name "a" coords (76 100))  (loc-b isa maploc name "b" coords (132 100))
   (loc-c isa maploc name "c" coords (151 100)) (loc-d isa maploc name "d" coords (181 100))
   (loc-e isa maploc name "e" coords (189 100)) (loc-f isa maploc name "f" coords (253 100))
   (loc-g isa maploc name "g" coords (268 100)) (loc-h isa maploc name "h" coords (283 100))
   (loc-i isa maploc name "i" coords (302 100)) (loc-j isa maploc name "j" coords (340 100))
   (loc-k isa maploc name "k" coords (359 100)) (loc-l isa maploc name "l" coords (378 100))
   (loc-m isa maploc name "m" coords (408 100)) (loc-n isa maploc name "n" coords (442 100))
   (loc-o isa maploc name "o" coords (480 100)) (loc-p isa maploc name "p" coords (548 100))
   (loc-q isa maploc name "q" coords (567 100)) (loc-r isa maploc name "r" coords (605 100))
   (loc-s isa maploc name "s" coords (624 100)) (loc-t isa maploc name "t" coords (699 100))
   (loc-u isa maploc name "u" coords (718 100)))

  (goal-focus g1)
  (declare-buffer-usage imaginal imagery-chunk :all)

;;; ========================
;;; ========================

  (p attend-to-starting-image-and-listen-for-sound
    =goal>
     state encode
    =visual-location>
    ?visual>
     state free
    =aural-location>
    ?aural>
     state free
   ==>
    +visual>
     isa move-attention
     screen-pos =visual-location
    +aural>
     event =aural-location)

;;; ==============
;;; ==============

  (p store-starting-image-and-request-destination-location
    =goal>
     state encode
    =visual>
     points =points
    ?imaginal>
     state free
    =aural>
     isa sound
     content =landmark
   ==>
    !eval! (setf *start-time* (mp-time))
    +imaginal>
     isa imagery-chunk
     current-image =points
    +retrieval>
     isa maploc
     name =landmark)

;;; ==============
;;; ==============

  (p destination-location-retrieved-get-distance
    =goal>
     state encode
    =imaginal>
    =retrieval>
     isa maploc
     coords =coords
   ==>
    !eval! (mod-buffer-chunk 'imaginal `(target-image ,*current-target*))
    !eval! (calculate-step-size *start* *current-target*)
    =goal>
     state compare
    =imaginal>)

;;; ==============
;;; ==============

  (p destination-location-not-retrieved
    =goal>
     state encode
    =imaginal>
    ?retrieval>
     state error
    ?manual>
     state free
   ==>
    =goal>
     state done
    +manual>
     cmd press-key
     key "n")

;;; ==============
;;; ==============

  (p target-reached-so-stop
    =goal>
     state compare
    ?imaginal>
     state free
    =imaginal>
     current-image =crrnt-pts
     target-image =targ-pts
    ?manual>
     state free
    !eval! (<= (compare-points =crrnt-pts =targ-pts) *proximity-threshold*)
    ==>
    =goal>
     state done
    +manual>
     cmd press-key
     key "j")

;;; ==============
;;; ==============

  (p target-not-reached-so-move
    =goal>
     state compare
    ?imaginal>
     state free
    =imaginal>
     current-image =crrnt-pts
     target-image =targ-pts
    ?imaginal-action>
     state free
     !eval! (> (compare-points =crrnt-pts =targ-pts) *proximity-threshold*)
   ==>
    +imaginal-action>
     action transform-image
     slots (current-image)
    =imaginal>)

  )
