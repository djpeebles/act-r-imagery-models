;;;; =========================================================
;;;;
;;;;
;;;; =========================================================
;; load file containing statistics functions

(defparameter *distances* '(0 20 40 60 80 100 120 140 160 180))
(defparameter *human-data* '(1.050 1.300 1.450 1.600 1.750 2.000 1.900 2.150 2.350 2.700))

(actr-load "ACT-R:models;imagery-models;package.fasl")
(actr-load "ACT-R:models;imagery-models;lhstats.fasl")
(actr-load "ACT-R:models;imagery-models;shared-code.lisp")

(defparameter *rotation-increment* 18)
(defparameter *initial-rotation* 0)


;;; =====================================================================
;;; x and y coordinate points, height and width, for the sub-features and
;;; compound shape.
;;; =====================================================================

(defparameter *target-image-dimensions* '((100 100 50 50) (151 100 50 50)
                                          (100 151 50 50) (151 151 50 50)
                                          (100 100 100 100)))

(defparameter *rotated-image-dimensions* '((400 400 50 50) (451 400 50 50)
                                           (400 451 50 50) (451 451 50 50)
                                           (400 400 100 100)))

(actr-load "ACT-R:models;imagery-models;imagery-functions.lisp")
(actr-load "ACT-R:models;imagery-models;rotation-stimuli.lisp")

(defparameter *proximity-threshold* 10.0 "How close does the angle have to be to stop.")
(defvar *imaginal-delay-time* 0.1)
(defparameter *perceptual-noise* 2)

;;; ==========================================================
;;;
;;; ==========================================================

(defun transform-image (slot-to-change)
  (let ((delay (randomize-time *imaginal-delay-time*)))
    (schedule-mod-buffer-chunk 'imaginal
                               (list slot-to-change
                                     (rotate-counter-clockwise-around-point
                                      (chunk-slot-value-fct (buffer-read 'imaginal) slot-to-change)
                                      (+ *rotation-increment*
                                         (act-r-noise *perceptual-noise*))))
                               delay)
    (schedule-event-relative delay 'set-imaginal-free)))

;;; ==========================================================
;;; ==========================================================

(defun run-model (initial-rotation)
  (let* ((target-patterns (create-stimulus-patterns))
         (target-image (create-target-image *target-image-dimensions* target-patterns))
         (rotated-image (create-rotated-image target-image 'clockwise initial-rotation)))
    (delete-all-visicon-features)
    (mapcar #'add-visicon-features target-image)
    (mapcar #'add-visicon-features rotated-image)
    (setf *start-time* (mp-time))
    (run 10)))

;;; ==========================================================
;;; ==========================================================

(defun runsim (nsubjects)
  (let ((final-averages nil))
    (add-act-r-command "record-time" 'record-time "Save the current mp-time.")
    (monitor-act-r-command "output-key" "record-time")
    (dotimes (subj nsubjects)
      (format t "Subject: ~A~%" subj)
      (setf *model-data* nil)
      (loop for angle in *distances*
            do (reset)
               (install-device '("motor" "keyboard"))
               (run-model angle))
      (setf *model-data* (reverse *model-data*))
      (push *model-data* *all-model-data*))
    (remove-act-r-command-monitor "output-key" "record-time")
    (remove-act-r-command "record-time")
    (format t "Human mean RTs:~A~%" *human-data*)
    (format t "Model RTs: ~A~%" *all-model-data*)
    (setf final-averages (average *all-model-data*))
    (format t "Model RTs averaged over ~A subjects:~%~A~%" nsubjects final-averages)
    (format t "95% Confidence Intervals:~%~A~%" (map 'list #'caclulate-ci (rotate *all-model-data*)))
    (correlation *human-data* final-averages)
    (mean-deviation *human-data* final-averages)
    (output-data-to-file "rotation-output.csv")))

;    (sb-ext:run-program
;     "/usr/bin/Rscript"
;     (list "/home/david/Dropbox/actr/actr7.x/models/imagery-models/rotation-plots.R") :input t)))

;;; ==========================================================
;;; ==========================================================

(define-model holistic-rotation

  (sgp :v nil
       :trace-detail medium
       :esc t
       :er t
       :randomize-time t)

  (chunk-type rotation-goal state xloc)
  (chunk-type (polygon-feature (:include visual-location)) points centre-x centre-y regular)
  (chunk-type (polygon (:include visual-object) (:include polygon-feature)) sides (polygon t) regular points centre-x centre-y)
  (chunk-type (square (:include polygon)) (sides 4) (square t))
  (chunk-type (compound-square (:include square)) sub1 sub2 sub3 sub4)
  (chunk-type imagery-chunk current-image)

  (define-chunks (true isa chunk) (false isa chunk) (square isa chunk) (rotate isa chunk)
    (polygon isa chunk) (compound isa chunk) (start isa chunk) (stop isa chunk) (compare isa chunk)
    (g1 isa rotation-goal state start) (encode isa chunk) (process-pattern isa chunk))

  (goal-focus g1)
  (declare-buffer-usage imaginal imagery-chunk :all)

;;; ==================================================================
;;; IF   you're starting a trial,
;;; THEN look for a rotated compound image on the right of the screen.
;;; ==================================================================

  (p start-trial
    =goal>
     state start
   ==>
    +visual-location>
     isa visual-location
     > screen-x 300
     color compound
    =goal>
     state encode)

;;; ============================================
;;; If   there's a compound image on the screen,
;;; THEN attend to it.
;;; ============================================

  (p attend-to-stimulus-pattern
    =goal>
     state encode
    =visual-location>
     color compound
     screen-x =screenx
    ?visual>
     state free
   ==>
    +visual>
     isa move-attention
     screen-pos =visual-location
    =goal>
     state process-pattern
     xloc =screenx)

;;; ========================================================================
;;; If   you're looking at the rotated image at the right of the screen,
;;; THEN store the points representing the global pattern in working memory,
;;; AND  look for the target compound image at the left of the screen.
;;; ========================================================================

  (p store-rotated-pattern-and-look-for-target-pattern
    =goal>
     state process-pattern
     xloc =screenx
    =visual>
     points =points
    ?imaginal>
     state free
    !eval! (> =screenx 300)
   ==>
    +imaginal>
     isa imagery-chunk
     current-image =points
    +visual-location>
     isa visual-location
     < screen-x 300
     color compound
     =goal>
     state encode)

;;; ====================================================================
;;; IF   you're looking at the target pattern at the left of the screen,
;;; AND  you have the pattern of the rotated image in working memory,
;;; AND  the angular displacement between them is sufficiently small
;;; THEN stop.
;;; ====================================================================

  (p target-reached-so-stop
    =goal>
     state process-pattern
     xloc =screenx
    =visual>
     points =target
    =imaginal>
     current-image =current
    !eval! (< =screenx 300)
    !bind! =angle-disp (angular-disparity =current =target)
    !eval! (<= =angle-disp *proximity-threshold*)
    ?manual>
     state free
   ==>
    =goal>
     state stop
    +manual>
     cmd press-key
     key "m")

;;; ====================================================================
;;; IF   you're looking at the target pattern at the left of the screen,
;;; AND  you have the pattern of the rotated image in working memory,
;;; AND  the angular displacement between them is too large
;;; THEN rotate the pattern closer to the target.
;;; ====================================================================

  (p target-not-reached-so-move
    =goal>
     state process-pattern
     xloc =screenx
    =visual>
     points =target
    =imaginal>
     current-image =current
    !eval! (< =screenx 300)
    !bind! =angle-disp (angular-disparity =current =target)
    !eval! (> =angle-disp *proximity-threshold*)
    ?imaginal-action>
     state free
   ==>
    =visual>
    =imaginal>
    +imaginal-action>
     action transform-image
     slots (current-image))

  )
