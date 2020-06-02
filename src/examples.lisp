(in-package #:launchpad)

#+nil
(progn
  (reset)
  (change-layout :drum)
  (button-drum (random 99))
  (button-automap (random 8)))

#+nil
(progn
  (reset)
  (change-layout :xy)
  (button-on      (random 8) (random 8))
  (button-automap (random 8))
  (button-scene   (random 8)))


#+nil
(progn
  (command (list 144 (+ 3 (* 3 16)) (color :LO)))
  (command (list 144 (+ 3 (* 3 16)) (alexandria:random-elt +codes+))))

#+nil
(progn
  (reset)
  (change-layout :xy)
  (button-on      (random 8) (random 8))
  (button-automap (random 8))
  (button-scene   (random 8))
  (command (list 176 0 (logior *db-mask* *db-flash*))))

;;--------------------------------------------------
;; Basic MIDI debug
(defun test-input ()
  "IN debug print what is pressed"
  (cl-rtmidi:with-midi-oss-in
      (cl-rtmidi:*default-midi-in-stream* "/dev/midi1")
    (loop (print (slot-value (cl-rtmidi:read-midi-message)
                             'cl-rtmidi::raw-midi))
          (force-output))))

;;--------------------------------------------------
;; Button MIDI light feedback

(defun handle-loop (raw-midi)
  "led pressure"
  (destructuring-bind (mtype mkey mvel) raw-midi
    (->> (if (zerop mvel) 0 (a:random-elt +codes+))
         (list mtype mkey)
         (raw-command))))

(let ((mat8 (muniform 8 8 0)))
  (defun handle-reset ()
    (setf mat8 (muniform 8 8 0))
    (reset))
  (defun handle-loop (raw-midi)
    "led switch"
    (destructuring-bind (mtype mkey mvel) raw-midi
      (multiple-value-bind (x y) (xy mkey)
        (when (plusp mvel)
          (->> (if (zerop (mcref mat8 x y))
                   (round (setf (mcref mat8 x y) (a:random-elt +codes+)))
                   (round (setf (mcref mat8 x y) 0)))
               (list mtype mkey)
               (raw-command)))))))

(defun test-io()
  "IN debug print what is pressed"
  (cl-rtmidi::with-midi-oss-io ("/dev/midi1")
    (loop (handle-loop
           (slot-value (cl-rtmidi:read-midi-message)
                       'cl-rtmidi::raw-midi))
          (force-output))))

;;--------------------------------------------------
;; Draw Matrix

(defun list-to-mat (list)
  (->> list
       (substitute 0 '_)
       (substitute 1 'X)
       (apply #'mat)))

(defparameter *five*
  '(_ _ _ _ X _ _ _
    _ _ X X X _ _ _
    _ _ _ _ X _ _ _
    _ _ _ _ X _ _ _
    _ _ _ _ X _ _ _
    _ _ _ _ X _ _ _
    _ _ _ _ X _ _ _
    _ _ _ X X X _ _))

(defun test-draw ()
  "OUT send a blink color for 2A pattern"
  (let ((m (if (> (random 1f0) .5)
               (list-to-mat *five*)
               (->> (list-to-mat *five*)
                    (marr)
                    (reverse)
                    (map 'list #'identity)
                    (apply #'mat)))))
    (reset)
    (with-fast-matref (e m 8)
      (dotimes (row 8)
        (dotimes (col 8)
          (when (= 1 (e col row))
            (command (list 144 (+ row (* col 16)) #b0110010))))))))

;;--------------------------------------------------
;; Draw random colors

(let ((colors (list (color :lg)
                    (color :lr)
                    (color :lo)
                    (color :off)
                    (color :off))))
  (defun draw-random ()
    (sleep .1)
    (raw-command
     (list
      144
      (key
       (round
        (* 7
           (+ .5
              (* .5
                 (sin (get-internal-real-time))))))
       (round
        (* 7
           (+ .5
              (* .5
                 (cos (get-internal-real-time)))))))
      (alexandria:random-elt colors)))))

(defun test-random()
  "IN debug print what is pressed"
  (reset)
  (cl-rtmidi::with-midi-oss-out
      (cl-rtmidi:*default-midi-out-stream* "/dev/midi1")
    (loop (draw-random)
          (force-output))))


;;--------------------------------------------------
(defvar *scheduler* (make-instance 'scheduler:scheduler))

(defmacro at (time function &rest arguments)
  `(scheduler:sched-add *scheduler*
                        (+ ,time (scheduler:sched-time *scheduler*))
                        ,function ,@arguments))

(declaim (inline now))
(defun now ()
  (scheduler:sched-time *scheduler*))

(defun timed-command (raw-midi duration)
  (command raw-midi)
  (at duration
      (lambda () (command (list (first raw-midi) (second raw-midi) 0)))))

(defvar *server* (make-instance 'cloud::csound))

#+nil
(progn
  (cloud::send
   *server*
   "")
  (cloud::readscore
   *server*
   "")

  (cloud::schedule *server* 1 0 -1 0 60)

  (cloud::send *server* "instr 2
a1        oscil     p4, p5, 1      ; p4=amp
          out       a1             ; p5=freq
          endin")
  (cloud::readscore *server* "f1   0    4096 10 1")
  (cloud::schedule *server* 2 0 -1 200 440))

(defun main-loop ())
(defun main-loop (&optional (arg (alexandria:circular-list 0 1 2 3 4 5 6 7)))
  (timed-command (list 144 (key (car arg) 2) (color :lg)) 1)
  (timed-command (list 144 (key (mod (+ 1 (car arg)) 8) 3) (color :lg)) 1)
  (timed-command (list 144 (key (mod (+ -1 (car arg)) 8) 4) (color :lg)) 1)
  (timed-command (list 144 (key (mod (+ -2 (car arg)) 8) 6) (color :lg)) 1)
  (at 1 #'main-loop (cdr arg)))
