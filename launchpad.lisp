;;;; launchpad.lisp

(in-package #:launchpad)

(defun list-to-mat (list)
  (->> list
       (substitute 0 '_)
       (substitute 1 'X)
       (apply #'mat)))

(defun command (raw-midi)
  (cl-rtmidi:with-midi-oss-out (cl-rtmidi:*default-midi-out-stream* "/dev/midi1")
    (cl-rtmidi:write-midi-message
     (make-instance 'cl-rtmidi:midi-message :raw-midi raw-midi))))

#+nil
(progn
  (command '(176 0 0))
  (command (list 144 (+ (random 8) (* (random 8) 16)) #b0110010))
  (command (list 176 (+ (random 8) 104)               #b0110010)))

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
    (command '(176 0 0))
    (dotimes (row 8)
      (dotimes (col 8)
        (when (= 1 (mcref m col row))
          (command (list 144 (+ row (* col 16)) #b0110010)))))))

(defun test-input ()
  "IN debug print what is pressed"
  (cl-rtmidi:with-midi-oss-in (cl-rtmidi:*default-midi-in-stream* "/dev/midi1")
    (loop (print (slot-value (cl-rtmidi:read-midi-message)
                             'cl-rtmidi::raw-midi))
          (force-output))))
