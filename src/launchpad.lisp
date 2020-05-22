;;;; launchpad.lisp

(in-package #:launchpad)

(defun command (raw-midi)
  (cl-rtmidi:with-midi-oss-out (cl-rtmidi:*default-midi-out-stream* "/dev/midi1")
    (cl-rtmidi:write-midi-message
     (make-instance 'cl-rtmidi:midi-message :raw-midi raw-midi))))

(defun button-xy (x y)
  (declare (type (integer 0 7) x y))
  (command (list 144 (+ x (* y 16))
                 #b0110010)))

;; TODO: support drum-rack mode
(defun button-scene (button)
  (declare (type (integer 0 7) button))
  (let ((n (aref #(1 3 5 7 9 11 13 15) button)))
    (command (list 144 (* 8 n) #b0110010))))

(defun button-automap (button)
  (declare (type (integer 0 7) button))
  (command (list 176 (+ 104 button) #b0110000)))

(defun all-low     () (command '(176 0 125)))
(defun all-med     () (command '(176 0 126)))
(defun all-hig     () (command '(176 0 127)))
(defun reset       () (command '(176 0   0)))
(defun layout-xy   () (command '(176 0   1)))
(defun layout-drum () (command '(176 0   2)))

#+nil
(progn
  (reset)
  (layout-drum)
  (button-automap (random 8)))

#+nil
(progn
  (reset)
  (layout-xy)
  (button-xy      (random 8) (random 8))
  (button-automap (random 8))
  (button-scene   (random 8)))

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
