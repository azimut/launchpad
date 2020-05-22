;;;; launchpad.lisp

(in-package #:launchpad)

(defmethod command :around (raw-midi)
  (cl-rtmidi:with-midi-oss-out (cl-rtmidi:*default-midi-out-stream* "/dev/midi1")
    (call-next-method)))

(defmethod command (raw-midi)
  (cl-rtmidi:write-midi-message
   (make-instance 'cl-rtmidi:midi-message :raw-midi raw-midi)))

;;(command '(176 0 0))
;;(command (list 144 (+ (random 8) (* (random 8) 16)) #b0110010))

(defparameter *five*
  #2A((_ _ _ _ X _ _ _)
      (_ _ X X X _ _ _)
      (_ _ _ _ X _ _ _)
      (_ _ _ _ X _ _ _)
      (_ _ _ _ X _ _ _)
      (_ _ _ _ X _ _ _)
      (_ _ _ _ X _ _ _)
      (_ _ _ X X X _ _)))

(defun test-draw ()
  "OUT send a blink color for 2A pattern"
  (cl-rtmidi:with-midi-oss-out (cl-rtmidi:*default-midi-out-stream* "/dev/midi1")
    (cl-rtmidi:write-midi-message
     (make-instance 'cl-rtmidi:midi-message :raw-midi (list 176 0 0)))
    (dotimes (row 8)
      (dotimes (col 8)
        (when (eql (aref *five* row col) 'X)
          (cl-rtmidi:write-midi-message
           (make-instance 'cl-rtmidi:midi-message
                          :raw-midi (list 144 (+ row (* col 16)) #b0110010))))))))

(defun test-on-cc ()
  "OUT send a blink color"
  (cl-rtmidi:with-midi-oss-out (cl-rtmidi:*default-midi-out-stream* "/dev/midi1")
    (cl-rtmidi:write-midi-message
     (make-instance 'cl-rtmidi:midi-message :raw-midi (list 176 0 0)))
    (cl-rtmidi:write-midi-message
     (make-instance 'cl-rtmidi:midi-message :raw-midi (list 176 (alexandria:random-elt
                                                                 (alexandria:iota 8 :start 104))
                                                            #b0110010)))))

(defun test ()
  "IN debug print what is pressed"
  (cl-rtmidi:with-midi-oss-in (cl-rtmidi:*default-midi-in-stream* "/dev/midi1")
    (loop (print (slot-value (cl-rtmidi:read-midi-message)
                             'cl-rtmidi::raw-midi))
          (force-output))))
