;;;; launchpad.lisp

(in-package #:cl-rtmidi)

(defmacro with-midi-oss-io ((device-filename) &body body)
  (let ((byte-stream (gensym "byte-stream")))
    `(with-open-file (,byte-stream ,device-filename
				   :direction :io
				   :if-exists :overwrite
				   :element-type '(unsigned-byte 8))
       (let* ((iostream (make-instance 'midi-oss-stream
                                       :byte-stream ,byte-stream))
              (cl-rtmidi:*default-midi-in-stream* iostream)
              (cl-rtmidi:*default-midi-out-stream* iostream))
	 ,@body))))

(in-package #:launchpad)

(defun raw-command (raw-midi)
  (cl-rtmidi:write-midi-message
   (make-instance 'cl-rtmidi:midi-message :raw-midi raw-midi)))

(defun command (raw-midi)
  (cl-rtmidi:with-midi-oss-out
      (cl-rtmidi:*default-midi-out-stream* "/dev/midi1")
    (raw-command raw-midi)))

(defun xy-to-key (x y) (+ x (* y 16)))
(defun key-to-xy (key) (floor key 16))

(defun button-xy (x y)
  (declare (type (integer 0 7) x y))
  (command (list 144 (xy-to-key x y) #b0110011)))

(defun button-xy-off (x y)
  (declare (type (integer 0 7) x y))
  (command (list 128 (xy-to-key x y) 0)))

;; TODO: support drum-rack mode
(defun button-scene (button)
  (declare (type (integer 0 7) button))
  (let ((n (aref #(1 3 5 7 9 11 13 15) button)))
    (command (list 144 (* 8 n) #b0110001))))

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

;;--------------------------------------------------

(defun interleave (list &rest lists)
  "Return a list whose elements are taken from LIST and each of LISTS like this:
   1st of list, 1st of 1st of lists,..., 1st of last of lists, 2nd of list,..."
  (apply #'mapcan (lambda (&rest els)
                    els)
         list lists))

(a:define-constant +colors+
    '(:OFF :LR :MR :HR
      :LG  :LO :MO :HO
      :MG  :LO :MO :HO
      :HG  :LO :MO :HO)
  :test #'equal)
(a:define-constant +green+ '(#b0000000 #b0010000 #b0100000 #b0110000) :test #'equal)
(a:define-constant +red+   '(#b0000000 #b0000001 #b0000010 #b0000011) :test #'equal)
(a:define-constant +codes+  (a:map-product #'logior +green+ +red+)    :test #'equal)
(a:define-constant +prop+   (interleave +colors+ +codes+)             :test #'equal)

(defun color (code) (getf +prop+ code))

#+nil
(progn
  (command (list 144 (+ 3 (* 3 16)) (color :LO)))
  (command (list 144 (+ 3 (* 3 16)) (alexandria:random-elt +codes+))))

;;--------------------------------------------------

(defparameter *db-mask*      #b0100000)
(defparameter *db-copy*      #b0010000)
(defparameter *db-flash*     #b0001000)
(defparameter *db-update-1*  #b0000100)
(defparameter *db-display-1* #b0000001)

#+nil
(progn
  (reset)
  (layout-xy)
  (button-xy      (random 8) (random 8))
  (button-automap (random 8))
  (button-scene   (random 8))
  (command (list 176 0 (logior *db-mask* *db-flash*))))

;;--------------------------------------------------

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

(defun test-input ()
  "IN debug print what is pressed"
  (cl-rtmidi:with-midi-oss-in
      (cl-rtmidi:*default-midi-in-stream* "/dev/midi1")
    (loop (print (slot-value (cl-rtmidi:read-midi-message)
                             'cl-rtmidi::raw-midi))
          (force-output))))

;;--------------------------------------------------

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
      (multiple-value-bind (x y) (key-to-xy mkey)
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
