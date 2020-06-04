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

(defvar *db-mask*      #b0100000)
(defvar *db-copy*      #b0010000)
(defvar *db-flash*     #b0001000)
(defvar *db-update-1*  #b0000100)
(defvar *db-display-1* #b0000001)

(defun raw-command (a b c)
  (cl-rtmidi:write-midi-message
   (make-instance 'cl-rtmidi:midi-message :raw-midi (list a b c))))

(defun command (a b c)
  (cl-rtmidi:with-midi-oss-out
      (cl-rtmidi:*default-midi-out-stream* "/dev/midi1")
    (raw-command a b c)))

(defun key (col row) (+ col (* row 16)))
(defun xy  (key)     (floor key 16))

(defun button-xy-on (col row color)
  (declare (type (integer 0 7) col row))
  (raw-command #x90 (key col row) color))
(defun button-xy-off (col row)
  (declare (type (integer 0 7) col row))
  (raw-command #x80 (key col row) 0))
(defun button-drum-on (note color)
  (declare (type (integer 36 99) note))
  (raw-command #x90 note color))
(defun button-drum-off (note)
  (declare (type (integer 36 99) note))
  (raw-command #x80 note 0))

(defun button-scene-xy-on (button color)
  (declare (type (integer 0 7) button))
  (let ((n (aref #(1 3 5 7 9 11 13 15) button)))
    (raw-command #x90 (* 8 n) color)))
(defun button-scene-xy-off (button)
  (declare (type (integer 0 7) button))
  (let ((n (aref #(1 3 5 7 9 11 13 15) button)))
    (raw-command #x80 (* 8 n) 0)))
(defun button-scene-drum-on (button color)
  (declare (type (integer 0 7) button))
  (raw-command #x90 (+ 100 button) color))
(defun button-scene-drum-off (button)
  (declare (type (integer 0 7) button))
  (raw-command #x80 (+ 100 button) 0))

(defun button-automap-on (button color)
  (declare (type (integer 0 7) button))
  (command #xb0 (+ 104 button) color))
(defun button-automap-off (button)
  (declare (type (integer 0 7) button))
  (command #xb0 (+ 104 button) 0))

(defun flash   () (command #xb0 0 (logior *db-mask* *db-flash*)))
(defun all-low () (command #xb0 0 125))
(defun all-med () (command #xb0 0 126))
(defun all-hig () (command #xb0 0 127))
(defun reset   () (command #xb0 0   0))

;; DRUM: 36-99 keynum
(defun change-layout (xy-or-drum)
  (ecase xy-or-drum
    (:xy   (command #xb0 0 1))
    (:drum (command #xb0 0 2))))

(defun grid-keys-on-drum-layout ()
  (a:iota (* 8 8) :start 36))

(defun grid-keys-on-xy-layout ()
  (loop :for col :below 8 :append
        (loop :for row :below 8
              :collect (key row col))))

(defun get-keys (name)
  (ecase name
    (:xy        #.'(grid-keys-on-xy-layout))
    (:drum      #.'(grid-keys-on-drum-layout))
    (:side-xy   #.'(loop :repeat 8 :for k :from 1 :by 2 :collect (* 8 k)))
    (:side-drum #.'(a:iota 8 :start 100))))

;;--------------------------------------------------

(a:define-constant +colors+
    '(:OFF :LR :MR :HR
      :LG  :LO :MO :HO
      :MG  :LO :MO :HO
      :HG  :LO :MO :HO)
  :test #'equal)

(a:define-constant +green+ '(#b0000000 #b0010000 #b0100000 #b0110000) :test #'equal)
(a:define-constant +red+   '(#b0000000 #b0000001 #b0000010 #b0000011) :test #'equal)
(a:define-constant +mixed+  (a:map-product #'logior +green+ +red+)    :test #'equal)
(a:define-constant +prop+   (interleave +colors+ +mixed+)             :test #'equal)

(defun color (code) (getf +prop+ code))
(defun colors ()
  (->> +colors+
       (remove-duplicates)
       (remove :off)))
