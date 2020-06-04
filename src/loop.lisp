(in-package #:launchpad)

(defclass launchpad ()
  ())

(defmethod disconnect ((server launchpad))
  (stop-io-thread))

(defmethod connect ((server launchpad))
  (start-io-thread))

(defmethod reconnect ((server launchpad))
  (reset))

;;--------------------------------------------------

(defgeneric handle-input (server raw-midi)
  (:method (server raw-midi)))

(defmethod main-loop ((server launchpad))
  "IN debug print what is pressed"
  (cl-rtmidi::with-midi-oss-io ("/dev/midi1")
    (loop (handle-input server (slot-value (cl-rtmidi:read-midi-message)
                                           'cl-rtmidi::raw-midi)))))

(defun get-io-thread ()
  (find "launchpad-io" (bt:all-threads)
        :key #'bt:thread-name
        :test #'string=))

(defun alive-p ()
  (alexandria:when-let ((thread (get-io-thread)))
    (bt:thread-alive-p thread)))

(defun start-io-thread ()
  (when (not (alive-p))
    (bt:make-thread (lambda () (main-loop (make-instance 'launchpad)))
                    :name "launchpad-io")))

(defun stop-io-thread ()
  (when (alive-p)
    (bt:destroy-thread (get-io-thread))))
