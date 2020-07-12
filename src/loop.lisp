(in-package #:launchpad)

(defclass controller ()
  ())

(defmethod initialize-instance :after ((obj controller) &key)
  (connect obj))

(defmethod disconnect ((server controller))
  (reset)
  (stop-io-thread))

(defmethod connect ((server controller))
  (start-io-thread server))

(defmethod reconnect ((server controller))
  (disconnect server)
  (connect server))

(defgeneric handle-input (server raw-midi)
  (:method (server raw-midi)))

;;--------------------------------------------------

(defmethod main-loop ((server controller))
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

(defun start-io-thread (obj)
  (when (not (alive-p))
    (bt:make-thread (lambda () (main-loop obj))
                    :name "launchpad-io")))

(defun stop-io-thread ()
  (when (alive-p)
    (bt:destroy-thread (get-io-thread))))
