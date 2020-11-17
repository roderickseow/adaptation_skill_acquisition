#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


;#+:quicklisp (ql:quickload :usocket)
;#-:quicklisp (load "ACT-R-support:uni-files.lisp")


(pushnew :sfapi *features*)

#+openmcl
(defun sfapi-run-program (cmd args)
  (ccl:run-program cmd args
                   :wait nil
                   :input :stream
                   :output :stream
                   :error :output
                   :external-format :utf-8))

#+openmcl
(defun sfapi-process-input (proc)
  (ccl:external-process-input-stream proc))

#+openmcl
(defun sfapi-process-output (proc)
  (ccl:external-process-output-stream proc))

#+openmcl
(defun sfapi-process-kill (proc)
  ;; How do we kill a subprocess?
  (close (sfapi-process-output proc))
  (close (sfapi-process-input proc)))

#+allegro
(defstruct allegro-external-process
  input output pid)

#+allegro
(defun sfapi-process-input (proc)
  (allegro-external-process-input proc))

#+allegro
(defun sfapi-process-output (proc)
  (allegro-external-process-output proc))

#+allegro
(defun sfapi-process-kill (proc)
  ;; How do we kill a subprocess?
  (close (sfapi-process-output proc))
  (close (sfapi-process-input proc)))

#+allegro
(defun sfapi-run-program (cmd args)
  (multiple-value-bind (in out err pid)
      (excl:run-shell-command (format nil "~A~{~^ ~A~}" cmd args)
                              :wait nil
                              :separate-streams t
                              :input :stream
                              :output :stream
                              :error-output :output)
    (make-allegro-external-process :input in :output out :pid pid)))


;;;; API

(defgeneric sfapi-connect (obj &key id condition display-level))
(defgeneric sfapi-connected-p (obj))
(defgeneric sfapi-write (obj command))
(defgeneric sfapi-read (obj))
(defgeneric sfapi-disconnect (obj))
(defgeneric sfapi-check (obj))

;;;; TCP implementation

(defclass sfapi-tcp-client ()
  ((host :initform "127.0.0.1" :initarg :host :accessor sfapi-host)
   (port :initform 3000 :initarg :port :accessor sfapi-port)
   (socket :initform nil :accessor sfapi-socket)))

(defmethod sfapi-check ((obj sfapi-tcp-client))
  (with-slots (host port) obj
    (handler-case (let ((result nil)
                        (c #+:quicklisp (usocket:socket-connect host port :nodelay :if-supported)
                           #-:quicklisp (uni-make-socket host port)))
                    (when c
                      #+:quicklisp (usocket:socket-close c)
                      #-:quicklisp (close c)
                      (setf result t))
                    result)
      ((or error condition)
       nil))))


(defmethod sfapi-connect ((obj sfapi-tcp-client) &key id condition display-level)
  (declare (ignorable id condition display-level))
  (with-slots (host port socket) obj
    (setf socket (handler-case (let ((result 
                                      #+:quicklisp (usocket:socket-connect host port :nodelay :if-supported)
                                      #-:quicklisp (uni-make-socket host port)
                                      ))
                                 result)
                   ((or error condition) (x)
                    (format t "~%Could not connect to server.~%")
                    nil)))
    (values)))

(defmethod sfapi-connected-p ((obj sfapi-tcp-client))
  (with-slots (socket) obj
    #+:quicklisp (open-stream-p (usocket:socket-stream socket))
    #-:quicklisp (not (uni-stream-closed socket))
    ))

(defmethod sfapi-write ((obj sfapi-tcp-client) command)
  (with-slots (socket) obj
    #+:quicklisp (progn
                   (write-line command (usocket:socket-stream socket))
                   (finish-output (usocket:socket-stream socket)))
    #-:quicklisp (uni-send-string socket command)
    ))

(defmethod sfapi-read ((obj sfapi-tcp-client))
  (with-slots (socket) obj
    #+:quicklisp (read-line (usocket:socket-stream socket))
    #-:quicklisp (uni-socket-read-line socket)
    ))

(defmethod sfapi-disconnect ((obj sfapi-tcp-client))
  (with-slots (socket) obj
    (when socket
      #+:quicklisp (usocket:socket-close socket)
      #-:quicklisp (close socket)
      (setf socket nil))))

;;;; External Process implementation

(defvar *sfapi-server-command* '("node" "cli_server.js")
  "The command and arguments needed to start the external process server.")

(defun set-server-script-location-for-cluster (node script)
  (setf *sfapi-server-command* (list node script)))


(defclass sfapi-external-process-client ()
  ((command :initform *sfapi-server-command* :initarg :command)
   (info :initform nil)))

(defmethod sfapi-check ((obj sfapi-external-process-client))
  (with-slots (command) obj
    (probe-file (first command))))

  
(defmethod sfapi-connect ((obj sfapi-external-process-client) &key id condition display-level)
  (declare (ignorable id condition display-level))
  (with-slots (command info) obj
    (setf info (sfapi-run-program (first command) (cdr command)))
    (values)))

(defmethod sfapi-connected-p ((obj sfapi-external-process-client))
  (with-slots (info) obj
    (open-stream-p (sfapi-process-output info))))

(defmethod sfapi-write ((obj sfapi-external-process-client) command)
  (with-slots (info) obj
    ;; (format t ">>> ~S" command)
    (write-string command (sfapi-process-input info))
    (finish-output (sfapi-process-input info))))

(defmethod sfapi-read ((obj sfapi-external-process-client))
  (with-slots (info) obj
    (let ((l (read-line (sfapi-process-output info))))
      ;; (format t "<<< ~S" l)
      l)))


(defmethod sfapi-disconnect ((obj sfapi-external-process-client))
  (with-slots (info) obj
    (when info
      (sfapi-process-kill info)
      (setf info nil))))


;;;; Default client

(defvar *sfapi-client* :tcp
  "The default sfapi client type to use when connecting to the server.")

(defun sfapi-create-client (&optional (client *sfapi-client*))
  (ecase client
    (:tcp (make-instance 'sfapi-tcp-client))
    (:external-process (make-instance 'sfapi-external-process-client))))
