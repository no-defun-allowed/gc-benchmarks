(load "harness-client")

(declaim (optimize (speed 3) (safety 1)))

(defmacro with-gensyms (syms &body body)
  `(let ,(loop for s in syms collect `(,s (gensym)))
     ,@body))

(defmacro measure (&body body)
  "Eval body and returns two values: the time it took in seconds as a rational and the result"
  (with-gensyms (start-sym result-sym elapsed-sym)
    `(let* ((,start-sym (get-internal-real-time))
            (,result-sym (progn ,@body))
            (,elapsed-sym
              (float (/ (- (get-internal-real-time) ,start-sym)
                        internal-time-units-per-second))))
       (values ,elapsed-sym ,result-sym))))


(defconstant +win-size+ 200000)
(defconstant +msg-count+ 1000000)
(defconstant +msg-size+ 1024)
(defvar *worst* 0)

(defun make-message (n)
  (make-array +msg-size+ :element-type '(unsigned-byte 8)
			 :initial-element (mod n 256)))

(defun push-message (store id)
  (let ((elapsed
	  (measure
	   (setf (aref store (mod id +win-size+)) (make-message id)))))
    (when (> elapsed *worst*)
      (setf *worst* elapsed))))

(time* (:worst-latency (* 1000 #|ms/s|# *worst*)) 
  (dotimes (i 10)
    (let ((store (make-array +win-size+)))
      (dotimes (i +msg-count+) (push-message store i)))))
