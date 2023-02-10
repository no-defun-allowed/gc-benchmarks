(ql:quickload '(:alexandria :jsown :drakma))

(defun averages (lists)
  (assert (not (endp lists)))
  (loop for (k nil) on (first lists) by #'cddr
        unless (member k '(:page-faults))
        collect k
        and collect (float
                     (/ (reduce #'+ lists :key (lambda (l) (getf l k)))
                        (length lists)))))
(defun run (program script gc-threads heap-size)
  (let ((program
          (sb-ext:run-program
           program
           (list "--dynamic-space-size" (format nil "~DMB" heap-size)
                 "--script" script)
           :output :stream
           :environment (list (format nil "GC_THREADS=~d" gc-threads)))))
    (assert (zerop (sb-ext:process-exit-code program)))
    (read (sb-ext:process-output program))))

(defvar *iterations* 5)
(defvar *warmups* 1)
(defvar *configurations*
  '((:gencgc "../../Downloads/swcl-gencgc/run-sbcl.sh" 0)
    (:par-gc "../../Downloads/swcl/run-sbcl.sh" 0 1 3 11)))

(defmacro repeat (n &body body)
  `(progn
     (dotimes (,(gensym) *warmups*) (write-char #\w) ,@body)
     (loop repeat ,n do (write-char #\.) collect (progn ,@body))))

(defun map-configurations (function)
  (loop for (name path . threads) in *configurations*
        appending (loop for th in threads
                        collect (funcall function name path th))))

(defun headers (stream)
  (format stream
          "~10@A, ~{~10@A~^, ~}~%"
          "Heap size"
          (map-configurations
           (lambda (name path threads)
             (declare (ignore path))
             (format nil "~A/~D" name threads)))))

(defvar *transformers* (make-hash-table))
(defun transform (metric value)
  (funcall (gethash metric *transformers* #'identity) value))

(defvar *webhook* (ignore-errors (string-trim '(#\Newline) (alexandria:read-file-into-string "webhook-url"))))
(defun hook (control &rest stuff)
  (unless (null *webhook*)
    (ignore-errors
     (drakma:http-request *webhook*
                          :method :post
                          :content-type "application/json"
                          :content (jsown:to-json `(:obj ("content" . ,(apply #'format nil control stuff)))))))
  (values))
(defun herald-test (program heap-size)
  (format t "~&Testing ~A, ~D MB" program heap-size)
  (hook "Now testing **~(~A~)** with a ~:D megabyte heap." program heap-size))

(ensure-directories-exist "results/")

(defun %test-case (name program start by end metrics minimum-sizes)
  (hook "Starting **~(~A~)**" name)
  (let ((buffers (loop for m in metrics
                       collect (make-broadcast-stream
                                (open (format nil "results/~(~a ~a~).csv" name m)
                                      :direction :output
                                      :if-exists :supersede)
                                (make-string-output-stream)))))
    (mapc #'headers buffers)
    (loop for heap-size from start to end by by
          for nil = (herald-test program heap-size)
          for result-table = (map-configurations
                              (lambda (name implementation threads)
                                (cond
                                  ((>= heap-size (getf minimum-sizes name))
                                   (format t " (~A/~D)" name threads)
                                   (finish-output)
                                   (averages
                                    (repeat *iterations*
                                      (run implementation program threads heap-size))))
                                  (t :skipped))))
          do (loop for metric in metrics
                   for buffer in buffers
                   do (format buffer "~10F, ~{~10@A~^, ~}~%"
                              (/ heap-size 1000)
                              (mapcar
                               (lambda (r)
                                 (if (eql r :skipped)
                                     ""
                                     (transform metric (getf r metric))))
                               result-table))))
    (loop for b in buffers
          for m in metrics
          for (file s-o-s) = (broadcast-stream-streams b)
          do (close file)
             (hook "**Results for ~(~a~):**~%```~%~A~%```" m (get-output-stream-string s-o-s)))))

(defmacro test-case (name pathname start by end metrics minimum-sizes)
  `(%test-case ',name ,pathname ,start ,by ,end ',metrics ',minimum-sizes))

(defmacro define-transformer (metric (value) &body body)
  `(setf (gethash ,metric *transformers*)
         (lambda (,value) ,@body)))

;;; :REAL-TIME-MS :USER-RUN-TIME-US 64 :SYSTEM-RUN-TIME-US :GC-REAL-TIME-MS :GC-RUN-TIME-MS :PROCESSOR-CYCLES :BYTES-CONSED

(define-transformer :real-time-ms (v) (/ v 1000))
(define-transformer :mutator-real-time-ms (v) (/ v 1000))
(define-transformer :mutator-run-time-ms (v) (/ v 1000))
(define-transformer :gc-real-time-ms (v) (/ v 1000))

(defun run-tests ()
  (test-case gc-latency "gc-latency.lisp" 500 500 8000
             (:worst-latency :real-time-ms)
             (:gencgc 2000 :par-gc 500))
  (test-case boehm-gc "boehm-gc.lisp" 2000 1000 8000
             (:gc-real-time-ms :mutator-real-time-ms :mutator-run-time-ms :real-time-ms)
             (:gencgc 3000 :par-gc 2000))
  (test-case regrind-interpret "regrind.lisp" 1000 1000 8000
             (:gc-real-time-ms :mutator-real-time-ms :mutator-run-time-ms :real-time-ms :small-allocation-count)
             (:gencgc 1000 :par-gc 1000))
  (test-case regrind-compile "regrind-compiling.lisp" 1000 1000 8000
             (:gc-real-time-ms :mutator-real-time-ms :mutator-run-time-ms :real-time-ms :small-allocation-count)
             (:gencgc 1000 :par-gc 1000)))
