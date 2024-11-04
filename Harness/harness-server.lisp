(in-package :gc-benchmarks)

(defstruct (benchmark (:conc-name "B-"))
  name
  program
  start
  by
  end
  metrics
  minimum-sizes)

(defvar *benchmarks* '())

(defmacro define-benchmark (name program start by end metrics minimum-sizes)
  `(progn
     (pushnew (make-benchmark
               :name ',name
               :program ,program
               :start ,start
               :by ,by
               :end ,end
               :metrics ',metrics
               :minimum-sizes ',minimum-sizes)
              *benchmarks*)
     ',name))

(defun run (directory script gc-threads heap-size)
  (let ((program
          (sb-ext:run-program
           (truename (make-pathname :directory directory :name "run-sbcl" :type "sh"))
           (list "--dynamic-space-size" (format nil "~DMB" heap-size)
                 "--script" script)
           :directory (asdf:system-relative-pathname :gc-benchmarks "../Benchmarks/")
           :output :stream
           :environment (list (format nil "GC_THREADS=~d" gc-threads)))))
    (unwind-protect
         (progn
           (assert (zerop (sb-ext:process-exit-code program)))
           (read (sb-ext:process-output program)))
      (close (sb-ext:process-output program)))))

(defvar *iterations* 5)
(defvar *warmups* 1)

(defmacro repeat (n &body body)
  `(progn
     (dotimes (,(gensym) *warmups*)
       (write-char #\w)
       (finish-output)
       ,@body)
     (loop repeat ,n
           do (write-char #\.)
              (finish-output)
           collect (progn ,@body))))

(defun map-configurations (function)
  (loop for (name path . threads) in *configurations*
        appending (loop for th in threads
                        collect (funcall function name path th))))

(defvar *webhook*
  (ignore-errors
   (string-trim '(#\Newline)
                (alexandria:read-file-into-string
                 (asdf:system-relative-pathname :gc-benchmarks "webhook-url")))))

(defun hook (control &rest stuff)
  (unless (null *webhook*)
    (ignore-errors
     (drakma:http-request *webhook*
                          :method :post
                          :content-type "application/json"
                          :content (jsown:to-json `(:obj ("content" . ,(apply #'format nil control stuff)))))))
  (values))

(defun hook-upload (pathname)
  (unless (null *webhook*)
    (ignore-errors
     (drakma:http-request *webhook*
                          :method :post
                          :parameters `(("file" ,pathname))
                          :form-data t)))
  (values))

(defun herald-test (program heap-size)
  (format t "~&Testing ~A, ~D MB" program heap-size)
  (finish-output)
  (hook "Now testing **~(~A~)** with a ~:D megabyte heap." program heap-size))

(defvar *directory*)

(defun run-benchmark (benchmark)
  (ensure-directories-exist (make-pathname :directory *directory*))
  (hook "Starting **~(~A~)**" (b-name benchmark))
  (let ((pathname (make-pathname :directory *directory* :name (string-downcase (b-name benchmark)) :type "txt")))
    (with-open-file (record pathname :direction :output)
      (loop for heap-size from (b-start benchmark) to (b-end benchmark) by (b-by benchmark)
            do (herald-test (b-name benchmark) heap-size)
               (map-installations
                (lambda (installation threads)
                  (when (>= heap-size (getf (b-minimum-sizes benchmark) (i-type installation)))
                    (format t " (~A/~D)" (i-name installation) threads)
                    (finish-output)
                    (let ((results
                            (repeat *iterations*
                              (run (i-directory installation) (b-program benchmark) threads heap-size))))
                      (print `(,(i-name installation) ,heap-size ,threads ,@results) record)))))))
    (hook "Results for ~A" (b-name benchmark))
    (hook-upload pathname)))

(defun run-benchmarks (directory)
  (let ((*directory* directory))
    (mapc #'run-benchmark *benchmarks*)))

(define-benchmark ring-buffer "ring-buffer.lisp" 500 500 6000
  (:worst-latency :real-time-ms)
  (:gencgc 2000 :pmrgc 500))
(define-benchmark boehm-gc "boehm-gc.lisp" 2000 1000 6000
  (:gc-real-time-ms :mutator-real-time-ms :mutator-run-time-ms :real-time-ms)
  (:gencgc 3000 :pmrgc 2000))
(define-benchmark regrind-interpret "regrind.lisp" 1000 1000 6000
  (:gc-real-time-ms :mutator-real-time-ms :mutator-run-time-ms :real-time-ms)
  (:gencgc 1000 :pmrgc 1000))
(define-benchmark regrind-compile "regrind-compiling.lisp" 1000 1000 6000
  (:gc-real-time-ms :mutator-real-time-ms :mutator-run-time-ms :real-time-ms)
  (:gencgc 1000 :pmrgc 1000))
