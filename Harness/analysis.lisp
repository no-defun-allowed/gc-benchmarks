(in-package :gc-benchmarks)

(defun load-results-from-file (pathname)
  (with-open-file (s pathname)
    (loop for l = (read s nil :eof) until (eq l :eof)
          do (assert (> (length l) 3))
          collecting l)))

(defun csv-from-results (results metric &key (scale 1000) (stream *standard-output*))
  (let ((configurations
          (remove-duplicates
           (loop for (setup nil threads . nil) in results collect (list setup threads))
           :test #'equal))
        (heap-sizes (remove-duplicates (mapcar #'second results))))
    (flet ((find-result (config heap-size)
             (find-if (lambda (result)
                        (and (eq (first result) (first config))
                             (= (second result) heap-size)
                             (eq (third result) (second config))))
                      results))
           (average (run)
             (destructuring-bind (setup heap threads &rest runs) run
               (declare (ignore setup heap threads))
               (/ (loop for r in runs sum (getf r metric)) (length runs) scale))))
      (format stream "Heap size, ~{~{~A/~D~}~^, ~}~%" configurations)
      (loop for size in heap-sizes
            do (format stream "~$, ~{~A~^, ~}~%"
                       (/ size 1000)
                       (loop for config in configurations
                             for (name threads) = config
                             for result = (find-result config size)
                             collect (if (null result)
                                         ""
                                         (format nil "~$" (average (find-result config size))))))))))

(defun csvs-from-results (results &key (metrics '(:real-time-ms :gc-real-time-ms :mutator-run-time-ms)))
  (let ((name (pathname-name results))
        (results (load-results-from-file results)))
    (dolist (metric metrics)
      (with-open-file (s (format nil "/tmp/~A ~(~A~).csv" name metric) :direction :output)
        (csv-from-results results metric :stream s)))))
