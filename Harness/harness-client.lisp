(defun parse-vmhwm ()
  (with-open-file (s "/proc/self/status")
    (loop for l = (read-line s nil :eof)
          until (eq l :eof)
          when (search "VmHWM" l)
            return (parse-integer l
                                  :start (position-if #'digit-char-p l)
                                  :junk-allowed t))))

(defmacro time* ((&rest extra) &body body)
  `(progn
     (sb-ext:call-with-timing
      (lambda (&rest r)
        (print
         (append
          (list ,@extra)
          (list :mutator-real-time-ms
                (- (getf r :real-time-ms 0)
                   (getf r :gc-real-time-ms 0))
                :mutator-run-time-ms
                (- (floor (+ (getf r :system-run-time-us 0)
                             (getf r :user-run-time-us 0))
                          1000)
                   (getf r :gc-run-time-ms 0))
                :medium-allocations (ignore-errors (extern-alien "medium_allocation_count" int))
                :small-allocations (ignore-errors (extern-alien "small_allocation_count" int))
                :memory-hwm (parse-vmhwm))
          r)))
      (lambda () ,@body))
     (finish-output)
     (ignore-errors (alien-funcall (extern-alien "mr_print_meters" (function void))))))
