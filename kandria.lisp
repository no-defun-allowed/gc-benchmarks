;; Use a more precise real time counter.
(setf sb-impl::*ignored-package-locks* t) ; we clobber G-I-R-T
(defun get-internal-real-time ()
  (with-alien ((base (struct sb-unix::timespec) :extern "lisp_init_time"))
    (multiple-value-bind (c-sec c-nsec)
        ;; By scaling down we end up with far less resolution than clock-realtime
        ;; offers, and COARSE is about twice as fast, so use that, but only for linux.
        ;; BSD has something similar.
        (sb-unix::clock-gettime 4) ; clock-monotonic-raw
      (declare (optimize (sb-c::type-check 0)))
      (let ((delta-sec (the fixnum (- c-sec (the fixnum (slot base 'sb-unix::tv-sec)))))
            (delta-nsec (the fixnum (- c-nsec (the fixnum (slot base 'sb-unix::tv-nsec))))))
        ;; Avoid going back in time.
        (max 0
             (the sb-kernel:internal-time
                  (+ (the fixnum (* delta-sec internal-time-units-per-second))
                     (truncate delta-nsec sb-unix::nanoseconds-per-internal-time-unit))))))))

(load (merge-pathnames #p"quicklisp/setup" (user-homedir-pathname)))
(ql:quickload :kandria)

(defconstant +buckets+ 150)
(defconstant +precision+ 0.002)
(sb-ext:defglobal **running** nil)
(sb-ext:defglobal **frame-histogram** (make-array (1+ +buckets+) :initial-element 0))
(sb-ext:defglobal **pause-histogram** (make-array (1+ +buckets+) :initial-element 0))
(defun bump-histogram (measurement hist)
  (when **running**
    (let ((index (min +buckets+ (floor measurement +precision+))))
      (incf (aref hist index)))))

(defun dump-histogram (pathname hist)
  (with-open-file (s pathname :direction :output :if-exists :supersede)
    (format s "time, count~%")
    (loop with total = (reduce #'+ hist)
          for bucket across hist
          for latency from 0 by +precision+
          do (format s "~$, ~D~%" (* 1000 latency) (float (/ bucket total))))))

(defvar *last-gc-time* sb-impl::*gc-real-time*)
(defvar *pauses* '())
(push (lambda ()
        (bump-histogram
         (/ (- sb-impl::*gc-real-time* *last-gc-time*)
            internal-time-units-per-second)
         **pause-histogram**)
        (setf *last-gc-time* sb-impl::*gc-real-time*))
      sb-impl::*after-gc-hooks*)

org.shirakumo.fraf.trial::
(defmethod render-loop ((render-loop render-loop))
  (declare (optimize speed))
  (let ((fc 0))
    (declare (type fixnum fc))
    (restart-case
        (unwind-protect
             (with-retry-restart (reset-render-loop "Reset the render loop timing, not catching up with lost frames.")
               (let ((tt 0.0d0)
                     (fdt (coerce (delta-time render-loop) 'single-float))
                     (dt (coerce (delta-time render-loop) 'double-float))
                     (current-time (current-time))
                     (accumulator 0.0d0)
                     (new-time 0.0d0)
                     (frame-time 0.0d0))
                 (declare (type double-float tt dt current-time
                                accumulator new-time frame-time))
                 (with-error-logging (:trial.render-loop "Error in render thread")
                   (loop while (thread render-loop)
                         do (setf new-time (current-time))
                            (setf frame-time (- new-time current-time))
                            (setf current-time new-time)
                            (incf accumulator frame-time)
                            (cl-user::bump-histogram frame-time cl-user::**frame-histogram**)
                            (loop while (<= dt accumulator)
                                  do (when (<= 10d0 accumulator)
                                       (setf accumulator dt))
                                     (update render-loop tt fdt fc)
                                     (decf accumulator dt)
                                     (incf tt dt)
                                     (incf fc))
                            ;; FIXME: interpolate state
                            ;;        See http://gafferongames.com/game-physics/fix-your-timestep/
                            (setf (frame-time render-loop) frame-time)
                            (with-simple-restart (abort "Abort the update and retry.")
                              (render render-loop render-loop))))))
          (v:info :trial.render-loop "Exiting render-loop for ~a." render-loop))
      (exit-render-loop ()
        :report "Exit the render loop entirely."
        (quit *context*)))))

(in-package :kandria)

;; Disable the main menu.
(defmethod hide :after ((screen startup-screen))
  (unwind-protect
       (resume-state (find-canonical-save "1") +main+)
    (write-line "Starting recording")
    (ignore-errors
     (sb-alien:alien-funcall (sb-alien:extern-alien "mr_reset_meters" (function sb-alien:void))))
    (setf cl-user::**running** t)))
(defmethod finalize :after ((main main))
  (write-line "Stopping recording")
  (setf +world+ NIL
        cl-user::**running** nil))

(launch)
(cl-user::dump-histogram #p"/tmp/kandria-frame.csv" cl-user::**frame-histogram**)
(cl-user::dump-histogram #p"/tmp/kandria-pause.csv" cl-user::**pause-histogram**)
(ignore-errors
 (sb-alien:alien-funcall (sb-alien:extern-alien "mr_print_meters" (function sb-alien:void))))
