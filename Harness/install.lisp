(in-package :gc-benchmarks)

(defstruct (install (:conc-name "I-"))
  name
  type
  remote
  commit
  make-flags
  directory
  threads)

(defvar *installations* '())

(defmacro define-installation (name type remote commit make-flags directory threads)
  `(progn
     (pushnew (make-install :name ',name
                            :type ',type
                            :remote ',remote
                            :commit ',commit
                            :make-flags ',make-flags
                            :directory ',directory
                            :threads ',threads)
              *installations*)
     ',name))

(defun map-installations (function)
  (dolist (install *installations*)
    (dolist (th (i-threads install))
      (funcall function install th))))

(define-installation mark-region :pmrgc
  "https://github.com/no-defun-allowed/swcl" ""
  ("--with-mark-region-gc") "/home/hayleyp/swcl/" (0 1 3 11))
(define-installation gencgc :gencgc
  "https://github.com/sbcl/sbcl" ""
  () "/home/hayleyp/sbcl/" (0))

(defun install ()
  (dolist (install *installations*)
    (format t "-- Installing ~A --~%" (i-name install))
    (sb-ext:run-program "/usr/bin/git" `("clone" ,(i-remote install) ,(i-directory install)) :output t)
    (sb-ext:run-program "/usr/bin/git" `("reset" "--hard" ,(i-commit install)))
    (sb-ext:run-program "./make.sh" (i-make-flags install) :directory (i-directory install) :output t)))
