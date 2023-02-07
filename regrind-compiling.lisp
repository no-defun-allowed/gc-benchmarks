(load (merge-pathnames #p"quicklisp/setup" (user-homedir-pathname)))
(ql:quickload '(:bordeaux-threads :random-state :one-more-re-nightmare) :silent t)
(load "harness-client")

;;;; It's regrind time!

(defvar *start*)
(defvar *end*)

(defun checked-string-ref (string index)
  (assert (and (<= *start* index) (< index *end*)))
  (aref string index))

(defvar *layout*
  (one-more-re-nightmare::make-layout
   :ref 'checked-string-ref))

(defvar *remaining-depth* 4)
(defvar *repro-random-state* (random-state:make-generator :mersenne-twister-32 42))
(defun repro-random (n) (random-state:random n *repro-random-state*))
(defun random-re ()
  (macrolet ((terminal ()
               ;; A random element of [A-Z].
               '(string (code-char (+ 65 (random 26)))))
             (recurse (control n)
               `(format nil ,control ,@(loop repeat n collect '(random-re)))))
    (if (zerop *remaining-depth*)
        (terminal)
        (let ((*remaining-depth* (1- *remaining-depth*)))
          (case (repro-random 8)
            (0 (terminal))
            (1 (recurse "~a~a" 2))
            (2 (recurse "(~a)" 1))
            (3 (recurse "«~a»" 1))
            (4 (recurse "(~a)|(~a)" 2))
            (5 (recurse "(~a)&(~a)" 2))
            (6 (recurse "(¬~a)" 1))
            (7 (recurse "(~a)*" 1)))))))

(defun random-haystack ()
  (let* ((n (repro-random 80))
         (haystack (make-string n)))
    (dotimes (i n)
      (setf (char haystack i) (code-char (+ 65 (repro-random 26)))))
    haystack))

(defvar *tasks* 5000)
(defvar *threads* 12)

(defun generate-work ()
  (loop repeat *threads*
        collect (loop repeat (floor *tasks* *threads*)
                      collect (cons (random-re) (random-haystack)))))

(defun regrind-worker (work)
  (lambda ()
    (let ((one-more-re-nightmare::*code-type* :compiled))
      (loop for (pattern . haystack) in work
            do (handler-case
                   (one-more-re-nightmare::%compile-regular-expression
                    pattern
                    :layout *layout*)
                 (error ())
                 (:no-error (code registers)
                   (let ((result (make-array registers))
                         (*start* 0)
                         (*end* (length haystack)))
                     (handler-case
                         (funcall code haystack 0 (length haystack) result
                                  (lambda ()
                                    (loop for p across result
                                          do (assert (or (null p) (<= 0 p *end*))))))
                       (error ())))))))))

;; Go ahead, fuse _these_ MAPCARs.
(time* ()
  (mapcar #'bt:join-thread
          (mapcar (alexandria:compose #'bt:make-thread #'regrind-worker)
                  (generate-work))))
