e;;;; kitchen-sink.lisp

(in-package #:kitchen-sink)

(defvar *whitespaces* '(#\Backspace #\Tab #\Linefeed #\Newline #\Vt #\Page
                        #\Return #\Space #\Rubout #\Next-Line #\No-break_space))

(defun is-space (c) (member c *whitespaces*))

;; Haskellisms
(defun take (count list)
  (when (< count 0) (return-from take nil))
  (if (<= count (length list))
      (subseq list 0 count)
      list))

(defun drop (count list)
  (when (< count 0) (return-from drop list))
  (nthcdr count list))

(defun chunks-of (i ls)
  (if (> i (length ls)) (cons ls nil)
      (loop while ls
            collect (loop repeat i
                          while ls
                          collect (pop ls)))))

;; recursive
(defun drop-while (p xs)
  (if (null xs) nil
      (if (funcall p (car xs)) (drop-while p (cdr xs))
          xs)))

(defun tuple (x y) (cons x (cons y nil)))

;; recursive
(defun break- (p xs)
  ;; (break- (lambda (y) (= y 3)) '(1 2 3 4 5)) => ((1 2) (3 4 5))
  (if (null xs) (tuple nil nil)
      (if (funcall p (car xs)) (tuple nil xs)
          (destructuring-bind (ys zs) (break- p (cdr xs))
            (tuple (cons (car xs) ys) zs)))))



;; hacky littler symbole parser example
(defmacro keyword-parser (name &rest options)
  (let ((dummy-data (assoc :dummy-data options)))
    (progn (format t "pulled out dummy data: ~a" dummy-data)
           (dolist (option options)
             (ecase (first option)
               ((:dummy-data))
               ((:maybe) (print "maybe")))))))

(keyword-parser :test
                (:dummy-data '(oh my))
                ;; this will throw an error, as its not covered by the ecase. Use case if you don't want to cover all the options
                ;; (:documentatio n
                ;;  "This is a string")
                (:maybe))



(defun to (type var) (coerce var type))

;; (zip #'+ '(1 2 3) '(1 1 1)) => (2 3 4)
(defun zip (fn lst1 lst2)
  (do ((x lst1 (cdr x))
       (y lst2 (cdr y))
       (z '() (cons (funcall fn (car x) (car y)) z)))
      ((or (null x) (null y))
       (nreverse z))))

;; (for (x 10 1000) (print x))
(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))


;; (funcall (compose #'cdr #'cdr) '(a b c d)) => (c d)   -- cddr
;; (funcall (compose #'car #'nthcdr) 2 '(a b c d)) => c  -- nth
;; (funcall (compose #'not #'consp) :a) => T             -- atom
;; (funcall (compose #'car #'nthcdr) 2 '(a b c d)) => c  -- nth
(defun compose (&rest fns)
  (destructuring-bind (fn1 . rest) (reverse fns)
    #'(lambda (&rest args)
        (reduce #'(lambda (v f) (funcall f v))
                rest
                :initial-value (apply fn1 args)))))



(apply #'< '( 5 10  15))

;; I think the type is (but I'm not certain):
;; any :: [([a] -> bool)] -> [a] -> bool
;; (mapcar (any #'integerp #'symbolp) '(a "a" 2 3)) => (t nil t t)
;; (funcall (any #'< #'=) 5 10 15) => t
(defun any (fn &rest fns)
  (if (null fns)
      fn
      (let ((disjoin (apply #'any fns)))
        #'(lambda (&rest args)
            (or (apply fn args) (apply disjoin args))))))

;; (mapcar (all #'integerp #'oddp) '(a "a" 2 3)) => (nil nil nil t)
(defun all (fn &rest fns)
  (if (null fns)
      fn
      (let ((conjoin (apply #'all fns)))
        #'(lambda (&rest args)
            (and (apply fn args) (apply conjoin args))))))

;; (funcall (curry #'- 3) 5) => -2
;; (funcall (curry #'+ 1) 5) => 6  == 1+
;; (curry #'compose #'not) == complement
(defun curry (fn &rest args)
  #'(lambda (&rest args2)
      (apply fn (append args args2))))

;; (funcall (rcurry #'- 3) 5) => 2
;; (rcurry #'typep 'atom) == atom
;; ()
(defun rcurry (fn &rest args)
  #'(lambda (&rest args2)
      (apply fn (append args2 args))))


(defmacro macro-apply (macro-name args) `(,macro-name ,@args))

(defmacro macro-map (macro-name args)
  `(progn ,@(loop for a in args collecting `(macro-apply ,macro-name ,a))))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro --open-with-args (open-args filename &body body)
  (with-gensyms (result)
    `(let ((in (open ,filename ,@open-args)))
       (when in
         (let ((,result ,@body))
           (close in)
           ,result)))))

(defmacro --open (filename &body body)
  `(--open-with-args nil ,filename ,@body))

(defmacro --open-as-bytes (filename &body body)
  `(--open-with-args (:element-type '(unsigned-byte 8)) ,filename ,@body))

(defun -print (x) (format t "~a~%" x))

(defun ~ (x y &optional (step 1))
  "a better range function
  (~ 0 3)       => (0 1 2 3)
  (~ 0 3 2)     => (0 2)
  (~ 0 (~ 0 2)) => ((0 0) (0 1) (0 2))

  (~ (~ 0 2) (~ 0 -2)))
  => ((0 0) (0 -1) (0 -2) (1 0) (1 -1) (1 -2) (2 0) (2 -1) (2 -2))

  (~ (~ 0 2) (~ 0 -2) 2)
  => ((0 0) (0 -2) (2 0) (2 -2))"
  (flet ((stepper (x) (nthcdr step x)))
    (cond ((and (listp x) (listp y))
             (loop for i in x by #'stepper appending
               (loop for j in y by #'stepper collecting (list i j))))
          ((listp x)  (loop for i in x by #'stepper collecting (list i y)))
          ((listp y)  (loop for i in y by #'stepper collecting (list x i)))
          ((<= x y)   (loop for i from x to y by step collecting i))
          (:otherwise (loop for i from x downto y by step collecting i)))))

(defmacro nested (fn-name args &body body)
  "allows for nesting of functions, but doesn't collect the result.

   (nested dolist ((x (~ 0 2)) (y (~ 3 4))) (print (list x y)))
   => STD-OUT: (0 3) (0 4) (1 3) (1 4) (2 3) (2 4)

   (nested dolist ((x (~ 0 2)) (y (~ 3 4))) (list x y)) => NIL"
  (if (not (null args))
      `(,fn-name ,(car args) (nested ,fn-name ,(cdr args) ,@body))
      (car body)))

(defmacro listcomp-explicit (collecting over filters)
  "the internals for listcomp

   (listcomp-explicit x ((x (~ 0 5))) ())                   => (0 1 2 3 4 5)
   (listcomp-explicit x ((x (~ 0 5))) ((evenp x)))          => (0 2 4)
   (listcomp-explicit x ((x (~ 0 5))) ((evenp x) (> x 2)))) => (4)

   (listcomp-explicit (list x y) ((x (~ 0 2)) (y (~ 3 4))) ((evenp x)))
   => ((0 3) (0 4) (2 3) (2 4))
   (listcomp-explicit (list x y) ((x (~ 0 2)) (y (~ 3 4))) ((evenp x) (evenp y)))
   => ((0 4) (2 4))"
  (let ((result (gensym)))
    `(let ((,result nil))
       (nested dolist ,over (and ,@filters (push ,collecting ,result)))
       (reverse ,result))))

(defun pair (args) (loop for (k v) on args by #'cddr collecting (list k v)))

(defmacro listcomp (to-collect over &rest filters)
  " (listcomp x (x (~ 0 2)))                  => (0 1 2)
    (listcomp (x) (x (~ 0 2)))                => (0 1 2)
    (listcomp (list x) (x (~ 0 2)))           => ((0) (1) (2))

    (listcomp (x y) (x '(0) y (~ 3 4))))      => ((0 3) (0 4))
    (listcomp (list x y) (x '(0) y (~ 3 4)))) => ((0 3) (0 4))

    (listcomp (x y) (x (~ 0 2) y (~ 3 4))))
    => ((0 3) (0 4) (1 3) (1 4) (2 3) (2 4))

    (listcomp (x y z) (x (~ 0 2) y (~ 3 4) z (~ 1 2)) (eq z 2) (eq x 0))
    => ((0 3 2) (0 4 2))"
  ;; this is a passthrough macro to clean up the parameters for listcomp-explicit
  ;; the API is a bit inconsistent, but I went with my gut.
  `(listcomp-explicit
     ,(cond ((atom to-collect)                to-collect)
            ((= (length to-collect) 1)        (car to-collect))
            ((equalp (car to-collect) 'list)  to-collect)
            (:otherwise                       (cons 'list to-collect)))
     ,(pair over)
     ,filters))


;; vector syntax reader --------------------------------------------------------
;; CL-USER> [12, 23, 3] => #(12 23 3)
;;
;; (defconstant +left-bracket+ #\[)
;; (defconstant +right-bracket+ #\])
;; (defconstant +comma+ #\,)
;;
;; (defun read-next-object (separator delimiter &optional (in-stream *standard-input*))
;;   (flet ((discard-next-char () (progn (read-char in-stream t nil t) nil))
;;          (next-is (c) (char= (peek-char t in-stream t nil t) c)))
;;     (if (next-is delimiter) (discard-next-char)
;;         (let* ((object (read in-stream t nil t)))
;;           (cond ((next-is separator) (discard-next-char))
;;                 ((next-is delimiter) nil))
;;           object))))
;;
;; (defun dont-read (stream char) (declare (ignore stream)) (error "read ~S alone") char)
;;
;; (defun read-left-bracket (stream char)
;;   (declare (ignore char))
;;   (let ((*readtable* (copy-readtable)))
;;     (set-macro-character +comma+ 'dont-read)
;;     (loop
;;        for object = (read-next-object +comma+ +right-bracket+ stream)
;;        while object
;;        collect object into objects
;;        finally (return `(vector ,@objects)))))
;;
;; (set-macro-character +right-bracket+ 'dont-read)
;; (set-macro-character +left-bracket+ 'read-left-bracket)




;; phue lights

'((nightshift
   ((turn on sunset, reading-light)
    (fade to night-light :finished, slow))
   :finished 11 pm)

  (wakeup
   ((fade in slow :start, bright-light)
    (turn off :finished))
   :start 9:30 am
   :finished 11:30 am))

'((bedroom
   ((master-left master-right master-overhead)
    ((monday tuesday wednesday thursday
      ((wakeup)
       (nightshift))))
    (friday
     ((wakeup)
      (turn on sunset, reading-light)))
    (saturday sunday
     ((nightshift)))))

  (living-room
   ((coat-closet)
    ((all
      ((mirror master-left))))

    (left spotlight right lamp winelights)
    ((monday tuesday wednesday thursday sunday
      ((wakeup)
       (nightshift)))
     (friday saturday
      ((turn on sunset, reading-light))))))

  (kitchen
   ((overhead)
    ((all
      ((wakeup)
       (turn on 5:30 pm)
       (turn off 8:30 pm))))))

  (office
   ((lamp overhead records)
    ((monday wednesday thursday friday
      ((turn on 9:30 am, bright-light)
       (flash 5:45 pm)
       (turn off 6:15 pm)))
     (tuesday
      ((turn on 9:30 am, bright-light)
       (flash 5 pm)
       (turn off 5:30 pm))))))

  (outside
   ((garage-east garage-west landscape-lights)
    ((all
      ((turn on sunset)
       (turn off master-left))))

    (porch-north porch-south)
    ((all
      ((turn on sunset)
       (turn off 8:30 pm)))))))
