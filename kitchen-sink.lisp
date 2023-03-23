;;;; kitchen-sink.lisp

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

(defun to (type var) (coerce var type))

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
    (listcomp (list x y z) (x (~ 0 2) y (~ 3 4) z (~ 1 2)) (eq z 2) (eq x 0))
    => ((0 3 2) (0 4 2))"
  ;; this is a passthrough macro to clean up the parameters for listcomp-explicit
  ;; the API is a bit inconsistent, but I went with my gut.
  `(listcomp-explicit
     ,(if (atom to-collect) to-collect
          (if (= (length to-collect) 1) (car to-collect)
              (if (equalp (car to-collect) 'list) to-collect
                  (cons 'list to-collect))))
     ,(pair over)
     ,filters))
