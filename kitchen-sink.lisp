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
k
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
