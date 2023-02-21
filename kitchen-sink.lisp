;;;; kitchen-sink.lisp

(in-package #:kitchen-sink)

(defvar *whitespaces* '(#\Backspace #\Tab #\Linefeed #\Newline #\Vt #\Page
                        #\Return #\Space #\Rubout #\Next-Line #\No-break_space))

(defun is-space (c) (member c *whitespaces*))

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
