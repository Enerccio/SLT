(cl:in-package #:eclector.base)

;;; We have our own READ-CHAR function so we can signal our own
;;; END-OF-FILE condition which stores the position in the input
;;; stream in a portable way.  Since READ-CHAR is relatively critical
;;; for performance, we use a compiler macro to transform our
;;; READ-CHAR to CL:READ-CHAR when we can statically determine that
;;; END-OF-FILE will not be signaled.

(defun read-char (&optional (input-stream *standard-input*)
                            (eof-error-p t)
                            eof-value
                            recursive-p)
  (if eof-error-p
      (let ((result (cl:read-char input-stream
                                  nil '#1=#.(gensym "EOF") recursive-p)))
        (if (eq result '#1#)
            (%reader-error input-stream 'end-of-file)
            result))
      (cl:read-char input-stream nil eof-value recursive-p)))

(define-compiler-macro read-char
    (&whole whole &optional (input-stream '*standard-input*)
            (eof-error-p nil eof-error-p-supplied-p)
            eof-value recursive-p)
  (if (and eof-error-p-supplied-p
           (constantp eof-error-p) (not (eval eof-error-p)))
      `(cl:read-char ,input-stream nil ,eof-value ,recursive-p)
      whole))

(defun read-char-or-error (input-stream datum &rest arguments)
  (let ((result (cl:read-char input-stream nil '#1=#.(gensym "EOF") t)))
    (if (eq result '#1#)
        (apply #'%reader-error input-stream datum arguments)
        result)))

(defun read-char-or-recoverable-error (input-stream recover-value
                                       datum &rest arguments)
  (let ((result (cl:read-char input-stream nil '#1=#.(gensym "EOF") t)))
    (if (eq result '#1#)
        (progn
          (apply #'%recoverable-reader-error input-stream datum arguments)
          recover-value)
        result)))
