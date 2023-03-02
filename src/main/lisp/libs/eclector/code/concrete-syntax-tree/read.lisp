(cl:in-package #:eclector.concrete-syntax-tree)

(defclass cst-client (eclector.parse-result:parse-result-client)
  ())

(defmethod eclector.parse-result:make-expression-result
    ((client cst-client) expression children source)
  (labels ((make-atom-cst (expression &optional source)
             (make-instance 'cst:atom-cst
                            :raw expression
                            :source source))

           (make-list-cst (expression children source)
             (loop for expression in (loop with reversed = '()
                                           for sub-expression on expression
                                           do (push sub-expression reversed)
                                           finally (return reversed))
                   for child in (reverse children)
                   for previous = (make-instance 'cst:atom-cst :raw nil) then node
                   for node = (make-instance 'cst:cons-cst :raw expression
                                                           :first child
                                                           :rest previous)
                   finally (return (reinitialize-instance node :source source)))))
    (cond
      ((atom expression)
       (make-atom-cst expression source))
      ;; List structure with corresponding elements.
      ((and (eql (ignore-errors (list-length expression))
                 (length children))
            (every (lambda (sub-expression child)
                     (eql sub-expression (cst:raw child)))
                   expression children))
       (make-list-cst expression children source))
      ;; Structure mismatch, try heuristic reconstruction.
      (t
       ;; We don't use
       ;;
       ;;   (cst:reconstruct expression children client)
       ;;
       ;; because we want SOURCE for the outer CONS-CST but not any of
       ;; its children.
       (destructuring-bind (car . cdr) expression
         (make-instance 'cst:cons-cst
                        :raw expression
                        :first (cst:reconstruct car children client)
                        :rest (cst:reconstruct cdr children client)
                        :source source))))))

(defvar *cst-client* (make-instance 'cst-client))

(defun read (&optional (input-stream *standard-input*)
                       (eof-error-p t)
                       (eof-value nil))
  (eclector.parse-result:read
   (or eclector.reader:*client* *cst-client*)
   input-stream eof-error-p eof-value))

(defun read-preserving-whitespace (&optional (input-stream *standard-input*)
                                             (eof-error-p t)
                                             (eof-value nil))
  (eclector.parse-result:read-preserving-whitespace
   (or eclector.reader:*client* *cst-client*)
   input-stream eof-error-p eof-value))

(locally (declare #+sbcl (sb-ext:muffle-conditions eclector.base:&optional-and-&key-style-warning))
  (defun read-from-string (string &optional
                                  (eof-error-p t)
                                  (eof-value nil)
                                  &key
                                  (start 0)
                                  (end nil)
                                  (preserve-whitespace nil))
    (eclector.parse-result:read-from-string
     (or eclector.reader:*client* *cst-client*)
     string eof-error-p eof-value :start start :end end
     :preserve-whitespace preserve-whitespace)))
