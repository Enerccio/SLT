(cl:in-package #:eclector.reader)

;;; We have to provide our own PEEK-CHAR function because CL:PEEK-CHAR
;;; obviously does not use Eclector's readtable.

(defun peek-char (&optional peek-type
                            (input-stream *standard-input*)
                            (eof-error-p t)
                            eof-value
                            recursive-p)
  (flet ((done (value)
           (cond ((not (eq value '#1=#.(gensym "EOF")))
                  (return-from peek-char value))
                 (eof-error-p
                  (%reader-error input-stream 'end-of-file))
                 (t
                  (return-from peek-char eof-value)))))
    (if (not (eq peek-type t))
        (done (cl:peek-char peek-type input-stream nil '#1# recursive-p))
        (loop with readtable = *readtable*
              for char = (cl:peek-char nil input-stream nil '#1# recursive-p)
              while (and (not (eq char '#1#))
                         (eq (eclector.readtable:syntax-type readtable char)
                             :whitespace))
              do (read-char input-stream) ; consume whitespace char
              finally (done char)))))

;;; Establishing context

(defmethod call-as-top-level-read (client thunk input-stream
                                   eof-error-p eof-value preserve-whitespace-p)
  (declare (ignore eof-error-p eof-value))
  (let* ((labels (make-hash-table))
         (values (multiple-value-list
                  (let ((*labels* labels))
                    (funcall thunk))))
         (result (first values)))
    ;; LABELS maps labels to conses of the form
    ;;
    ;;   (TEMPORARY-OBJECT . FINAL-OBJECT)
    ;;
    ;; where TEMPORARY-OBJECT is EQ-comparable and its sub-structure
    ;; does not matter here. For the fixup step, convert these conses
    ;; into a hash-table mapping temporary objects to final objects.
    (unless (zerop (hash-table-count labels))
      (let ((seen (make-hash-table :test #'eq))
            (mapping (alexandria:alist-hash-table
                      (alexandria:hash-table-values labels)
                      :test #'eq)))
        (fixup client result seen mapping)))
    ;; All reading in READ-COMMON and its callees was done in a
    ;; whitespace-preserving way. So we skip zero to one whitespace
    ;; characters here if requested via PRESERVE-WHITESPACE-P.
    (unless preserve-whitespace-p
      (skip-whitespace input-stream))
    (values-list values)))

(defmethod read-common (client input-stream eof-error-p eof-value)
  (loop for (value what . rest) = (multiple-value-list
                                   (read-maybe-nothing
                                    client input-stream eof-error-p eof-value))
        do (ecase what
             ((:eof :suppress :object)
              (return (apply #'values value rest)))
             ((:whitespace :skip)))))

(defun %read-maybe-nothing (client input-stream eof-error-p eof-value)
  (let ((char (read-char input-stream eof-error-p)))
    (when (null char)
      (return-from %read-maybe-nothing (values eof-value :eof)))
    (let ((readtable *readtable*))
      (case (eclector.readtable:syntax-type readtable char)
        (:whitespace
         (skip-whitespace* input-stream)
         (values nil :whitespace))
        ((:terminating-macro :non-terminating-macro)
         ;; There is no need to consider the value of EOF-ERROR-P in
         ;; reader macros since: "read signals an error of type
         ;; end-of-file, regardless of eof-error-p, if the file ends
         ;; in the middle of an object representation."  (HyperSpec
         ;; entry for READ)
         (let* ((*skip-reason* nil)
                (values (multiple-value-list
                         (call-reader-macro
                          client input-stream char readtable))))
           (cond ((null values)
                  (note-skipped-input client input-stream
                                      (or *skip-reason* :reader-macro))
                  (values nil :skip))
                 ;; This case takes care of reader macro not returning
                 ;; nil when *READ-SUPPRESS* is true.
                 (*read-suppress*
                  (note-skipped-input client input-stream
                                      (or *skip-reason* '*read-suppress*))
                  (values nil :suppress))
                 (t
                  (values (first values) :object)))))
        (t
         (unread-char char input-stream)
         (let* ((*skip-reason* nil)
                (object (read-token client input-stream eof-error-p eof-value)))
           (cond ((and (eq object *consing-dot*)
                       (not *consing-dot-allowed-p*))
                  (%recoverable-reader-error
                   input-stream 'invalid-context-for-consing-dot
                   :position-offset -1 :report 'skip-token)
                  (values nil :skip))
                 (t
                  (values object (if *read-suppress* :suppress :object))))))))))

(defmethod read-maybe-nothing (client input-stream eof-error-p eof-value)
  (%read-maybe-nothing client input-stream eof-error-p eof-value))
