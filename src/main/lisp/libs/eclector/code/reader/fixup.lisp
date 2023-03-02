(cl:in-package #:eclector.reader)

(defmethod fixup :around (client object seen-objects mapping)
  (declare (ignore client mapping))
  (unless (gethash object seen-objects)
    (setf (gethash object seen-objects) t)
    (call-next-method)))

(defmethod fixup (client object seen-objects mapping)
  (declare (ignore client object seen-objects mapping))
  nil)

(macrolet ((fixup-place (place)
             `(let ((current-value ,place))
                (multiple-value-bind (value found-p)
                    (gethash current-value mapping)
                  (if found-p
                      (setf ,place value)
                      (fixup client current-value seen-objects mapping))))))

  (defmethod fixup (client (object cons) seen-objects mapping)
    (fixup-place (car object))
    (fixup-place (cdr object)))

  (defmethod fixup (client (object array) seen-objects mapping)
    (loop for i from 0 below (array-total-size object)
          do (fixup-place (row-major-aref object i))))

  (defmethod fixup (client (object standard-object) seen-objects mapping)
    (loop for slot-definition in (closer-mop:class-slots (class-of object))
          for name = (closer-mop:slot-definition-name slot-definition)
          when (slot-boundp object name)
          do (fixup-place (slot-value object name))))

  (defmethod fixup (client (object hash-table) seen-objects mapping)
    (maphash (lambda (key val)
               (multiple-value-bind (value found-p)
                   (gethash val mapping)
                 (if found-p
                     (setf (gethash key object) value)
                     (fixup client value seen-objects mapping))))
             object)))
