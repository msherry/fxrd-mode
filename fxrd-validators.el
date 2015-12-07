;;; fxrd-validators.el --- Validators for fixed-field-width files  -*- lexical-binding:t -*-
;;; We need lexical-binding so we can create closures.

(require 'eieio-base)

(defclass fxrd-validator (eieio-named)
  (;; Public slots
   (pad :initarg :pad
        :initform ""
        :type string
        :custom string
        :documentation "The value to pad with")
   (align :initarg :align
          :initform "RIGHT"
          :type string
          :custom string
          :documentation "The alignment of the field")
   (const :initarg :const
          :initform nil
          :type (or null integer string)
          :custom (or null integer string)
          :documentation "A constant value for this field")
   (enum :initarg :enum
         :initform ()
         :type list
         :custom list
         :documentation "Possible enum values for this field")
   ;; Private slots
   (const-transform :initform #'identity
                    :documentation "Transform to be used when validating const fields")
   (const-eq :initform #'eq
             :documentation "Equality function for const values")
   (regex :initform "^.*$"
          :documentation "Regex to validate field against"))
  "The base validator class for all field validation types")
(defmethod fxrd-validate (val field)
  "Validate the field with the given validator"
  t)

(defun fxrd-general-validator (val field-value)
  (let ((const (slot-value val 'const))
        (const-transform (slot-value val 'const-transform))
        (const-eq (slot-value val 'const-eq))
        (pad (slot-value val 'pad))
        (regex (slot-value val 'regex)))
    ;; TODO: alignment goes here
    (and (string-match (concat "^" pad "*" regex "$") field-value)
         ;; If const is set, we must match it
         (if const (funcall const-eq const (funcall const-transform field-value))
           t))))


(defclass fxrd-numeric-v (fxrd-validator)
  ((pad :initform "0")
   (const-transform :initform #'string-to-int)
   (regex :initform "[[:digit:]]*"))
  "Integer fields")
(defmethod fxrd-validate ((val fxrd-numeric-v) field-value)
  (fxrd-general-validator val field-value))

(defclass fxrd-decimal-v (fxrd-numeric-v)
  ((const-transform :initform #'string-to-number)
   (regex :initform "[[:digit:]]*\\(\\.[[:digit:]]+\\)"))
  "Numeric fields with a decimal point (floating-point values)")
(defmethod fxrd-validate ((val fxrd-decimal-v) field-value)
  (fxrd-general-validator val field-value))

(defclass fxrd-alphanumeric-v (fxrd-validator)
  ((pad :initform " ")
   (const-eq :initform #'string=)
   (regex :initform "[[:print:]]*" field-value))
  "Any printable characters")
(defmethod fxrd-validate ((val fxrd-alphanumeric-v) field-value)
  (fxrd-general-validator val field-value))

(provide 'fxrd-validators)
