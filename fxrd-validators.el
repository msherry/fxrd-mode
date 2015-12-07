;;; fxrd-validators.el --- Validators for fixed-field-width files  -*- lexical-binding:t -*-
;;; We need lexical-binding so we can create closures.

(require 'eieio-base)

(defclass fxrd-validator (eieio-named)
  ((pad :initarg :pad
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
         :documentation "Possible enum values for this field"))
  "The base validator class for all field validation types")
(defmethod fxrd-validate (val field)
  "Validate the field with the given validator"
  t)

(defclass fxrd-numeric-v (fxrd-validator)
  ((pad :initform "0")))
(defmethod fxrd-validate ((val fxrd-numeric-v) field-value)
  (let ((const (slot-value val 'const)))
    (and (string-match (concat "^" (slot-value val 'pad) "*" "[[:digit:]]*$")
                       field-value)
         ;; If const is set, we must match it
         (if const (eq const (string-to-int field-value))
           t))))

(defclass fxrd-decimal-v (fxrd-numeric-v) ()
  "Numeric fields with a decimal point (floating-point values)")
(defmethod fxrd-validate ((val fxrd-decimal-v) field-value)
  (string-match (concat "^" (slot-value val 'pad) "*"
                        "[[:digit:]]*\\(\\.[[:digit:]]+\\)$")
                field-value))

(defclass fxrd-alphanumeric-v (fxrd-validator) ()
  "Any printable characters")
(defmethod fxrd-validate ((val fxrd-alphanumeric-v) field-value)
  (let ((const (slot-value val 'const)))
    (and (string-match "^[[:print:]]*$" field-value)
         ;; If const is set, we must match it
         (if const (string= const field-value)
           t))))

(provide 'fxrd-validators)
