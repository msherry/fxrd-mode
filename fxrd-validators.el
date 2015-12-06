;;; fxrd-validators.el --- Validators for fixed-field-width files  -*- lexical-binding:t -*-
;;; We need lexical-binding so we can create closures.

(defun fxrd-const (const)
  #'(lambda (value)
      (string= value const)))

(defun fxrd-numeric (value)
  (string-match "^[[:digit:]]*$" (current-field-value)))

(defun fxrd-padded-decimal-numeric (value)
  (string-match "^ *[[:digit:]]*\\(\\.[[:digit:]]+\\)$" (current-field-value)))

(defun fxrd-padded-numeric (value)
  (string-match "^ *[[:digit:]]*$" (current-field-value)))

(defun fxrd-alphanumeric (value)
  "Matches any printable character"
  (string-match "^[[:print:]]*$" (current-field-value)))

(provide 'fxrd-validators)
