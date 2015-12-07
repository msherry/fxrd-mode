(require 'fxrd-mode)
(require 'fxrd-validators)

;;; Mastercard rebate transaction file

(defconst rm37-spec
  ;; File Header Record
  `(("H" (
          (1 1 "Record Type (H)" ,(fxrd-const "H"))
          (2 9 "Record Date" fxrd-numeric)
          (10 15 "Record Time" fxrd-numeric)
          (16 26 "Member ICA" fxrd-numeric)
          ;; Undocumented, but supported by Mastercard
          (27 86 "File Name" fxrd-alphanumeric)
          (87 201 "Filler" fxrd-alphanumeric)))
    ("D" (
          (1 1 "Record Type (D)" ,(fxrd-const "D"))
          ;; NOTE: Mastercard's spec says numeric, but they allow alphanumerics
          (2 14 "Transaction Sequence Number" fxrd-alphanumeric)
          ;; NOTE: spec says numeric, but omitting this is fine.
          ;; TODO: space-padded numeric type
          (15 33 "Bank Account Number" fxrd-alphanumeric)
          (34 46 "Transaction Amount" fxrd-padded-decimal-numeric)
          (47 54 "Transaction Date" fxrd-numeric)
          (55 67 "Rebate Amount" fxrd-padded-decimal-numeric)
          (68 71 "Merchant Category Code" fxrd-alphanumeric)
          (72 93 "Transaction Description" fxrd-alphanumeric)
          (94 94 "Reversal Indicator" fxrd-alphanumeric)
          (95 116 "Merchant ID" fxrd-alphanumeric)
          (117 122 "Issuer ICA Code" fxrd-numeric)
          (123 124 "Program Code" fxrd-alphanumeric)
          (125 144 "Bank Product Code" fxrd-alphanumeric)
          (145 174 "Bank Customer Number" fxrd-alphanumeric)
          (175 200 "Filler" fxrd-alphanumeric)))
    ("T" (
          (1 1 "Record Type (T)" ,(fxrd-const "T"))
          (2 13 "Record Count" fxrd-numeric)
          (14 24 "Member ICA" fxrd-numeric)
          (25 200 "Filler" fxrd-alphanumeric)))))

;;;###autoload
(define-derived-mode rm37-mode fxrd-mode "RM37"
  "Major mode for editing RM37 fixed field width files.

\\{fxrd-mode-map}"
  (setq fxrd-current-spec rm37-spec))

(autoload 'rm37-mode "fxrd-mode" "RM37 mode" t)
(add-to-list 'auto-mode-alist '("\\.rm37\\($\\|\\.\\)" . rm37-mode))
(add-to-list 'auto-mode-alist '("\\.rm39\\($\\|\\.\\)" . rm37-mode))

(provide 'rm37-mode)
