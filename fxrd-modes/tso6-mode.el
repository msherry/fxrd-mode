(require 'fxrd-mode)
(require 'fxrd-validators)

;;; Mastercard rebate confirmation file
(defconst tso6-spec
  ;; Header Record
  `(("H" (
          (1 1 "Record Type (H)" ,(fxrd-const "H"))
          (2 9 "Record Date" fxrd-numeric)
          (10 15 "Record Time" fxrd-numeric)
          (16 26 "Member ICA" fxrd-numeric)
          ;; Undocumented, but supported by Mastercard
          (27 86 "File Name" fxrd-alphanumeric)
          (87 201 "Filler" fxrd-alphanumeric)))
    ;; Data Record
    ("D" (
          (1 1 "Record Type (D)" ,(fxrd-const "D"))
          (2 31 "Bank Customer Number" fxrd-alphanumeric)
          (32 50 "Bank Account Number" fxrd-padded-numeric)
          (51 70 "Bank Product Code" fxrd-alphanumeric)
          (71 92 "Transaction Description" fxrd-alphanumeric)
          (93 105 "Rebate Amount" fxrd-padded-decimal-numeric)
          (106 106 "Exception Reason Code" fxrd-alphanumeric) ;TODO: enums
          (107 136 "Exception Reason Description" fxrd-alphanumeric)
          (137 144 "Rebate File Sent Date" fxrd-numeric) ;TODO: date
          ;; NOTE: Mastercard's spec says numeric, but they allow alphanumerics
          (145 157 "Transaction Sequence Number" fxrd-alphanumeric)
          (157 201 "Filler" fxrd-alphanumeric)))
    ;; Trailer Record
    ("T" (
          (1 1 "Record Type (T)" ,(fxrd-const "T"))
          (2 13 "Exception Record Count" fxrd-numeric)
          (14 25 "Success Record Count" fxrd-numeric)
          (26 37 "Total Processed Record Count" fxrd-numeric)
          (38 48 "Member ICA" fxrd-numeric)
          (49 201 "Filler" fxrd-alphanumeric)))))

;;;###autoload
(define-derived-mode tso6-mode fxrd-mode "TSO6"
  "Major mode for editing TSO6 fixed field width files.

\\{fxrd-mode-map}"
  (setq fxrd-current-spec tso6-spec))


(autoload 'tso6-mode "fxrd-mode" "TSO6 mode" t)
(add-to-list 'auto-mode-alist '("\\.tso6\\($\\|\\.\\)" . tso6-mode))
(add-to-list 'auto-mode-alist '("\\.tso8\\($\\|\\.\\)" . tso6-mode))

(provide 'tso6-mode)
