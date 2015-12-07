(require 'fxrd-mode)
(require 'fxrd-validators)

;;; Nacha transaction file
(defconst nacha-spec
  ;; File Header Record
  `(("1" (
          (1 1 "Record Type (1)" ,(fxrd-const "1"))
          (2 3 "Priority Code" fxrd-numeric)
          (4 13 "Immediate Destination" fxrd-padded-numeric)
          (14 23 "Immediate Origin" fxrd-numeric)
          (24 29 "File Creation Date" fxrd-numeric) ;TODO: date type
          (30 33 "File Creation Time" fxrd-numeric) ;TODO: time type
          (34 34 "File ID Modifier" fxrd-alphanumeric)
          (35 37 "Record Size" fxrd-numeric) ;TODO: constant (94, zero-padded)
          (38 39 "Blocking Factor" fxrd-numeric) ;TODO: constant 10
          (40 40 "Format Code" ,(fxrd-const "1"))
          (41 63 "Immediate Destination Name" fxrd-alphanumeric)
          (64 86 "Immediate Origin Name" fxrd-alphanumeric)
          (87 94 "Reference Code" fxrd-alphanumeric)))
    ;; Batch Header Record
    ("5" (
          (1 1 "Record Type (5)" ,(fxrd-const "5"))
          (2 4 "Service Class Code" fxrd-numeric)   ;TODO: enums
          (5 20 "Company Name" fxrd-alphanumeric)
          (21 40 "Company Discretionary Data" fxrd-alphanumeric)
          (41 50 "Company Identification" fxrd-alphanumeric)
          (51 53 "Standard Entry Class Code" fxrd-alphanumeric) ;TODO: enums
          (54 63 "Company Entry Description" fxrd-alphanumeric)
          (64 69 "Company Descriptive Date" fxrd-alphanumeric)
          (70 75 "Effective Entry Date" fxrd-numeric)
          (76 78 "Settlement Date" fxrd-alphanumeric)
          (79 79 "Originator Status Code" ,(fxrd-const "1"))
          (80 87 "Originating DFI Identification" fxrd-numeric)
          (88 94 "Batch Number" fxrd-numeric)))
    ;; Entry Detail Record/Report
    ("6" (
          (1 1 "Record Type (6)" ,(fxrd-const "6"))
          ;; TODO: transaction code parsing/descriptions
          (2 3 "Transaction Code" fxrd-numeric) ;TODO: enums
          (4 11 "Receiving DFI Identification" fxrd-numeric)
          (12 12 "Check Digit" fxrd-numeric)
          (13 29 "DFI Account Number" fxrd-alphanumeric) ;TODO: alignment/padding
          (30 39 "Amount" fxrd-numeric)
          (40 54 "Individual Identification Number" fxrd-alphanumeric)
          (55 76 "Individual Name" fxrd-alphanumeric)
          (77 78 "Discretionary Data" fxrd-alphanumeric)
          (79 79 "Addenda Record Indicator" fxrd-numeric)
          (80 94 "Trace Number" fxrd-numeric)))
    ;; CCD Addenda Record
    ("7" (
          (1 1 "Record Type (7)" ,(fxrd-const "7"))
          (2 3 "Addenda Type Code" ,(fxrd-const "05"))
          (4 83 "Payment Related Information" fxrd-alphanumeric)
          (84 87 "Addenda Sequence Number" fxrd-numeric)
          (88 94 "Entry Detail Sequence Number" fxrd-numeric)))
    ;; Batch Control Record
    ("8" (
          (1 1 "Record Type (8)" ,(fxrd-const "8"))
          (2 4 "Service Class Code" fxrd-numeric)
          (5 10 "Entry/Addenda Count" fxrd-numeric)
          (11 20 "Entry Hash" fxrd-numeric)
          (21 32 "Total Debit Entry Dollar Amount" fxrd-numeric)
          (33 44 "Total Credit Entry Dollar Amount" fxrd-numeric)
          (45 54 "Company Identification" fxrd-alphanumeric)
          (55 73 "Message Authentication Code" fxrd-alphanumeric) ;TODO: reserved
          (74 79 "Reserved" fxrd-alphanumeric)
          (80 87 "Originating DFI Identification" fxrd-numeric)
          (88 94 "Batch Number" fxrd-numeric)))
    ;; File Control Record
    ("9" (
          (1 1 "Record Type (9)" ,(fxrd-const "9"))
          (2 7 "Batch Count" fxrd-numeric)
          (8 13 "Block Count" fxrd-numeric)
          (14 21 "Entry/Addenda Count" fxrd-numeric)
          (22 31 "Entry Hash" fxrd-numeric)
          (32 43 "Total Debit Entry Dollar Amount in File" fxrd-numeric)
          (44 55 "Total Credit Entry Dollar Amount in File" fxrd-numeric)
          (56 94 "Reserved" fxrd-alphanumeric)))))


;;;###autoload
(define-derived-mode nacha-mode fxrd-mode "NACHA"
"Major mode for editing NACHA fixed field width files.

\\{fxrd-mode-map}"
(setq fxrd-current-spec nacha-spec))

(autoload 'nacha-mode "fxrd-mode" "NACHA mode" t)
(add-to-list 'auto-mode-alist '("\\.nacha\\($\\|\\.\\)" . nacha-mode))

(provide 'nacha-mode)
