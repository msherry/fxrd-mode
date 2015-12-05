(defgroup FXRD nil
  "Major mode for editing fixed field width files"
  :group 'convenience)

(defface fxrd-current-field-face
  '((t (:inherit highlight
        :background "pink")))
  "Highlight the current field."
  :group 'FXRD)
(defvar fxrd-current-field-face 'fxrd-current-field-face)

(defconst fxrd-mode-line-help-echo
  ;; See bindings.el for details of `mode-line-format' construction.
  (get-text-property 0 'help-echo (car default-mode-line-format))
  "Primary default mode line help echo text.")

(defconst fxrd-mode-line-format
  ;; See bindings.el for details of `mode-line-format' construction.
  (append (butlast default-mode-line-format 2)
	  (cons `(fxrd-field-name-string
		  ("" fxrd-field-name-string
		   ,(propertize "" 'help-echo fxrd-mode-line-help-echo)))
		(last default-mode-line-format 2)))
  "Mode line format string for FXRD mode.")

(defconst fxrd-font-lock-keywords-1
  (list
   '()
   '("\\('\\w*'\\)" . font-lock-variable-name-face))
  "Minimal highlighting expressions for FXRD mode")

(defvar fxrd-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-<right>") 'next-field)
    (define-key map (kbd "M-<left>") 'previous-field)
    map)
  "Keymap for FXRD major mode")

(defvar fxrd-mode-syntax-table
  (let ((st (make-syntax-table)))
    st))

(defvar fxrd-font-lock-keywords fxrd-font-lock-keywords-1
  "Default highlighting expressions for FXRD mode")

(defvar fxrd-current-spec nil)
(make-variable-buffer-local 'fxrd-current-spec)

(defvar fxrd-mode-hook nil)

(defun disable-fxrd-mode ()
  (fxrd-field-name-mode -1)
  (fxrd-clear-overlays))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fxrd file specifications
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst tso6-spec
  ;; Header Record
  '(("H" (
          (1 1 "Record Type (H)")
          (2 9 "Record Date")
          (10 15 "Record Time")
          (16 26 "Member ICA")
          (27 86 "File Name")
          (87 201 "Filler")))
    ;; Data Record
    ("D" (
          (1 1 "Record Type (D)")
          (2 31 "Bank Customer Number")
          (32 50 "Bank Account Number")
          (51 70 "Bank Product Code")
          (71 92 "Transaction Description")
          (93 105 "Rebate Amount")
          (106 106 "Exception Reason Code")
          (107 136 "Exception Reason Description")
          (137 144 "Rebate File Sent Date")
          (145 157 "Transaction Sequence Number")
          (157 201 "Filler")))
    ;; Trailer Record
    ("T" (
          (1 1 "Record Type (T)")
          (2 13 "Exception Record Count")
          (14 25 "Success Record Count")
          (26 37 "Total Processed Record Count")
          (38 48 "Member ICA")
          (49 201 "Filler")))))

(defconst nacha-spec
  ;; File Header Record
  '(("1" (
          (1 1 "Record Type (1)" "1")
          (2 3 "Priority Code" "N")
          (4 13 "Immediate Destination")
          (14 23 "Immediate Origin")
          (24 29 "File Creation Date")
          (30 33 "File Creation Time")
          (34 34 "File ID Modifier")
          (35 37 "Record Size")
          (38 39 "Blocking Factor")
          (40 40 "Format Code")
          (41 63 "Immediate Destination Name")
          (64 86 "Immediate Origin Name")
          (87 94 "Reference Code")))
    ;; Batch Header Record
    ("5" (
          (1 1 "Record Type (5)")
          (2 4 "Service Class Code")
          (5 20 "Company Name")
          (21 40 "Company Discretionary Data")
          (41 50 "Company Identification")
          (51 53 "Standard Entry Class Code")
          (54 63 "Company Entry Description")
          (64 69 "Company Descriptive Date")
          (70 75 "Effective Entry Date")
          (76 78 "Settlement Date")
          (79 79 "Originator Status Code")
          (80 87 "Originating DFI Identification")
          (88 94 "Batch Number")))
    ;; Entry Detail Record/Report
    ("6" (
          (1 1 "Record Type (6)")
          ;; TODO: transaction code parsing/descriptions
          (2 3 "Transaction Code")
          (4 11 "Receiving DFI Identification")
          (12 12 "Check Digit")
          (13 29 "DFI Account Number")
          (30 39 "Amount")
          (40 54 "Individual Identification Number")
          (55 76 "Individual Name")
          (77 78 "Discretionary Data")
          (79 79 "Addenda Record Indicator")
          (80 94 "Trace Number")))
    ;; CCD Addenda Record
    ("7" (
          (1 1 "Record Type (7)")
          (2 3 "Addenda Type Code")
          (4 83 "Payment Related Information")
          (84 87 "Addenda Sequence Number")
          (88 94 "Entry Detail Sequence Number")))
    ;; Batch Control Record
    ("8" (
          (1 1 "Record Type (8)")
          (2 4 "Service Class Code")
          (5 10 "Entry/Addenda Count")
          (11 20 "Entry Hash")
          (21 32 "Total Debit Entry Dollar Amount")
          (33 44 "Total Credit Entry Dollar Amount")
          (45 54 "Company Identification")
          (55 73 "Message Authentication Code")
          (74 79 "Reserved")
          (80 87 "Originating DFI Identification")
          (88 94 "Batch Number")))
    ;; File Control Record
    ("9" (
          (1 1 "Record Type (9)")
          (2 7 "Batch Count")
          (8 13 "Block Count")
          (14 21 "Entry/Addenda Count")
          (22 31 "Entry Hash")
          (32 43 "Total Debit Entry Dollar Amount in File")
          (44 55 "Total Credit Entry Dollar Amount in File")
          (56 94 "Reserved")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun current-line-pos ()
  "Yields the current position within the line"
  (+ 1 (- (point) (line-beginning-position))))

(defun get-spec-for-line ()
  "Finds the correct record spec to use for the current line, based on the first character."
  (when fxrd-current-spec
    (let* ((type (line-type))
           (record-spec (cadr (assoc type fxrd-current-spec))))
      record-spec)))

(defun first-spec-hit (record-spec pos)
  "Given a record spec and a position, find and return the first spec-item hit.

Returns nil if no hit found"
  (dolist (spec-item record-spec)
    (let ((start (nth 0 spec-item))
          (end (nth 1 spec-item)))
      (when (and (<= start pos end))
        (return spec-item)))))

(defun get-name-from-spec-item (spec-item)
  "Given a spec item, extract the name part."
  (nth 2 spec-item))

(defun line-type ()
  "Determines the record type of the current line"
  (let* ((char (char-after (line-beginning-position)))
         (type (if char (char-to-string char))))
    type))

(defun current-field-name ()
  "Find the name of the field at the current position in the current line."
  (let ((record-spec (get-spec-for-line)))
    (if record-spec
        ;; TODO: find a better way to find position within a line
        (let ((line-pos (current-line-pos)))
          (get-name-from-spec-item (first-spec-hit record-spec line-pos))))))

(defun current-field-boundaries ()
  "Find the (absolute) start and end position of the field at the current position."
  (let ((record-spec (get-spec-for-line)))
    (if record-spec
        ;; TODO: find a better way to find position within a line
        (let ((line-pos (current-line-pos)))
          (let* ((line-start (line-beginning-position))
                 (spec-item (first-spec-hit record-spec line-pos)))
            (when spec-item
              (let ((start (1- (+ line-start (nth 0 spec-item))))
                    (end (+ line-start (nth 1 spec-item))))
                (list start end))))))))

(defun fxrd-clear-overlays ()
  (remove-overlays nil nil 'fxrd-overlay t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun next-field ()
  "Move to the start of the next field."
  (interactive)
  (let* ((field-boundaries (current-field-boundaries))
         (next-field-start (nth 1 field-boundaries)))
    (goto-char (min next-field-start (point-max)))))

(defun previous-field ()
  "Move to the start of the previous field."
  (interactive)
  (let ((field-boundaries (current-field-boundaries)))
    (when field-boundaries
      (let ((prev-field-end (1- (nth 0 field-boundaries))))
        (goto-char (max prev-field-end (point-min)))
        (let ((prev-field-boundaries (current-field-boundaries)))
          (when prev-field-boundaries
            (let ((begin (nth 0 prev-field-boundaries)))
              (goto-char begin))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Field name mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom fxrd-field-name-delay 0.125
  "Time in seconds to delay before updating field name display."
  :group 'FXRD
  :type '(number :tag "seconds"))

(defvar fxrd-field-name-idle-timer nil)

(defvar fxrd-field-name-string nil)
(make-variable-buffer-local 'fxrd-field-name-string)

(defvar fxrd-field-name-string-old nil)
(make-variable-buffer-local 'fxrd-field-name-string-old)

(define-minor-mode fxrd-field-name-mode
  "Toggle FXRD-field-name mode.
When enabled, the name of the current field appears in the mode line."
  :group 'FXRD
  :global t
  :init-value t
  ;; First, always disable current timer to avoid having two timers.
  (when fxrd-field-name-idle-timer
    (cancel-timer fxrd-field-name-idle-timer)
    (setq fxrd-field-name-idle-timer nil))
  ;; Now, if mode is on and any buffer is in FXRD mode then re-initialize and
  ;; enable by setting up a new timer
  (if fxrd-field-name-mode
      (if (memq t (mapcar (lambda (buffer)
                            (with-current-buffer buffer
                              (when (derived-mode-p'fxrd-mode)
                                (setq fxrd-field-name-string nil
                                      fxrd-field-name-string-old nil)
                                t)))
                          (buffer-list)))
          (setq fxrd-field-name-idle-timer
                (run-with-idle-timer fxrd-field-name-delay t
                                     'fxrd-field-name-display)))
    ;; but if the mode is off then remove the display from the mode lines of
    ;; all FXRD buffers
    (mapc (lambda (buffer)
            (with-current-buffer buffer
              (when (derived-mode-p 'fxrd-mode)
                (setq fxrd-field-name-string nil
                      fxrd-field-name-string-old nil)
                (force-mode-line-update)
                (fxrd-clear-overlays))))
          (buffer-list))))

(defun fxrd-field-name-display ()
  "Construct `fxrd-field-name-string' to display in mode line.
Called by `fxrd-field-name-idle-timer'."
  (if (derived-mode-p 'fxrd-mode)
      (let ((field-name (current-field-name))
            (field-boundaries (current-field-boundaries)))
        (when (not (string= field-name fxrd-field-name-string-old))
          ;; Update modeline
          (setq fxrd-field-name-string-old field-name
                fxrd-field-name-string
                (and field-name (propertize (format "%s" field-name)
                                            'help-echo fxrd-mode-line-help-echo)))
          (force-mode-line-update)
          ;; Highlight current field
          (when field-boundaries
            (remove-overlays nil nil 'fxrd-overlay t)
            (let* ((line-start (line-beginning-position))
                   (begin (nth 0 field-boundaries))
                   (end (nth 1 field-boundaries))
                   (overlay (make-overlay begin end)))
              (overlay-put overlay 'fxrd-overlay t)
              (overlay-put overlay 'face fxrd-current-field-face)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Entry point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-derived-mode fxrd-mode nil "FXRD"
  "Major mode for editing fixed field width files.

\\{fxrd-mode-map}"
  :group 'fxrd
  :syntax-table fxrd-mode-syntax-table
  (use-local-map fxrd-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(fxrd-font-lock-keywords))
  (set (make-local-variable 'mode-line-format) fxrd-mode-line-format)
  (set (make-local-variable 'show-trailing-whitespace) nil)
  (fxrd-field-name-mode 1)
  (overwrite-mode)
  (remove-hook 'before-save-hook 'delete-trailing-whitespace)
  (add-hook (make-local-variable 'change-major-mode-hook) 'disable-fxrd-mode))

;;;###autoload
(define-derived-mode tso6-mode fxrd-mode "TSO6"
  "Major mode for editing TSO6 fixed field width files.

\\{fxrd-mode-map}"
  (setq fxrd-current-spec tso6-spec))

;;;###autoload
(define-derived-mode nacha-mode fxrd-mode "NACHA"
  "Major mode for editing NACHA fixed field width files.

\\{fxrd-mode-map}"
  (setq fxrd-current-spec nacha-spec))

(provide 'fxrd-mode)
