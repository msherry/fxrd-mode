(defun load-all-modes (dir)
  "Loads all modes under a given directory"
  (let ((libraries-loaded (mapcar #'file-name-sans-extension
                                  (delq nil (mapcar #'car load-history)))))
    (dolist (file (directory-files dir t ".+\\.elc?$"))
      (let ((library (file-name-sans-extension file)))
        (unless (member library libraries-loaded)
          (load library nil t)
          (push library libraries-loaded))))))

(load-all-modes (concat (file-name-directory load-file-name) "fxrd-modes"))

(provide 'fxrd-autoloads)
