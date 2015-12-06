(autoload 'tso6-mode "fxrd-mode" "TSO6 mode" t)
(add-to-list 'auto-mode-alist '("\\.tso6\\($\\|\\.\\)" . tso6-mode))
(add-to-list 'auto-mode-alist '("\\.tso8\\($\\|\\.\\)" . tso6-mode))

(autoload 'nacha-mode "fxrd-mode" "NACHA mode" t)
(add-to-list 'auto-mode-alist '("\\.nacha\\($\\|\\.\\)" . nacha-mode))

(provide 'fxrd-autoloads)
