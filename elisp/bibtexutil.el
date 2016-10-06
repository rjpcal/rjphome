(defvar bibtex-uniqid-re "uniqid[ \t\n]*=[ \t\n]*\\([01-9]+\\),")

(defvar bibtex-last-uniqid 0)

(defun bibtex-find-last-uniqid ()
  (interactive)
  (save-excursion
    (widen)
    (beginning-of-buffer)
    (setq max-id 0)
    (while (re-search-forward bibtex-uniqid-re (point-max) t)
      (if (> (string-to-int (match-string 1)) max-id)
	  (setq max-id (string-to-int (match-string 1)))
	()))
    (setq bibtex-last-uniqid max-id)
    (message "last uniqid is %d" bibtex-last-uniqid)))

(defun bibtex-add-uniqid ()
  (interactive)
  (bibtex-beginning-of-entry)
  (forward-line 1)
  (bibtex-find-last-uniqid)
  (setq bibtex-last-uniqid (+ bibtex-last-uniqid 1))
  (insert "  uniqid =       " (int-to-string bibtex-last-uniqid) ",\n"))

(defun bibtex-find-uniqid (id)
  (interactive "sfind uniqid #: ")
  (beginning-of-buffer)
  (re-search-forward (concat "uniqid[ \t\n]*=[ \t\n]*" id ","))
  (bibtex-beginning-of-entry))

(defun bibtex-recase ()
  (interactive)
  (save-excursion
    (bibtex-find-text t)
    (setq beg (point))
    (bibtex-find-text nil)
    (setq end (point))
    (downcase-region beg end)
    (upcase-initials-region beg end)
    (replace-regexp " An\\>"   " an" nil beg end)
    (replace-regexp " And\\>"  " and" nil beg end)
    (replace-regexp " But\\>"  " but" nil beg end)
    (replace-regexp " For\\>"  " for" nil beg end)
    (replace-regexp " In\\>"   " in" nil beg end)
    (replace-regexp " Of\\>"   " of" nil beg end)
    (replace-regexp " On\\>"   " on" nil beg end)
    (replace-regexp " Or\\>"   " or" nil beg end)
    (replace-regexp " The\\>"  " the" nil beg end)
    (replace-regexp " With\\>" " with" nil beg end)
    )
  )

(defun my-bibtex-mode-hook ()
  (local-set-key "\C-c\C-u" 'bibtex-add-uniqid)
  (local-set-key "\C-c\C-f" 'bibtex-find-uniqid)
  (local-set-key "\C-c\C-a" 'bibtex-recase))

(setq bibtex-mode-hook 'my-bibtex-mode-hook)

(provide 'bibtexutil)

;; bibtexutil.el ends here

