;; Application initialization file for emacs

;; Rob Peters <rjpeters@klab.caltech.edu>

;; $Id$

;;-----------------------------------------------------------------------
;; Turn off the "quit Emacs" key binding
;;-----------------------------------------------------------------------
;;(global-unset-key "\C-x\C-c")

;;-----------------------------------------------------------------------
;; LISP search path
;;-----------------------------------------------------------------------
(setq load-path (append load-path (list "~/.home/elisp")))

;;-----------------------------------------------------------------------
;; General function definitions
;;-----------------------------------------------------------------------
(defun fixup-whitespace ()
  "Untabify the buffer, and kill trailing whitespace on all lines."
  (interactive)
  (untabify (point-min) (point-max))
  (save-excursion
    (save-restriction
      (save-match-data
	(widen)
	(goto-char (point-min))
	(let ((rxp "[ \t]+$"))
	  (while (re-search-forward rxp nil t)
	    (replace-match "" t t)))))))

(defun unmangle-email ()
  "Fix formatting stuff in emails."
  (interactive)
  (save-excursion
    (save-restriction
      (save-match-data
	;; try man ascii or man iso_8859-1 to see character codes

	(goto-char (point-min))
	(while (search-forward "=20" nil t)
	  ;; space
	  (replace-match " " t t))

	(goto-char (point-min))
	(while (search-forward "=2E" nil t)
	  ;; period
	  (replace-match "." t t))

	(goto-char (point-min))
	(while (search-forward "=92" nil t)
	  (replace-match "'" t t))

	(goto-char (point-min))
	(while (search-forward "=96" nil t)
	  (replace-match "-" t t))

	(goto-char (point-min))
	(while (search-forward "=A0" nil t)
	  ;; non-breaking space
	  (replace-match " " t t))

	(goto-char (point-min))
	(while (search-forward "=AD" nil t)
	  ;; non-breaking space
	  (replace-match "-" t t))

	(goto-char (point-min))
	(while (search-forward "=B2" nil t)
	  (replace-match "\"" t t))

	(goto-char (point-min))
	(while (search-forward "=B3" nil t)
	  (replace-match "\"" t t))

	(goto-char (point-min))
	(while (search-forward "=B7" nil t)
	  ;; middle dot
	  (replace-match "*" t t))

	(goto-char (point-min))
	(while (search-forward "=B9" nil t)
	  ;; superscript numeral 1
	  (replace-match "'" t t))

	(goto-char (point-min))
	(while (search-forward "¹" nil t)
	  ;; superscript numeral 1
	  (replace-match "'" t t))

	(goto-char (point-min))
	(while (search-forward "²" nil t)
	  ;; superscript numeral 2
	  (replace-match "\"" t t))

	(goto-char (point-min))
	(while (search-forward "³" nil t)
	  ;; superscript numeral 3
	  (replace-match "\"" t t))

	(goto-char (point-min))
	(while (search-forward "=\n" nil t)
	  (replace-match "" t t))

	(goto-char (point-min))
	(while (search-forward "
" nil t)
	  (replace-match "" t t))

	(goto-char (point-min))
	(while (search-forward "" nil t)
	  (replace-match "..." t t))

	(goto-char (point-min))
	(while (search-forward "" nil t)
	  (replace-match "'" t t))

	(goto-char (point-min))
	(while (search-forward "" nil t)
	  (replace-match "'" t t))

	(goto-char (point-min))
	(while (search-forward "" nil t)
	  (replace-match "\"" t t))

	(goto-char (point-min))
	(while (search-forward "" nil t)
	  (replace-match "\"" t t))

	(goto-char (point-min))
	(while (search-forward "" nil t)
	  (replace-match "---" t t))

	))))

;;-----------------------------------------------------------------------
;; Matlab mode
;;-----------------------------------------------------------------------
(autoload 'matlab-mode "matlab" "Enter Matlab mode." t)
(setq auto-mode-alist (cons '("\\.m$" . matlab-mode) auto-mode-alist))
(defun my-matlab-mode-hook ()
  (add-hook 'local-write-file-hooks 'fixup-whitespace)
  ;; This line controls whether code inside a function gets indented
  ;; one level, or is completely left-justified... I've just changed
  ;; my opinion on this -- I used to not like the extra indentation
  ;; level, but now I think that having the code indented helps to
  ;; visually separate the multiple functions that can exist in a
  ;; single matlab source file.
  (setq matlab-indent-function t)
  (setq matlab-return-function 'matlab-plain-ret)
  (setq matlab-verify-on-save-flag nil)
  (setq matlab-indent-level 4)
  (setq matlab-case-level 4)
  (setq fill-column 70)
  (turn-on-auto-fill))
(setq matlab-mode-hook 'my-matlab-mode-hook)
(autoload 'matlab-shell "matlab" "Interactive Matlab mode." t)

;;-----------------------------------------------------------------------
;; C++ utilities
;;-----------------------------------------------------------------------
(load "ccutil")

;;-----------------------------------------------------------------------
;; BibTeX utilities
;;-----------------------------------------------------------------------
(load "bibtexutil")

;;-----------------------------------------------------------------------
;; General settings
;;-----------------------------------------------------------------------


; Make underscore "_" be considered a word-character instead of a
; whitespace character (so that characters on either side of an
; underscore aren't seen as falling on word boundaries).
(modify-syntax-entry ?_ "w")

; To make underscore "_" be considered whitespace again, do the
; following:
; (modify-syntax-entry ?_ " ")

(setq-default case-fold-search nil)
(setq-default case-replace nil)

(set-foreground-color "white")
(set-background-color "black")

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

(setq text-mode-hook 'turn-on-auto-fill)
(setq latex-mode-hook 'turn-on-auto-fill)

(setq auto-mode-alist (cons '("\\.[ch]+$" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.dxy$" . c++-mode) auto-mode-alist))

;Need to explicitly load "smgl-mode" in order to avoid weird problems
;where sgml documents (eg *.xsl) don't get fontified properly, UNLESS
;a *.html document gets loaded first. This involves errors of this
;variety: "key sequence C-c C-c - uses invalid prefix characters"
(load "sgml-mode")
(setq auto-mode-alist (cons '("\\.xsl$" . sgml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.gvx$" . sgml-mode) auto-mode-alist))

(setq-default column-number-mode t)

(setq-default fill-column 70)
(setq-default default-tab-width 8)

;(setq display-time-24hr-format t)  ; show time in 24hr format
;(setq display-time-day-and-date t) ; show date in addition to time
(setq display-time-format "%a%b%d:%H%M") ; specify mode line date format
(display-time)			   ; show time(+date) in mode line

(defun abbreviate-frame-title (s)
  (setq s (replace-regexp-in-string "^.*science/projects/" "sp::" s))
  (setq s (replace-regexp-in-string "^.*projects/" "p::" s))
  (setq s (replace-regexp-in-string "^/cit/rjpeters/" "c::" s))
  (setq s (replace-regexp-in-string "^/home/rjpeters/" "h::" s))
  (setq s (replace-regexp-in-string "^/lab/rjpeters/" "h::" s))
  (setq s (replace-regexp-in-string "^~/" "h::" s))
  (setq s (replace-regexp-in-string "/$" "" s))
  )

(defun abbreviate-system-name (s)
  (setq s (replace-regexp-in-string "\\.klab\\.caltech\\.edu" "" s))
  (setq s (replace-regexp-in-string "\\.caltech\\.edu" "" s))
  (setq s (replace-regexp-in-string "\\.usc.edu" "" s))
  )

;; can also use (invocation-name) here in the title formats if desired
;; (it's part of the default title formats, but I've left it out for
;; brevity of these customized formats)

(setq frame-title-format
      (concat (abbreviate-frame-title default-directory)
	      " [" (abbreviate-system-name system-name) "]"))

(setq icon-title-format
      (concat (abbreviate-frame-title default-directory)
	      " [" (abbreviate-system-name system-name) "]"))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; to get Mac OSX <return> key to do the same as <kp-enter>
(define-key function-key-map [return] [?\C-m])

(defun use-fancy-splash-screens-p () nil)
(setq inhibit-startup-message t)

;; flash the screen instead of beeping the system bell
;; (setq visible-bell t)

;; don't load site-lisp/default.el
(setq inhibit-default-init t)

(when window-system
  ;; enable wheelmouse support by default
  (mwheel-install)
  ;; make switching frames works properly under the default click-to-focus
  (setq focus-follows-mouse nil))

;; automatically read compressed files
(auto-compression-mode t)

;;-----------------------------------------------------------------------
;; Mail Settings and Aliases
;;-----------------------------------------------------------------------

(setq mail-mode-hook 'turn-on-auto-fill)
(setq mail-yank-prefix "> ")

(setq rmail-file-name "~/mail/Inbox")
(setq mail-archive-file-name "~/mail/Outbox")

;;-----------------------------------------------------------------------
;; Variables set by emacs' customize wizard
;;-----------------------------------------------------------------------

(custom-set-variables
  ;; custom-set-variables was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 '(blink-cursor-delay 1.0)
 '(blink-cursor-interval 0.25)
 '(confirm-kill-emacs (quote yes-or-no-p))
 '(rmail-enable-mime t t)
 '(show-trailing-whitespace t)
 '(user-mail-address "rjpeters@klab.caltech.edu")
 '(widget-image-enable nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 '(cursor ((t (:background "salmon1"))))
 '(font-lock-comment-face ((((class color) (background dark)) (:foreground "firebrick1"))))
 '(font-lock-function-name-face ((((class color) (background dark)) (:foreground "dodger blue" :weight bold))))
 '(font-lock-keyword-face ((((class color) (background dark)) (:foreground "Cyan" :weight bold))))
 '(font-lock-string-face ((((class color) (background dark)) (:foreground "LightSalmon"))))
 '(font-lock-variable-name-face ((((class color) (background dark)) (:foreground "gold"))))
)
