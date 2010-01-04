;; Application initialization file for emacs -*- emacs-lisp -*-

;; Rob Peters <rjpeters@klab.caltech.edu>

;; $Id$

;;-----------------------------------------------------------------------
;; Turn off the "quit Emacs" key binding
;;-----------------------------------------------------------------------
;;(global-unset-key "\C-x\C-c")

;(global-set-key "O5A" 'backward-paragraph) ; ctrl-up-arrow
(global-set-key "\eO5A" 'backward-paragraph) ; ctrl-up-arrow
(global-set-key "\eO5B" 'forward-paragraph)  ; ctrl-down-arrow
(global-set-key "\eO5C" 'forward-word)       ; ctrl-right-arrow
(global-set-key "\eO5D" 'backward-word)      ; ctrl-left-arrow

(global-set-key "\e[5A" 'backward-paragraph) ; ctrl-up-arrow
(global-set-key "\e[5B" 'forward-paragraph)  ; ctrl-down-arrow
(global-set-key "\e[5C" 'forward-word)       ; ctrl-right-arrow
(global-set-key "\e[5D" 'backward-word)      ; ctrl-left-arrow

(global-set-key "\e[A" 'backward-paragraph) ; ctrl-up-arrow
(global-set-key "\e[B" 'forward-paragraph)  ; ctrl-down-arrow
(global-set-key "\e[C" 'forward-word)       ; ctrl-right-arrow
(global-set-key "\e[D" 'backward-word)      ; ctrl-left-arrow

(global-set-key "\e[1;5A" 'backward-paragraph) ; ctrl-up-arrow
(global-set-key "\e[1;5B" 'forward-paragraph)  ; ctrl-down-arrow
(global-set-key "\e[1;5C" 'forward-word)       ; ctrl-right-arrow
(global-set-key "\e[1;5D" 'backward-word)      ; ctrl-left-arrow

;;-----------------------------------------------------------------------
;; LISP search path
;;-----------------------------------------------------------------------
(setq load-path (append load-path (list "~/home/base/elisp")))

(if (member 'SVN vc-handled-backends)
    ()
  (load "~/home/base/elisp/vc-svn.el")
  (add-to-list 'vc-handled-backends 'SVN))

;(load "~/home/base/elisp/vc-svn.el")
;(add-to-list 'vc-handled-backends 'SVN)

;;-----------------------------------------------------------------------
;; Org mode
;;-----------------------------------------------------------------------

(setq load-path (cons "~/home/base/elisp/org-6.33f/lisp" load-path))
;; (load "~/home/base/elisp/org.elc")

(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-log-done t)
(setq org-agenda-files (list "~/home/org/gtd.org"))
(setq org-agenda-custom-commands
      '(("f" tags "FUTUREPROJECT" nil)

	("n" tags-todo "WORK/NEXT" nil)
	("o" tags-todo "HOME/NEXT" nil)
	("p" tags-todo "ERRAND/NEXT" nil)
	("q" tags-todo "COMMUTE/NEXT" nil)

	("u" tags-todo "-WORK-HOME-ERRAND-COMMUTE" nil)
	("v" tags-todo "WORK" nil)
	("w" tags-todo "HOME" nil)
	("x" tags-todo "ERRAND" nil)
	("y" tags-todo "COMMUTE" nil)

	("N" todo "NEXT" nil)
	("W" todo "WAITING" nil)
	))

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

(defun unhtml-region ()
  "Kill HTML tags."
  (interactive)
  (save-excursion
    (save-restriction
      (save-match-data

        (narrow-to-region (point) (mark))

        (goto-char (point-min))
        (while (re-search-forward "<BR>" nil t)
          (replace-match "\n" t t))

        (goto-char (point-min))
        (while (re-search-forward "\\(<[^>]+>\\)+" nil t)
          (replace-match " " t t))

        (goto-char (point-min))
        (while (search-forward "&nbsp;" nil t)
          (replace-match " " t t))

        (goto-char (point-min))
        (while (search-forward "&gt;" nil t)
          (replace-match ">" t t))

        (goto-char (point-min))
        (while (search-forward "&lt;" nil t)
          (replace-match "<" t t))

        ))))

(defun unmangle-region ()
  "Fix formatting stuff in emails."
  (interactive)
  (save-excursion
    (save-restriction
      (save-match-data

	(narrow-to-region (point) (mark))

        ;; this one needs to stay first
        (goto-char (point-min))
        (while (search-forward "=\n" nil t)
          (replace-match "" t t))

        ;; try man ascii or man iso_8859-1 to see character codes

        (goto-char (point-min))
        (while (search-forward "=0A" nil t)
          ;; newline
          (replace-match "\n" t t))

        (goto-char (point-min))
        (while (search-forward "=0D" nil t)
          ;; newline
          (replace-match "\n" t t))

        (goto-char (point-min))
        (while (search-forward "=20" nil t)
          ;; space
          (replace-match " " t t))

        (goto-char (point-min))
        (while (search-forward "=21" nil t)
          (replace-match "!" t t))

        (goto-char (point-min))
        (while (search-forward "=22" nil t)
          (replace-match "\"" t t))

        (goto-char (point-min))
        (while (search-forward "=23" nil t)
          (replace-match "#" t t))

        (goto-char (point-min))
        (while (search-forward "=24" nil t)
          (replace-match "$" t t))

        (goto-char (point-min))
        (while (search-forward "=2C" nil t)
          ;; comma
          (replace-match "," t t))

        (goto-char (point-min))
        (while (search-forward "=2E" nil t)
          ;; period
          (replace-match "." t t))

        (goto-char (point-min))
        (while (search-forward "=3D" nil t)
          (replace-match "=" t t))

        (goto-char (point-min))
        (while (search-forward "=40" nil t)
          (replace-match "@" t t))

        (goto-char (point-min))
        (while (search-forward "=92" nil t)
          (replace-match "'" t t))

        (goto-char (point-min))
        (while (search-forward "=93" nil t)
          (replace-match "\"" t t))

        (goto-char (point-min))
        (while (search-forward "=94" nil t)
          (replace-match "\"" t t))

        (goto-char (point-min))
        (while (search-forward "=96" nil t)
          (replace-match "-" t t))

        (goto-char (point-min))
        (while (search-forward "=A0" nil t)
          ;; non-breaking space
          (replace-match " " t t))

        (goto-char (point-min))
        (while (search-forward "  " nil t)
          ;; space
          (replace-match " " t t))

        (goto-char (point-min))
        (while (search-forward " " nil t)
          ;; space
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
        (while (search-forward "=E9" nil t)
          ;; 'e' with accent acute
          (replace-match "e" t t))

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
        (while (search-forward "“" nil t)
          (replace-match "\"" t t))

        (goto-char (point-min))
        (while (search-forward "”" nil t)
          (replace-match "\"" t t))

        (goto-char (point-min))
        (while (search-forward "’" nil t)
          (replace-match "'" t t))

        (goto-char (point-min))
        (while (search-forward "´" nil t)
          (replace-match "'" t t))

        (goto-char (point-min))
        (while (search-forward "‘" nil t)
          (replace-match "'" t t))

        (goto-char (point-min))
        (while (search-forward "–" nil t)
          (replace-match "--" t t))

        (goto-char (point-min))
        (while (search-forward "" nil t)
          (replace-match "..." t t))

        (goto-char (point-min))
        (while (search-forward "…" nil t)
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
          (replace-match "--" t t))

        (goto-char (point-min))
        (while (search-forward "" nil t)
          (replace-match "--" t t))

        ))))

(require 'sort)
(require 'rmail)

(defun rmail-sort-by-spam-level (reverse)
  "Sort messages of current Rmail file by X-Spam-Level.
If prefix argument REVERSE is non-nil, sort them in reverse order."
  (interactive "P")
  (rmail-sort-messages reverse
		       (function
			(lambda (msg)
			  (let ((key (or (rmail-fetch-field msg "X-Spam-Status") ""))
				(case-fold-search t))
			    (if (string-match "score=\\(-?[0-9]+.[0-9]+\\)" key)
				(string-to-number (substring key (match-beginning 1) (match-end 1)))
			      0))))))

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
;; C++
;;-----------------------------------------------------------------------
(load "ccutil")

(setq auto-mode-alist (cons '("\\.[ch]+$" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.dxy$" . c++-mode) auto-mode-alist))

;;-----------------------------------------------------------------------
;; LaTeX
;;-----------------------------------------------------------------------
(defun my-latex-mode-hook ()
  ;;(setq line-spacing 4)
  (turn-on-auto-fill)
  ;;(set-face-attribute 'default nil :font "-*-LucidaTypewriter-Medium-R-Normal-Sans-12-120-*-*-*-*-*-*")
)

(add-hook 'latex-mode-hook 'my-latex-mode-hook)

;;-----------------------------------------------------------------------
;; BibTeX
;;-----------------------------------------------------------------------
(load "bibtexutil")

;;-----------------------------------------------------------------------
;; outline-mode
;;-----------------------------------------------------------------------

(make-face 'outline-done-face)
(set-face-attribute 'outline-done-face nil
                    :foreground "dodgerblue4"
                    :strike-through t
                    )

(make-face 'outline-todo-face)
(set-face-attribute 'outline-todo-face nil
                    :foreground "firebrick1"
                    :box '(:line-width 1)
                    )

(make-face 'outline-todo-home-face)
(set-face-attribute 'outline-todo-home-face nil
                    :foreground "gold1"
                    :box '(:line-width 1)
                    )

(make-face 'outline-todo-lab-face)
(set-face-attribute 'outline-todo-lab-face nil
                    :foreground "springgreen1"
                    :box '(:line-width 1)
                    )

(font-lock-add-keywords
 'outline-mode
 '( ("^[ \t\\*]*<< \\(.*\\)$" 1 'outline-done-face t)
    ("^[ \t\\*]*>> \\(.*\\)$" 1 'outline-todo-face t)
    ("^[ \t\\*]*>> \\(HOME.*\\)$" 1 'outline-todo-home-face t)
    ("^[ \t\\*]*>> \\(LAB.*\\)$" 1 'outline-todo-lab-face t)
    ))

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

(setq-default case-replace nil)
(setq-default tags-case-fold-search nil)

;(set-foreground-color "white")
;(set-background-color "black")
(set-foreground-color "black")
(set-background-color "white")

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

(setq text-mode-hook 'turn-on-auto-fill)

; Handle temporary svn commit log files in text-mode
(setq auto-mode-alist (cons '("svn-commit.*\\.tmp$" . text-mode) auto-mode-alist))

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
(display-time)                     ; show time(+date) in mode line

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

(when (string-equal window-system "x")
  ;; enable wheelmouse support by default
  (mwheel-install)
  ;; make switching frames works properly under the default click-to-focus
  (setq focus-follows-mouse nil)
  ;; set default font
  ; (set-face-attribute 'default nil :font "-*-LucidaTypewriter-Medium-R-Normal-Sans-14-*-*-*-*-*-*-*")
  )

;; automatically read compressed files
(auto-compression-mode t)

;;-----------------------------------------------------------------------
;; Mail Settings and Aliases
;;-----------------------------------------------------------------------

(setq mail-mode-hook 'turn-on-auto-fill)
(setq mail-yank-prefix "> ")

(setq rmail-file-name "~/home/mail/Inbox")
(setq mail-archive-file-name "~/home/mail/Outbox")

;;-----------------------------------------------------------------------
;; File-save history
;;-----------------------------------------------------------------------

(defvar emacs-history-filename "~/home/history/emacs-history"
  "*File to which emacs history (record of file opens and saves)
should be written.")

(defvar emacs-history-last-save-time 0
  "Time when emacs save history was last updated for the current
buffer.")
(make-variable-buffer-local 'emacs-history-last-save-time)

(defvar emacs-history-save-interval 120
  "*Minimum interval, in seconds, between consecutive writes to
emacs-history-filename, for a given buffer.")

(defun save-history-line (type)
  "Generate a history line of the given type, and write it to the
file specified by emacs-history-filename."
  (condition-case error-data
      (if (and emacs-history-filename
	       (> (length emacs-history-filename) 0)
	       (file-writable-p emacs-history-filename))
	  (let ((file-name-to-save (buffer-file-name)))
	    (with-temp-buffer
	      (insert (format "%s | host=%s | location=%s | type=%s | user=%s | uid=%d | pid=%d | file=%s\n"
			      (format-time-string "%Y-%m-%d %H:%M:%S %Z %a")
			      (system-name)
			      (getenv "LOCATION")
			      type
			      (user-login-name)
			      (user-uid)
			      (emacs-pid)
			      file-name-to-save))
	      (append-to-file (point-min) (point-max)
			      emacs-history-filename))))
    (error (message "caught an error in save-history-line"))))

(defun update-history-for-file-save ()
  "Hook for updating the file specified by emacs-history-filename
whenever a buffer is saved, but not more often than the interal given
by emacs-history-save-interval."

; this is not in emacs 21.3
;  (if (time-less-p (seconds-to-time emacs-history-save-interval)
;		   (time-since emacs-history-last-save-time))

  (if (or (not emacs-history-last-save-time)
	  (< emacs-history-save-interval
	     (- (float-time) emacs-history-last-save-time)))
      (progn
	(save-history-line "emacs-save")
	(setq emacs-history-last-save-time (float-time))))
  nil)

(defun update-history-for-file-open ()
  "Hook for updating the file specified by emacs-history-filename
whenever a file is opened into a buffer."
  (save-history-line "emacs-open")
  nil)

(add-hook 'after-save-hook 'update-history-for-file-save)
(add-hook 'find-file-hooks 'update-history-for-file-open) ; switch to find-file-hook (no 's') after emacs 22.1

;;-----------------------------------------------------------------------
;; Variables set by emacs' customize wizard
;;-----------------------------------------------------------------------

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(blink-cursor-delay 1.0)
 '(blink-cursor-interval 0.25)
 '(confirm-kill-emacs (quote yes-or-no-p))
 '(org-stuck-projects (quote ("+LEVEL=2/-DONE" ("NEXT" "WAITING") ("FUTUREPROJECT" "ONGOING" "DONEPROJECT") "")))
 '(pc-select-meta-moves-sexps t)
 '(pc-select-selection-keys-only t)
 '(pc-selection-mode t nil (pc-select))
 '(rmail-enable-mime t)
 '(rmail-user-mail-address-regexp ".*rjp.*")
 '(safe-local-variable-values (quote ((do-fixup-whitespace))))
 '(show-trailing-whitespace t)
 '(user-mail-address "rjpeters@klab.caltech.edu")
 '(widget-image-enable nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "salmon1"))))
 '(font-lock-comment-face ((((class color) (background dark)) (:foreground "FireBrick1"))
			   (((class color) (background light)) (:foreground "FireBrick3"))))
 '(font-lock-constant-face ((((class color) (background dark)) (:foreground "Purple1"))
			    (((class color) (background light)) (:foreground "Purple1"))))
 '(font-lock-function-name-face ((((class color) (background dark)) (:foreground "dodger blue" :weight bold))
				 (((class color) (background light)) (:foreground "blue" :weight bold))))
 '(font-lock-keyword-face ((((class color) (background dark)) (:foreground "Cyan" :weight bold))
			   (((class color) (background light)) (:foreground "DarkCyan" :weight bold)))) ;; Purple
 '(font-lock-string-face ((((class color) (background dark)) (:foreground "Sienna2"))
			  (((class color) (background light)) (:foreground "chocolate4")))) ;; RosyBrown
 '(font-lock-type-face ((((class color) (background dark)) (:foreground "LawnGreen"))
			(((class color) (background light)) (:foreground "ForestGreen")))) ;; ForestGreen
 '(font-lock-variable-name-face ((((class color) (background dark)) (:foreground "gold"))
				 (((class color) (background light)) (:foreground "DarkGoldenRod"))))) ;; DarkGoldenRod
