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
(setq load-path (append load-path (list "/usr/share/emacs/site-lisp")))
(setq load-path (append load-path (list "~/.home/emacs_lisp")))

(autoload 'w3 "w3" "WWW Browser" t)

;;-----------------------------------------------------------------------
;; Matlab mode
;;-----------------------------------------------------------------------
(autoload 'matlab-mode "matlab" "Enter Matlab mode." t)
(setq auto-mode-alist (cons '("\\.m$" . matlab-mode) auto-mode-alist))
(defun my-matlab-mode-hook ()
  (setq matlab-indent-function nil)
  (setq matlab-return-function 'matlab-plain-ret)
  (setq matlab-verify-on-save-flag nil)
  (setq fill-column 78)
  (turn-on-auto-fill))
(setq matlab-mode-hook 'my-matlab-mode-hook)
(autoload 'matlab-shell "matlab" "Interactive Matlab mode." t)
(defun my-matlab-shell-mode-hook ()
	'())
(setq matlab-shell-mode-hook 'my-matlab-shell-mode-hook)

;;-----------------------------------------------------------------------
;; C++ utilities
;;-----------------------------------------------------------------------
(setq-default case-fold-search nil)
(setq-default case-replace nil)
(load "ccutil")

;;-----------------------------------------------------------------------
;; BibTeX utilities
;;-----------------------------------------------------------------------
(load "bibtexutil")

;;-----------------------------------------------------------------------
;; Settings
;;-----------------------------------------------------------------------
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

(setq text-mode-hook 'turn-on-auto-fill)
(setq latex-mode-hook 'turn-on-auto-fill)
(setq mail-mode-hook 'turn-on-auto-fill)
(setq mail-yank-prefix "> ")

(setq auto-mode-alist (cons '("\\.[ch]+$" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.dxy$" . c++-mode) auto-mode-alist))

(setq-default column-number-mode t)

(setq fill-column 75)
(setq default-fill-column 75)

;;(setq-default default-tab-width 3)
;;(setq-default tab-width 3)

(setq display-time-day-and-date t)
(display-time)

;;-----------------------------------------------------------------------
;; Mail Settings and Aliases
;;-----------------------------------------------------------------------

(setq rmail-file-name "~/people/Inbox")
(setq mail-archive-file-name "~/people/Outbox")

(setq nndraft-directory "~/.news")

(put 'downcase-region 'disabled nil)

(custom-set-variables
  ;; custom-set-variables was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 '(blink-cursor-delay 1.0)
 '(blink-cursor-interval 0.25)
 '(confirm-kill-emacs (quote yes-or-no-p))
 '(rmail-enable-mime t t)
 '(user-mail-address "rjpeters@klab.caltech.edu")
 '(widget-image-enable nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 '(cursor ((t (:background "salmon1"))))
 '(scroll-bar ((t (:background "gray60" :foreground "black")))))
