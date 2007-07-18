;;-----------------------------------------------------------------------
;; C++ utilities
;;
;; $Id$
;;-----------------------------------------------------------------------

(font-lock-add-keywords
 'c++-mode
 '(("\\<\\(FIXME\\)" 1 'font-lock-warning-face t)))
(set-face-attribute 'font-lock-warning-face nil
                    :foreground "red"
                    :background "yellow"
                    :weight 'bold
                    :underline "true")

;(defun reset-font-lock ()
;  "Just calls font-lock-mode twice. Can be used to fix annoying font-lock
; problem after checking in a file with version control."
;  (interactive)
;  (font-lock-mode)
;  (font-lock-mode))

(defun ccutil-doc-scope ()
  (interactive)
  (insert
   "//@{\n"
   "//@}\n"))

(defun ccutil-big-comment ()
  (interactive)
  (insert
   "////////////////////////////////////////////////////////////\n"
   "//\n"
   "//\n"
   "//\n"
   "////////////////////////////////////////////////////////////\n"))

(defun ccutil-med-comment ()
  (interactive)
  (insert
   "//----------------------------------------------------------\n"
   "//\n"
   "//\n"
   "//\n"
   "//----------------------------------------------------------\n"))

(defun ccutil-small-comment ()
  (interactive)
  (insert
   "//----------------------------------------------------------\n"
   "//\n"
   "//\n"
   "//\n"))

(defun ccutil-init-source-file ()
  "Place an appropriate comment at the beginning of a C++ source file, and
insert the appropriate include guards (i.e. #ifndef filename_DEFINED, etc.)"
  (interactive)
  (save-excursion
    (setq filename (buffer-file-name))
    (if (string-match "/src/" filename)
	(setq filename (replace-regexp-in-string ".*/src/" "" filename))
      (setq filename (buffer-name)))

    (let ((include-guard
           (upcase (replace-regexp-in-string
		    "\\(\\.\\|-\\|/\\)" "_" filename)))
          )

      (goto-char (point-min))
      (insert
       "/** @file " filename " */\n"
       "\n"
       "///////////////////////////////////////////////////////////////////////\n"
       "//\n"
       "// Copyright (c) 2007-2007 University of Southern California\n"
       "// Rob Peters <rjpeters at usc dot edu>\n"
       "//\n"
       "// created: " (current-time-string) "\n"
       "// commit: $" "Id" "$\n"
       "// $" "HeadURL" "$\n")

      (setq pkg-prefix "")
      (if (string-match "groovx" (buffer-file-name))
	  (setq pkg-prefix "GROOVX_"))

      (if (string-match "src/rutz" (buffer-file-name))
	  (setq pkg-prefix "GROOVX_"))

      (if (string-match "src/tcl" (buffer-file-name))
	  (setq pkg-prefix "GROOVX_"))

      (if (string-match "src/nub" (buffer-file-name))
	  (setq pkg-prefix "GROOVX_"))

      (if (string-match "GROOVX_" pkg-prefix)
	  (insert
	   "//\n"
	   "// --------------------------------------------------------------------\n"
	   "//\n"
	   "// This file is part of GroovX.\n"
	   "//   [http://www.klab.caltech.edu/rjpeters/groovx/]\n"
	   "//\n"
	   "// GroovX is free software; you can redistribute it and/or modify it\n"
	   "// under the terms of the GNU General Public License as published by\n"
	   "// the Free Software Foundation; either version 2 of the License, or\n"
	   "// (at your option) any later version.\n"
	   "//\n"
	   "// GroovX is distributed in the hope that it will be useful, but\n"
	   "// WITHOUT ANY WARRANTY; without even the implied warranty of\n"
	   "// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU\n"
	   "// General Public License for more details.\n"
	   "//\n"
	   "// You should have received a copy of the GNU General Public License\n"
	   "// along with GroovX; if not, write to the Free Software Foundation,\n"
	   "// Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.\n"))

      (setq date-string (format-time-string "_UTC%Y%m%d%H%M%S"
					    (current-time) t))

      (setq full-symname (concat pkg-prefix include-guard date-string))

      (insert
       "//\n"
       "///////////////////////////////////////////////////////////////////////\n"
       "\n#ifndef " full-symname "_DEFINED\n"
       "#define " full-symname "_DEFINED\n\n")

      (goto-char (point-max))
      (insert
       "\n"
       "static const char vcid_" (downcase full-symname) "[] = \"$" "Id" "$ $" "HeadURL" "$\";\n"
       "#endif // !" full-symname "DEFINED\n")
      )
    )
  )

(defun ccutil-nocopy (classname)
  (interactive "sName of class: ")
  (insert
   "  " classname "(const " classname "&);\n"
   "  " classname "& operator=(const " classname "&);\n\n"))

(defun ccutil-new-class (classname)
  "Generate appropriate skeleton files 'classname.h' and
'classname.cc' for a new class. Does nothing if either file already exists."
  (interactive "sName for new class: ")
  (let ((h-file (downcase (concat classname ".h")))
        (cc-file (downcase (concat classname ".cc"))))
    (if (or (file-exists-p h-file) (file-exists-p cc-file))
        (message "files already exist.")
      (find-file h-file)
      (ccutil-init-source-file)
      (re-search-forward "#define")
      (forward-line 2)
      (insert
       "class " classname " {\n"          ;class declaration
       "public:\n"                        ;access specifier
       "\t" classname "();\n"             ;default constructor
       "\tvirtual ~" classname "();\n"    ;default destructor
       "};\n")

      (find-file cc-file)
      (ccutil-init-source-file)
      (re-search-forward "#define")
      (forward-line 2)
      (insert
       "#include \"" h-file "\"\n"
       "\n"
       classname "::" classname " () {\n\n}\n\n"
       classname "::~" classname " () {\n\n}\n"))))

;; Regex variables for C++ grammar
(defvar ccutil-re-white
  "[ \t\n]+"
  "Regex to match white space in C++ code.")

(defvar ccutil-re-opt-white
  (concat "[ \t\n]*")
  "Regex to match optional white space in C++ code.")

(defvar ccutil-re-ident
  "[A-Za-z_][A-Za-z01-9_<>,]*"
  "Regex to match a C++ identifier.")

(defvar ccutil-re-namespace
  (concat "\\(" ccutil-re-ident "\\)?"
          ccutil-re-opt-white "::" ccutil-re-opt-white)
  "Regex to match a C++ namespace qualifier")

(defvar ccutil-re-opt-namespace
  (concat "\\(" ccutil-re-namespace "\\)*")
  "Regex to match an optional C++ namespace qualifier")

(defvar ccutil-re-qualified-ident
  (concat ccutil-re-opt-namespace ccutil-re-ident)
  "Regex to match a namespace-qualified C++ identifier.")

(defvar ccutil-re-typename
  (concat "\\(unsigned\\|const\\)*" ccutil-re-opt-white
          ccutil-re-qualified-ident
          ccutil-re-opt-white "[*&]*")
  "Regex to match a C++ type name.")

(defvar ccutil-re-arg-list "([]\\[, \t\nA-Za-z_01-9\\*&\\:<>]*)"
  "Regex to match a C++ argument list.")

(defvar ccutil-re-func-decl
  (concat "^"                            ;beginning of line
          "\\(" ccutil-re-typename ccutil-re-white "\\)?" ;return type optional
          "\\("
            "\\(" ccutil-re-namespace "\\)*"   ;optional scope specifier
            "~?"                         ;optional ~ for destructor
            "\\(" ccutil-re-ident ccutil-re-opt-white "\\)" ;function name
          "\\)"                         ;fully qualified name
          ccutil-re-arg-list)                   ;argument list
  "Regex to match a C++ function declaration.")

(defun ccutil-next-defun ()
  "Move to the beginning of the body of the next function."
  (interactive)
  (re-search-forward ccutil-re-func-decl)
  (re-search-forward "{"))

(defun ccutil-prev-defun ()
  "Move to the beginning of the body of the previous function."
  (interactive)
  (re-search-backward "^}")
  (re-search-backward ccutil-re-func-decl)
  (re-search-forward "{"))

(defun ccutil-add-trace ()
  "Add a trace statement to the beginning of the current function body."
  (interactive)
  (save-excursion
    (re-search-backward ccutil-re-func-decl)
    (setq str (match-string 5))
    (re-search-forward "{")
    (insert "\nGVX_TRACE(\"" str "\");")
    (message "traced %s." str)))

(defun ccutil-backward-nomenclature-kill ()
  (interactive)
  (let ((end (point)))
    (c-backward-into-nomenclature 1)
    (let ((start (point)))
      (delete-region start end))))

;; Writestamps
(defun ccutil-update-writestamps ()
  "Find writestamps and replace them with the current time."
  (interactive)
  (save-excursion
    (save-restriction
      (save-match-data
        (widen)
        (goto-char (point-min))
        (let ((rxp "^// written: \\(.*\\)$"))
          (while (re-search-forward rxp nil t)
            (replace-match (current-time-string) t t nil 1))))))
  nil)

(defun ccutil-write-file-hook ()
  (if do-fixup-whitespace
      (fixup-whitespace)
    ()))

(defun ccutil-c++-mode-hook ()
  ;; Make underscore "_" be considered a word-character instead of a
  ;; whitespace character (so that characters on either side of an
  ;; underscore aren't seen as falling on word boundaries).
  (modify-syntax-entry ?_ "w")

  ;; To make underscore "_" be considered whitespace again, do the
  ;; following:
  ;(modify-syntax-entry ?_ " ")

  ;; Have turned off this hook for now since I'm not actually using
  ;; writestamps (i.e. "written: ...") in any of my source files at
  ;; the moment:
  ;(add-hook 'local-write-file-hooks 'ccutil-update-writestamps)

  (add-hook 'local-write-file-hooks 'ccutil-write-file-hook)

  ;; key bindings
  (local-unset-key "\C-c\C-f")
  (local-set-key "\C-c\C-f" 'ccutil-init-source-file)

  (local-unset-key "\C-c\C-t")
  (local-set-key "\C-c\C-t" 'ccutil-add-trace)

  (local-unset-key "\C-c\C-n")
  (local-set-key "\C-c\C-n" 'ccutil-next-defun)

  (local-unset-key "\C-c\C-p")
  (local-set-key "\C-c\C-p" 'ccutil-prev-defun)

;  (local-unset-key "\C-f")
;  (local-set-key "\C-f" 'c-forward-into-nomenclature)

;  (local-unset-key "\C-b")
;  (local-set-key "\C-b" 'c-backward-into-nomenclature)

  (local-unset-key "\M-\C-?")             ; i.e., ESC-DEL
  (local-set-key "\M-\C-?" 'ccutil-backward-nomenclature-kill)

  (setq column-number-mode t)

  (defvar do-fixup-whitespace t
    "*Whether to fixup whitespace when the current buffer is saved.")
  (make-variable-buffer-local 'do-fixup-whitespace)
  (setq do-fixup-whitespace t)

  ;; turn off abbrev mode (by passing a negative value)
  (abbrev-mode -1)

  ;; turn off "electric indentation" (toggle with C-c C-l)
  (c-toggle-electric-state -1)
)

(setq c++-mode-hook 'ccutil-c++-mode-hook)

(provide 'ccutil)

;; ccutil.el ends here
