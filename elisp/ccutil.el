;;-----------------------------------------------------------------------
;; C++ utilities
;;-----------------------------------------------------------------------

(font-lock-add-keywords
 'c++-mode
 '(("\\<\\(FIXME\\)" 1 font-lock-warning-face t)))
(modify-face (quote font-lock-warning-face) "Red" "yellow" nil t nil t nil nil)

(defun reset-font-lock ()
  "Just calls font-lock-mode twice. Can be used to fix annoying font-lock
 problem after checking in a file with version control."
  (interactive)
  (font-lock-mode)
  (font-lock-mode))

(defun creators ()
  (interactive)
  (insert
   "//////////////\n"
   "// creators //\n"
   "//////////////\n"))

(defun accessors ()
  (interactive)
  (insert
   "///////////////\n"
   "// accessors //\n"
   "///////////////\n"))

(defun manipulators ()
  (interactive)
  (insert
   "//////////////////\n"
   "// manipulators //\n"
   "//////////////////\n"))

(defun actions ()
  (interactive)
  (insert
   "/////////////\n"
   "// actions //\n"
   "/////////////\n"))

(defun big-doc ()
  (interactive)
  (insert 
   "///////////////////////////////////////////////////////////////////////\n"
   "/**\n"
   " *\n"
   " *\n"
   " *\n"
   " **/\n"
   "///////////////////////////////////////////////////////////////////////\n"))

(defun med-doc ()
  (interactive)
  (insert 
   "/**\n"
   " *\n"
   " *\n"
   " *\n"
   " **/\n"))

(defun small-doc ()
  (interactive)
  (insert 
   "/**   */\n"))

(defun doc-scope ()
  (interactive)
  (insert
   "//@{\n"
   "//@}\n"))

(defun big-comment ()
  (interactive)
  (insert 
   "///////////////////////////////////////////////////////////////////////\n"
   "//\n"
   "//\n"
   "//\n"
   "///////////////////////////////////////////////////////////////////////\n"))

(defun med-comment ()
  (interactive)
  (insert 
   "//---------------------------------------------------------------------\n"
   "//\n"
   "//\n"
   "//\n"
   "//---------------------------------------------------------------------\n"))

(defun small-comment ()
  (interactive)
  (insert 
   "//---------------------------------------------------------------------\n"
   "//\n"
   "//\n"
   "//\n"))

(defun init-cc-file ()
  "Place an appropriate comment at the beginning of a C++ source file, and
insert the appropriate include guards (i.e. #ifndef filename_DEFINED, etc.)"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (insert 
     "///////////////////////////////////////////////////////////////////////\n"
     "//\n"
     "// " (buffer-name) "\n"
     "//\n"
     "// Copyright (c) 2004-2004 Rob Peters rjpeters@klab.caltech.edu\n"
     "//\n"
     "// created: " (current-time-string) "\n"
     "// commit: $Id$\n"
     "//\n"
     "///////////////////////////////////////////////////////////////////////\n"
     "\n#ifndef " (upcase (buffer-name)) "_DEFINED\n"
     "#define " (upcase (buffer-name)) "_DEFINED\n\n")
    (goto-char (point-max))
    (insert
     "\n"
     "static const char vcid_" (buffer-name) "[] = \"$Header$\";\n"
     "#endif // !" (upcase (buffer-name)) "_DEFINED\n")
    (goto-char (point-min))
    (replace-regexp "\\([A-Z]\\)\\.\\([CH]+_DEFINED\\)" "\\1_\\2")
    (goto-char (point-min))
    (replace-string ".h[]" "_h[]")
    (goto-char (point-min))
    (replace-string ".cc[]" "_cc[]")))

(defun nocopy (classname)
  (interactive "sName of class: ")
  (insert
   "  " classname "(const " classname "&);\n"
   "  " classname "& operator=(const " classname "&);\n\n"))

(defun new-class (classname)
  "Generate appropriate skeleton files 'classname.h' and
'classname.cc' for a new class. Does nothing if either file already exists."
  (interactive "sName for new class: ")
  (let ((h-file (downcase (concat classname ".h")))
        (cc-file (downcase (concat classname ".cc"))))
    (if (or (file-exists-p h-file) (file-exists-p cc-file))
        (message "files already exist.")
      (find-file h-file)
      (init-cc-file)
      (re-search-forward "#define")
      (forward-line 2)
      (insert
       "class " classname " {\n"          ;class declaration
       "public:\n"                      ;access specifier
       "\t" classname "();\n"             ;default constructor
       "\tvirtual ~" classname "();\n"    ;default destructor
       "};\n")

      (find-file cc-file)
      (init-cc-file)
      (re-search-forward "#define")
      (forward-line 2)
      (insert
       "#include \"" h-file "\"\n"
       "\n"
       classname "::" classname " () {\n\n}\n\n"
       classname "::~" classname " () {\n\n}\n"))))

;; Regex variables for C++ grammar
(defvar cc-white "[ \t\n]+"
  "Regex to match white space in C++ code.")
(defvar cc-opt-white (concat "[ \t\n]*")
  "Regex to match optional white space in C++ code.")
(defvar cc-ident "[A-Za-z_][A-Za-z01-9_<>,]*"
  "Regex to match a C++ identifier.")
(defvar cc-namespace
  (concat "\\(" cc-ident "\\)?"
	  cc-opt-white "::" cc-opt-white)
  "Regex to match a C++ namespace qualifier")
(defvar cc-opt-namespace (concat "\\(" cc-namespace "\\)*")
  "Regex to match an optional C++ namespace qualifier")
(defvar cc-qualified-ident
  (concat cc-opt-namespace
	  cc-ident)
  "Regex to match a namespace-qualified C++ identifier.")
(defvar cc-typename
  (concat "\\(unsigned\\|const\\)*" cc-opt-white
	  cc-qualified-ident
	  cc-opt-white "[*&]*")
  "Regex to match a C++ type name.")
(defvar cc-arg-list "([]\\[, \t\nA-Za-z_01-9\\*&\\:<>]*)"
  "Regex to match a C++ argument list.")
(defvar cc-func-decl 
  (concat "^"                            ;beginning of line
          "\\(" cc-typename cc-white "\\)?" ;return type optional
          "\\("
            "\\(" cc-namespace "\\)*"   ;optional scope specifier
            "~?"                         ;optional ~ for destructor
            "\\(" cc-ident cc-opt-white "\\)" ;function name
          "\\)"                         ;fully qualified name
          cc-arg-list)                   ;argument list
  "Regex to match a C++ function declaration.")

(defun next-defun ()
  "Move to the beginning of the body of the next function."
  (interactive)
  (re-search-forward cc-func-decl)
  (re-search-forward "{"))

(defun prev-defun ()
  "Move to the beginning of the body of the previous function."
  (interactive)
  (re-search-backward "^}")
  (re-search-backward cc-func-decl)
  (re-search-forward "{"))

(defun add-trace ()
  "Add a trace statement to the beginning of the current function body."
  (interactive)
  (save-excursion
    (re-search-backward cc-func-decl)
    (setq str (match-string 5))
    (re-search-forward "{")
    (insert "\nDOTRACE(\"" str "\");")
    (message "traced %s." str)))

(defun add-method (classname methodname access declaration)
  (interactive "sclass name: \nsmethod name: \nsaccess specifier: \ndeclaration: ")
  ;find relevant .h and .cc files
  ;check if method exists already
  ;see if access specifier exists
  ;if not, add new access specifier
  ;put function declaration in appropriate spot
  ;put function definition skeleton in appropriate spot
)

(defun backward-nomenclature-kill ()
  (interactive)
  (let ((end (point)))
    (c-backward-into-nomenclature 1)
    (let ((start (point)))
      (delete-region start end))))

;; Writestamps
(defun update-writestamps ()
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

(defun safe-fixup-whitespace ()
  "Untabify the buffer, and kill trailing whitespace on all lines."
  (interactive)
  ;; Skip fixup-whitespace for [.CH] files (i.e. those from iLab cvs system)
  ;;(if (or (string= (substring (buffer-name) -2) ".C")
  ;;	  (string= (substring (buffer-name) -2) ".H"))
  ;;    ()
  ;;  (fixup-whitespace)))
  (fixup-whitespace))

(defun my-c++-mode-hook ()
  (add-hook 'local-write-file-hooks 'update-writestamps)
  (add-hook 'local-write-file-hooks 'safe-fixup-whitespace)

  ;; key bindings
  (local-unset-key "\C-c\C-f")
  (local-set-key "\C-c\C-f" 'init-cc-file)

  (local-unset-key "\C-c\C-t")
  (local-set-key "\C-c\C-t" 'add-trace)

  (local-unset-key "\C-c\C-n")
  (local-set-key "\C-c\C-n" 'next-defun)

  (local-unset-key "\C-c\C-p")
  (local-set-key "\C-c\C-p" 'prev-defun)

;  (local-unset-key "\C-f")
;  (local-set-key "\C-f" 'c-forward-into-nomenclature)
  
;  (local-unset-key "\C-b")
;  (local-set-key "\C-b" 'c-backward-into-nomenclature)

  (local-unset-key "\M-\C-?")             ; i.e., ESC-DEL
  (local-set-key "\M-\C-?" 'backward-nomenclature-kill)

  (setq column-number-mode t)
)

(setq c++-mode-hook 'my-c++-mode-hook)

(provide 'ccutil)

;; ccutil.el ends here
