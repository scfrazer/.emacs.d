(require 'vhdl-mode)

;;; auto-insert

;; (define-auto-insert "\.vhd.*$"
;;   (lambda ()
;;     (let ((design-unit (file-name-sans-extension (file-name-nondirectory
;;                                                   buffer-file-name))))
;;       (goto-char (point-max))
;;       (if (string-match "\\(.+\\)_pkg$" design-unit)
;;           (insert-string (concat
;;                           "\nPACKAGE " design-unit " IS\n\n"
;;                           "  COMPONENT " (match-string 1 design-unit) "\n"
;;                           "    PORT( rst_n : IN std_logic;\n"
;;                           "          clk   : IN std_logic;\n"
;;                           "          );\n"
;;                           "  END COMPONENT;\n\n"
;;                           "END " design-unit ";\n"))
;;         (insert-string (concat
;;                         "USE work." design-unit "_pkg.ALL;\n\n"
;;                         "ENTITY " design-unit " IS\n"
;;                         "END " design-unit ";\n\n"
;;                         "-------------------------------------------------------------------------------\n\n"
;;                         "ARCHITECTURE rtl of " design-unit " IS\n"
;;                         "BEGIN\nEND rtl;\n"))))))
;; (define-auto-insert "\.vhd.*$" "header.vhd")

;;; tempo

(defvar my-vhdl-tempo-tags nil)
(tempo-define-template "vhdl-process-rising"
  '(
    (p "Label: " label) ": PROCESS( " > (p "Clock " clock nil "clk") ", "
    (p "Reset " reset nil "rst_n") " )" n
    "BEGIN" > n
    "IF( " (s reset) " = '0' ) THEN" > n
    > p n
    "ELSIF( " (s clock) "'event AND " (s clock) " = '1' ) THEN" > n
    "END IF;" > n
    "END PROCESS " (s label) ";" > n
    ) "pr" "VHDL process with rising edge." my-vhdl-tempo-tags)
(tempo-define-template "vhdl-process-falling"
  '(
    (p "Label: " label) ": PROCESS( " > (p "Clock " clock nil "clk") ", "
    (p "Reset " reset nil "rst_n") " )" n
    "BEGIN" > n
    "IF( " (s reset) " = '0' ) THEN" > n
    > p n
    "ELSIF( " (s clock) "'event AND " (s clock) " = '0' ) THEN" > n
    "END IF;" > n
    "END PROCESS " (s label) ";" > n
    ) "pf" "VHDL process with falling edge." my-vhdl-tempo-tags)
(tempo-define-template "vhdl-process-enable-rising"
  '(
    (p "Label: " label) ": PROCESS( " > (p "Clock " clock nil "clk") ", "
    (p "Reset " reset nil "rst_n") " )" n
    "BEGIN" > n
    "IF( " (s reset) " = '0' ) THEN" > n
    > p n
    "ELSIF( " (s clock) "'event AND " (s clock) " = '1' ) THEN" > n
    "IF( " (p "Enable: ") " = '1' ) THEN" > n
    "END IF;" > n
    "END IF;" > n
    "END PROCESS " (s label) ";" > n
    ) "pre" "VHDL process with rising edge and enable." my-vhdl-tempo-tags)
(tempo-define-template "vhdl-process-enable-falling"
  '(
    (p "Label: " label) ": PROCESS( " > (p "Clock " clock nil "clk") ", "
    (p "Reset " reset nil "rst_n") " )" n
    "BEGIN" > n
    "IF( " (s reset) " = '0' ) THEN" > n
    > p n
    "ELSIF( " (s clock) "'event AND " (s clock) " = '0' ) THEN" > n
    "IF( " (p "Enable: ") " = '1' ) THEN" > n
    "END IF;" > n
    "END IF;" > n
    "END PROCESS " (s label) ";" > n
    ) "pfe" "VHDL process with falling edge and enable." my-vhdl-tempo-tags)

;;; func-menu

;(require 'func-menu)
;
;(defvar my-fume-function-name-regexp-vhdl
;  "^\\s-*\\([a-zA-Z0-9_]+\\s-*:\\s-*[a-zA-Z0-9_]+\\)[ \t\n]+\\(PORT\\|port\\|GENERIC\\|generic\\)"
;  "My expression to get VHDL instance names.")
;
;(setq fume-function-name-regexp-alist
;      (cons '(vhdl-mode . my-fume-function-name-regexp-vhdl)
;            fume-function-name-regexp-alist))
;
;(defun my-fume-find-next-vhdl-function-name (buffer)
;  "Search for the next VHDL instance in BUFFER."
;  (set-buffer buffer)
;  (if (re-search-forward fume-function-name-regexp nil t)
;      (let ((beg (match-beginning 1))
;            (end (match-end 1)))
;        (cons (buffer-substring beg end) beg))))
;
;(setq fume-find-function-name-method-alist
;      (cons '(vhdl-mode . my-fume-find-next-vhdl-function-name)
;            fume-find-function-name-method-alist))

;;; Modified imenu instance entry to allow entity instantiations

(defconst vhdl-imenu-generic-expression
  '(
    ("Subprogram"
     "^\\s-*\\(\\(\\(impure\\|pure\\)\\s-+\\|\\)function\\|procedure\\)\\s-+\\(\"?\\(\\w\\|\\s_\\)+\"?\\)"
     4)
    ("Instance"
;     "^\\s-*\\(\\(\\w\\|\\s_\\)+\\s-*:\\(\\s-\\|\n\\)*\\(\\w\\|\\s_\\)+\\)\\(\\s-\\|\n\\)+\\(generic\\|port\\)\\s-+map\\>"
     "^\\s-*\\([a-zA-Z0-9_]+\\s-*:\\s-*\\(entity\\s-+\\|\\s-*\\)[a-zA-Z0-9_.()]+\\)\\(\\s-\\|\n\\)+\\(generic\\|port\\)\\s-+map"
     1)
    ("Component"
     "^\\s-*\\(component\\)\\s-+\\(\\(\\w\\|\\s_\\)+\\)"
     2)
    ("Procedural"
     "^\\s-*\\(\\(\\w\\|\\s_\\)+\\)\\s-*:\\(\\s-\\|\n\\)*\\(procedural\\)"
     1)
    ("Process"
     "^\\s-*\\(\\(\\w\\|\\s_\\)+\\)\\s-*:\\(\\s-\\|\n\\)*\\(\\(postponed\\s-+\\|\\)process\\)"
     1)
    ("Block"
     "^\\s-*\\(\\(\\w\\|\\s_\\)+\\)\\s-*:\\(\\s-\\|\n\\)*\\(block\\)"
     1)
    ("Package"
     "^\\s-*\\(package\\( body\\|\\)\\)\\s-+\\(\\(\\w\\|\\s_\\)+\\)"
     3)
    ("Configuration"
     "^\\s-*\\(configuration\\)\\s-+\\(\\(\\w\\|\\s_\\)+\\s-+of\\s-+\\(\\w\\|\\s_\\)+\\)"
     2)
    ("Architecture"
     "^\\s-*\\(architecture\\)\\s-+\\(\\(\\w\\|\\s_\\)+\\s-+of\\s-+\\(\\w\\|\\s_\\)+\\)"
     2)
    ("Entity"
     "^\\s-*\\(entity\\)\\s-+\\(\\(\\w\\|\\s_\\)+\\)"
     2)
    )
  "Imenu generic expression for VHDL Mode.  See `imenu-generic-expression'.")

;;; goto associated file

(defun my-vhdl-goto-associated-file ()
  "Goto the associated file."
  (interactive)
  (save-match-data
    (let ((ext (file-name-extension buffer-file-truename))
          (root (file-name-sans-extension buffer-file-truename)))
      (if (string-match "\\(.+\\)_pkg$" root)
          (find-file (concat (match-string 1 root) "." ext))
        (find-file (concat root "_pkg." ext))))))

;;; fix case and insert

(defun my-vhdl-fix-case-and-insert ()
  (interactive)
  (unless (or (vhdl-in-comment-p) (vhdl-in-string-p))
    (vhdl-fix-case-word 1))
  (self-insert-command 1))

;;; get rid of annoying keymap

(setq my-vhdl-mode-map (make-keymap "my-vhdl-mode-map"))
(define-key my-vhdl-mode-map " " 'my-vhdl-fix-case-and-insert)
(define-key my-vhdl-mode-map "(" 'my-vhdl-fix-case-and-insert)
(define-key my-vhdl-mode-map ")" 'my-vhdl-fix-case-and-insert)
(define-key my-vhdl-mode-map ";"
  (lambda()
    (interactive)
    (my-vhdl-fix-case-and-insert)
    (indent-for-tab-command)))
(define-key my-vhdl-mode-map [(return)]
  (lambda()
    (interactive)
    (unless (vhdl-in-comment-p)
      (vhdl-fix-case-word 1))
    (save-excursion (indent-for-tab-command))
    (newline)))
(define-key my-vhdl-mode-map [(tab)]
  (lambda()
    (interactive)
    (unless (or (vhdl-in-comment-p) (vhdl-in-string-p))
      (vhdl-fix-case-word 1))
    (indent-for-tab-command)))
(define-key my-vhdl-mode-map "\C-c$"
  (lambda()
    (interactive)
    (vhdl-fix-case-buffer)
    (my-delete-trailing-whitespace)))
(define-key my-vhdl-mode-map [f12] 'my-vhdl-goto-associated-file)

(defun vhdl-update-mode-menu ()
  "Update VHDL Mode menu."
  (interactive)
  (easy-menu-remove vhdl-mode-menu-list) ; for XEmacs
  (setq vhdl-mode-menu-list (vhdl-create-mode-menu))
  (easy-menu-add vhdl-mode-menu-list) ; for XEmacs
  (easy-menu-define vhdl-mode-menu my-vhdl-mode-map
    "Menu keymap for VHDL Mode." vhdl-mode-menu-list))

;;; hook in new functionality

(defun my-vhdl-mode-hook ()
  (tempo-use-tag-list 'my-vhdl-tempo-tags)
  (use-local-map my-vhdl-mode-map)
  (align-set-vhdl-rules)
  (turn-off-filladapt-mode)
  (setq truncate-lines t))
(add-hook 'vhdl-mode-hook 'my-vhdl-mode-hook)

(provide 'my-vhdl-extras)
