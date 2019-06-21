;;; my-verilog-mode.el

(setq-default verilog-active-low-regexp ".+_n$"
              verilog-auto-endcomments nil
              verilog-auto-indent-on-newline nil
              verilog-auto-inst-dot-name t
              verilog-auto-inst-interfaced-ports t
              verilog-auto-newline nil
              verilog-case-indent 4
              verilog-cexp-indent 4
              verilog-highlight-grouping-keywords t
              verilog-indent-level 4
              verilog-indent-level-behavioral 4
              verilog-indent-level-declaration 4
              verilog-indent-level-directive 0
              verilog-indent-level-module 4)

(require 'verilog-mode)

;; ;;; Enhancements to verilog-mode
;;
;; (unless (fboundp 'hs-special-modes-alist)
;;   (defvar hs-special-modes-alist nil))
;; (require 'verilog-mode)
;;
;; ;;; Port copy/paste
;;
;; (require 'align)
;;
;; (defvar my-verilog-inst-name nil)
;; (defvar my-verilog-inst-ports nil)
;;
;; (defun my-verilog-get-module-name ()
;;   "Get module name"
;;   (save-excursion
;;     (when (verilog-re-search-backward "\\bmodule\\b" nil t)
;;       (skip-syntax-forward "w_")
;;       (when (verilog-re-search-forward "[a-zA-Z][a-zA-Z0-9_]*" nil t)
;;         (match-string-no-properties 0)))))
;;
;; (defun my-verilog-port-copy ()
;;   "Copy a module's ports when in it's definition."
;;   (interactive)
;;   (setq my-verilog-inst-name (my-verilog-get-module-name))
;;   (setq my-verilog-inst-ports (verilog-read-decls)))
;;
;; (defun my-verilog-insert-port-list (port-list port-comment)
;;   (dolist (port port-list)
;;     (insert (concat "." (car port) "(" (car port) ")," port-comment))
;;     (verilog-indent-line-relative)
;;     (verilog-indent-line-relative)
;;     (end-of-line)
;;     (insert "\n")))
;;
;; (defun my-verilog-get-instance-name ()
;;   "Prompt for an instance name."
;;   (let ((name (concat "i" (upcase my-verilog-inst-name)))
;;         result)
;;     (setq result (read-from-minibuffer (format "Instance name (default `%s'): " name)))
;;     (if (string= result "") name result)))
;;
;; (defun my-verilog-port-paste-inst ()
;;   "Paste an instance of a copied module."
;;   (interactive)
;;   (when (and my-verilog-inst-name my-verilog-inst-ports)
;;     (save-excursion
;;       (let (beg end rule)
;;         (insert my-verilog-inst-name " " (my-verilog-get-instance-name) "(")
;;         (verilog-indent-line-relative)
;;         (verilog-indent-line-relative)
;;         (end-of-line)
;;         (setq beg (point))
;;         (my-verilog-insert-port-list (verilog-decls-get-inputs my-verilog-inst-ports) " // input")
;;         (my-verilog-insert-port-list (verilog-decls-get-inouts my-verilog-inst-ports) " // inout")
;;         (my-verilog-insert-port-list (verilog-decls-get-outputs my-verilog-inst-ports) " // output")
;;         (save-excursion (when (re-search-backward "," beg t)
;;                           (replace-match " ")))
;;         (insert ");")
;;         (beginning-of-line)
;;         (verilog-indent-line-relative)
;;         (end-of-line)
;;         (setq end (point))
;;         (setq rule (list
;;                     (list nil
;;                           (cons 'regexp "\\(\\s-*\\)(")
;;                           (cons 'group 1))))
;;         (align-region beg end 'entire rule nil nil)
;;         (setq end (point))
;;         (setq rule (list
;;                     (list nil
;;                           (cons 'regexp "\\(\\s-*\\)//")
;;                           (cons 'group 1))))
;;         (align-region beg end 'entire rule nil nil)))))
;;
;; (defun my-verilog-port-paste-wires ()
;;   "Paste wires of a copied module."
;;   (interactive)
;;   (when my-verilog-inst-ports
;;     (save-excursion
;;       (let ((beg (point)) end)
;;         (verilog-insert-definition (verilog-decls-get-inputs my-verilog-inst-ports)) "wire" 0 nil)
;;         (verilog-insert-definition (verilog-decls-get-inouts my-verilog-inst-ports) "wire" 0 nil)
;;         (verilog-insert-definition (verilog-decls-get-outputs my-verilog-inst-ports) "wire" 0 nil)
;;         (align beg (point))))))
;;
;; ;;; Imenu
;;
;; (if (string-match "XEmacs" emacs-version)
;;     (fset 'verilog-match-string 'match-string)
;;   (fset 'verilog-match-string 'match-string-no-properties))
;;
;; (defvar verilog-imenu-flatten t
;;   "*Non-nil means flatten the heirarchical imenu output.")
;;
;; (defvar verilog-imenu-show-instance-type t
;;   "*Non-nil means show the instance type with the instance name.")
;;
;; (defvar verilog-imenu-qualify-names nil
;;   "*Non-nil means qualify names with the module they are in.")
;;
;; (defun verilog-trim-trailing-whitespace (string)
;;   (if (string-match "XEmacs" emacs-version)
;;       (replace-in-string string "[ \t\n]+$" "")
;;     (replace-regexp-in-string "[ \t\n]+$" "" string)))
;;
;; (defun verilog-sort-alist-by-car-string (alist)
;;   (sort alist '(lambda (x y) (string< (car x) (car y)))))
;;
;; (defun verilog-imenu-create-add-item-alist (name item-alist final-alist)
;;   (when item-alist
;;     (push (imenu--split-menu (verilog-sort-alist-by-car-string item-alist) name) final-alist))
;;   final-alist)
;;
;; (defun verilog-imenu-create-find-instances-or-modports (end)
;;   (save-excursion
;;     (let ((instance-alist '()))
;;       (while (re-search-forward
;;               "^\\s-*\\([a-zA-Z0-9_]+\\)\\([ \t\n]+#(.*)\\)?[ \t\n]+\\([a-zA-Z0-9_]+\\)[ \t\n]*("
;;               end t)
;;         (condition-case nil
;;             (let ((instance-type (verilog-match-string 1)) (instance-name (verilog-match-string 3))
;;                   (instance-pos (match-beginning 0)))
;;               (backward-char)
;;               (forward-sexp)
;;               (when (looking-at "[ \t\n]*;")
;;                 (if (string= instance-type "modport")
;;                     (push (cons instance-name instance-pos) instance-alist)
;;                   (if verilog-imenu-show-instance-type
;;                       (push (cons (concat instance-name " <" instance-type ">") instance-pos) instance-alist)
;;                     (push (cons instance-name instance-pos) instance-alist)))))
;;           (error nil)))
;;       instance-alist)))
;;
;; (defun verilog-imenu-create-find-data-types (data-type end)
;;   (save-excursion
;;     (let ((type-alist '()))
;;       (while (re-search-forward (concat "^\\s-*\\(typedef\\s-+\\)?\\(" data-type "\\)\\s-*\\([^{;]*?\\)\\s-*\\([{;]\\)") end t)
;;         (condition-case nil
;;             (let ((type-id (verilog-match-string 3)) (type-terminator (verilog-match-string 4))
;;                   (type-pos (match-beginning 0)))
;;               (if (string= type-terminator "{")
;;                   (progn (backward-char)
;;                          (forward-sexp)
;;                          (re-search-forward "\\([a-zA-Z0-9_]+\\)[ \t\n]*;" end t)
;;                          (push (cons (verilog-match-string 1) type-pos) type-alist))
;;                 (push (cons (verilog-trim-trailing-whitespace type-id) type-pos) type-alist)))
;;           (error nil)))
;;       type-alist)))
;;
;; (defun verilog-imenu-create-parse-entity ()
;;   (when (re-search-forward "^\\s-*\\(module\\|interface\\|package\\)[ \t\n]+\\([a-zA-Z0-9_]+\\)" nil t)
;;     (let ((entity-type (verilog-match-string 1)) (entity-name (verilog-match-string 2))
;;           (entity-start (match-beginning 0)) (entity-end) (end 0) (final-alist '())
;;           (nested-entity) (found-routine) (routine-type)
;;           (instance-alist '()) (modport-alist '()) (module-alist '()) (interface-alist '()) (package-alist '())
;;           (enum-alist '()) (struct-alist '()) (union-alist '()) (function-alist '()) (task-alist '()))
;;
;;       ;; Find entity end
;;       (let ((depth 1))
;;         (while (progn
;;                  (re-search-forward (concat "^\\s-*\\(end" entity-type "\\|" entity-type "\\)") nil t)
;;                  (if (string= (verilog-match-string 1) entity-type)
;;                      (setq depth (1+ depth))
;;                    (setq depth (1- depth)))
;;                  (> depth 0))))
;;       (setq entity-end (point-at-eol))
;;
;;       ;; Work through entity
;;       (goto-char entity-start)
;;       (end-of-line)
;;       (while (< end entity-end)
;;
;;         ;; Look for a nested entity or routine
;;         (save-excursion
;;           (if (re-search-forward "^\\s-*\\(module\\|interface\\|package\\|function\\|task\\)[ \t\n]+\\([a-zA-Z0-9_]+\\)" entity-end t)
;;               (let ((found-item (verilog-match-string 1)))
;;                 (setq end (point-at-bol))
;;                 (if (or (string= found-item "function") (string= found-item "task"))
;;                     (progn
;;                       (beginning-of-line)
;;                       (re-search-forward "^\\s-*\\(function\\|task\\).*?\\([a-zA-Z0-9_]+\\)\\s-*[(;]" nil t)
;;                       (if (string= found-item "function")
;;                           (push (cons (verilog-match-string 2) (point-at-bol)) function-alist)
;;                         (push (cons (verilog-match-string 2) (point-at-bol)) task-alist))
;;                       (setq nested-entity nil
;;                             found-routine t
;;                             routine-type found-item))
;;                   (setq nested-entity t
;;                         found-routine nil)))
;;             (setq nested-entity nil
;;                   found-routine nil
;;                   end entity-end)))
;;
;;         ;; Find instances or modports
;;         (if (string= entity-type "interface")
;;             (setq modport-alist (append modport-alist (verilog-imenu-create-find-instances-or-modports end)))
;;           (setq instance-alist (append instance-alist (verilog-imenu-create-find-instances-or-modports end))))
;;
;;         ;; Find enums, structs, and unions
;;         (setq enum-alist (append enum-alist (verilog-imenu-create-find-data-types "enum" end)))
;;         (setq struct-alist (append struct-alist (verilog-imenu-create-find-data-types "struct" end)))
;;         (setq union-alist (append union-alist (verilog-imenu-create-find-data-types "union" end)))
;;
;;         ;; Goto next point of interest
;;         (goto-char end)
;;
;;         ;; If a routine was found, jump over it
;;         (when found-routine
;;           (re-search-forward (concat "^\\s-*end" routine-type) entity-end t)
;;           (end-of-line))
;;
;;         ;; If a nested entity was found, parse it
;;         (when nested-entity
;;           (let (sub-entity)
;;             (setq sub-entity (verilog-imenu-create-parse-entity))
;;             (when sub-entity
;;               (let ((entity-type (car sub-entity)) (entity-info (cdr sub-entity)))
;;                 (cond ((string= entity-type "module")
;;                        (push entity-info module-alist))
;;                       ((string= entity-type "interface")
;;                        (push entity-info interface-alist))
;;                       ((string= entity-type "package")
;;                        (push entity-info package-alist))))))))
;;
;;       ;; Assemble
;;       (if verilog-imenu-flatten
;;           (progn
;;             (push (cons entity-name entity-start) final-alist)
;;             (setq final-alist (verilog-imenu-add-flattened entity-name instance-alist final-alist))
;;             (setq final-alist (verilog-imenu-add-flattened entity-name modport-alist final-alist))
;;             (setq final-alist (verilog-imenu-add-flattened entity-name task-alist final-alist))
;;             (setq final-alist (verilog-imenu-add-flattened entity-name function-alist final-alist))
;;             (setq final-alist (verilog-imenu-add-flattened entity-name union-alist final-alist))
;;             (setq final-alist (verilog-imenu-add-flattened entity-name struct-alist final-alist))
;;             (setq final-alist (verilog-imenu-add-flattened entity-name enum-alist final-alist))
;;             (setq final-alist (verilog-imenu-add-flattened entity-name package-alist final-alist))
;;             (setq final-alist (verilog-imenu-add-flattened entity-name interface-alist final-alist))
;;             (setq final-alist (verilog-imenu-add-flattened entity-name module-alist final-alist))
;;             (goto-char entity-end)
;;             final-alist)
;;         (push (cons "*Definition*" entity-start) final-alist)
;;         (setq final-alist (verilog-imenu-create-add-item-alist "Instances" instance-alist final-alist))
;;         (setq final-alist (verilog-imenu-create-add-item-alist "Modports" modport-alist final-alist))
;;         (setq final-alist (verilog-imenu-create-add-item-alist "Tasks" task-alist final-alist))
;;         (setq final-alist (verilog-imenu-create-add-item-alist "Functions" function-alist final-alist))
;;         (setq final-alist (verilog-imenu-create-add-item-alist "Unions" union-alist final-alist))
;;         (setq final-alist (verilog-imenu-create-add-item-alist "Structs" struct-alist final-alist))
;;         (setq final-alist (verilog-imenu-create-add-item-alist "Enums" enum-alist final-alist))
;;         (setq final-alist (verilog-imenu-create-add-item-alist "Packages" package-alist final-alist))
;;         (setq final-alist (verilog-imenu-create-add-item-alist "Interfaces" interface-alist final-alist))
;;         (setq final-alist (verilog-imenu-create-add-item-alist "Modules" module-alist final-alist))
;;         (goto-char entity-end)
;;         (cons entity-type (cons entity-name final-alist))))))
;;
;; (defun verilog-imenu-add-flattened (name alist final-alist)
;;   (mapc (lambda (item)
;;           (if verilog-imenu-qualify-names
;;               (push (cons (concat name "::" (car item)) (cdr item)) final-alist)
;;             (push (cons (car item) (cdr item)) final-alist)))
;;         alist)
;;   final-alist)
;;
;; (defun verilog-imenu-create-index-function ()
;;   "Create Verilog imenu index."
;;   (goto-char (point-min))
;;   (let ((final-alist '()) (module-alist '()) (interface-alist '()) (package-alist '())
;;         (entity))
;;     (while (progn
;;              (setq entity (verilog-imenu-create-parse-entity))
;;              (when entity
;;                (if verilog-imenu-flatten
;;                    (mapc (lambda (x) (push x final-alist)) entity)
;;                  (let ((entity-type (car entity)) (entity-info (cdr entity)))
;;                    (cond ((string= entity-type "module")
;;                           (push entity-info module-alist))
;;                          ((string= entity-type "interface")
;;                           (push entity-info interface-alist))
;;                          ((string= entity-type "package")
;;                           (push entity-info package-alist))))))))
;;     (if verilog-imenu-flatten
;;         (setq final-alist (verilog-sort-alist-by-car-string final-alist))
;;       (setq final-alist (verilog-imenu-create-add-item-alist "Packages" package-alist final-alist))
;;       (setq final-alist (verilog-imenu-create-add-item-alist "Interfaces" interface-alist final-alist))
;;       (setq final-alist (verilog-imenu-create-add-item-alist "Modules" module-alist final-alist)))
;;     final-alist))
;;
;;; Align

(require 'align)

(defcustom align-verilog-rules-list
  `(
    (verilog-declaration
     (regexp . "\\(logic\\|input\\|output\\|inout\\|wire\\|reg\\)\\(\\s-+[[][^]]+[]]\\|\\)\\(\\s-+\\)\\S-")
     (group . (3)))

    (verilog-asgn_param
     (regexp . "\\(assign\\|parameter\\)\\(\\s-+\\)\\S-")
     (group . (2)))

    (verilog-assign
     (regexp . "\\S-+\\(\\s-*\\)[!=><]+\\(\\s-*\\)\\S-")
     (group . (1 2)))

    (verilog-ports-no-comment
     (regexp . "[.][a-zA-Z0-9_]+\\(\\s-+\\)\\S-")
     (group . (1)))

    (verilog-ports-comment
     (regexp . "[.][a-zA-Z0-9_]+\\(\\s-+\\)\\S-.*\\(\\s-+\\)[/]+")
     (group . (1 2)))
    )
  "Verilog alignment rules."
  :type align-rules-list-type
  :group 'align)

(defcustom align-exclude-verilog-rules-list
  `(
    (exc-dq-string
     (regexp . "\"\\([^\"\n]+\\)\"")
     (repeat . t)
     (modes . align-dq-string-modes))

    (exc-open-comment
     (regexp . ,(function (lambda (end reverse)
        (funcall (if reverse 're-search-backward 're-search-forward)
                 (concat "[^ \t\n\\\\]" (regexp-quote comment-start)
                         "\\(.+\\)$") end t))))
     (modes . align-open-comment-modes))
    )
  "Verilog alignment exclusion rules."
  :type align-exclude-rules-list-type
  :group 'align)

(put 'align-verilog-rules-list 'risky-local-variable t)
(put 'align-exclude-verilog-rules-list 'risky-local-variable t)

(add-to-list 'align-dq-string-modes 'verilog-mode)
(add-to-list 'align-open-comment-modes 'verilog-mode)

;;  ;;; Hook
;;
;; (defvar verilog-port-menu
;;       '("VerilogPorts"
;;         ["Copy port" my-verilog-port-copy t]
;;         ["Paste port as instance" my-verilog-port-paste-inst (and my-verilog-inst-name my-verilog-inst-ports)]
;;         ["Paste port wires" my-verilog-port-paste-wires my-verilog-inst-ports]
;;         )
;;       "Verilog port helper functions")

(defun my-verilog-mode-auto-inst ()
  "Auto-inst stuff."
  (interactive)
  (verilog-auto-inst))

(define-key verilog-mode-map (kbd ":") nil)
(define-key verilog-mode-map (kbd ";") nil)
(define-key verilog-mode-map (kbd "RET") nil)
(define-key verilog-mode-map (kbd "TAB") nil)
(define-key verilog-mode-map (kbd "`") nil)

(defun my-verilog-mode-hook ()
  (modify-syntax-entry ?` ".")
  ;; (setq imenu-generic-expression nil)
  ;; (setq imenu-create-index-function 'verilog-imenu-create-index-function)
  (setq align-mode-rules-list align-verilog-rules-list)
  (setq align-exclude-rules-list align-exclude-verilog-rules-list))

(add-hook 'verilog-mode-hook 'my-verilog-mode-hook)

(provide 'my-verilog-mode)
