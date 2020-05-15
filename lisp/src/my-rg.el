;;; my-rg.el

(require 'rg)

(setq-default rg-ignore-ripgreprc nil
              rg-custom-type-aliases nil
              rg-hide-command t)

(rg-define-toggle "--max-depth 1" "m")
(rg-define-toggle "-uu" "u")
(rg-define-toggle "--word-regexp" "w")
(rg-define-toggle "-z" "z")

(defun my-rg-toggle-group ()
  "Toggle grouping and rerun."
  (interactive)
  (setq rg-group-result (not rg-group-result))
  (rg-rerun))

(rg-define-search my-rg-regexp-ask
  :query ask
  :format regexp
  :files ask
  :dir current)

(rg-define-search my-rg-current-ask
  :query ask
  :format literal
  :files current
  :dir current)

(rg-define-search my-rg-current-region
  :query (buffer-substring-no-properties (region-beginning) (region-end))
  :format literal
  :files current
  :dir current)

(defun my-rg-current (&optional arg)
  "Ripgrep in current directory.
With ARG do literal with current region."
  (interactive "P")
  (if arg
      (call-interactively 'my-rg-current-region)
    (call-interactively 'my-rg-current-ask)))

(rg-define-search my-rg-project-ask
  :query ask
  :format literal
  :files current
  :dir project)

(rg-define-search my-rg-project-region
  :query (buffer-substring-no-properties (region-beginning) (region-end))
  :format literal
  :files current
  :dir project)

(defun my-rg-project (&optional arg)
  "Ripgrep in current project.
With ARG do literal with current region."
  (interactive "P")
  (if arg
      (call-interactively 'my-rg-project-region)
    (call-interactively 'my-rg-project-ask)))

(define-key rg-mode-map (kbd "C-n") nil)
(define-key rg-mode-map (kbd "C-p") nil)
(define-key rg-mode-map (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode)
(define-key rg-mode-map (kbd "G") 'my-rg-toggle-group)
(define-key rg-mode-map (kbd "L") 'rg-list-searches)
(define-key rg-mode-map (kbd "N") 'rg-next-file)
(define-key rg-mode-map (kbd "P") 'rg-prev-file)
(define-key rg-mode-map (kbd "l") 'rg-rerun-change-literal)

(defun my-rg-header-render-toggle (on)
  "Return a fontified toggle symbol.
If ON is non nil, render \"on\" string, otherwise render \"off\"
string."
  `(:eval (let* ((on ,on)
                 (value (if on "on" "off"))
                 (face (if on 'rg-toggle-on-face 'rg-toggle-off-face)))
            (propertize value 'face face))))

(defun rg-create-header-line (search full-command)
  "Create the header line for SEARCH.
If FULL-COMMAND specifies if the full command line search was done."
  (let ((itemspace "  "))
    (setq header-line-format
          (if full-command
              (list (rg-header-render-label "command line") "no refinement")
            (list
             (rg-header-render-label `((rg-search-literal ,search)
                                       ("literal" rg-literal-face)
                                       ("regexp" rg-regexp-face)))
             `(:eval (rg-search-pattern ,search))
             itemspace
             (rg-header-render-label "files")
             `(:eval (let ((str (rg-search-files ,search)))
                       (if (or (string= str "all")
                               (string= str "any"))
                           str
                         (propertize str 'face 'font-lock-warning-face))))
             itemspace
             (rg-header-render-label "case")
             (my-rg-header-render-toggle
              `(not (member "-i" (rg-search-toggle-flags ,search))))
             itemspace
             (rg-header-render-label "word")
             (my-rg-header-render-toggle
              `(member "--word-regexp" (rg-search-toggle-flags ,search)))
             itemspace
             (rg-header-render-label ".ignore")
             (my-rg-header-render-toggle
              `(not (member "--no-ignore" (rg-search-toggle-flags ,search))))
             itemspace
             (rg-header-render-label "unrestricted")
             (my-rg-header-render-toggle
              `(member "-uu" (rg-search-toggle-flags ,search)))
             itemspace
             (rg-header-render-label "max-depth")
             (my-rg-header-render-toggle
              `(member "--max-depth 1" (rg-search-toggle-flags ,search)))
             itemspace
             (rg-header-render-label "zip")
             (my-rg-header-render-toggle
              `(member "-z" (rg-search-toggle-flags ,search)))
             itemspace
             (rg-header-render-label "hits")
             '(:eval (format "%d" rg-hit-count)))))))

(provide 'my-rg)
