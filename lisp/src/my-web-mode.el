;;; my-web-mode.el

(require 'web-beautify)
(require 'flymake-easy)
(require 'flymake-eslint)

(setq web-mode-auto-close-style 1
      web-mode-enable-auto-closing t
      web-mode-enable-auto-indentation nil
      web-mode-enable-auto-pairing t
      web-mode-enable-current-element-highlight t
      web-mode-enable-html-entities-fontification t)

(add-to-list 'web-mode-comment-formats '("jsx" . "//"))

(defun my-web-mode-comment-insert ()
  (interactive)
  (web-mode-comment-insert))

(bind-keys :map web-mode-map
           ("C-c C-o" . my-web-mode-comment-insert))

(defun my-web-mode-beautify ()
  "Beautify according to mode."
  (interactive)
  (cond
   ((string= (file-name-extension (buffer-file-name)) "js")
    (web-beautify-js))
   ((string= (file-name-extension (buffer-file-name)) "html")
    (web-beautify-html))
   ((string= (file-name-extension (buffer-file-name)) "css")
    (web-beautify-css))))

(defun my-web-mode-electric-closer ()
  "Indent when entering a closer."
  (interactive "*")
  (insert last-command-event)
  (indent-according-to-mode))

(defvar my-web-mode-js-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd ")") 'my-web-mode-electric-closer)
    (define-key map (kbd "]") 'my-web-mode-electric-closer)
    (define-key map (kbd "}") 'my-web-mode-electric-closer)
    map)
  "my-web-mode-js-map keymap.")

(define-abbrev web-mode-abbrev-table
  "bind"
  ""
  (lambda ()
    (when (looking-back "this\.\\([^[:space:]]+\\)[[:space:]]+" (point-at-bol))
      (insert "= this." (match-string-no-properties 1) ".bind(this);"))))

(defun my-web-mode-js-imenu-index ()
  "Get imenu items in web-mode js variant."
  (let ((start (point-min)) end
        (eob (point-max))
        (item-alist '()))
    (goto-char start)
    (while start
      (setq start (text-property-any start eob 'face 'web-mode-function-name-face))
      (when start
        (setq end (or (next-single-property-change start 'face) eob))
        (push (cons (buffer-substring-no-properties start end) start) item-alist)
        (setq start end)))
    (goto-char (point-min))
    (while (re-search-forward "^\\s-*\\([^[:space:]]+\\)\\s-*=\\s-*function\\s-+" nil t)
      (push (cons (match-string-no-properties 1) (match-beginning 1)) item-alist))
    (nreverse item-alist)))

(define-minor-mode my-web-mode-js-mode
  "A minor mode for web-mode editing JavaScript."
  :init-value nil
  :lighter "/js")

(defun my-web-mode-hook ()
  (when (string= (file-name-extension (buffer-file-name)) "js")
    (web-mode-set-content-type "jsx")
    (setq comment-start "// "
          comment-end ""
          imenu-create-index-function 'my-web-mode-js-imenu-index)
    (my-web-mode-js-mode 1)
    (flymake-eslint-load)))

(add-hook 'web-mode-hook 'my-web-mode-hook)

(provide 'my-web-mode)
