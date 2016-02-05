;;; my-js2-mode.el

(require 'tern)
(require 'js-doc)
(require 'web-beautify)
(require 'flymake-easy)
(require 'flymake-eslint)

(setq-default js2-basic-offset 4
              js2-global-externs '("window" "require" "define" "jQuery")
              js-doc-file-doc-lines '(js-doc-top-line
                                      " * @fileOverview\n"
                                      " * @name %F\n"
                                      js-doc-bottom-line)
              js-doc-bottom-line " */\n\n")

;; Copied from js2-mode, but don't use font-lock-doc-face for jsdoc
(defun js2-record-comment (token)
  "Record a comment in `js2-scanned-comments'."
  (let ((ct (js2-token-comment-type token))
        (beg (js2-token-beg token))
        (end (js2-token-end token)))
    (push (make-js2-comment-node :len (- end beg)
                                 :format ct)
          js2-scanned-comments)
    (when js2-parse-ide-mode
      (js2-record-face 'font-lock-comment-face token)
      (when (memq ct '(html preprocessor))
        ;; Tell cc-engine the bounds of the comment.
        (js2-record-text-property beg (1- end) 'c-in-sws t)))))

(defun my-js2-mode-electric-closer ()
  "Indent when entering a closer."
  (interactive "*")
  (insert last-command-event)
  (indent-according-to-mode))

(defun my-js2-mode-insert-doc ()
  "Insert JSDoc file or function doc."
  (interactive "*")
  (if (bobp)
      (progn (call-interactively 'js-doc-insert-file-doc)
             (insert "\n")
             (search-backward "@fileOverview")
             (goto-char (match-end 0))
             (insert " "))
    (call-interactively 'js-doc-insert-function-doc)
    (search-forward "* ")))

(defun my-js2-prev-error ()
  "Goto js2-mode previous error."
  (interactive)
  (js2-next-error -1))

(defun my-js2-mode-hook ()
  (setq-local mode-name "js2")
  (auto-complete-mode 1)
  (bind-keys :map js2-mode-map
             ("M-\\" . ac-start)
             ("C-c C-j" . my-js2-mode-insert-doc)
             ("C-c `" . js2-next-error)
             ("C-c ~" . my-js2-prev-error)
             ("@" . js-doc-insert-tag)
             (")" . my-js2-mode-electric-closer)
             ("]" . my-js2-mode-electric-closer)
             ("}" . my-js2-mode-electric-closer))
  (flymake-eslint-load)
  (my-tern-mode))

(add-hook 'js2-mode-hook 'my-js2-mode-hook)

(provide 'my-js2-mode)
