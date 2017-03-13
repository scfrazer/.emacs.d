;;; my-js2-mode.el

;; (require 'tern)
(require 'js-doc)
(require 'web-beautify)
(require 'flymake-easy)
(require 'flymake-eslint)

(setq-default js2-basic-offset 4
              js2-global-externs '("window" "process" "require" "define" "jQuery" "tinymce" "moment")
              js2-idle-timer-delay 0.5
              js2-mode-show-parse-errors nil
              js2-mode-show-strict-warnings nil
              js2-strict-trailing-comma-warning nil
              js-doc-file-doc-lines '(js-doc-top-line
                                      " * @fileOverview\n"
                                      " * @name %F\n"
                                      js-doc-bottom-line)
              js-doc-top-line "/**************************************************************************\n"
              js-doc-bottom-line " **************************************************************************/\n\n")

;; Copied from js2-mode, but always reparse while ignore AMD wrapper
(defun js2-mode-create-imenu-index ()
  "Return an alist for `imenu--index-alist'."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char 0)
      (when (looking-at "\\s-*define\\s-*(")
        (search-forward "(")
        (let ((beg (point)))
          (search-forward "{")
          (backward-char)
          (forward-sexp)
          (narrow-to-region beg (point))))
      (js2-reparse 'force)
      (prog1
          (js2-build-imenu-index)
        (setq js2-imenu-recorder nil
              js2-imenu-function-map nil)))))

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
  "Insert function doc or big delimiter comment."
  (interactive "*")
  (back-to-indentation)
  (if (looking-at ".*\\_<function")
      (call-interactively 'js-doc-insert-function-doc)
    (beginning-of-line)
    (let ((pos (point)))
      (insert js-doc-top-line
              js-doc-description-line
              js-doc-bottom-line)
      (indent-region pos (point))
      (goto-char pos)))
  (search-forward "* "))

(defun my-js2-prev-error ()
  "Goto js2-mode previous error."
  (interactive)
  (js2-next-error -1))

(define-abbrev js2-mode-abbrev-table
  "lint"
  "// eslint-disable-line no-unused-vars")

;; TODO Make this advice
;;     (defun my-find-tag (&optional arg)
;;       "Find tag at point or plain find tag"
;;       (interactive "P")
;;       (if (eq major-mode 'js2-mode)
;;           (if arg (tern-find-definition-by-name) (tern-find-definition))
;;         (if arg (etags-select-find-tag) (etags-select-find-tag-at-point))))
;;     (defun my-pop-tag-mark ()
;;       "Pop tag mark."
;;       (interactive)
;;       (if (eq major-mode 'js2-mode)
;;           (tern-pop-find-definition)
;;         (pop-tag-mark)))

(defun my-js2-mode-hook ()
  (setq-local mode-name "js2")
  ;; (auto-complete-mode 1)
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
  ;; (tern-mode)
  )

(add-hook 'js2-mode-hook 'my-js2-mode-hook)

(provide 'my-js2-mode)
