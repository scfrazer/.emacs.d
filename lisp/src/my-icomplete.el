;;; my-icomplete.el  -*- lexical-binding: t; -*-

(require 'icomplete)
(require 'orderless)
(require 'icomplete-vertical)

(setq-default completion-ignore-case t
              completion-styles '(orderless)
              icomplete-compute-delay 0.2
              icomplete-delay-completions-threshold 100
              icomplete-max-delay-chars 2
              icomplete-tidy-shadowed-file-names t
              read-buffer-completion-ignore-case t
              read-file-name-completion-ignore-case t)

(define-key icomplete-minibuffer-map (kbd "<down>") 'icomplete-forward-completions)
(define-key icomplete-minibuffer-map (kbd "<up>")   'icomplete-backward-completions)
(define-key icomplete-minibuffer-map (kbd "C-j")    'exit-minibuffer)
(define-key icomplete-minibuffer-map (kbd "C-n")    'icomplete-forward-completions)
(define-key icomplete-minibuffer-map (kbd "C-p")    'icomplete-backward-completions)

(defun my-icomplete--fido-mode-setup ()
  "Customize icomplete fido mode."
  (setq-local completion-styles '(orderless)))

(advice-add #'icomplete--fido-mode-setup :after #'my-icomplete--fido-mode-setup)

(icomplete-vertical-mode 1)
(fido-mode 1)

(provide 'my-icomplete)
