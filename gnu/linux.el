;; X vs. terminal
(when window-system
  (normal-erase-is-backspace-mode 1))

;; Theme
(setq modus-vivendi-tinted-palette-overrides '((fg-main "#e0e0e0")))
(load-theme 'modus-vivendi-tinted t)
