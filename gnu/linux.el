;; X vs. terminal
(when window-system
  (normal-erase-is-backspace-mode 1))

;; Theme

;; Dark
;; (setq modus-vivendi-tinted-palette-overrides '((fg-main "#e0e0e0")))
;; (load-theme 'modus-vivendi-tinted t)

;; Light
(setq modus-themes-bold-constructs 't)
(setq modus-operandi-palette-overrides
      '((comment "#c4c4c4")
        (constant magenta-faint)
        (variable cyan-intense)
        (string green-intense)))
(load-theme 'modus-operandi t)
