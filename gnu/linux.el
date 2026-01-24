;; X vs. terminal
(when window-system
  (normal-erase-is-backspace-mode 1))

;; Theme

;; Dark
(setq modus-vivendi-tinted-palette-overrides 
      '(
        (fg-main "#dddddd")
        (comment "#888888")
        ))

(defun dark ()
  "Load dark theme."
  (interactive)
  (my-theme-disable-all)
  (setq modus-themes-bold-constructs nil)
  (load-theme 'modus-vivendi-tinted t)
  (set-face-foreground 'bm-face nil)
  (set-face-background 'bm-face "#47284a")
  )

;; Light
(setq modus-operandi-palette-overrides
      '((comment "#c4c4c4")
        (constant magenta-faint)
        (variable cyan-intense)
        (string green-intense)))

(defun light ()
  "Load light theme."
  (interactive)
  (my-theme-disable-all)
  (setq modus-themes-bold-constructs 't)
  (load-theme 'modus-operandi t)
  (set-face-foreground 'bm-face nil)
  (set-face-background 'bm-face "#e9e0ee")
  )

;; Choose based on terminal
(let ((mode (getenv "TERM_MODE")))
  (if (or (not mode) (equal mode "light"))
      (light)
    (dark)))
