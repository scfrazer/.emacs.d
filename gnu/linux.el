;; X vs. terminal
(when window-system
  (normal-erase-is-backspace-mode 1))

;; Theme
;; (my-theme-light)
(my-theme-disable-all)
(load-theme 'smf-github t)
