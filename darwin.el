(setq default-frame-alist
      '((width . 120) (height . 60)))

;(insert "\n(set-default-font \"" (cdr (assoc 'font (frame-parameters))) "\")\n")
; (set-default-font "-apple-Fixed-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1")

(setq grep-program "egrep")
(setq igrep-program "egrep")
(setq igrep-find-program "find")
(setq ispell-program-name "/opt/local/bin/ispell")

;; Clipboard problems

(setq-default save-interprogram-paste-before-kill nil
              select-active-regions nil)

;; Theme

(my-theme-whiteboard)

(setq my-set-cursor-color-normal-color "Green3"
      my-set-cursor-color-read-only-color "Yellow3"
      my-set-cursor-color-overwrite-color "Red3")

(setq-default org-priority-faces '((?A . (:foreground "IndianRed3" :weight bold))
                                   (?B . (:foreground "SteelBlue3" :weight bold))
                                   (?C . (:foreground "SpringGreen3" :weight bold)))
              org-todo-keyword-faces '(("TODO"       . (:foreground "Red3" :weight bold))
                                       ("STARTED"    . (:foreground "Blue" :weight bold))
                                       ("WAITING"    . (:foreground "Orange3" :weight bold))
                                       ("DONE"       . (:foreground "Green4" :weight bold))
                                       ("MAYBE"      . (:foreground "Cyan4" :weight bold))
                                       ("SOMEDAY"    . (:foreground "Cyan4" :weight bold))
                                       ("CANCELED"   . (:foreground "Green4" :weight bold))
                                       ("REASSIGNED" . (:foreground "Green4" :weight bold))))
(eval-after-load "org"
  '(progn
     (set-face-foreground 'org-tag "RoyalBlue1")
     (set-face-foreground 'org-checkbox-statistics-todo "HotPink4")
     (set-face-foreground 'org-checkbox-statistics-done "Green4")
     (set-face-foreground 'org-special-keyword "DarkOrange4")
     (set-face-foreground 'org-date "DeepSkyBlue4")))
