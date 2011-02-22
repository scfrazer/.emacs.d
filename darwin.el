(setq default-frame-alist
      '((width . 100) (height . 60)))

;; Exec this line to insert the current font as a setting:
;; (insert "\n(set-default-font \"" (cdr (assoc 'font (frame-parameters))) "\")\n")
;; (set-default-font "-apple-Monaco-medium-normal-normal-*-12-*-*-*-m-0-iso10646-1")
(set-default-font "-apple-Droid_Sans_Mono_Slashed-medium-normal-normal-*-12-*-*-*-m-0-iso10646-1")

(setq exec-path (append exec-path (list "/usr/local/bin"
                                        "/usr/local/git/bin"
                                        "/usr/X11/bin"
                                        "/opt/local/bin")))

(setq-default save-interprogram-paste-before-kill nil
              select-active-regions nil)

(grep-apply-setting 'grep-template "grep -nH -d skip -I -E -e <R> <C> <F>")
(grep-apply-setting 'grep-find-template "find <D> <X> -type f <F> -print0 | xargs -0 -e grep -nH -I -E -e <R> <C>")

;; Theme

(my-theme-whiteboard)

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
