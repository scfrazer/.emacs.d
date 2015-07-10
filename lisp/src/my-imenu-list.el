;;; my-imenu-list.el

(require 'imenu-list)

(defadvice delete-other-windows (around my-imenu-list-delete-other-windows activate)
  "Don't let the imenu-list window get deleted by `delete-other-windows'."
  (let ((iwin (get-buffer-window imenu-list-buffer-name)))
    (if iwin
        (let ((current-window (selected-window)))
          (dolist (win (window-list))
            (when (and (window-live-p win)
                       (not (eq current-window win))
                       (not (eq win iwin)))
              (delete-window win))))
      ad-do-it)))

(defadvice imenu-list-minor-mode (after my-imenu-list-minor-mode activate)
  "Kill imenu-list buffer when minor mode is off."
  (unless imenu-list-minor-mode
    (let ((buf (get-buffer imenu-list-buffer-name)))
      (when buf
        (kill-buffer buf)))))

(provide 'my-imenu-list)
