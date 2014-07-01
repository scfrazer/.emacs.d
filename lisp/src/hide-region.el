;;; hide-region.el

(defvar hide-region-string "<...>")

(defface hide-region-face
  '((t (:background "#D7AFAF" :foreground "black")))
  "Face for hide-region markers."
  :group 'faces)

(defun hide-region-toggle ()
  "Hide/unhide region at point."
  (interactive)
  (let (unhidden)
    (dolist (ovl (overlays-in (1- (point)) (1+ (point))))
      (when (plist-get (overlay-properties ovl) 'hide-region)
        (delete-overlay ovl)
        (setq unhidden t)))
    (unless unhidden
      (let ((ovl (make-overlay (mark) (point))))
        (overlay-put ovl 'hide-region t)
        (overlay-put ovl 'invisible t)
        (overlay-put ovl 'before-string (propertize hide-region-string 'font-lock-face 'hide-region-face))))))

(provide 'hide-region)
