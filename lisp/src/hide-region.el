;;; hide-region.el

(defvar hide-region-before-string "@[")
(defvar hide-region-after-string "]@")

(defface hide-region-face
  '((t (:background "pink1")))
  "Face for hide-region markers."
  :group 'faces)

(defun hide-region ()
  "Hide the current region."
  (interactive)
  (let ((ovl (make-overlay (mark) (point))))
    (overlay-put ovl 'invisible t)
    (overlay-put ovl 'before-string
                 (propertize hide-region-before-string 'font-lock-face 'hide-region-face))
    (overlay-put ovl 'after-string
                 (propertize hide-region-after-string 'font-lock-face 'hide-region-face))))

(defun unhide-region ()
  "Unhide the region at point."
  (interactive)
  (dolist (ovl (overlays-in (1- (point)) (1+ (point))))
    (when (string= (plist-get (overlay-properties ovl) 'before-string) hide-region-before-string)
      (delete-overlay ovl))))

(provide 'hide-region)
