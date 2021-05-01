;;; my-bm.el  -*- lexical-binding: t; -*-

(require 'bm)
(require 'desktop)

(setq bm-goto-position nil
      bm-recenter t
      bm-wrap-immediately nil)

(defun my-bm-toggle-or-show (&optional arg)
  "Toggle or show bookmarks"
  (interactive "P")
  (if arg (bm-show-all) (my-bm-toggle)))

(defun my-bm-toggle ()
  "Like `bm-toggle', but records position for `desktop'."
  (call-interactively 'bm-toggle)
  (my-bm-save-positions))

(defvar-local my-bm-positions nil
  "`bm' positions in a buffer")
(setq desktop-locals-to-save (add-to-list 'desktop-locals-to-save 'my-bm-positions))

(defun my-bm-save-positions ()
  "Save positions of bm bookmarks in this buffer."
  (dolist (buf (buffer-list))
    (when (buffer-file-name buf)
      (setq my-bm-positions
            (mapcar (lambda (ovl)
                      (overlay-start ovl))
                    (bm-overlay-in-buffer))))))
(add-hook 'after-save-hook #'my-bm-save-positions)

(defun my-bm-restore-positions ()
  "Restore positions of bm bookmarks after desktop is loaded."
  (dolist (buf (buffer-list))
    (with-current-buffer
      buf
      (save-restriction
        (widen)
        (save-excursion
          (dolist (pos my-bm-positions)
            (goto-char pos)
            (unless (bm-bookmark-at (point))
              (bm-bookmark-add))))))))

(add-hook 'desktop-after-read-hook #'my-bm-restore-positions)

(provide 'my-bm)
