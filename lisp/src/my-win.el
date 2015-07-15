;;; my-win.el

(require 'my-buf)

;; Popup special buffers in windows below current window

(add-to-list 'display-buffer-alist
             '(my-win-popup-filter
               (display-buffer-reuse-window display-buffer-below-selected)
               (reusable-frames . visible)))

(defun my-win-popup-filter (buffer alist)
  "Filter for `display-buffer-alist' to popup these buffers at
below current window."
  (string-match (concat "\\`" (regexp-opt '(
                                            "*Compile-Log"
                                            "*Find"
                                            "*Help"
                                            "*Ibuffer"
                                            "*Occur"
                                            "*ag"
                                            "*compilation"
                                            "*grep"
                                            "*magit-diff"
                                            "*regman"
                                            "*vcs-compile"
                                            ))) buffer))


;; Splits

(defun my-win-split-vertically (&optional arg)
  "Like `split-window-vertically', but switch to other window after split.
With prefix arg, stay in current window but show different buffer in new window."
  (interactive "P")
  (split-window-vertically)
  (recenter)
  (other-window 1)
  (when arg
    (my-buf-toggle))
  (recenter)
  (when arg
    (other-window -1)))

(defun my-win-split-horizontally (&optional arg)
  "Like `split-window-horizontally', but switch to other window after split.
With prefix arg, stay in current window but show different buffer in new window."
  (interactive "P")
  (split-window-horizontally)
  (other-window 1)
  (when arg
    (my-buf-toggle)
    (other-window -1)))

;; Dynamic resizing

(defvar my-win-resize-transient-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "^") 'my-win-resize-up)
    (define-key map (kbd "V") 'my-win-resize-down)
    (define-key map (kbd "{") 'my-win-resize-left)
    (define-key map (kbd "}") 'my-win-resize-right)
    map)
  "Keymap used in window resizing transient mode.")

(defun my-win-resize-transient-mode ()
  "Start window resizing transient mode."
  (set-transient-map my-win-resize-transient-map t))

(defun my-win-get-vertical-position ()
  "Figure out if the current window is at the top, the bottom, or
in the middle of the frame."
  (let ((edges (window-edges)))
    (if (eq 0 (nth 1 edges))
        'top
      (if (eq (- (frame-height) 1) (nth 3 edges))
          'bottom
        'middle))))

(defun my-win-get-horizontal-position ()
  "Figure out if the current window is at the left, the right, or
in the middle of the frame."
  (let ((edges (window-edges)))
    (if (eq 0 (nth 0 edges))
        'left
      (if (eq (frame-width) (nth 2 edges))
          'right
        'middle))))

(defun my-win-resize-up ()
  "Resize window depending on where it is in the frame."
  (interactive)
  (enlarge-window (if (eq (my-win-get-vertical-position) 'bottom) 1 -1)))

(defun my-win-resize-down ()
  "Resize window depending on where it is in the frame."
  (interactive)
  (enlarge-window (if (eq (my-win-get-vertical-position) 'bottom) -1 1)))

(defun my-win-resize-left ()
  "Resize window depending on where it is in the frame."
  (interactive)
  (enlarge-window-horizontally (if (eq (my-win-get-horizontal-position) 'right) 1 -1)))

(defun my-win-resize-right ()
  "Resize window depending on where it is in the frame."
  (interactive)
  (enlarge-window-horizontally (if (eq (my-win-get-horizontal-position) 'right) -1 1)))

(defun my-win-resize-up-dwim ()
  "Resize window and start transient mode."
  (interactive)
  (my-win-resize-up)
  (my-win-resize-transient-mode))

(defun my-win-resize-down-dwim ()
  "Resize window and start transient mode."
  (interactive)
  (my-win-resize-down)
  (my-win-resize-transient-mode))

(defun my-win-resize-left-dwim ()
  "Resize window and start transient mode."
  (interactive)
  (my-win-resize-left)
  (my-win-resize-transient-mode))

(defun my-win-resize-right-dwim ()
  "Resize window and start transient mode."
  (interactive)
  (my-win-resize-right)
  (my-win-resize-transient-mode))

;; Done

(provide 'my-win)
