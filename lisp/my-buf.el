;;; my-buf.el

;; Ignore buffers

(defvar my-buf-always-show-regexps
  (list (concat "^"
                (regexp-opt (list
                             "*Customize"
                             "*Fd"
                             "*Find"
                             "*Man"
                             "*Occur"
                             "*asic-compile"
                             "*compilation"
                             "*git-simple"
                             "*grep"
                             "*info"
                             "*rg"
                             "*scratch"
                             "*vc-dir"
                             ))))
  "*Buffer regexps to always show when buffer switching.")

(defvar my-buf-never-show-regexps '("^\\s-" "^\\*" "TAGS$")
  "*Buffer regexps to never show when buffer switching.")

(defun my-buf-str-in-regexp-list (str regexp-list)
  "Return non-nil if str matches anything in regexp-list."
  (let ((case-fold-search nil))
    (catch 'done
      (dolist (regexp regexp-list)
        (when (string-match regexp str)
          (throw 'done t))))))

(defun my-buf-ignore-buffer (name)
  "Return non-nil if the named buffer should be ignored."
  (and (not (my-buf-str-in-regexp-list name my-buf-always-show-regexps))
       (my-buf-str-in-regexp-list name my-buf-never-show-regexps)))

;; Toggle buffers

(defun my-buf-toggle ()
  "Toggle buffers, ignoring certain ones."
  (interactive)
  (catch 'done
    (dolist (buf (buffer-list))
      (unless (or (equal (current-buffer) buf)
                  (my-buf-ignore-buffer (buffer-name buf)))
        (switch-to-buffer buf)
        (throw 'done t)))))

;; Done

(provide 'my-buf)
