;;; my-tags.el  -*- lexical-binding: t; -*-

(defun my-complete-tag ()
  (interactive)
  (or tags-table-list
      tags-file-name
      (visit-tags-table-buffer))
  (let ((case-fold-search nil))
    (call-interactively 'complete-tag)))

;; (defun etags-tags-completion-table () ; Doc string?
;;   (let (table
;;         (progress-reporter
;;          (make-progress-reporter
;;           (format "Making tags completion table for %s..." buffer-file-name)
;;           (point-min) (point-max))))
;;     (save-excursion
;;       (goto-char (point-min))
;;       ;; This regexp matches an explicit tag name or the place where
;;       ;; it would start.
;;       (while (re-search-forward
;;               "[\f\t\n\r()=,; ]?\177\\(?:\\([^\n\001]+\\)\001\\)?"
;;               nil t)
;;         (push   (prog1 (if (match-beginning 1)
;;                            ;; There is an explicit tag name.
;;                            (buffer-substring (match-beginning 1) (match-end 1))
;;                          ;; No explicit tag name.  Backtrack a little,
;;                          ;; and look for the implicit one.
;;                          (goto-char (match-beginning 0))
;;                          (skip-chars-backward "^\f\t\n\r()=,; ")
;;                          (prog1
;;                              (buffer-substring (point) (match-beginning 0))
;;                            (goto-char (match-end 0))))
;;                   (progress-reporter-update progress-reporter (point)))
;;                 table)))
;;     table))

(provide 'my-tags)
