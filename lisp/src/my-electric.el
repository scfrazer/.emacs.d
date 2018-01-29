;;; my-electric.el

(when (fboundp 'electric-indent-mode)
  (electric-indent-mode -1))

(electric-pair-mode 1)

(defun my-electric-pair-post-self-insert-function (orig-fun)
  (let ((indent-after (and (eq last-command-event ?\n)
                           (< (1+ (point-min)) (point) (point-max))
                           (eq (save-excursion
                                 (skip-chars-backward "\t\s")
                                 (char-before (1- (point))))
                               (matching-paren (char-after))))))
    (apply orig-fun nil)
    (if indent-after
        (progn
          (indent-according-to-mode)
          (save-excursion
            (forward-line 1)
            (indent-according-to-mode)))
      (when (member last-command-event '(?\) ?\] ?\}))
        (indent-according-to-mode)))))

(advice-add 'electric-pair-post-self-insert-function :around #'my-electric-pair-post-self-insert-function)

(defun my-electric-pair-inhibit (char)
  (or
   ;; Same char is next
   (eq char (char-after))
   ;; Open paren next to word
   (and (eq (char-syntax char) ?\()
        (eq (char-syntax (following-char)) ?w))
   ;; Quotes closes an open string
   (and (eq (char-syntax char) ?\")
        (not (nth 3 (syntax-ppss))))))

(setq electric-pair-inhibit-predicate 'my-electric-pair-inhibit)

(provide 'my-electric)
