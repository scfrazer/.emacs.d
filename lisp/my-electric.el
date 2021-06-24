;;; my-electric.el

(when (fboundp 'electric-indent-mode)
  (electric-indent-mode -1))
(when (fboundp 'minibuffer-electric-default-mode)
  (minibuffer-electric-default-mode 1))

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
      (when (and (member last-command-event '(?\) ?\] ?\}))
                 (looking-back "^\\s-*[])}]" (point-at-bol)))
        (indent-according-to-mode)))))

(advice-add 'electric-pair-post-self-insert-function :around #'my-electric-pair-post-self-insert-function)

(defun my-electric-pair-inhibit (char)
  ;; (let ((syn-next (char-syntax (following-char))))
  ;;   (or (eq syn-next ?w)
  ;;       (and (eq (char-syntax char) ?\()
  ;;            (eq syn-next ?\())
  ;;       (and (eq (char-syntax char) ?\")
  ;;            (not (nth 3 (syntax-ppss)))))))
  (not
   (let ((syn-next (char-syntax (following-char))))
     (or
      (eq syn-next ?\ )
      (eq syn-next ?\))
      (eolp)))))

(setq electric-pair-inhibit-predicate 'my-electric-pair-inhibit)

(provide 'my-electric)
