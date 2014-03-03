;;; my-sgml-xml.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default sgml-basic-offset 4
              sgml-xml-mode t)

(eval-after-load "sgml-mode"
  '(progn

     (defadvice sgml-xml-guess (around my-sgml-xml-guess activate)
       ad-do-it
       (unless ad-return-value
         (when (string= "html" (file-name-extension (or buffer-file-name "")))
           (setq ad-return-value t))))

     (defun my-sgml-mode-hook ()
       (define-key sgml-mode-map (kbd "C-c >") 'sgml-skip-tag-forward)
       (define-key sgml-mode-map (kbd "C-c <") 'sgml-skip-tag-backward)
       (define-key sgml-mode-map (kbd "C-c &") 'sgml-name-char))

     (add-hook 'sgml-mode-hook 'my-sgml-mode-hook)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-after-load "nxml-mode"
  '(progn

     (defun my-nxml-mode-hook ()
       (define-key nxml-mode-map (kbd "C-c >") (lambda () (interactive) (let ((nxml-sexp-element-flag t)) (nxml-forward-balanced-item))))
       (define-key nxml-mode-map (kbd "C-c <") (lambda () (interactive) (let ((nxml-sexp-element-flag t)) (nxml-forward-balanced-item -1))))
       (define-key nxml-mode-map (kbd "C-c &") 'nxml-insert-named-char))

     (add-hook 'nxml-mode-hook 'my-nxml-mode-hook)))

(provide 'my-sgml-xml)
