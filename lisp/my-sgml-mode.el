;;; my-sgml-mode.el

(setq-default sgml-basic-offset 4
              sgml-xml-mode t)

(defvar my-sgml-force-xml nil)
(defun xml ()
  "Set buffer as SGML/XML mode."
  (interactive)
  (setq my-sgml-force-xml t)
  (sgml-mode))

(defadvice sgml-xml-guess (around my-sgml-xml-guess activate)
  ad-do-it
  (unless ad-return-value
    (when (string= "html" (file-name-extension (or buffer-file-name "")))
      (setq ad-return-value t))
    (when my-sgml-force-xml
      (setq ad-return-value t
            my-sgml-force-xml nil))))

(defun my-sgml-mode-hook ()
  (define-key sgml-mode-map (kbd "C-c >") 'sgml-skip-tag-forward)
  (define-key sgml-mode-map (kbd "C-c <") 'sgml-skip-tag-backward)
  (define-key sgml-mode-map (kbd "C-c &") 'sgml-name-char))

(add-hook 'sgml-mode-hook 'my-sgml-mode-hook)

(provide 'my-sgml-mode)
