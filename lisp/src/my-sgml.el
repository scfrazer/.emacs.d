;;; my-sgml.el

(require 'sgml-mode)

(setq-default sgml-basic-offset 4
              sgml-xml-mode t)

(defadvice sgml-xml-guess (around my-sgml-xml-guess activate)
  ad-do-it
  (unless ad-return-value
    (when (string= "html" (file-name-extension (or buffer-file-name "")))
      (setq ad-return-value t))))

(provide 'my-sgml)
