;;; my-python.el

(require 'flymake)
(require 'flymake-cursor)

(setq-default python-check-command "pylint_etc_wrapper.py -c"
              python-continuation-offset 4
              python-indent 4)

(defvar my-python-flymake-checker "pylint_etc_wrapper.py")

(defun my-flymake-python ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))
         (local-file (file-relative-name temp-file (file-name-directory buffer-file-name))))
    (list my-python-flymake-checker (list local-file))))

(add-to-list 'flymake-allowed-file-name-masks '("\\.py\\'" my-flymake-python))

(provide 'my-python)
