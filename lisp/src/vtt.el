;;; vtt.el

(require 'php-mode)
(require 'll-debug)

(defun ll-debug-get-java-function-name ()
  (interactive)
  (save-excursion
    (save-match-data
      (c-beginning-of-defun)
      (re-search-forward "\\s-+\\([~a-zA-Z0-9_:]+\\)\\s-*(")
      (match-string 1))))

(ll-debug-register-mode 'java-mode
;;                        "global.log(" ");"
                        "Log.info(" ");"
                        '(nil "\""
                              (concat (ll-debug-create-next-debug-string)
                                      " (" (buffer-name) " - "
                                      (ll-debug-get-java-function-name)
                                      ")")
                              "\"")
                        '(nil "\""
                              (concat (ll-debug-create-next-debug-string)
                                      " (" (buffer-name) " - "
                                      (ll-debug-get-java-function-name)
                                      ")")
                              "\""
                              ("Variable name: "
                               "+\"  " str ":\"+" str)))

(defun ll-debug-get-php-function-name ()
  (interactive)
  (save-excursion
    (save-match-data
      (php-beginning-of-defun)
      (if (bobp)
          "<none>"
        (re-search-forward "\\s-\\([^ \t\n]+\\)\\s-*(")
        (match-string 1)))))

(ll-debug-register-mode 'php-mode
;;                         "Vtt_Log::getInstance()->log(" ", Vtt_Log::INFO);"
                        "trigger_error(" ");"
                        '(nil "\""
                              (concat (ll-debug-create-next-debug-string)
                                      " (" (buffer-name) " - "
                                      (ll-debug-get-php-function-name)
                                      ")")
                              "\"")
                        '(nil "\""
                              (concat (ll-debug-create-next-debug-string)
                                      " (" (buffer-name) " - "
                                      (ll-debug-get-php-function-name)
                                      ")")
                              "\""
                              ("Variable name: "
                               ".\"  \\" str ":\".print_r(" str ", true)")))

(defun flymake-php-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "/auto/vtt/www/prod/dev/local/bin/php-lint" (list local-file))))

(defun my-php-mode-hook ()
  (flymake-mode 1))
(add-hook 'php-mode-hook 'my-php-mode-hook)

;; (add-to-list 'my-compile-command "cd /auto/vtt/www/prod/dev/scfrazer/vtt/vtt ; make")
(add-to-list 'my-compile-command "cd /auto/vtt/www/prod/dev/scfrazer/vtt3 ; make")

(provide 'vtt)
