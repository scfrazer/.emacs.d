;;; vtt.el

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

;; (add-to-list 'my-compile-command "cd /auto/vtt/www/prod/dev/scfrazer/vtt/vtt ; make")
(add-to-list 'my-compile-command "cd /auto/vtt/www/prod/dev/scfrazer/vtt3 ; make")

(provide 'vtt)
