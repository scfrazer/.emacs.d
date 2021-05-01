;;; sb-imenu-ctags.el

(require 'sb-imenu)

(setq sb-imenu-populate-tag-function 'sb-imenu-ctags-populate-tag)

(defvar sb-imenu-ctags-executable "ctags")

(defun sb-imenu-ctags-create-index ()
  (let ((filename (buffer-file-name))
        (mode major-mode)
        (pos nil)
        (prev-pos -1)
        (item-alist '()))
    (with-temp-buffer
      (shell-command
       (concat sb-imenu-ctags-executable " -e "
               (cond ((equal mode 'c-mode)
                      "--language-force=c --c-kinds=cdfnpstuvx")
                     ((equal mode 'c++-mode)
                      "--language-force=c++ --c++-kinds=cdfmnpstuvx")
                     ((equal mode 'java-mode)
                      "--language-force=java --java-kinds=-efg"))
               " --extras=+q -f- " filename) t)
      (goto-char (point-max))
      (while (not (bobp))
        (forward-line -1)
        (when (looking-at ".+\\(.+\\)[0-9]+,\\([0-9]+\\)")
          (setq pos (1+ (string-to-number (match-string-no-properties 2))))
          (unless (= pos prev-pos)
            (push (cons (match-string-no-properties 1) pos) item-alist))
          (setq prev-pos pos))))
    item-alist))

;; % ctags --list-kinds=c++
;; c  classes
;; d  macro definitions
;; e  enumerators (values inside an enumeration)
;; f  function definitions
;; g  enumeration names
;; l  local variables [off]
;; m  class, struct, and union members
;; n  namespaces
;; p  function prototypes [off]
;; s  structure names
;; t  typedefs
;; u  union names
;; v  variable definitions
;; x  external and forward variable declarations [off]

;; % ctags --list-kinds=java
;; c  classes
;; e  enum constants
;; f  fields
;; g  enum types
;; i  interfaces
;; l  local variables [off]
;; m  methods
;; p  packages

;; ctags --c++-kinds=cdfmnpstuvx --extra=+q -x --sort=no -f- foo.cpp
;;
;; (defun my-create-index ()
;;   (let ((filename "/auto/asic-sjc-legacy/users/scfrazer/ws/scfrazer-sjc/scfrazer-lsp-sjc1/asic/shared/ver/chipdv/env/src/pktgen/sse_pkt_mon_lz.cpp")
;;         (mode major-mode)
;;         (pos nil)
;;         (prev-pos -1)
;;         (item-alist '()))
;;     (with-temp-buffer
;;       (shell-command
;;        (concat sb-imenu-ctags-executable
;;                (cond ((equal mode 'c-mode)
;;                       "--language-force=c --c-kinds=cdfnpstuvx")
;;                      ((equal mode 'c++-mode)
;;                       "--language-force=c++ --c++-kinds=cdfmnpstuvx")
;;                      ((equal mode 'java-mode)
;;                       "--language-force=java --java-kinds=-efg"))
;;                " --extra=+q -f- " filename) t)
;;       (goto-char (point-max))
;;       (while (not (bobp))
;;         (forward-line -1)
;;         (when (looking-at ".+\\(.+\\)[0-9]+,\\([0-9]+\\)")
;;           (setq pos (1+ (string-to-number (match-string-no-properties 2))))
;;           (unless (= pos prev-pos)
;;             (push (cons (match-string-no-properties 1) pos) item-alist))
;;           (setq prev-pos pos))))
;;     item-alist))

(defun sb-imenu-ctags-populate-tag (tag is-expandable)
  "Take a tag and return a list with the transformed tag and a face to use."
  (list tag (if is-expandable 'font-lock-keyword-face 'font-lock-variable-name-face)))

(provide 'sb-imenu-ctags)
