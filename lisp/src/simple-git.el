;;; simple-git.el

(defconst simple-git-buffer-name "*simple-git*")

;; Root directory: git rev-parse --show-toplevel
;; Origin URL: git config --get remote.origin.url
;; Status: git status -b --porcelain

(provide 'simple-git)
