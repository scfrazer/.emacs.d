;(insert (prin1-to-string (w32-select-font)))

(setenv "USER" "scfrazer")

;; (setq bdf-directory-list '("~/fonts"))
;; (setq w32-bdf-filename-alist (w32-find-bdf-fonts bdf-directory-list))
;; (create-fontset-from-fontset-spec
;;  "-*-fixed-medium-r-normal-*-14-*-*-*-c-*-fontset-bdf,
;;  scfrazer:-sfrazer-Fixed-Medium-R-Normal--14-130-75-75-C-70-ISO8859-1")
;; (create-fontset-from-fontset-spec
;;  "-*-fixed-bold-r-normal-*-14-*-*-*-c-*-fontset-bdf,
;;  scfrazer:-sfrazer-Fixed-Bold-R-Normal--14-130-75-75-C-70-ISO8859-1")

(custom-set-faces
 '(variable-pitch ((t (:family "Tahoma")))))

;  (set-default-font "fontset-bdf")
;  (set-default-font "-outline-DejaVu Sans Mono-normal-r-normal-normal-13-97-96-96-c-*-iso8859-1")
  (set-default-font "-outline-DejaVu Sans Mono-normal-r-normal-normal-13-97-96-96-c-*-iso8859-1")

(setq default-frame-alist
      '((width . 120) (height . 60)))
;        (font . "-outline-Bitstream Vera Sans Mono-medium-r-normal-normal-13-97-96-96-c-*-iso10646-1")))

(setq grep-program "c:\\Progra~1\\GnuWin32\\bin\\grep.exe")
(setq igrep-program "c:\\Progra~1\\GnuWin32\\bin\\grep.exe")
(setq igrep-find-program "c:\\Progra~1\\GnuWin32\\bin\\find.exe")
