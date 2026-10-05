;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "rg" "20261004.2129"
  "A search tool based on ripgrep."
  '((emacs     "28.1")
    (transient "0.9.2")
    (wgrep     "2.1.10"))
  :url "https://github.com/dajva/rg.el"
  :commit "7d4839686031754ff86e4ad3ab76da108777edb3"
  :revdesc "7d4839686031"
  :keywords '("matching" "tools")
  :authors '(("David Landell" . "david.landell@sunnyhill.email")
             ("Roland McGrath" . "roland@gnu.org"))
  :maintainers '(("David Landell" . "david.landell@sunnyhill.email")
                 ("Roland McGrath" . "roland@gnu.org")))
