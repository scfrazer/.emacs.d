;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "rg" "20250914.716"
  "A search tool based on ripgrep."
  '((emacs     "26.1")
    (transient "0.9.2")
    (wgrep     "2.1.10"))
  :url "https://github.com/dajva/rg.el"
  :commit "76429112cc351bb8c4f400dc039ec8ec4cf5eecb"
  :revdesc "76429112cc35"
  :keywords '("matching" "tools")
  :authors '(("David Landell" . "david.landell@sunnyhill.email")
             ("Roland McGrath" . "roland@gnu.org"))
  :maintainers '(("David Landell" . "david.landell@sunnyhill.email")
                 ("Roland McGrath" . "roland@gnu.org")))
