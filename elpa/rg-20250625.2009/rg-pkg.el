;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "rg" "20250625.2009"
  "A search tool based on ripgrep."
  '((emacs     "26.1")
    (transient "0.9.2")
    (wgrep     "2.1.10"))
  :url "https://github.com/dajva/rg.el"
  :commit "7611852b5517212a4f0fdab9cd9ecb0cf3995f08"
  :revdesc "7611852b5517"
  :keywords '("matching" "tools")
  :authors '(("David Landell" . "david.landell@sunnyhill.email")
             ("Roland McGrath" . "roland@gnu.org"))
  :maintainers '(("David Landell" . "david.landell@sunnyhill.email")
                 ("Roland McGrath" . "roland@gnu.org")))
