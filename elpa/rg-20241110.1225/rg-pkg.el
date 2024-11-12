;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "rg" "20241110.1225"
  "A search tool based on ripgrep."
  '((emacs     "26.1")
    (transient "0.3.0")
    (wgrep     "2.1.10"))
  :url "https://github.com/dajva/rg.el"
  :commit "9ab1aefd8a835f41aa2d9518e01414d50680ddc2"
  :revdesc "9ab1aefd8a83"
  :keywords '("matching" "tools")
  :authors '(("David Landell" . "david.landell@sunnyhill.email")
             ("Roland McGrath" . "roland@gnu.org"))
  :maintainers '(("David Landell" . "david.landell@sunnyhill.email")
                 ("Roland McGrath" . "roland@gnu.org")))
