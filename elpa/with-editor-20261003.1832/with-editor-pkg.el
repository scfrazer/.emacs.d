;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "with-editor" "20261003.1832"
  "Use the Emacsclient as $EDITOR."
  '((emacs    "28.1")
    (compat   "31.0")
    (cond-let "1.1")
    (llama    "1.0"))
  :url "https://github.com/magit/with-editor"
  :commit "ca956bbfd1c9f163d2a8390716fcd39799d23f34"
  :revdesc "ca956bbfd1c9"
  :keywords '("processes" "terminals")
  :authors '(("Jonas Bernoulli" . "emacs.with-editor@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.with-editor@jonas.bernoulli.dev")))
