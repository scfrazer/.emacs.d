;;; my-erc.el

(defun my-erc-start ()
  "Start ERC"
  (interactive)

  (set-frame-font "-apple-Menlo-medium-normal-normal-*-*-*-*-*-m-0-iso10646-1")
  (my-theme-whiteboard)

  ;; ERC
  (require 'erc)
  (erc-autojoin-mode t)
  (erc-track-mode t)
  (setq erc-autojoin-channels-alist '((".*\\.freenode.net" "#emacs"))
        erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE" "324" "329" "332" "333" "353" "477")
        erc-hide-list '("JOIN" "PART" "QUIT" "NICK")
        erc-header-line-format nil
        erc-fill-column 90
        erc-fill-function 'erc-fill-static
        erc-fill-static-center 15
        erc-timestamp-only-if-changed-flag nil
        erc-timestamp-format "%H:%M "
        erc-insert-timestamp-function 'erc-insert-timestamp-left)

  (add-to-list 'erc-modules 'scrolltobottom)
  (setq erc-input-line-position -2)

  ;; ERC Nicklist
  (require 'erc-nicklist)
  (setq erc-nicklist-use-icons nil)
  (add-hook 'erc-join-hook 'my-erc-join-hook)

  ;; ERC Highlight Nicknames
  (load-library "erc-highlight-nicknames")
  (add-to-list 'erc-modules 'highlight-nicknames)
  (erc-update-modules)

  ;; SOCKS5 Proxy
  (when (my-erc-use-socks)
    (require 'socks)
    (setq socks-noproxy '("localhost")
          socks-override-functions 1
          socks-password (read-passwd "SOCKS password? ")
          socks-server '("socks.cisco.com" "socks.cisco.com" "1080" 5)
          erc-server-connect-function 'socks-open-network-stream))

  ;; Start ERC
  (erc :server "irc.freenode.net" :port 6667 :nick "scfrazer" :full-name "Scott Frazer"))

(defun my-erc-use-socks ()
  "Do we need to use a SOCKS5 proxy (i.e. at work?)"
  (let ((ip-addr (format-network-address (car (network-interface-info "eth0")) t)))
    (unless ip-addr
      (setq ip-addr (format-network-address (car (network-interface-info "en0")) t)))
    (unless ip-addr
      (setq ip-addr (format-network-address (car (network-interface-info "utun0")) t)))
    (unless ip-addr
      (setq ip-addr (format-network-address (car (network-interface-info "en1")) t)))
    (if (not ip-addr)
        (error "Can't get network address")
      (or (string-match "^64\\." ip-addr)
          (string-match "^10\\." ip-addr)))))

(defun my-erc-join-hook ()
  "Stuff to do per-channel"
  ;; (erc-nicklist)
  (filladapt-mode 0)
  (set (make-local-variable 'erc-prompt)
       (erc-propertize (concat (erc-default-target) ">") 'read-only t 'rear-nonsticky t 'front-nonsticky t)))

(provide 'my-erc)
