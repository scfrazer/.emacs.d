;;; my-flymake.el

(require 'flymake)
(require 'popup)

(setq-default flymake-no-changes-timeout nil
              flymake-start-on-flymake-mode nil
              flymake-wrap-around nil)

(setq-default popup-tip-max-width 160)

(setq-default flymake-diagnostic-types-alist
  `((:error
     . ((flymake-category . flymake-error)
        (priority . -1)))
    (:warning
     . ((flymake-category . flymake-warning)
        (priority . -2)))
    (:note
     . ((flymake-category . flymake-note)
        (priority . -3)))))

(put 'flymake-note 'mode-line-face 'caution)

(defface my-flymake-error-face
  '((t (:inherit popup-tip-face :foreground "red3" :italic nil)))
  "Error dot face."
  :group 'faces)

(defface my-flymake-warning-face
  '((t (:inherit popup-tip-face :foreground "yellow1" :italic nil)))
  "Warning dot face."
  :group 'faces)

(defface my-flymake-note-face
  '((t (:inherit popup-tip-face :foreground "dodgerblue3" :italic nil)))
  "Note dot face."
  :group 'faces)

(defun my-flymake-show-current-error ()
  "Show the current error point is on."
  (interactive)
  (when (and (boundp 'flymake-mode) flymake-mode)
    (let ((diags (flymake-diagnostics (point)))
          (msg "") text)
      (dolist (diag diags)
        (unless (string= msg "")
          (setq msg (concat msg "\n")))
        (setq text (concat (propertize " ● " 'face
                                       (cl-case (flymake-diagnostic-type diag)
                                         (:error 'my-flymake-error-face)
                                         (:warning 'my-flymake-warning-face)
                                         (:note 'my-flymake-note-face)))
                           (flymake-diagnostic-text diag) " "))
        (setq msg (concat msg text)))
      (when diags
        (popup-tip msg :nostrip t)))))

(defvar my-flymake-timer nil)
(unless my-flymake-timer
  (setq my-flymake-timer (run-with-idle-timer 1.0 t 'my-flymake-show-current-error)))

;; Don't show a count of zero in a non-default face
(defun flymake--mode-line-format ()
  "Produce a pretty minor mode indicator."
  (let* ((known (hash-table-keys flymake--backend-state))
         (running (flymake-running-backends))
         (disabled (flymake-disabled-backends))
         (reported (flymake-reporting-backends))
         (diags-by-type (make-hash-table))
         (all-disabled (and disabled (null running)))
         (some-waiting (cl-set-difference running reported)))
    (maphash (lambda (_b state)
               (mapc (lambda (diag)
                       (push diag
                             (gethash (flymake--diag-type diag)
                                      diags-by-type)))
                     (flymake--backend-state-diags state)))
             flymake--backend-state)
    `((:propertize " Lint"
                   mouse-face mode-line-highlight
                   help-echo
                   ,(concat (format "%s known backends\n" (length known))
                            (format "%s running\n" (length running))
                            (format "%s disabled\n" (length disabled))
                            "mouse-1: Display minor mode menu\n"
                            "mouse-2: Show help for minor mode")
                   keymap
                   ,(let ((map (make-sparse-keymap)))
                      (define-key map [mode-line down-mouse-1]
                        flymake-menu)
                      (define-key map [mode-line mouse-2]
                        (lambda ()
                          (interactive)
                          (describe-function 'flymake-mode)))
                      map))
      ,@(pcase-let ((`(,ind ,face ,explain)
                     (cond ((null known)
                            `("?" mode-line "No known backends"))
                           (some-waiting
                            `("Wait" compilation-mode-line-run
                              ,(format "Waiting for %s running backend(s)"
                                       (length some-waiting))))
                           (all-disabled
                            `("!" compilation-mode-line-run
                              "All backends disabled"))
                           (t
                            `(nil nil nil)))))
          (when ind
            `((":"
               (:propertize ,ind
                            face ,face
                            help-echo ,explain
                            keymap
                            ,(let ((map (make-sparse-keymap)))
                               (define-key map [mode-line mouse-1]
                                 'flymake-switch-to-log-buffer)
                               map))))))
      ,@(unless (or all-disabled
                    (null known))
          (cl-loop
           for (type . severity)
           in (cl-sort (mapcar (lambda (type)
                                 (cons type (flymake--lookup-type-property
                                             type
                                             'severity
                                             (warning-numeric-level :error))))
                               (cl-union (hash-table-keys diags-by-type)
                                         '(:error :warning)))
                       #'>
                       :key #'cdr)
           for diags = (gethash type diags-by-type)
           for face = (flymake--lookup-type-property type
                                                     'mode-line-face
                                                     'compilation-error)
           when (or diags
                    (>= severity (warning-numeric-level :warning)))
           collect `(:propertize
                     ,(format "%d" (length diags))
                     face ,(when (> (length diags) 0) face)
                     mouse-face mode-line-highlight
                     keymap
                     ,(let ((map (make-sparse-keymap))
                            (type type))
                        (define-key map (vector 'mode-line
                                                mouse-wheel-down-event)
                          (lambda (event)
                            (interactive "e")
                            (with-selected-window (posn-window (event-start event))
                              (flymake-goto-prev-error 1 (list type) t))))
                        (define-key map (vector 'mode-line
                                                mouse-wheel-up-event)
                          (lambda (event)
                            (interactive "e")
                            (with-selected-window (posn-window (event-start event))
                              (flymake-goto-next-error 1 (list type) t))))
                        map)
                     help-echo
                     ,(concat (format "%s diagnostics of type %s\n"
                                      (propertize (format "%d"
                                                          (length diags))
                                                  'face face)
                                      (propertize (format "%s" type)
                                                  'face face))
                              (format "%s/%s: previous/next of this type"
                                      mouse-wheel-down-event
                                      mouse-wheel-up-event)))
           into forms
           finally return
           `((:propertize "[")
             ,@(cl-loop for (a . rest) on forms by #'cdr
                        collect a when rest collect
                        '(:propertize " "))
             (:propertize "]")))))))

(provide 'my-flymake)
