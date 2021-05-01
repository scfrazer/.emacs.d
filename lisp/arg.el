;;; arg.el

(defvar arg-separator ",")

(defun arg-next ()
  "Go to next argument."
  (interactive)
  (ignore-errors
    (let* ((forward-sexp-function nil)
           (end (save-excursion
                  (backward-up-list)
                  (forward-sexp)
                  (1- (point))))
           (regexp (concat "\\s-*" arg-separator "\\s-*"))
           (done nil))
      (forward-sexp)
      (while (not done)
        (if (>= (point) end)
            (setq done t)
          (if (looking-at regexp)
              (progn
                (goto-char (match-end 0))
                (setq done t))
            (forward-sexp)))))))

(defun arg-prev ()
  "Go to previous argument."
  (interactive)
  (ignore-errors
    (let* ((forward-sexp-function nil)
           (beg (save-excursion
                  (backward-up-list)
                  (1+ (point))))
           (regexp (concat "\\s-*" arg-separator "\\s-*"))
           (done nil))
      (backward-sexp)
      (while (not done)
        (if (or (looking-back regexp beg)
                (<= (point) beg))
            (setq done t)
          (backward-sexp))))))

(defun arg-kill ()
  (interactive)
  ;; TODO
  )

(defun arg-copy ()
  (interactive)
  ;; TODO
  )

(defun arg-yank ()
  (interactive)
  ;; TODO
  )

(defun arg-move-left ()
  (interactive)
  ;; TODO
  )

(defun arg-move-right ()
  (interactive)
  ;; TODO
  )

(defun arg-reformat ()
  (interactive)
  ;; TODO
  )

(defhydra arg
  (:foreign-keys run :hint nil)
  "
^Move^             ^Action^
^^-----------------^^------
_n_ext Next arg    
_p_rev Prev arg    

"
  ("n" arg-next)
  ("p" arg-prev)
  ("\C-g" nil :exit t)
  ("q" nil "Quit" :exit t))

(provide 'arg)
