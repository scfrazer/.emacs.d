;;; my-undo.el

;; Adapted from: https://lists.gnu.org/archive/html/emacs-devel/2007-02/msg01136.html

(defun remove-nils-until-sentinel (sentinel list)
  "Remove NILs from LIST until SENTINEL is found, then remove SENTINEL."
  (while (and (consp list) (null (car list)))
    (setq list (cdr list)))
  (let* ((head list)
         (tail (cdr head)))
    (while (progn (while (and (consp tail) (null (car tail)))
                    (setcdr head (setq tail (cdr tail))))
                  (if (and (consp tail) (not (eq (car tail) sentinel)))
                      (setq head tail
                            tail (cdr head)))))
    (if (null tail)
        (error "Sentinel not found"))
    (setcdr head (cdr tail)))
  list)

(defmacro with-no-undo-boundaries (&rest body)
  "Execute the BODY, ensuring that no undo boundaries are created therein.
An undo boundary is created before executing BODY, but any undo boundaries
added to the `buffer-undo-list' by the execution of BODY are stripped out
afterwards, including in the case of abnormal exit."
  (let ((sentinel (make-symbol "sentinel"))
        (cb (make-symbol "cb")))
    `(let ((,cb (current-buffer)))
       (undo-boundary)
       (push ',sentinel buffer-undo-list)
       (unwind-protect
           (progn
             ,@body)
         (with-current-buffer ,cb
           (setq buffer-undo-list
                 (remove-nils-until-sentinel ',sentinel buffer-undo-list)))))))

(put 'with-no-undo-boundaries 'lisp-indent-function 0)

(provide 'my-undo)
