;; Additional Emacs configuration not requiring unquoting.

(defun rde-cleanup-buffers ()
  "Close buffers visiting files or directories that no longer exist.

This is convenient in particular for iteration on building a Guix package."
  (interactive)
  (let ((count 0))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (let ((filename (buffer-file-name))
              (dir (and (derived-mode-p 'dired-mode) default-directory)))
          (cond
           ;; Kill file buffers whose files don't exist
           ((and filename (not (file-exists-p filename)))
            (kill-buffer buf)
            (setq count (+ 1 count)))
           ;; Kill dired buffers whose directories don't exist
           ((and dir (not (file-directory-p dir)))
            (kill-buffer buf)
            (setq count (+ 1 count)))))))
    (message "Closed %d buffer(s) with nonexistent files or directories." count)))
