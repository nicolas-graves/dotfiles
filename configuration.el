;; Additional Emacs configuration not requiring unquoting.

(defun rde-emacs--reload-command (selector value &optional no-wait)
  "Run the shared CLI for SELECTOR and VALUE and return its output."
  (with-temp-buffer
    (let ((status (apply #'process-file
                         "guix" nil (current-buffer) nil
                         (append (list "rde" "emacs" "reload" selector)
                                 (if no-wait
                                     (list "--no-wait" value)
                                   (list value))))))
      (unless (zerop status)
        (error "Emacs reload failed (%s): %s" status (string-trim (buffer-string))))
      (string-trim (buffer-string)))))

(defun rde-emacs-reload (&optional file no-wait)
  "Reload FILE (default: the current buffer's file) into the rde-dev
Emacs 31 development daemon, per plans/rde-emacs/04-on-demand-live-reload.md.

This is the Emacs frontend for `guix rde emacs reload': it shells out to
the same CLI command and reports its structured result, so behavior
always matches the CLI (\"CLI and Emacs results match\" is part of plan
04's exit gate). It does not itself talk to Shepherd or decide reload
scope -- the CLI's `emacs-reload' (in (guix-rde emacs)) shells out to
`herd reload-file rde-emacs-live-controller FILE' and (guix-rde emacs
controller) owns all of that."
  (interactive)
  (let ((file (or file (buffer-file-name))))
    (unless file
      (user-error "Buffer is not visiting a file"))
    (let* ((default-directory (locate-dominating-file file ".git"))
           (result (rde-emacs--reload-command
                    "--file" (expand-file-name file) no-wait)))
      (message "%s" (string-trim result)))))

(defun rde-emacs-reload-package (package &optional no-wait)
  "Reload logical PACKAGE through the same controller as the CLI."
  (interactive "sPackage: ")
  (message "%s" (rde-emacs--reload-command "--package" package no-wait)))

(defun rde-emacs-reload-fragment (fragment &optional no-wait)
  "Classify and submit FRAGMENT through the same controller as the CLI."
  (interactive "sFragment: ")
  (message "%s" (rde-emacs--reload-command "--fragment" fragment no-wait)))

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
