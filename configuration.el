;; Additional Emacs configuration not requiring unquoting.

(require 'persid)

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

(defun rename-bibtex-key (entry)
  "Rename the key of a BibTeX ENTRY by appending the first word of the title.
Returns the modified entry as a string."
  (message entry)
  (let ((entries (make-hash-table :test #'equal)))
    (with-temp-buffer
      (insert entry)
      (parsebib-parse-bib-buffer :entries entries)

      ;; Get the first (and only) entry
      (and-let* ((v (car (hash-table-values entries)))
                 (old-key (cdr (assoc "=key=" v)))
                 (title (cdr (assoc "title" v)))
                 ;; Extract first word (lowercase, alphanumeric only)
                 (first-word (car
                              (split-string
                               (string-trim-left
                                (substring-no-properties title)
                                "[ \\t\\n\\r\\{]+")
                               "[^a-zA-Z0-9]+")))
                 ;; Generate new key
                 (new-key (concat old-key "_" (downcase first-word))))
        (unless title
          (error "Entry %s has no valid title in the CrossRef API."))
        ;; Replace the old key with the new one in the original string
        (replace-regexp-in-string
         (concat "@\\([^ ]+\\){\\(" (regexp-quote old-key) "\\),")
         (concat "@\\1{" new-key ",")
         entry)))))


(defun refresh-gen-biblio ()
  "Regenerates the generated gen.bib file based on the list in dois.txt."
  (interactive)
  (if (file-readable-p "~/resources/gen.bib")
      (with-temp-file "/tmp/retrieved_dois.txt"
        (maphash
         (lambda (k v)
           (insert (cdr (assoc "DOI" v)) "\n"))
         (parsebib-parse "~/resources/gen.bib" :fields '("DOI"))))
    (f-touch "/tmp/retrieved_dois.txt"))
  (with-current-buffer (find-file-noselect "~/resources/gen.bib")
    (save-excursion
      (goto-char (point-max))
      (dolist (doi (cl-set-exclusive-or
                    (with-temp-buffer
                      (insert-file-contents "~/resources/dois.txt")
                      (split-string (buffer-string) "\n" t "doi:"))
                    (with-temp-buffer
                      (insert-file-contents "/tmp/retrieved_dois.txt")
                      (split-string (buffer-string) "\n" t))
                    :test 'string-equal-ignore-case))
        (when-let ((entry (persid-bibtex-from doi))
                   (renamed (rename-bibtex-key entry)))
          (insert renamed "\n"))))
    (bibtex-reformat)))
