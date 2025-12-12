;; Additional Emacs configuration not requiring unquoting.

(autoload 'persid-bibtex-from "persid.el")

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
  (let ((entries (make-hash-table :test (function equal))))
    (with-temp-buffer
      (insert entry)
      (parsebib-parse-bib-buffer :entries entries)

      ;; Get the first (and only) entry
      (and-let* ((v (car (hash-table-values entries)))
                 (old-key (cdr (assoc "=key=" v)))
                 (title (cdr (assoc "title" v)))
                 ;; Extract first word (lowercase, alphanumeric only)
                 (first-word (cadr
                              (split-string
                               (substring-no-properties title)
                               "[^a-zA-Z0-9]+")))
                 ;; Generate new key
                 (new-key (downcase (concat old-key "_" first-word))))
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



(require 'bibtex)

(defun biblio--parse-bibtex-entries (content)
  "Parse CONTENT and return list of (key . doi) pairs."
  (with-temp-buffer
    (insert content)
    ;; (bibtex-set-dialect 'BibTeX)
    (goto-char (point-min))
    (let (entries)
      ;; XXX: Here there is an issue: not all common fields are accepted.
      (while (bibtex-skip-to-valid-entry)
        (let* ((key (bibtex-key-in-head))
               (doi (bibtex-text-in-field "doi" t)))
          (when key
            (push (cons key (if doi (downcase (string-trim doi "[\"{]+" "[\"}]+")) ""))
                  entries))
          (forward-line 1)))
      (nreverse entries))))

(defun biblio--parse-git-diff (repo-path)
  "Parse git diff for REPO-PATH and return only added lines if no deletions."
  (let ((default-directory repo-path))
    (with-temp-buffer
      (when (zerop (call-process "git" nil t nil "diff" "--cached" "--unified=0"))
        (goto-char (point-min))
        (let ((only-additions t)
              (added-lines '()))
          (while (not (eobp))
            (let ((line (buffer-substring-no-properties
                        (line-beginning-position)
                        (line-end-position))))
              (cond
               ((string-prefix-p "++" line) nil) ; skip file markers
               ((string-prefix-p "+" line)
                (push (substring line 1) added-lines))
               ((string-prefix-p "-" line)
                (unless (string-prefix-p "---" line)
                  (setq only-additions nil))))
              (forward-line 1)))
          (when only-additions
            (string-join (nreverse added-lines) "\n")))))))

(defun biblio--git-index-status (repo-path)
  "Get git status for REPO-PATH as cons of (file . index-flag)."
  (let ((default-directory repo-path))
    (with-temp-buffer
      (when (zerop (call-process "git" nil t nil "status" "--porcelain"))
        (seq-reduce
         (lambda (acc line)
           (if (string-match "\\`\\([AM]\\). \\(.+\\)\\'" line)
               (cons (cons (intern (match-string 1 line)) ;index status
                           (match-string 2 line)) ;filepath
                     acc)
             acc))
         (split-string (buffer-string) "\n" t)
         '())))))

(defun biblio--generate-commit-message (item)
  "Generate commit message based on status ITEM."
  (pcase item
    ;; New files.
    (`(A . ,(and (rx (seq "files/library/" any)) path))
     (format "Add file for %s" (file-name-base path)))
    (`(A . ,(and (rx (seq "roam/references/" any)) path))
     (format "Add bibliographic note for %s" (file-name-base path)))
    (`(A . ,(and (rx (seq "roam/main/" any)) path))
     (format "Add note for %s" (file-name-base path)))
    ;; Modified files
    (`(M . ,(and (rx (seq "files/library/" any)) path))
     (format "Update file for %s" (file-name-base path)))
    (`(M . ,(and (rx (seq "roam/references/" any)) path))
     (format "Update bibliographic note for %s" (file-name-base path)))
    (`(M . ,(and (rx (seq "roam/main/" any)) path))
     (format "Update note for %s" (file-name-base path)))
    (`(M . "biblio.bib")
     (when-let* ((modified-lines (biblio--parse-git-diff repo-path))
                 (entries (ignore-errors
                            (biblio--parse-bibtex-entries modified-lines))))
       (string-join (mapcar (lambda (entry)
                              (format "Add entry for %s" (car entry)))
                            entries)
                    "\n")))
    (`(M . "dois.txt")
     (when-let* ((modified-lines (biblio--parse-git-diff repo-path))
                 (doi-to-find (string-trim (substring modified-lines 4)))
                 (gen-bib-path (expand-file-name "gen.bib" repo-path))
                 (_ (file-exists-p gen-bib-path))
                 (bibtex-content (with-temp-buffer
                                   (insert-file-contents gen-bib-path)
                                   (buffer-string)))
                 (entries (ignore-errors
                            (biblio--parse-bibtex-entries bibtex-content))))
       ;; XXX: TODO :  the same.
       (dolist (entry entries)
         (when (string= (cdr entry) doi-to-find)
           (push (format "Add doi for %s" (car entry)) messages)))))
    (_
     (format "Error: Couldn't generate adequate commit message for %s"
             (cdr item)))))

(defun biblio--generate-commit-messages (repo-path)
  "Generate commit messages based on staged changes in REPO-PATH."
  (let ((status (biblio--git-index-status repo-path))
        messages)
    (dolist (item status)
      (push (biblio--generate-commit-message item) messages))
    (nreverse (delq nil messages))))

(defun biblio--write-commit-message (output-file)
  "Generate and write commit message to OUTPUT-FILE."
  (let* ((repo-path (expand-file-name ".." (file-name-directory output-file)))
         (messages (biblio--generate-commit-messages repo-path)))
    (with-temp-file output-file
      (insert (string-join messages "\n")))))
