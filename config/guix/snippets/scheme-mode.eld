-*- mode: lisp-data -*-

scheme-mode

(package...
 "(define-public " (s name)
 n> "(package"
 n  > "(name \"" (s name) "\")"
 n  > "(version \"" p "\")"
 n  > "(source origin...)"
 n  > "(build-system " (p "gnu") "-build-system)"
 n  > "(home-page \"" p "\")"
 n  > "(synopsis \"" p "\")"
 n  > "(description \"" p "\")"
 n  > "(license license:" (p "unknown") ")))" n)

(origin...
 "(origin"
 n> "(method " (p "url-fetch" method) ")"
 n> "(uri " (cl-case (and method (intern method))
              ('git-fetch "git-reference...")
              ('svn-fetch "svn-reference...")
              ('hg-fetch  "hg-reference...")
              ('cvs-fetch "cvs-reference...")
              ('bzr-fetch "bzr-reference...")
              (t          "\"https://...\""))
 ")"
 n>
 (cl-case (and method (intern method))
   ('git-fetch
    (insert "(file-name (git-file-name name version))")
    (newline)
    (indent-according-to-mode))
   ('hg-fetch
    (insert "(file-name (hg-file-name name version))")
    (newline)
    (indent-according-to-mode))
   ('svn-fetch
    (insert "(file-name (string-append name \"-\" version \"-checkout\"))")
    (newline)
    (indent-according-to-mode))
   ('cvs-fetch
    (insert "(file-name (string-append name \"-\" version \"-checkout\"))")
    (newline)
    (indent-according-to-mode))
   ('bzr-fetch
    (insert "(file-name (string-append name \"-\" version \"-checkout\"))")
    (newline)
    (indent-according-to-mode))
   (t          ""))
 > "(sha256"
 n > "(base32 \""
 ;; hash of an empty directory
 (p "0sjjj9z1dhilhpc8pq4154czrb79z9cm044jvn75kxcjv6v5l2m5") "\")))")

(git-reference...
 "(git-reference"
 n> "(url \"" p "\")"
 n> "(commit \"" p "\"))")

(svn-reference...
 "(svn-reference"
 n> "(url \"" p "\")"
 n> "(revision \"" p "\"))")

(cvs-reference...
 "(cvs-reference"
 n> "(root-directory \"" p "\")"
 n> "(module \"" p "\")"
 n> "(revision \"" p "\"))")

(hg-reference...
 "(hg-reference"
 n> "(url \"" p "\")"
 n> "(changeset \"" p "\"))")

(bzr-reference...
 "(bzr-reference"
 n> "(url \"" p "\")"
 n> "(revision \"" p "\"))")

(:phases\ "#:phases (modify-phases %standard-phases"
          n> p ")")

(add-before\ "(add-before '" p " '" p
             n > p ")")
(add-after\ "(add-after '" p " '" p
            n > p ")")
(replace\ "(replace '" p " " p")")
