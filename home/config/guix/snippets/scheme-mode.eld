-*- mode: lisp-data -*-

scheme-mode

(package...
 "(define-public " (s name)
 n> "(package"
 n> "(name \"" (s name) "\")"
 n> "(version \"" p "\")"
 n> "(source origin...)"
 n> "(build-system "
 (p (completing-read "build-system:"
                     '("android-ndk"
                       "ant"
                       "asdf"
                       "cargo"
                       "chicken"
                       "clojure"
                       "cmake"
                       "copy"
                       "dub"
                       "dune"
                       "emacs"
                       "font"
                       "glib-or-gtk"
                       "gnu"
                       "go"
                       "guile"
                       "haskell"
                       "julia"
                       "linux-module"
                       "maven"
                       "meson"
                       "minetest"
                       "minify"
                       "node"
                       "ocaml"
                       "perl"
                       "python"
                       "qt"
                       "r"
                       "rakudo"
                       "rebar"
                       "renpy"
                       "ruby"
                       "scons"
                       "texlive"
                       "trivial"
                       "waf")))
 "-build-system)"
 n> "(home-page \"" p "\")"
 n> "(synopsis \"" p "\")"
 n> "(description \"" p "\")"
 n> "(license " p ")))" n)

(origin...
 "(origin"
 n> "(method "
 (p (completing-read "method:"
                     '("url-fetch"
                       "url-fetch/tarbomb"
                       "url-fetch/zipbomb"
                       "cvs-fetch"
                       "git-fetch"
                       "hg-fetch"
                       "svn-fetch"
                       "bzr-fetch")
                     nil t) method) ")"
 n> "(uri "  (cl-case (intern method)
               ('git-fetch "git-reference...")
               ('svn-fetch "svn-reference...")
               ('hg-fetch  "hg-reference...")
               ('cvs-fetch "cvs-reference...")
               ('bzr-fetch "bzr-reference...")
               (t          "(string-append \"https://"))
 p
 (if (equal (substring method 0 9)  "url-fetch") "\")")
 ")"
 n> (cl-case (intern method)
      ('git-fetch "(file-name (git-file-name name version))\n")
      ('hg-fetch "(file-name (hg-file-name name version))\n")
      ('svn-fetch "(file-name (string-append name \"-\" version \"-checkout\"))\n")
      ('cvs-fetch "(file-name (string-append name \"-\" version \"-checkout\"))\n")
      ('bzr-fetch "(file-name (string-append name \"-\" version \"-checkout\"))\n")
      (t          ""))
 > "(sha256"
 n> "(base32 \"" p "\")))")

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

(:phases
 ":phases (modify-phases %standard-phases" n>
 "(" (p (completing-read "modify:"
                         '("add-before"
                           "replace"
                           "add-after"
                           "delete ")
                         nil t))
 " '" q "))" )

(license:
 "license:"
 (p (completing-read "license:"
                     '("agpl1"
                       "agpl3"
                       "agpl3+"
                       "apsl2"
                       "asl1.1"
                       "asl2.0"
                       "boost1.0"
                       "bsd-0"
                       "bsd-1"
                       "bsd-2"
                       "bsd-3"
                       "bsd-4"
                       "non-copyleft"
                       "cc0"
                       "cc-by-sa4.0"
                       "cc-by-sa3.0"
                       "cc-by-sa2.0"
                       "cc-by4.0"
                       "cc-by3.0"
                       "cc-by2.0"
                       "cddl1.0"
                       "cddl1.1"
                       "cecill"
                       "cecill-b"
                       "cecill-c"
                       "artistic2.0"
                       "clarified-artistic"
                       "copyleft-next"
                       "cpl1.0"
                       "cua-opl1.0"
                       "edl1.0"
                       "epl1.0"
                       "epl2.0"
                       "eupl1.2"
                       "expat"
                       "expat-0"
                       "freetype"
                       "giftware"
                       "gpl1"
                       "gpl1+"
                       "gpl2"
                       "gpl2+"
                       "gpl3"
                       "gpl3+"
                       "gfl1.0"
                       "fdl1.1+"
                       "fdl1.2+"
                       "fdl1.3+"
                       "freebsd-doc"
                       "opl1.0+"
                       "osl2.1"
                       "isc"
                       "ijg"
                       "ibmpl1.0"
                       "imlib2"
                       "ipa"
                       "knuth"
                       "lal1.3"
                       "lgpl2.0"
                       "lgpl2.0+"
                       "lgpl2.1"
                       "lgpl2.1+"
                       "lgpl3"
                       "lgpl3+"
                       "llgpl"
                       "lppl"
                       "lppl1.0+"
                       "lppl1.1+"
                       "lppl1.2"
                       "lppl1.2+"
                       "lppl1.3"
                       "lppl1.3+"
                       "lppl1.3a"
                       "lppl1.3a+"
                       "lppl1.3b"
                       "lppl1.3b+"
                       "lppl1.3c"
                       "lppl1.3c+"
                       "miros"
                       "mpl1.0"
                       "mpl1.1"
                       "mpl2.0"
                       "ms-pl"
                       "ncsa"
                       "nmap"
                       "ogl-psi1.0"
                       "openssl"
                       "openldap2.8"
                       "perl-license"
                       "psfl"
                       "public-domain"
                       "qpl"
                       "qwt1.0"
                       "repoze"
                       "ruby"
                       "sgifreeb2.0"
                       "silofl1.1"
                       "sleepycat"
                       "tcl/tk"
                       "vim"
                       "unicode"
                       "unlicense"
                       "w3c"
                       "wtfpl2"
                       "wxwindows3.1+"
                       "x11"
                       "x11-style"
                       "zpl2.1"
                       "zlib"
                       "hpnd"
                       "fsf-free"
                       "fsdg-compatible")
                     nil t)))
