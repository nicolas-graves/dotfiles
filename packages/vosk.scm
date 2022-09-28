(define-module (packages vosk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix build utils)
  #:use-module (guix utils)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths))

(define-public openfst-for-vosk
  (package
    (inherit openfst)
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.openfst.org/twiki/pub/FST/"
                           "FstDownload/openfst-" version ".tar.gz"))
       (sha256
        (base32 "0h2lfhhihg63b804hrcljnkggijbjmp84i5g8q735wb09y9z2c4p"))))
    (arguments
     '(#:configure-flags
       '("--enable-shared" "--enable-far" "--enable-ngram-fsts"
         "--enable-lookahead-fsts" "--with-pic" "--disable-bin")))))

(define kaldi-for-vosk
  (let* ((commit "6417ac1dece94783e80dfbac0148604685d27579")
         (revision "0")
         (openfst openfst-for-vosk))
    (package
      (inherit kaldi)
      (name "kaldi")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/alphacep/kaldi")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "04xw2dpfvpla8skpk08azmgr9k97cd8hn83lj4l85q165gbzql4s"))))
      (inputs
       (list alsa-lib
             ;; `(,gfortran "lib") ;; replaced by lapack
             lapack
             glib
             gstreamer
             jack-1
             openblas
             openfst
             portaudio
             python))
      (arguments
       (list
        #:test-target "test"
        #:make-flags ''("online2" "lm" "rnnlm")
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'chdir
              (lambda _ (chdir "src") #t))
            (replace 'configure
              (lambda* (#:key build system inputs outputs #:allow-other-keys)
                (when (not (or (string-prefix? "x86_64" system)
                               (string-prefix? "i686" system)))
                  (substitute* "makefiles/linux_openblas.mk"
                    (("-msse -msse2") "")))
                (substitute* "makefiles/default_rules.mk"
                  (("/bin/bash") (which "bash")))
                (substitute* "Makefile"
                  (("ext_depend: check_portaudio")
                   "ext_depend:"))
                (substitute* '("online/Makefile"
                               "onlinebin/Makefile"
                               "gst-plugin/Makefile")
                  (("../../tools/portaudio/install")
                   (assoc-ref inputs "portaudio")))
                (substitute* "matrix/Makefile"     ;temporary test bypass
                  (("matrix-lib-test sparse-matrix-test") ""))

                ;; This `configure' script doesn't support variables passed as
                ;; arguments, nor does it support "prefix".
                (let ((out (assoc-ref outputs "out")))
                  (substitute* "configure"
                    (("check_for_slow_expf;") "")
                    ;; This affects the RPATH and also serves as the installation
                    ;; directory.
                    (("KALDILIBDIR=`pwd`/lib")
                     (string-append "KALDILIBDIR=" out "/lib"))
                    (("OPENBLASROOT=\\\"\\$\\(rel2abs ..\\/tools\\/OpenBLAS\\/install\\)\\\"")
                     (string-append "OPENBLASROOT=\"" #$openblas "\""))
                    (("-L\\$OPENBLASLIBDIR -l:libopenblas.a -l:libblas.a -l:liblapack.a -l:libf2c.a")
                     (string-append "-L$OPENBLASLIBDIR -lopenblas "
                                    "-L" #$lapack "/lib -lblas -llapack")))
                  (mkdir-p out) ; must exist
                  (setenv "CONFIG_SHELL" (which "bash"))
                  (setenv "OPENFST_VER" #$(package-version openfst))
                  (invoke "./configure"
                          "--use-cuda=no"
                          "--mathlib=OPENBLAS_CLAPACK"
                          "--shared"
                          (string-append "--fst-root=" #$openfst)))))
            (add-after 'configure 'optimize-build
                       (lambda _ (substitute* "kaldi.mk" ((" -O1") " -O3"))))
            (replace 'install
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (inc (string-append out "/include"))
                       (lib (string-append out "/lib")))
                  (mkdir-p lib)
                  ;; The build phase installed symlinks to the actual
                  ;; libraries.  Install the actual targets.
                  (for-each (lambda (file)
                              (let ((target (readlink file)))
                                (delete-file file)
                                (install-file target lib)))
                            (find-files lib "\\.so"))
                  ;; Install headers
                  (for-each (lambda (file)
                              (let ((target-dir (string-append inc "/" (dirname file))))
                                (install-file file target-dir)))
                            (find-files "." "\\.h")))))))))))

(define vosk
  (let* ((openfst openfst-for-vosk)
         (kaldi kaldi-for-vosk))
(package
   (name "vosk")
   (version "0.3.43")
   (source
   (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/alphacep/vosk-api")
           (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32 "0xmp8i140c2hd3rj9dap8a2rnsvzb1k9hnqm12xzbaxrw73rkc29"))))
   (build-system gnu-build-system)
   (arguments
    (list
     #:tests? #f
     #:phases
     #~(modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "src") #t))
         (replace 'configure
           (lambda _
             (substitute* "./Makefile"
               (("USE_SHARED\\?=0")
                "USE_SHARED?=1")
               (("-DFST_NO_DYNAMIC_LINKING")
                "")
               (("-lopenblas -llapack -lblas -lf2c")
                (string-append
                 "-L" #$openblas "/lib " "-lopenblas "
                 "-L" #$lapack "/lib " "-llapack -lblas "))
               (("-lfst -lfstngram")
                (string-append
                 "-L" #$openfst "/lib " "-lfst -lfstngram "))
               (("\\$\\(HOME\\)\\/travis\\/kaldi")
                #$(file-append kaldi "/include"))
               (("\\$\\(KALDI_ROOT\\)\\/tools\\/openfst")
                #$openfst)
               (("\\$\\(KALDI_ROOT\\)\\/tools\\/OpenBLAS\\/install")
                #$openblas)
               (("\\$\\(KALDI_ROOT\\)\\/libs")
                #$(file-append kaldi "/lib")))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib"))
                    (src (string-append out "/src")))
               (mkdir-p lib)
               (mkdir-p src)
               (install-file "libvosk.so" lib)
               (for-each
                (lambda (x) (install-file x src))
                (find-files "." "\\.h$"))))))))
   (inputs (list kaldi openfst lapack openblas))
   (home-page "https://alphacephei.com/vosk")
   (synopsis "Speech recognition toolkit based on @code{kaldi}")
   (description "Speech recognition toolkit based on @code{kaldi}")
   (license license:asl2.0))))

(define-public python-vosk
  (package
    (inherit vosk)
    (name "python-vosk")
    (build-system python-build-system)
    (propagated-inputs
     (list python-cffi python-requests python-tqdm python-srt python-websockets))
    (inputs (list vosk))
    (arguments
     (list
      #:tests? #f  ;; TODO There are tests but not run through Makefile.
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'config
            (lambda _
              (chdir "python")
              (setenv "VOSK_SOURCE" #$vosk)))
          (add-before 'build 'from-abi-to-api
            (lambda _
              (substitute* "vosk_builder.py"
                (("ffibuilder\\.set_source\\(\"vosk.vosk_cffi\", None\\)")
                 (string-append
                  "ffibuilder.set_source(\"vosk.vosk_cffi\", "
                  "r\"\"\"\n#include<vosk_api.h>\n#include<Python.h>\"\"\",\n\t"
                  "library_dirs=["
                  "'" #$vosk "/lib'"
                  "],\n\t"
                  "libraries=['vosk', 'python3.9'],\n\t"
                  "include_dirs=["
                  "'" #$vosk "/src'" "])")))
              (substitute* "vosk/__init__.py"
                (("_c = open_dll\\(\\)")
                 "")
                (("_ffi")
                 "ffi")
                (("from \\.vosk_cffi import ffi as ffi")
                 "from .vosk_cffi import ffi, lib")
                (("_c\\.")
                 "lib.")))))))))

(define python-nerd-dictation
  (let* ((commit "53ab129a5ee0f8b5df284e8cf2229219b732c59e")
         (revision "0"))
    (package
      (name "python-nerd-dictation")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ideasman42/nerd-dictation")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "184qijiva1h1x00dzicik0yzgh78pq2lqr5fkgicgp26mkarlyhc"))))
      (build-system python-build-system)
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'chdir
             (lambda _ (chdir "package/python"))))))
      (propagated-inputs (list python-vosk))
      (inputs (list pulseaudio xdotool))
      (home-page "https://github.com/ideasman42/nerd-dictation")
      (synopsis "Offline speech-to-text for desktop Linux")
      (description "\
This package provides simple access speech to text for using in
Linux without being tied to a desktop environment, using the excellent
@code{vosk-api}.  The user configuration lets you manipulate text using Python
string operations.  It has zero overhead, as this relies on manual activation
there are no background processes.  Dictation is accessed manually with
begin/end commands.")
      (license license:gpl3+))))

(define-public ydotool
  (package
    (name "ydotool")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ReimuNotMoe/ydotool")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1h19dh7kai0iikssr7sq0wfkh0sb18dylyfg7c3dkwc158cdg9cr"))))
    (build-system cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs (list scdoc))
    (home-page "https://github.com/ReimuNotMoe/ydotool")
    (synopsis "Generic Linux command-line automation tool (no X!)")
    (description "@code{ydotool} is a Linux command-line tool that simulates
keyboard input, mouse actions, etc.  programmatically or manually.")
    (license license:agpl3+)))

(define-public python-nerd-dictation/wayland
  (package
    (inherit python-nerd-dictation)
    (name "python-nerd-dictation-wayland")
    (inputs (list bash-minimal python-nerd-dictation sox ydotool))
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (let* ((out (assoc-ref %outputs "out"))
                 (exe (string-append out "/bin/nerd-dictation")))

            (mkdir-p (dirname exe))
            (call-with-output-file exe
              (lambda (port)
                (format port "#!~a
exec ~a $@ --input=SOX --simulate-input-tool=YDOTOOL"
                        #$(file-append bash-minimal "/bin/bash")
                        #$(file-append python-nerd-dictation
                                       "/bin/nerd-dictation"))))
            (chmod exe #o555)))))))
