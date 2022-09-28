(define-module (packages vosk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix build utils)
  #:use-module (guix utils)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages maths))

(define-public openfst-1.8.0
  (package (inherit openfst)
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.openfst.org/twiki/pub/FST/"
                           "FstDownload/openfst-" version ".tar.gz"))
       (sha256
        (base32 "0h2lfhhihg63b804hrcljnkggijbjmp84i5g8q735wb09y9z2c4p"))))
    (arguments '(#:configure-flags '("--enable-ngram-fsts")))))

(define kaldi-for-vosk
  (let* ((commit "6417ac1dece94783e80dfbac0148604685d27579")
         (revision "0")
         (openfst openfst-1.8.0))
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
                          ;; (string-append "--openblas-root=" openblas)
                          (string-append "--fst-root=" #$openfst)
                          ))))
            ;; (add-after 'configure 'optimize-build
                       ;; (lambda _ (substitute* "kaldi.mk" ((" -O1") " -O3"))))
            ;; (add-after 'build 'build-ext-and-gstreamer-plugin
            ;;   (lambda _
            ;;     (invoke "make" "-C" "online" "depend")
            ;;     (invoke "make" "-C" "online")
            ;;     (invoke "make" "-C" "onlinebin" "depend")
            ;;     (invoke "make" "-C" "onlinebin")
            ;;     (invoke "make" "-C" "gst-plugin" "depend")
            ;;     (invoke "make" "-C" "gst-plugin")))
            ;; TODO: also install the executables.
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
                            (find-files "." "\\.h"))
                  ;; (install-file "gst-plugin/libgstonlinegmmdecodefaster.so"
                  ;; (string-append lib "/gstreamer-1.0"))
                  )))
            ))))))

(define vosk
  (let* ((openfst openfst-1.8.0))
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
                #$openfst-1.8.0)
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
                (find-files "." "\\.h$")))))
         )))
   (inputs (list kaldi-for-vosk openfst lapack openblas))
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
     (list python-cffi python-requests python-tqdm python-srt vosk))
    (native-inputs (list pkg-config))
    (arguments
     (list
      #:tests? #f
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
              (substitute* "vosk/__init__.py"
                (("_ffi\\.dlopen\\(os\\.path\\.join\\(dlldir, \"libvosk\\.so\"\\)\\)")
                 "_ffi.dlopen(\"./vosk/vosk_cffi.abi3.so\")")
                ;; (("_c = open_dll\\(\\)")
                 ;; "")
                (("from \\.vosk_cffi import ffi as _ffi")
                 "from vosk.vosk_cffi import ffi as _ffi\nfrom vosk.vosk_cffi import lib")
                ;; (("_c\\.")
                 ;; "lib.")
                ))))))))
                 (string-append
                  "ffibuilder.set_source(\"vosk.vosk_cffi\", "
                  "r\"\"\"\n#include<vosk_api.h>\n#include<Python.h>\"\"\",\n\t"
                  "library_dirs=["
                  "'" #$vosk "/lib'"
                  "],\n\t"
                  "libraries=['vosk', 'python3.9'],\n\t"
                  "include_dirs=["
                  "'" #$vosk "/src'"
                  "],\n\t"
                  "extra_link_args=['-Wl,-rpath="
                  #$glibc "/lib'"
                  "])")))


clapack-for-vosk
