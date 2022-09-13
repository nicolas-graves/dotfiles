(define-module (packages python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system python)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (gnu packages check)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-build))

(define-public python-pluggy
  (package
    (name "python-pluggy")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pluggy" version))
              (sha256
               (base32
                "0n8iadlas2z1b4h0fc73b043c7iwfvx9rgvqm1azjmffmhxkf922"))))
    (build-system python-build-system)
    (propagated-inputs (list python-importlib-metadata))
    (native-inputs (list python-pre-commit python-pytest
                         python-pytest-benchmark python-tox
                         python-setuptools-scm))
    (home-page "https://github.com/pytest-dev/pluggy")
    (synopsis "plugin and hook calling mechanisms for python")
    (description "plugin and hook calling mechanisms for python")
    (license license:expat)))

(define-public python-lsp-server
  (package
    (name "python-lsp-server")
    (version "1.5.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "python-lsp-server" version))
              (sha256
               (base32
                "039qi5x9sa1mjzinimxhiwzj8lxn5d5l33q6qhkjl0i5k70r9h75"))))
    (build-system python-build-system)
    ;; (arguments '(#:tests? #f))
    (propagated-inputs (list python-jedi python-lsp-jsonrpc python-pluggy
                             python-setuptools python-ujson))
    (native-inputs (list python-coverage
                         python-flaky
                         python-matplotlib
                         python-numpy
                         python-pandas
                         python-pylint
                         ;; python-pyqt5
                         python-pytest
                         python-pytest-cov
                         python-pycodestyle
                         python-pydocstyle
                         python-rope
                         python-pyflakes
                         python-autopep8
                         python-yapf
                         python-whatthepatch))
    (home-page "https://github.com/python-lsp/python-lsp-server")
    (synopsis "Python Language Server")
    (description "This package implements a Language Server for
python, complying with the Language Server Protocol.")
    (license license:expat)))
