(use-modules (guix gexp)
             (guix packages)
             (guix licenses)
             (guix git-download)
             (guix build-system gnu)
             ((guix build utils) #:select (with-directory-excursion))
             (gnu packages)
             (gnu packages autotools)
             (gnu packages guile)
             (gnu packages pkg-config)
             (gnu packages gnome)
             (gnu packages texinfo))

(define %source-dir (dirname (current-filename)))

(package
  (name "guile-libnotify")
  (version "0.1.0")
  (source (local-file %source-dir
                      #:recursive? #t
                      #:select? (git-predicate %source-dir)))
  (build-system gnu-build-system)
  (native-inputs (list autoconf automake pkg-config #;texinfo))
  (inputs (list guile-3.0-latest libnotify))
  (synopsis "Guile bindings for libnotify")
  (description "Provides bindings for GNOME's libnotify C library to Guile")
  (home-page "https://github.com/ekaitz-zarraga/guile-libnotify")
  (license gpl3+))
