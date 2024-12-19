(define-module (notify glib)
  #:use-module (notify config)
  #:use-module (oop goops)
  #:use-module (ice-9 match)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:use-module (system foreign-object)
  #:export (g-variant-new
            g-main-loop-new
            g-main-loop-is-running
            g-main-loop-run
            g-main-loop-quit
            unwrap-g
            g-signal-disconnect
            g-signal-connect
            wrap-g-object))

;; We need variants for the hints
;; How should I integrate this with the garbage collector? I think i need a finalizer
;; https://www.gnu.org/software/guile/manual/guile.html#Foreign-Objects-and-Scheme-1
;; https://www.gnu.org/software/guile/manual/html_node/Guardians.html

;; TODO: Implement iiibiiay
(define %g-variant-new-boolean
  (foreign-library-function glib "g_variant_new_boolean"
                            #:return-type '*
                            #:arg-types (list int)))
(define %g-variant-new-byte
  (foreign-library-function glib "g_variant_new_byte"
                            #:return-type '*
                            #:arg-types (list uint8)))
(define %g-variant-new-int
  (foreign-library-function glib "g_variant_new_int32"
                            #:return-type '*
                            #:arg-types (list int)))
(define %g-variant-new-string
  (foreign-library-function glib "g_variant_new_string"
                            #:return-type '*
                            #:arg-types (list '*)))

(define %g-variant-unref
  (foreign-library-function glib "g_variant_unref"
                            #:return-type void
                            #:arg-types (list '*)))

(define (finalize-g-variant variant)
  (%g-variant-unref (unwrap-g variant)))

(define-foreign-object-type <g-variant>
  %wrap-g-variant
  (var)
  #:finalizer finalize-g-variant)

(define (g-variant-new type val)
  (wrap-g-variant
    (match type
      ('string  (%g-variant-new-string  (string->pointer val)))
      ('integer (%g-variant-new-int     val))
      ('boolean (%g-variant-new-boolean (if val 1 0)))
      ('byte    (%g-variant-new-byte    val))
      (else     (error "Unknown type for variant" type)))))


;; We need some minimal loop support for the actions
(define %g-main-loop-new
  (foreign-library-function glib "g_main_loop_new"
                            #:return-type '*
                            #:arg-types (list '* int)))

(define %g-main-loop-run
  (foreign-library-function glib "g_main_loop_run"
                            #:return-type void
                            #:arg-types (list '*)))

(define %g-main-loop-quit
  (foreign-library-function glib "g_main_loop_quit"
                            #:return-type void
                            #:arg-types (list '*)))

(define %g-main-loop-unref
  (foreign-library-function glib "g_main_loop_unref"
                            #:return-type void
                            #:arg-types (list '*)))

(define %g-main-loop-is-running
  (foreign-library-function glib "g_main_loop_is_running"
                            #:return-type int
                            #:arg-types (list '*)))

(define (finalize-g-main-loop loop)
  (let ((var (slot-ref loop 'var)))
    (%g-main-loop-unref (make-pointer var))))

(define-foreign-object-type <g-main-loop>
  %wrap-g-main-loop
  (var)
  #:finalizer finalize-g-main-loop)

(define* (g-main-loop-new #:key (context %null-pointer)
                               (is-running 0))
  (wrap-g-main-loop (%g-main-loop-new %null-pointer 0)))

(define (g-main-loop-is-running loop)
  (= 0 (%g-main-loop-is-running (unwrap-g loop))))

(define (g-main-loop-quit loop)
  (%g-main-loop-quit (unwrap-g loop)))

(define (g-main-loop-run loop)
  (%g-main-loop-run (unwrap-g loop)))


; g_signal_connect_data ((instance),
;                        (detailed_signal),
;                        (c_handler),
;                        (data),
;                        NULL,
;                        (GConnectFlags) 0)

; https://docs.gtk.org/gobject/func.signal_connect.html
(define %g-signal-connect-data
  (foreign-library-function gobject "g_signal_connect_data"
                            #:return-type unsigned-long
                            #:arg-types (list '* '* '* '* '* int)))
; https://docs.gtk.org/gobject/func.signal_handler_disconnect.html
(define %g-signal-disconnect
  (foreign-library-function gobject "g_signal_handler_disconnect"
                            #:return-type void
                            #:arg-types (list '* unsigned-long)))

(define (g-signal-connect instance signal handler data)
  (%g-signal-connect-data
    (unwrap-g instance)
    (string->pointer signal)
    (procedure->pointer void
                        handler
                        (list))
    (scm->pointer data)
    %null-pointer
    0))

(define (g-signal-disconnect instance handler-id)
  (%g-signal-disconnect
    (unwrap-g instance)
    handler-id))

;; GObject support
(define %g-object-unref
  (foreign-library-function gobject "g_object_unref"
                            #:return-type void
                            #:arg-types (list '*)))

(define (finalize-g-object loop)
  (let ((var (slot-ref loop 'var)))
    (%g-object-unref (make-pointer var))))

(define-foreign-object-type <g-object>
  %wrap-g-object
  (var)
  #:finalizer finalize-g-object)

(define (compose f g)
  (lambda (x) (g (f x))))
(define wrap-g-object    (compose pointer-address %wrap-g-object))
(define wrap-g-variant   (compose pointer-address %wrap-g-variant))
(define wrap-g-main-loop (compose pointer-address %wrap-g-object))

(define (unwrap-g x)
  (make-pointer (slot-ref x 'var)))
