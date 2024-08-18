;; Library for libnotify control.
;;
;; https://lists.nongnu.org/archive/html/guile-user/2016-01/msg00000.html
;: https://gnome.pages.gitlab.gnome.org/libnotify/
;;
;; The spec:
;; https://specifications.freedesktop.org/notification-spec/1.2/index.html
;;
;; Examples in python
;; https://www.devdungeon.com/content/desktop-notifications-linux-python
;;
;; Arch wiki
;; https://wiki.archlinux.org/title/Desktop_notifications

(define-module (notify internal)
  #:use-module (notify config)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:use-module (system foreign-object))

;; libnotify functions
(define-public notify-init
  (foreign-library-function libnotify "notify_init"
                            #:return-type int
                            #:arg-types (list '*)))
(define-public notify-uninit
  (foreign-library-function libnotify "notify_uninit"
                            #:return-type void
                            #:arg-types (list)))
(define-public notify-notification-new
  (foreign-library-function libnotify "notify_notification_new"
                            #:return-type '*
                            #:arg-types (list '* '* '*)))

(define-public notify-notification-show
  (foreign-library-function libnotify "notify_notification_show"
                            #:return-type int
                            #:arg-types (list '* '*)))

(define-public notify-notification-update
  (foreign-library-function libnotify "notify_notification_update"
                            #:return-type int
                            #:arg-types (list '* '* '* '*)))

(define-public notify-notification-close
  (foreign-library-function libnotify "notify_notification_close"
                            #:return-type int
                            #:arg-types (list '* '*)))

; From 0.8.4
; (define-public notify-notification-set-app-icon
;   (foreign-library-function libnotify "notify_notification_set_app_icon"
;                             #:return-type void
;                             #:arg-types (list '* '*)))

(define-public notify-notification-set-app-name
  (foreign-library-function libnotify "notify_notification_set_app_name"
                            #:return-type void
                            #:arg-types (list '* '*)))

(define-public notify-notification-set-category
  (foreign-library-function libnotify "notify_notification_set_category"
                            #:return-type void
                            #:arg-types (list '* '*)))

(define-public notify-notification-set-hint
  (foreign-library-function libnotify "notify_notification_set_hint"
                            #:return-type void
                            #:arg-types (list '* '* '*)))

(define-public notify-notification-set-image-from-pixbuf
  (foreign-library-function libnotify "notify_notification_set_image_from_pixbuf"
                            #:return-type void
                            #:arg-types (list '* '*)))

(define-public notify-notification-set-timeout
  (foreign-library-function libnotify "notify_notification_set_timeout"
                            #:return-type void
                            ;; Timeout can be NOTIFY_EXPIRES_DEFAULT / NOTIFY_EXPIRES_NEVER
                            #:arg-types (list '* int)))

(define-public notify-notification-set-urgency
  (foreign-library-function libnotify "notify_notification_set_urgency"
                           ; NOTIFY_URGENCY_LOW 0
                           ; NOTIFY_URGENCY_NORMAL 1
                           ; NOTIFY_URGENCY_CRITICAL 2
                            #:return-type void
                            #:arg-types (list '* int)))


(define-public notify-notification-clear-hints
  (foreign-library-function libnotify "notify_notification_clear_hints"
                            #:return-type void
                            #:arg-types (list '*)))

(define-public notify-notification-get-closed-reason
  (foreign-library-function libnotify "notify_notification_get_closed_reason"
                            #:return-type int
                            #:arg-types (list '*)))

;; For the actions, we need function pointers
;; https://www.gnu.org/software/guile/manual/guile.html#index-procedure_002d_003epointer
(define-public notify-notification-add-action
  (foreign-library-function libnotify "notify_notification_add_action"
                            #:return-type void
                            #:arg-types (list '* '* '* '* '*)))


;; We need variants for the hints
;; How should I integrate this with the garbage collector? I think i need a finalizer
;; https://www.gnu.org/software/guile/manual/guile.html#Foreign-Objects-and-Scheme-1
;; https://www.gnu.org/software/guile/manual/html_node/Guardians.html

;; TODO: Implement iiibiiay
(define g-variant-new
  (foreign-library-function glib "g_variant_new"
                            #:return-type '*
                            #:arg-types (list '* '*)))
(define g-variant-new-boolean
  (foreign-library-function glib "g_variant_new_boolean"
                            #:return-type '*
                            #:arg-types (list int)))
(define g-variant-new-byte
  (foreign-library-function glib "g_variant_new_byte"
                            #:return-type '*
                            #:arg-types (list uint8)))
(define g-variant-new-int
  (foreign-library-function glib "g_variant_new_int32"
                            #:return-type '*
                            #:arg-types (list int)))
(define g-variant-new-string
  (foreign-library-function glib "g_variant_new_string"
                            #:return-type '*
                            #:arg-types (list '*)))

(define g-variant-unref
  (foreign-library-function glib "g_variant_unref"
                            #:return-type void
                            #:arg-types (list '*)))

(define (finalize-g-variant variant)
  (let ((var (slot-ref variant 'var)))
    (g-variant-unref (make-pointer var))))

(define-foreign-object-type <g-variant>
  %make-g-variant
  (var)
  #:finalizer finalize-g-variant)

(define-public (make-g-variant type val)
  (%make-g-variant
    (pointer-address
      (case type
        ('string  (g-variant-new-string  (string->pointer val)))
        ('integer (g-variant-new-int     val))
        ('boolean (g-variant-new-boolean (if val 1 0)))
        ('byte    (g-variant-new-byte    val))
        (else     (error "Unknown type for variant" type))))))



;; How to use the low-level api
;; (notify-init (string->pointer "notify.scm"))
;; (define noti (notify-notification-new
;;                (string->pointer "Problem!")
;;                (string->pointer "Device died!")
;;                %null-pointer))
;; (notify-notification-set-category noti (string->pointer "device.error"))
;; (notify-notification-show noti %null-pointer)
