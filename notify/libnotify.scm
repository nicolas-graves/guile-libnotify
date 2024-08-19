(define-module (notify libnotify)
  #:use-module (notify config)
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
                            #:arg-types (list '* '* '* '* '* '*)))

(define-public notify-notification-clear-actions
  (foreign-library-function libnotify "notify_notification_clear_actions"
                            #:return-type void
                            #:arg-types (list '*)))


;; How to use the low-level api
;; (notify-init (string->pointer "notify.scm"))
;; (define noti (notify-notification-new
;;                (string->pointer "Problem!")
;;                (string->pointer "Device died!")
;;                %null-pointer))
;; (notify-notification-set-category noti (string->pointer "device.error"))
;; (notify-notification-show noti %null-pointer)
