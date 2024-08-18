(define-module (notify)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (notify internal)
  #:use-module (system foreign-object))

(define-syntax or-NULL
  (syntax-rules ()
    ((_ value conv-f) (if value (conv-f value) %null-pointer))
    ((_ value)        (or value %null-pointer))))

(define* (init #:key (app-name #f))
  (if (= 0 (notify-init (or-NULL app-name string->pointer)))
    (error "notify-init failed")))

(define (uninit)
  (notify-uninit))
;; TODO: add call-with-notify ?

(define* (notification-new summary #:key (body #f) (icon #f))
  (notify-notification-new (string->pointer summary)
                           (or-NULL body string->pointer)
                           (or-NULL icon string->pointer)))

(define* (notification-update summary #:key (body #f) (icon #f))
  (notify-notification-update (string->pointer summary)
                              (or-NULL body string->pointer)
                              (or-NULL icon string->pointer)))


(define (notification-show notification)
  ;; TODO send the GError** as second parameter and deal with it
  (if (= 0 (notify-notification-show notification %null-pointer))
    (error "notify-notification-show: error happened")))

(define (notification-close notification)
  ;; TODO send the GError** as second parameter and deal with it
  (if (= 0 (notify-notification-close notification %null-pointer))
    (error "notify-notification-close: error happened")))

; From 0.8.4
;(define (notification-set-app-icon notification icon)
;  (notify-notification-set-app-icon notification (string->pointer icon)))

(define (notification-set-app-name notification name)
  (notify-notification-set-app-name notification (string->pointer name)))

(define (notification-set-category notification category)
  (notify-notification-set-category notification (string->pointer category)))

;; TODO: maybe remove me
(define (notification-set-image-from-pixbuf notification gdk-pixbuf-image)
  (notify-notification-set-image-from-pixbuf notification gdk-pixbuf-image))

(define notification-set-timeout  notify-notification-set-timeout)

(define (notification-set-urgency notification urgency)
  (define (urgency->num urgency)
    (case urgency
      ('low      0)
      ('normal   1)
      ('critical 2)))
  (notify-notification-set-urgency notification (urgency->num urgency)))



(define (notification-set-hint notification key value)
 (let ((variant (make-g-variant value)))
   (notify-notification-set-hint notification
                                 (string->pointer key)
                                 (slot-ref variant 'var))))

(define notification-clear-hints notify-notification-clear-hints)



(define notification-get-closed-reason notify-notification-get-closed-reason)

(define* (notification-add-action . things)
  (error "Unimplemented"))


;; How to use the higher level interface
;;(init #:app-name "notify.scm")
;;(define noti (notification-new "Problem!" #:body "Device died!"))
;;(notification-set-category noti "device.error")
;;(notification-set-hint noti "resident" #t) ; This doesn't work yet
;;(notification-show noti)
;;(uninit)
