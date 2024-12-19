(define-module (notify)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module ((notify libnotify) #:prefix internal:)
  #:use-module ((notify glib) #:prefix glib:)
  #:use-module (ice-9 match)
  #:use-module (system foreign)
  #:use-module (system foreign-object)
  #:export (notify-init
            notify-uninit
            notification-new
            notification-update
            notification-show
            notification-close
            notification-set-app-name
            notification-set-category
            notification-set-image-from-pixbuf
            notification-set-timeout
            notification-set-urgency
            notification-set-hint
            notification-clear-hints
            notification-get-closed-reason
            notification-add-action
            notification-clear-actions))

(define-syntax or-NULL
  (syntax-rules ()
    ((_ value conv-f) (if value (conv-f value) %null-pointer))
    ((_ value)        (or value %null-pointer))))

(define* (notify-init #:key (app-name #f))
  (if (= 0 (internal:notify-init (or-NULL app-name string->pointer)))
    (error "notify-init failed")))

(define (notify-uninit)
  (internal:notify-uninit))
;; TODO: add call-with-notify ?

(define* (notification-new summary #:key (body #f) (icon #f))
  (glib:wrap-g-object
    (internal:notify-notification-new
      (string->pointer summary)
      (or-NULL body string->pointer)
      (or-NULL icon string->pointer))))


(define* (notification-update notification summary #:key (body #f) (icon #f))
  (internal:notify-notification-update (glib:unwrap-g notification)
                                       (string->pointer summary)
                                       (or-NULL body string->pointer)
                                       (or-NULL icon string->pointer)))


(define (notification-show notification)
  ;; TODO send the GError** as second parameter and deal with it
  (if (= 0 (internal:notify-notification-show (glib:unwrap-g notification)
                                              %null-pointer))
    (error "notify-notification-show: error happened")))

(define (notification-close notification)
  ;; TODO send the GError** as second parameter and deal with it
  (if (= 0 (internal:notify-notification-close (glib:unwrap-g notification)
                                               %null-pointer))
    (error "notify-notification-close: error happened")))

; From 0.8.4
;(define (notification-set-app-icon notification icon)
;  (notify-notification-set-app-icon notification (string->pointer icon)))

(define (notification-set-app-name notification name)
  (internal:notify-notification-set-app-name (glib:unwrap-g notification)
                                             (string->pointer name)))

(define (notification-set-category notification category)
  (internal:notify-notification-set-category (glib:unwrap-g notification)
                                             (string->pointer category)))

;; TODO: maybe remove me
(define (notification-set-image-from-pixbuf notification gdk-pixbuf-image)
  (internal:notify-notification-set-image-from-pixbuf
    (glib:unwrap-g notification)
    gdk-pixbuf-image))

(define (notification-set-timeout notification timeout)
  (define (timeout->num timeout)
    (match timeout
      ('never 0)
      ('default -1)
      (else timeout)))
  (internal:notify-notification-set-timeout (glib:unwrap-g notification)
                                            (timeout->num timeout)))

(define (notification-set-urgency notification urgency)
  (define (urgency->num urgency)
    (match urgency
      ('low      0)
      ('normal   1)
      ('critical 2)))
  (internal:notify-notification-set-urgency (glib:unwrap-g notification)
                                            (urgency->num urgency)))



(define (notification-set-hint notification key value)
  (define (key-type key)
    (match key
      ('action-icons   'boolean)
      ('category       'string)
      ('desktop-entry  'string)
      ('image-data     'iiibiiay) ;; Unimplemented
      ('image-path     'string)
      ('resident       'boolean)
      ('sound-file     'string)
      ('sound-name     'string)
      ('suppress-sound 'boolean)
      ('transient      'boolean)
      ('x              'integer)
      ('y              'integer)
      ('urgency        'byte)
      (else            (error "Unknown hint" key))))

  (let ((variant (glib:g-variant-new (key-type key) value)))
    (internal:notify-notification-set-hint
      (glib:unwrap-g notification)
      (string->pointer (symbol->string key))
      (glib:unwrap-g variant))))

(define (notification-clear-hints notification)
  (internal:notify-notification-clear-hints (glib:unwrap-g notification)))

(define (notification-get-closed-reason notification)
  (internal:notify-notification-get-closed-reason
    (glib:unwrap-g notification)))

(define (notification-clear-actions notification)
  (internal:notify-notification-clear-actions (glib:unwrap-g notification)))

(define* (notification-add-action notification action label callback user-data
                                  free-func)
  (internal:notify-notification-add-action
    (glib:unwrap-g notification)
    (string->pointer action)
    (string->pointer label)
    (procedure->pointer void
                        (lambda (notification action data)
                          (callback notification
                                    (pointer->string action)
                                    (pointer->scm data)))
                        (list '* '* '*))
    (scm->pointer user-data)
    (procedure->pointer void
                        (lambda (data)
                          (free-func (pointer->scm data)))
                        (list '*))))
