#lang racket/gui

;;; config

(define *root-dir*
  (string-append (path->string (find-system-path 'home-dir))
                 "src/"))

;;; helpers

(define s+ string-append)

(define (clear container)
  (for-each (lambda (c) (send container delete-child c))
    (send container get-children)))

(define (seconds-since-last-mod file)
  123)

(define (seconds->string x)
  (define minute 60)
  (define hour (* 60 minute))
  (define day  (* 24 hour))
  (define year (* 365 day))
  (define unit
    (cond
      [(x . < . minute) "seconds"]
      [(x . < . hour)   "minutes"]
      [(x . < . day)    "hours"]
      [(x . < . year)   "days"]
      [else             "years"]))
  (s+ (number->string x) " " unit))

(define (goal-files)
  (map path->string (find-files (curry regexp-match? #rx"GOAL$") *root-dir*)))
(define todo-regexp #rx"/TODO$")
(define (todo-files)
  (map path->string (find-files (curry regexp-match? todo-regexp) *root-dir*)))

(define (path->todo-location full-path)
  (regexp-replace todo-regexp
                  (regexp-replace (regexp-quote *root-dir*) full-path "")
                  ""))

(define (get-selected-path list-box)
  (define i (send list-box get-selection))
  (send list-box get-data i))

;;; GUI containers

(define *frame* (new frame% [label "Slacker"]
                          [width 400]
                          [height 400]))
(define *parent-pane* (new horizontal-pane% [parent *frame*]))
(define *side-pane* (new vertical-pane% [parent *parent-pane*]))
(define *main-pane* (new vertical-pane% [parent *parent-pane*]
                                        [min-width 400]))

;;; list boxes

(define *todo-entries* (make-hash))
(define *goal-entries* (make-hash))

;; list select callback
(define (show-entry list-box event)
  (define path (get-selected-path list-box))
  (clear *main-pane*)
  (define label (new message% [parent *main-pane*]
                              [label path]))
  (define text   (new text%))
  (send text insert (file->string path) 0)
  (define editor (new editor-canvas% [parent *main-pane*]
                                     [editor text]))
  (define button (new button% [parent *main-pane*]
                              [label "GOTO"]))
   #t)

(define *todo-list*
  (new list-box%  [parent *side-pane*]
                  [label "TODO"]
                  [columns '("age" "location")]
                  [style '(vertical-label single column-headers)]
                  [choices '()]
                  [callback show-entry]))
(define *goal-list*
  (new list-box%  [parent *side-pane*]
                  [label "GOAL"]
                  [columns '("age" "location")]
                  [style '(vertical-label single column-headers)]
                  [choices '()]
                  [callback show-entry]))

;;; adding and showing entries in the list boxes

(define (sync-list-box entry-ht list-box)
  (define box-entries
    (hash-map entry-ht
      (lambda (location data)
        (define age (number->string (first data)))
        (define path (second data))
        (list age location path))))
  ;; displayed data
  (send list-box set (map first box-entries) (map second box-entries))
  ;; hidden data
  (for ([e (in-list box-entries)]
        [i (in-naturals)])
    (send list-box set-data i (third e))))

(define (add-entry entry-ht list-box path)
  (define location (path->todo-location path))
  (hash-set! entry-ht location
                      (list (seconds-since-last-mod path)
                            path))
  (sync-list-box entry-ht list-box))

;;; invocation

(send *frame* show #t)

(for-each (curry add-entry *todo-entries* *todo-list*) (todo-files))
(for-each (curry add-entry *goal-entries* *goal-list*) (goal-files))

