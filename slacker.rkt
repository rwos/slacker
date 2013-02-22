#lang racket/gui

(require framework)

;;; config

(define *root-dir*
  (string-append (path->string (find-system-path 'home-dir))
                 "src/"))

(define (goto-action path)
  (system (string-append "xfterm4 "
                         *root-dir*
                         (path->location path))))

;;; helpers

(define s+ string-append)

(define (clear container)
  (for-each (lambda (c) (send container delete-child c))
    (send container get-children)))

(define (seconds-since-last-mod file)
  (define mod (file-or-directory-modify-seconds file))
  (define now (current-seconds))
  (- now mod))

;;; FIXME: feels dumb and verbose
(define (seconds->english x)
  (define-values (n unit)
    (cond
      [(< x              60 ) (values    x                       "second")]
      [(< x        (* 60 60)) (values (/ x              60)      "minute")]
      [(< x     (* 24 60 60)) (values (/ x        (* 60 60))     "hour")]
      [(< x (*   7 24 60 60)) (values (/ x     (* 24 60 60))     "day")]
      [(< x (* 365 24 60 60)) (values (/ x (*   7 24 60 60))     "week")]
      [else                   (values (/ x (* 365 24 60 60))     "year")]))
  (s+ (number->string (round n)) " " unit (if (> n 2) "s" "")))

(define goal-regexp #rx"/GOAL$")
(define todo-regexp #rx"/TODO$")
(define (goal-files)
  (map path->string (find-files (curry regexp-match? goal-regexp) *root-dir*)))
(define (todo-files)
  (map path->string (find-files (curry regexp-match? todo-regexp) *root-dir*)))

(define (path->location full-path)
  ;; FIXME: ahem...
  (regexp-replace todo-regexp
    (regexp-replace goal-regexp
      (regexp-replace (regexp-quote *root-dir*) full-path "")
      "")
    ""))

(define (get-selected-path list-box)
  (define i (send list-box get-selection))
  (send list-box get-data i))

;;; GUI containers

(define *frame* (new frame% [label "Slacker"]
                          [width 400]
                          [height 400]))

(define *parent-pane* (new panel:horizontal-dragable% [parent *frame*]))
(define *side-pane* (new vertical-pane% [parent *parent-pane*]))
(define *main-pane* (new vertical-pane% [parent *parent-pane*]))

;;; list boxes

(define *todo-entries* (make-hash))
(define *goal-entries* (make-hash))

(define (list-box-select list-box event)
  (define path (get-selected-path list-box))
  (show-entry path))

(define *tab-panel* (new tab-panel% [choices '("TODO" "GOAL")]
                                    [parent *side-pane*]
                                    ;; switch tab content - XXX works only for two children
                                    [callback (lambda (panel event)
                                                (define container (first (send panel get-children)))
                                                (define children  (send container get-children))
                                                (for-each (lambda (c)
                                                            (if (send c is-shown?)
                                                              (send c show #f)
                                                              (send c show #t)))
                                                  children))]))
(define *tab-content* (new panel:single% [parent *tab-panel*]))

(define *todo-list*
  (new list-box%  [parent *tab-content*]
                  [label #f]
                  [columns '("age" "location")]
                  [style '(vertical-label single column-headers)]
                  [choices '()]
                  [callback list-box-select]))
(define *goal-list*
  (new list-box%  [parent *tab-content*]
                  [label #f]
                  [columns '("age" "location")]
                  [style '(vertical-label single column-headers)]
                  [choices '()]
                  [callback list-box-select]))
(send *goal-list* show #f)

;;; adding and showing entries

(define (sync-list-box entry-ht list-box)
  (define box-entries
    (hash-map entry-ht
      (lambda (location data)
        (define age  (first data))
        (define path (second data))
        (list age location path))))
  (set! box-entries (sort box-entries > #:key first))
  ;; displayed data
  (send list-box set (map (compose seconds->english first) box-entries)
                     (map second box-entries))
  ;; hidden data
  (for ([e (in-list box-entries)]
        [i (in-naturals)])
    (send list-box set-data i (third e))))

(define (add-entry entry-ht list-box path)
  (define location (path->location path))
  (hash-set! entry-ht location
                      (list (seconds-since-last-mod path)
                            path))
  (sync-list-box entry-ht list-box))

(define (show-entry path)
  (clear *main-pane*)
  (define button (new button% [parent *main-pane*]
                              [label "GOTO"]
                              [callback (lambda (a b) (goto-action path))]))
  (define label (new message% [parent *main-pane*]
                              [label path]))
  (define text   (new text% [auto-wrap #t]))
  (send text insert (file->string path) 0)
  (new editor-canvas% [parent *main-pane*]
                      [editor text]))

;;; invocation

(define (reload a b)
  (for-each (curry add-entry *todo-entries* *todo-list*) (todo-files))
  (for-each (curry add-entry *goal-entries* *goal-list*) (goal-files)))

(define *reload-button* (new button% [parent *frame*]
                                     [label "reload"]
                                     [callback reload]))

(send *frame* show #t)
(reload #f #f)

