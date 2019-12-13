
(define-module (scm musicxml)
  #:use-module (ice-9 format)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 syncase)
  #:use-module (srfi srfi-1)
  #:use-module (scm xml-library)
  #:use-module (oop goops)
  #:use-module (lily)
)

;; todo: following line should be removed after development
(define print display)

;; todo: following functions should be removed after development
(define  print-stack-top (lambda()
  (let* ((stack (make-stack #t))
    (len (stack-length stack))
    (top 6))
    (if (> top len) (set! top len) '())
    ;top 3 is call of print-stack-top
    (do ((i 3 (1+ i))) ((> i top))
      (let ((frame (stack-ref stack i)))
      (newline)
      (display (frame-source frame)))))
))

; check whether debug is defined.
; if the environment variable LILYPOND_DEBUG is set return true
; otherwise return false
(define check-debug (lambda()
    (not (not (getenv "LILYPOND_DEBUG")))
))

; syntax: assert ?expr ?expr ... [report: ?r-exp ?r-exp ...]
;
; assert is a nop operation if at the moment of loading this module
; the (operating system) environment variable DEBUG was not defined
;
; otherwise :
;
; If (and ?expr ?expr ...) evaluates to anything but #f, the result
; is the value of that expression.
; If (and ?expr ?expr ...) evaluates to #f, an error is reported.
; The error message will show the failed expressions, as well
; as the values of selected variables (or expressions, in general).
; The user may explicitly specify the expressions whose
; values are to be printed upon assertion failure -- as ?r-exp that
; follow the basic-value 'report:'
; Typically, ?r-exp is either a variable or a string constant.
; If the user specified no ?r-exp, the values of variables that are
; referenced in ?expr will be printed upon the assertion failure.
;
(define-syntax assert
  (if (check-debug)
    (syntax-rules (report:)
    ((assert "doit" (expr ...) (r-exp ...))
     (cond
      ((and expr ...) => (lambda (x) x))
      (else
       (throw 'assertion-failure (list '(and expr ...) r-exp ...)))))
    ((assert "collect" (expr ...))
     (assert "doit" (expr ...) ()))
    ((assert "collect" (expr ...) report: r-exp ...)
     (assert "doit" (expr ...) (r-exp ...)))
    ((assert "collect" (expr ...) expr1 stuff ...)
     (assert "collect" (expr ... expr1) stuff ...))
    ((assert stuff ...)
     (assert "collect" () stuff ...)))
    (syntax-rules ()
      ((expr ...)
       '()))))

;  in this document the type basic-value is used.
;  In fact basic-value is not a type, but I use it for convenience.
;  A basic value is a type that can be converted to a string, and will be
;  converted to a string when we use (display) on a value of such a type. It
;  can be used as tag or as a value for a attribute.
;  Within the lifetime of the document the value type will not be changed.
;  So when we do a xml:setAttribute with a integer, the xml:getAttribute will
;  return an integer.
;  This can be useful when we want to use xml-nodes to exchange information.
;  At this moment string, symbol and integer can be used as basic-value, but it is easy to extend this in the future.

(define basic-value? (lambda(value)
    (or (string? value)(symbol? value)(integer? value))))


;insert item in ordered list. Ordering is done using the predicate
(define insert-in-ordered-list (lambda (lst value keyfn predicate)
  (assert (list? lst)
        report: '@insert-in-ordered-list )
    (letrec ((insert (lambda (lst v)
          (cond
            ((null? lst) (cons v lst))
            ((predicate (keyfn (car lst)) (keyfn v))
              (cons v lst))
            (else (cons (car lst) (insert (cdr lst) v)))))))
    (insert lst value))))

; insert key value in ordered alist.
; Ordering is done using the predicate on the key
(define insert-in-ordered-alist (lambda (lst key value predicate)
  (assert (list? lst)
        report: '@insert-in-ordered-alist )
  (letrec ((insert (lambda (lst k v)
          (cond
            ((null? lst) (acons k v lst))
            ((predicate (car (car lst)) k)
              (acons k v lst))
            (else (cons (car lst) (insert (cdr lst) k v)))))))
    (insert lst key value))))

; insert key value in ordered alist.
; only add items if the key does not exists
; Ordering is done using the predicate on the key
(define insert-in-ordered-alist-if-new (lambda (lst key value predicate)
  (assert (list? lst)
        report: '@insert-in-ordered-alist-if-new)
  (letrec ((cur-value (assq key lst)))
    (cond ((not (pair? cur-value))
      (insert-in-ordered-alist lst key value predicate))
      (else lst)))))

;; some warning error helpers
(define warning (lambda (txt)
  (ly:warning (string-append "musicxml.scm: " txt "\n"))))

;; some xml helpers
;; this is an addition an the methods found in xml-library
;; the methods in xml-library are according to the DOM specification,
;; this are additional methods, not found in the DOM specification

(define xml:xml:get-children-by-name (lambda (node tag-name)
  (assert (xml:element? node)
          (symbol? tag-name)
          report: '@xml:get-children-by-name)
  (filter (lambda (n)
    (and (xml:element? n)
        (equal? (xml:nodeName n) tag-name))) (xml:childNodes node))))

(define xml:get-child-by-name (lambda (node tag-name)
  (assert (xml:element? node)
          (symbol? tag-name)
          report: '@xml:get-child-by-name)
  (let ((node (find (lambda (n)
        (and (xml:element? n) (equal? (xml:nodeName n) tag-name)))
      (xml:childNodes node))))
    (if node node '()))))

(define xml:element-value (lambda (self)
  (assert (xml:element? self)
          report: '@xml:element-value)
  (let ((result '()))
  (for-each
    (lambda (node)
      (cond ((and
          (xml:text? node) (not (xml:isElementContentWhitespace node)))
        (cond ((null? result)
            (set! result (xml:nodeValue node)))
          (else
            (set! result (format #f "~a~a" (xml:nodeValue node) result)))))))
  (xml:childNodes self))
  result)))

(define make-xml-item (lambda (doc tag value)
  (assert (xml:document? doc)
          (symbol? tag)
          (or (symbol? value)
              (string? value)
              (integer? value)
              (list? value)
              (xml:element? value))
          report: '@make-xml-item)
  (let* ((result (xml:createElement doc tag)))
  (cond ((list? value)
    (for-each (lambda (attribute)
      (cond ((pair? attribute) (xml:setAttribute result (car attribute)
                                               (cdr attribute) )))) value)
    )
    ((xml:element? value)
      (xml:appendChild result value)
    )
    (else
      (let* ((text-node (xml:createTextNode doc value)))
        (xml:appendChild result text-node))))
  result)))

(define-class <time-signature> ()
  (numerator #:init-value 4 #:init-keyword #:numerator
      #:accessor time-signature:numerator #:type <integer>)
  (denominator #:init-value 4 #:init-keyword #:denominator
      #:accessor time-signature:denominator #:type <integer>))

(define time-signature? (lambda (self)  (is-a? self <time-signature>)))

(define make-time-signature (lambda (item)
  (assert (xml:element? item)
          report: '@make-time-signature)
  (begin
    (cond ((equal? (xml:nodeName item) 'time)
      (let* ((numerator-item (xml:get-child-by-name  item 'beats))
          (denominator-item (xml:get-child-by-name  item 'beat-type))
          (numerator (xml:element-value numerator-item))
          (denominator (xml:element-value denominator-item)))
        (make <time-signature>
          #:numerator  numerator
          #:denominator denominator )))))))

(define time-signature:get-measure-ticks (lambda (self)
  (assert (time-signature? self)
          report: '@time-signature:get-measure-ticks)
  (/ (* ticks-per-whole (time-signature:numerator self))
        (time-signature:denominator self))))

(define time-signature->xml-item (lambda (self doc)
  (assert (time-signature? self)
          (xml:document? doc)
          report: '@time-signature->xml-item)
  (let ((time-item (xml:createElement doc 'time))
      (beats-item (xml:createElement doc 'beats))
      (beat-type-item (xml:createElement doc 'beat-type))
      (beats-text-node (xml:createTextNode doc
                          (time-signature:numerator self)))
      (beat-type-text-node (xml:createTextNode doc
                          (time-signature:denominator self))))
    (xml:appendChild beats-item beats-text-node)
    (xml:appendChild beat-type-item beat-type-text-node)
    (xml:appendChild time-item beats-item)
    (xml:appendChild time-item beat-type-item)
    time-item)))

(define get-item-duration (lambda (item)
  (assert (xml:element? item)
          report: '@get-item-duration)
  (letrec ((duration-item-names '(note lyric figured-bass))
      (get-duration-item
        (lambda (item lst)
          (cond ((null? lst) '())
            ((equal? (xml:nodeName item) (car lst)) item)
            ((xml:element? (xml:get-child-by-name item (car lst)))
                (xml:get-child-by-name item (car lst)))
            (else (get-duration-item item (cdr lst)))))))
    (let ((duration-note (get-duration-item item duration-item-names)))
      (cond ((xml:element? duration-note)
        (let* ((duration (xml:get-child-by-name duration-note 'duration)))
          (xml:element-value duration)))
          (else 0))))))

;forward definition
(define-class <music-tree> ())

(define-class <music-tree> ()
  (ticks #:init-value 0 #:init-keyword #:ticks
      #:accessor music-tree:ticks #:type <integer>)
  (max-ticks #:init-value 0 #:init-keyword #:max-ticks
      #:accessor music-tree:max-ticks #:setter music-tree:set-max-ticks!
      #:type <integer>)
  (item-nr #:init-value 0 #:init-keyword #:item-nr
      #:accessor music-tree:item-nr #:type <integer>)
  (tag #:init-value '() #:init-keyword #:tag #:accessor music-tree:tag)
  (current-event #:init-value '() #:init-keyword #:current-event
      #:accessor music-tree:current-event #:type <symbol>)
  (parent #:init-value '() #:init-keyword #:parent
      #:accessor music-tree:parent #:type <music-tree>)
  (branches #:init-value '() #:init-keyword #:branches
      #:accessor music-tree:branches #:setter music-tree:set-branches!
      #:type <list>)
)

(define music-tree? (lambda (self)  (is-a? self <music-tree>)))

(define* (make-music-tree item-nr event current-branch #:optional (ticks 0))
  (assert (integer? item-nr)
          (symbol? event)
          (music-tree? current-branch)
          (integer? ticks)
          report: '@make-music-tree)
  (let ((result (make <music-tree>
      #:ticks ticks
      #:max-ticks ticks
      #:item-nr item-nr
      #:current-event event
      #:parent current-branch)))
    (music-tree:set-branches! current-branch
        (cons result (music-tree:branches current-branch)))
    result))

(define make-music-tree-copy (lambda (self current-branch tag)
  (assert (music-tree? self)
          (music-tree? current-branch)
          report: '@make-music-tree-copy)
  (let ((result (make <music-tree>
      #:current-event (music-tree:current-event self)
      #:item-nr (music-tree:item-nr self)
      #:tag tag
      #:parent current-branch)))
    (music-tree:set-branches! current-branch
        (insert-in-ordered-list (music-tree:branches current-branch)
            result music-tree:item-nr >))
    result)))

(define music-tree:previous (lambda (self)
  (assert (music-tree? self)
          report: '@music-tree:previous)
  (music-tree:parent self)))

(define-class <voice-list> ()
  (id #:init-value #f #:init-keyword #:id #:accessor voice-list:id  #:setter voice-list:set-id!)
  (cue-voice #:init-value #f #:init-keyword #:cue-voice
      #:accessor voice-list:cue-voice  #:setter voice-list:set-cue-voice!)
  (null-voice #:init-value #f #:init-keyword #:null-voice
      #:accessor voice-list:null-voice  #:setter voice-list:set-null-voice!)
  (figured-bass #:init-value #f #:init-keyword #:figured-bass
      #:accessor voice-list:figured-bass  #:setter voice-list:set-figured-bass!)
  ;(sorted) element alist each item is (ticks <integer> . element <xml:element>)
  (collected-xml-nodes #:init-value '() #:init-keyword #:syllabic-status
      #:accessor voice-list:collected-xml-nodes
      #:setter voice-list:set-collected-xml-nodes! #:type <list>))

(define make-voice-list (lambda () (make <voice-list>)))

(define voice-list? (lambda (self)  (is-a? self <voice-list>)))

(define voice-list:insert-xml-item  (lambda (self ticks item)
  (assert (voice-list? self)
          (integer? ticks)
          (xml:element? item)
          report: '@voice-list:insert-xml-item)
  (case (xml:nodeName item)
    ((figured-bass) (voice-list:set-figured-bass! self #t)))
  (voice-list:set-collected-xml-nodes! self
      (insert-in-ordered-alist
          (voice-list:collected-xml-nodes self) ticks item >))))

(define-class <part-list> ()
  (has-lyrics #:init-value #f #:init-keyword #:has-lyrics
      #:accessor part-list:has-lyrics #:setter part-list:set-has-lyrics!
      #:type <boolean>)
  (has-notes #:init-value #f #:init-keyword #:has-notes
      #:accessor part-list:has-notes #:setter part-list:set-has-notes!
      #:type <boolean>)
  (id #:init-value #f #:init-keyword #:id #:accessor part-list:id
      #:setter part-list:set-id!)
  ;; ordered voices alist : `(voice <integer> . lst <part-list>)
  (voices #:init-value '() #:init-keyword #:voices #:accessor part-list:voices
      #:setter part-list:set-voices! #:type <list>))

(define make-part-list (lambda () (make <part-list>)))

(define part-list? (lambda (self)  (is-a? self <part-list>)))

(define part-list:insert-xml-item  (lambda (self ticks voice item)
  (assert (part-list? self)
          (integer? ticks)
          (integer? voice)
          (xml:element? item)
          report: '@part-list:insert-xml-item)
  (let* ((lst (part-list:voices self))
      (collection-pair (assq voice lst))
      (collection '()))
    (case (xml:nodeName item)
      ((note) (part-list:set-has-notes! self #t))
      ((lyric) (part-list:set-has-lyrics! self #t))
    )
    (cond ((pair? collection-pair) (set! collection (cdr collection-pair)))
      (else
        (set! collection (make-voice-list))
        (part-list:set-voices! self
            (insert-in-ordered-alist lst voice collection >))))
    (voice-list:insert-xml-item collection ticks item))))

(define part-list:add-voice-id (lambda (self voice id)
  (assert (part-list? self)
          (integer? voice)
          report: '@part-list:add-voice-id)
  (let* ((lst (part-list:voices self))
      (collection-pair (assq voice lst))
      (collection '()))
    (cond ((pair? collection-pair) (set! collection (cdr collection-pair)))
      (else
        (set! collection (make-voice-list))
        (part-list:set-voices! self
            (insert-in-ordered-alist lst voice collection >))))
    (voice-list:set-id! collection id))))

(define part-list:get-voice (lambda (self associated)
  (assert (part-list? self)
          report: '@part-list:get-voice)
  (letrec ((lst (part-list:voices self))
    (get-voice (lambda (lst voice)
            (cond ((null? lst) '())
            ((not (pair? (car lst))) '())
            ((equal? (voice-list:id (cdar lst)) voice) (cdar lst))
            (else (get-voice (cdr lst) voice))))))
    (get-voice lst associated))))

(define part-list:set-element-voice (lambda (self voice fn)
  (assert (part-list? self)
          (integer? voice)
          report: '@part-list:set-element-voice)
  (let* ((lst (part-list:voices self))
      (collection-pair (assq voice lst))
      (collection '()))
    (cond ((pair? collection-pair) (set! collection (cdr collection-pair)))
      (else
        (set! collection (make-voice-list))
        (part-list:set-voices! self
            (insert-in-ordered-alist lst voice collection >))))
    (fn collection #t))))

(define part-list:set-cue-voice (lambda (self voice)
  (assert (part-list? self)
          (integer? voice)
          report: '@part-list:set-cue-voice)
  (part-list:set-element-voice self voice voice-list:set-cue-voice!)))

(define part-list:set-null-voice (lambda (self voice)
  (assert (part-list? self)
          (integer? voice)
          report: '@part-list:set-null-voice)
  (part-list:set-element-voice self voice voice-list:set-null-voice!)))

; a class to hold items common to all voices at the same time,
; together with a running time position

(define-class <music-information> ()
  (time-signatures #:init-value '() #:init-keyword #:time-signatures
    #:accessor music-information:time-signatures
    #:setter music-information:set-time-signatures! #:type <list>)
  ; a running value during conversion, reset each time a new voice is handled
  (ticks #:init-value 0 #:init-keyword #:ticks
    #:accessor music-information:ticks #:setter music-information:set-ticks!
    #:type <integer>)
  ; max value, used to calculate the number of measures needed.
  (max-ticks #:init-value 0 #:init-keyword #:max-ticks
    #:accessor music-information:max
      #:setter music-information:set-max! #:type <integer>)
  (document #:init-keyword #:document #:accessor music-information:document
      #:setter music-information:set-document! #:type <xml:document>)
  (staff-nr #:init-value 0 #:init-keyword #:staff-nr
      #:accessor music-information:staff-nr
      #:setter music-information:set-staff-nr! #:type <integer>)
  (voice-nr #:init-value 0 #:init-keyword #:voice-nr
      #:accessor music-information:voice-nr
      #:setter music-information:set-voice-nr! #:type <integer>)
  (max-voice-nr #:init-value 0 #:init-keyword #:max-voice-nr
      #:accessor music-information:max-voice-nr
      #:setter music-information:set-max-voice-nr! #:type <integer>)
  ;; ordered staff alist : `(staff <integer> . lst <part-list>)
  (staves #:init-value '() #:init-keyword #:staves
      #:accessor music-information:staves
      #:setter music-information:set-staves! #:type <list>))

(define make-music-information (lambda ()
  (make <music-information>)))

(define music-information? (lambda (self)  (is-a? self <music-information>)))

(define music-information:clone (lambda (self)
  (assert (music-information? self)
          report: '@music-information:clone)
  (make <music-information>
    #:time-signatures (music-information:time-signatures self )
    #:ticks (music-information:ticks self )
    #:document (music-information:document self )
    #:staff-nr (music-information:staff-nr self)
    #:voice-nr (music-information:voice-nr self)
    #:max-voice-nr (music-information:max-voice-nr self))))

(define music-information:append-ticks (lambda (self value)
  (assert (music-information? self)
          (integer? value)
          report: '@music-information:append-ticks)
  (music-information:set-ticks! self  (+ (music-information:ticks self) value))
  (cond ((> (music-information:ticks self) (music-information:max self))
    (music-information:set-max! self (music-information:ticks self))))))

(define music-information:insert-time-signature
      (lambda (self ts absolute-ticks)
  (assert (music-information? self)
          (time-signature? ts)
          (integer? absolute-ticks)
          report: '@music-information:insert-time-signature)
  (music-information:set-time-signatures! self
    (insert-in-ordered-alist-if-new (music-information:time-signatures self)
        absolute-ticks ts >))))

(define music-information:append-time-signature (lambda (self  element)
  (assert (music-information? self)
          (xml:element? element)
          report: '@music-information:append-time-signature)
  (let* ((absolute-ticks (music-information:ticks self))
      (ts (make-time-signature element)))
        (music-information:insert-time-signature self ts absolute-ticks))))

(define music-information:append-time-signature-with-offset
      (lambda (self element offset)
  (assert (music-information? self)
          (xml:element? element)
          (integer? offset)
          report: '@music-information:append-time-signature-with-offset)
  (let* ((absolute-ticks (+ (music-information:ticks self) offset))
      (ts (make-time-signature element)))
        (music-information:insert-time-signature self ts absolute-ticks))))

(define music-information:get-ts@-or-before (lambda (self ticks)
  (assert (music-information? self)
          (integer? ticks)
          report: '@music-information:get-ts@-or-before)
  (find (lambda (n) (<= (car n) ticks))
      (music-information:time-signatures self))))

;; find all time attributes
;; it is possible a time key is not in all parts,
;; but it should be applied to all parts at the same time

(define music-information:collect-time-signatures
    (lambda (self attributes note-item ignore-ticks)
  (assert (music-information? self)
          (xml:element? attributes)
          (xml:element? note-item)
          (boolean? ignore-ticks)
          report: '@music-information:collect-time-signatures)
  (let* ((ticks (get-item-duration note-item))
    (append-item (xml:get-child-by-name attributes 'partial))
    (time-item (xml:get-child-by-name attributes 'time))
    (measure-ticks (if (xml:element? time-item)
                            (music-xml->ticks time-item)
                             default-measure-ticks)))
    (begin
      (cond ((xml:element? time-item)
        (cond ((xml:element? append-item)
          (let ((partial (xml:element-value append-item)))
              (music-information:append-time-signature-with-offset
                  self time-item (- partial measure-ticks))))
          (else
          (music-information:append-time-signature self time-item))))
        ; will later be reinserted in all parts
        (xml:removeChild attribute time-item)
      )
      (if ignore-ticks
        '()
        (music-information:append-ticks self ticks ))))))


(define music-information:insert-xml-element@ (lambda (self item position)
  (assert (music-information? self)
          (xml:element? item)
          (integer? position)
          report: '@music-information:insert-xml-element@)
  (let* ((staff (music-information:staff-nr self))
      (voice (music-information:voice-nr self))
      (lst (music-information:staves self))
      (collection-pair (assq staff lst))
      (collection '()))
    (xml:setUserData item 'position position)
    (xml:setUserData item 'voice-nr (music-information:voice-nr self))
    (xml:setUserData item 'staff-nr (music-information:staff-nr self))
    (cond ((pair? collection-pair) (set! collection (cdr collection-pair)))
      (else
        (set! collection (make-part-list))
        (music-information:set-staves! self
            (insert-in-ordered-alist lst staff collection >))))
    (part-list:insert-xml-item collection position voice item))))

(define music-information:insert-xml-element (lambda (self item)
  (assert (music-information? self)
          (xml:element? item)
          report: '@music-information:insert-xml-element)
  (music-information:insert-xml-element@
    self item (music-information:ticks self))))

(define music-information:set-element-part (lambda (self staff voice fn)
  (assert (music-information? self)
          (integer? staff)
          (integer? voice)
          report: '@music-information:set-element-part)
  (let* ((lst (music-information:staves self))
      (collection-pair (assq staff lst))
      (collection '()))
    (cond ((pair? collection-pair) (set! collection (cdr collection-pair)))
      (else
        (set! collection (make-part-list))
        (music-information:set-staves! self
            (insert-in-ordered-alist lst staff collection >))))
    (fn collection voice))))

(define music-information:set-cue-voice (lambda (self staff voice)
  (assert (music-information? self)
          (integer? staff)
          (integer? voice)
          report: '@music-information:set-cue-voice)
  (music-information:set-element-part
      self staff voice part-list:set-cue-voice)))

(define music-information:set-null-voice (lambda (self staff voice)
  (assert (music-information? self)
          (integer? staff)
          (integer? voice)
          report: '@music-information:set-null-voice)
  (music-information:set-element-part
      self staff voice part-list:set-null-voice)))


(define music-information:add-staff-id (lambda (self staff id)
  (assert (music-information? self)
          (integer? staff)
          report: '@music-information:add-staff-id)
  (let* ((lst (music-information:staves self))
      (collection-pair (assq staff lst))
      (collection '()))
    (cond ((pair? collection-pair) (set! collection (cdr collection-pair)))
      (else
        (set! collection (make-part-list))
        (music-information:set-staves! self
            (insert-in-ordered-alist lst  staff collection >))))
    (part-list:set-id! collection id))))

(define music-information:add-voice-id (lambda (self staff voice id)
  (assert (music-information? self)
          (integer? staff)
          (integer? voice)
          report: '@music-information:add-voice-id)
  (let* ((lst (music-information:staves self))
      (collection-pair (assq staff lst))
      (collection '()))
    (cond ((pair? collection-pair) (set! collection (cdr collection-pair)))
      (else
        (set! collection (make-part-list))
        (music-information:set-staves! self
            (insert-in-ordered-alist lst staff collection >))))
    (part-list:add-voice-id collection voice id))))

(define music-information:get-voice (lambda (self associated)
  (assert (music-information? self)
          report: '@music-information:get-voice)
  (letrec ((lst (music-information:staves self))
    (get-voice (lambda (lst voice)
            (cond ((null? lst) '())
              ((not (pair? (car lst))) '())
              ((not (null? (part-list:get-voice (cdar lst) voice)))
                          (part-list:get-voice (cdar lst) voice))
              (else (get-voice (cdr lst) voice))))))
    (get-voice lst associated))))


(define-class <translation-status> (<music-information>)
  (chord-start-ticks #:init-value 0 #:init-keyword #:chord-start-ticks
      #:accessor translation-status:chord-start-ticks
      #:setter translation-status:set-chord-start-ticks! #:type <integer>)
  (chord-item #:init-value #f #:init-keyword #:chord-item
      #:accessor translation-status:chord-item
      #:setter translation-status:set-chord-item!)
  (tie-status #:init-value #f #:init-keyword #:tie-status
      #:accessor translation-status:tie-status
      #:setter translation-status:set-tie-status! #:type <boolean>)
  (beam-status #:init-value #f #:init-keyword #:beam-status
      #:accessor translation-status:beam-status
      #:setter translation-status:set-beam-status! #:type <boolean>)
  (syllabic-status #:init-value #f #:init-keyword #:syllabic-status
      #:accessor translation-status:syllabic-status
      #:setter translation-status:set-syllabic-status! #:type <boolean>)
  (voice-tree #:init-keyword #:voice-tree
      #:accessor translation-status:voice-tree
      #:setter translation-status:set-voice-tree! #:type <music-tree>)
  (context-spec #:init-keyword #:context-tree
      #:accessor translation-status:context-tree
      #:setter translation-status:set-context-tree! #:type <music-tree>)
)

(define make-translation-status (lambda (document)
  (assert (xml:document? document)
          report: '@make-translation-status)
  (make <translation-status>
    #:document document
    #:voice-tree (make <music-tree>)
    #:context-tree (make <music-tree>))))

(define translation-status? (lambda (self)  (is-a? self <translation-status>)))

; forward declaration
(define-class <music-item> ())

(define translation-status:add-id  (lambda (self item id)
  (assert (translation-status? self)
          (music-item? item)
          report: '@translation-status:add-id)
  (let* ((staff (music-information:staff-nr self))
    (voice (music-information:voice-nr self))
    (branch (translation-status:voice-tree self))
    (doc (music-information:document self))
    (type (music-item:get-attribute item 'context-type)))
    (if (equal? (music-tree:current-event branch) 'SimultaneousMusic)
      (set! voice (1+ voice))
      '()
    )
    (case type
      ((Staff)
        (music-information:add-staff-id self staff id))
      ((Voice NullVoice CueVoice)
        (music-information:add-voice-id self staff voice id))
    ))))

(define translation-status:add-branch-to-voice-tree (lambda (self name)
  (assert (translation-status? self))
  (assert (symbol? name))
  (let ((branch (translation-status:voice-tree self)))
    (translation-status:set-voice-tree! self
        (make-music-tree (music-information:voice-nr self)
          name branch (music-information:ticks self)))
    (cond ((equal? (music-tree:current-event branch) 'SimultaneousMusic)
; remark: for each branch of SimultaneousMusic voice-nr is incremented.
; so in the voice of the first branch will be the same as before
; SimultaneousMusic started
      (music-information:set-voice-nr! self
          (+ (music-information:voice-nr self) 1))
      (if (> (music-information:voice-nr self)
            (music-information:max-voice-nr self))
        (music-information:set-max-voice-nr! self
            (music-information:voice-nr self)) '() )
      (music-information:set-ticks! self (music-tree:ticks branch)))))))

(define translation-status:remove-branch-from-voice-tree (lambda (self)
  (assert (translation-status? self))
  (let ((branch (music-tree:previous (translation-status:voice-tree self))))
    (translation-status:set-voice-tree! self branch)
    (if (> (music-information:ticks self) (music-tree:max-ticks branch))
      (music-tree:set-max-ticks! branch (music-information:ticks self))
      '()
    ))))

(define translation-status:current-context-spec (lambda (self)
  (assert (translation-status? self))
  (let ((context-type (translation-status:context-tree self)))
  (if (null? context-type) '() (music-tree:current-event context-type)))))

(define pp-pitch-names '((0 . "C") (1 . "DES") (2 . "D") (3 . "ES") (4 . "E")
                        (5 . "F")  (6 . "GES") (7 . "G") (8 . "AS") (9 . "A")
                        (10 . "BES") (11 . "B")))

(define music-pitch->xml-item  (lambda (p element doc)
  (assert (ly:pitch? p)
          (xml:element? element)
          (xml:document? doc)
          report: '@music-pitch->xml-item)
  (let*  ((subpitch (ly:pitch-semitones p))
    (pitch (+ (* -2 (ly:pitch-alteration p)) subpitch))
    (result (xml:createElement doc 'pitch))
    (step (xml:createElement doc 'step))
    (step-text (xml:createTextNode doc
        (assoc-get (modulo pitch 12) pp-pitch-names)))
    (alter (xml:createElement doc 'alter))
    (alter-text (xml:createTextNode doc ( * 2 (ly:pitch-alteration p))))
    (octave (xml:createElement doc 'octave))
    (octave-text (xml:createTextNode doc ( + 4 (ly:pitch-octave p)))))
    (xml:setUserData element 'pitch pitch) ; used for chord analysis
    (xml:appendChild step step-text)
    (xml:appendChild alter alter-text)
    (xml:appendChild octave octave-text)
    (xml:appendChild result step)
    (xml:appendChild result alter)
    (xml:appendChild result octave)
    (xml:appendChild element result))))

(define ticks-per-whole 192)
(define default-measure-ticks ticks-per-whole)


(define music-duration->notename (lambda (duration)
  (assert (ly:duration? duration)
          report: '@music-duration->notename)
  (let* ((duration-names '(
      (10 . 1024th )
      (9 . 512th )
      (8 . 256th )
      (7 . 128th )
      (6 . 64th )
      (5 . 32nd )
      (4 . 16th )
      (3 . eighth )
      (2 . quarter )
      (1 . half )
      (0 . whole )
      (-1 . breve )
      (-2 . long  )
      (-3 . maxima )
      ))
      (dlog (ly:duration-log duration)))
    (assoc-get dlog duration-names))))

(define music-duration->dots (lambda (duration)
  (assert (ly:duration? duration)
          report: '@music-duration->dots)
  (ly:duration-dot-count duration)))


(define music-duration->xml-items (lambda (d doc parent)
  (assert (ly:duration? d)
          (xml:document? doc)
          (xml:element? parent)
          report: '@music-duration->xml-items)
  (let* ((dlength (ly:duration-length d))
      (dnumerator  (ly:moment-main-numerator dlength))
      (ddenominator  (ly:moment-main-denominator dlength))
      (duration-element (xml:createElement doc 'duration))
      (duration-value (/ (* ticks-per-whole dnumerator) ddenominator))
      (duration-text (xml:createTextNode doc duration-value))
      (type-element (xml:createElement doc 'type))
      (type-text (xml:createTextNode doc (music-duration->notename d))))
    (xml:appendChild duration-element duration-text)
    (xml:appendChild type-element type-text)
    (xml:appendChild parent duration-element)
    (xml:appendChild parent type-element)
    (do ((i (music-duration->dots d) (1- i)))((< i 1))
      (let ((dot (xml:createElement doc 'dot)))
        (xml:appendChild parent dot))))))

(define music-time->xml-item (lambda (expr element doc)
  (assert (ly:music? expr)
          (xml:element? element)
          (xml:document? doc)
          report: '@music-time->xml-item)
  (let* ((beats (ly:music-property expr 'numerator))
         (beat-type (ly:music-property expr 'denominator))
     (beat-element (make-xml-item doc 'beats beats))
     (type-element (make-xml-item doc 'beat-type beat-type))
     )
     (xml:appendChild element beat-element)
     (xml:appendChild element type-element))))

(define music-xml->ticks (lambda (item)
  (assert (xml:element? item)
          report: '@music-xml->ticks)
  (cond ((equal? (xml:nodeName item) 'time)
    (let* ((numerator-item (xml:get-child-by-name  item 'beats))
        (denominator-item (xml:get-child-by-name  item 'beat-type))
        (numerator (xml:element-value numerator-item))
        (denominator (xml:element-value denominator-item)))
      (/ (* numerator ticks-per-whole) denominator)))
    (else default-measure-ticks))))

(define pp-pitch-major-fifth
  '((0 . 0) (1 . -5) (2 . 2) (3 . -3) (4 . 4) (5 . -1)
  (6 . -6) (7 . 1) (8 . -4) (9 . 3) (10 . -2) (11 . 5)))

(define pp-reverse-pitch-major-fifth
  '((0 . 0) (1 . 7) (2 . 2) (3 . 9) (4 . 4) (5 . 11)
  (6 . 6) (7 . 1) (8 . 8) (9 . 3) (10 . 10) (11 . 5) ))

(define pp-pitch-minor-fifth
  '((0 . -3) (1 . 4) (2 . -1) (3 . -6) (4 . 1) (5 . -4)
  (6 . 3) (7 . -2) (8 . 5) (9 . 0) (10 . -5) (11 . 2) ))

(define pp-mode-list
  '((1 . "lydian") (0 . "major") (6 . "locrian") (7 . "phrygian")
   (8 . "minor") (9 . "dorian")))

;; todo: find out about used mode
;; minor -3
;; dorian -2
;; ionian 0
;; locrian -5
;; aeolian -3
;; lydian +1
;; phrygian -4

(define music-key->xml-item (lambda (key element doc)
  (assert (ly:music? key)
          (xml:element? element)
          (xml:document? doc)
          report: '@music-key->xml-item)
  (let* ((pitch-alist (ly:music-property key 'pitch-alist))
    (fifths (if (null? pitch-alist) 0
        (apply + (map (lambda(n) (* (cdr n) 2)) pitch-alist))))
    (fifths-element (make-xml-item doc 'fifths fifths))
    )
     (xml:appendChild element fifths-element))))

(define clef-list
  '(("clefs.G" . G)
    ("clefs.GG" . G)
    ("clefs.tenorG" . G)
    ("clefs.C" . C)
    ("clefs.varC" . C)
    ("clefs.F" . F)
    ("clefs.percussion" . percussion)
    ("clefs.varpercussion" . percussion)
    ("clefs.tab" . tab )
    ("clefs.vaticana.do" . C)
    ("clefs.vaticana.fa" . C)
    ("clefs.medicaea.do" . C)
    ("clefs.medicaea.fa" . C)
    ("clefs.hufnagel.do" . C)
    ("clefs.hufnagel.fa" . C)
    ("clefs.hufnagel.do.fa" . C)
    ("clefs.mensural.c" . C)
    ("clefs.mensural.f" . F)
    ("clefs.mensural.g" . G)
    ("clefs.blackmensural.c" . C)
    ("clefs.neomensural.c" . C)
    ("clefs.petrucci.c1" . C)
    ("clefs.petrucci.c2" . C)
    ("clefs.petrucci.c3" . C)
    ("clefs.petrucci.c4" . C)
    ("clefs.petrucci.c5" . C)
    ("clefs.petrucci.f2" . F)
    ("clefs.petrucci.f3" . F)
    ("clefs.petrucci.f4" . F)
    ("clefs.petrucci.f5" . F)
    ("clefs.petrucci.f" . F)
    ("clefs.petrucci.g1" . G)
    ("clefs.petrucci.g2" . G)
    ("clefs.petrucci.g" . G)
    ("clefs.kievan.do" . C)))

(define-class <music-item> ()
  (name #:init-value '() #:accessor music-item:name
      #:init-keyword #:name #:type <symbol>)
  (music #:init-value #f #:accessor music-item:music #:init-keyword #:music)
  (parent #:init-value '() #:accessor music-item:parent
      #:init-keyword #:parent)
  (context #:init-value #f #:accessor music-item:context
      #:setter music-item:set-context! #:init-keyword #:context
      #:type <music-item>)
  (part-content #:init-value '() #:accessor music-item:part-context
      #:setter music-item:set-part-context! #:init-keyword #:part-content
      #:type <xml:element>)
  (xml-content #:init-value '() #:accessor music-item:xml-content
      #:setter music-item:set-xml-content! #:init-keyword #:xml-content
      #:type <xml:element>)
  (attributes #:init-value '() #:accessor music-item:attributes
      #:setter music-item:set-attributes! #:init-keyword #:attributes
      #:type <list>)
  (children #:init-value '() #:accessor music-item:children
      #:setter music-item:set-children! #:init-keyword #:children
      #:type <list>)
      )

(define music-item? (lambda (self)  (is-a? self <music-item>)))

(define make-empty-music-item (lambda ()
  (make <music-item>)))

(define make-music-item (lambda (name music context parent)
  (assert (symbol? name)
          (ly:music? music)
          (music-item? context)
          (music-item? parent)
          report: '@make-music-item)
  (let ((result (make <music-item>
        #:name name
        #:music music
        #:context context
        #:parent parent
        )))
    (music-item:append-child parent result)
    result)))


(define music-item:get-attribute (lambda (self attr)
  (assert (music-item? self)
          (symbol? attr)
          report: '@music-item:get-attribute)
  (assoc-get attr (music-item:attributes self))))

(define music-item:set-attribute (lambda (self key value)
  (assert (music-item? self)
          (symbol? key)
          report: '@music-item:set-attribute)
  (let ((lst (music-item:attributes self)))
    (music-item:set-attributes! self (assoc-set! lst key value)))))

(define music-item:append-child (lambda (self child)
  (assert (music-item? self)
          (music-item? child)
          report: '@music-item:append-child)
  (letrec ((lst (music-item:children self))
      (item-append (lambda (i lst )
        (cond ((null? lst) (cons i '()))
          (else (cons (car lst) (item-append i (cdr lst))))))))
  (music-item:set-children! self (item-append child lst)))))

(define get-item-stop (lambda (music item)
  (assert (ly:music? music)
          (symbol? item)
          report: '@get-item-stop)
  (case (ly:music-property music 'span-direction)
    ((-1) item)
    ((0) 'continue)
    ((1) 'stop)
    (else 'start))))

(define get-start-stop (lambda (music)
  (assert (ly:music? music)
          report: '@get-start-stop)
  (case (ly:music-property music 'span-direction)
    ((-1) 'start)
    ((0) 'continue)
    ((1) 'stop)
    (else 'start))))

(define direction-articulations->xml-item (lambda (music context parent doc)
  (assert (ly:music? music)
          (music-item? context)
          (xml:element? parent)
          (xml:document? doc)
          report: '@direction-articulations->xml-item)
  (let* ((direction-type-names
        '(
        (DecrescendoEvent . wedge )
        (CrescendoEvent . wedge )
        (TextScriptEvent . words)
      ))
      (lilyname (ly:music-property music 'name))
      (name (assoc-get lilyname direction-type-names 'empty))
      (item (xml:createElement doc 'direction-type))
      (child-item (xml:createElement doc name)))
    (case lilyname
      ((DecrescendoEvent)
        (xml:setAttribute child-item 'type
              (get-item-stop music 'diminuendo ))
        (xml:appendChild item child-item)
        (xml:appendChild parent item))
      ((CrescendoEvent)
        (xml:setAttribute child-item 'type
              (get-item-stop music 'crescendo))
        (xml:appendChild item child-item)
        (xml:appendChild parent item))
      ((TextScriptEvent)
        (let* ((text (ly:music-property music 'text))
            (text-element (xml:createTextNode doc text)))
          (xml:appendChild child-item text-element)
          (xml:appendChild item child-item)
          (xml:appendChild parent item)))))))

(define articulations->xml-item (lambda (music context parent doc)
  (assert (ly:music? music)
          (music-item? context)
          (xml:element? parent)
          (xml:document? doc)
          report: '@articulations->xml-item)
  (let* ((lilyname (ly:music-property music 'name))
    (art-types `(
      ("accent" . accent)
      ("espressivo" . espressivo)
      ("stopped" . stopped)
      ("marcato" . strong-accent)
      ("staccato" . staccato)
      ("staccatissimo" . staccatissimo)
      ("portato" . detached-legato)
      ("tenuto" . tenuto)
      ("fermata" . fermata)
      ))
    (articulation-names '(
        (ArticulationEvent . articulations)
        (SlurEvent . slur)
        (PhrasingSlurEvent . slur)
        (AbsoluteDynamicEvent . dynamics)
        (TieEvent . tied)
      ))
    (item-names '(
      (HyphenEvent . syllabic)
      (BeamEvent . beam)
      ))
    (name (assoc-get lilyname articulation-names))
    (item-name (assoc-get lilyname item-names 'notations))
    (articulation-type (ly:music-property music 'articulation-type))
    (item-element (xml:get-child-by-name parent item-name))
    (should-append (not (xml:element? item-element)))
    (item (xml:createElement doc item-name ))
    (child-item (if (not name) '() (xml:createElement doc name))))
    (begin
      (if should-append '() (set! item item-element))
      (case lilyname
        ((AbsoluteDynamicEvent)
          (let* ((dynamic (ly:music-property music 'text))
              (dynamic-item (xml:createElement doc dynamic)))
            (xml:appendChild child-item dynamic-item)))
        ((BeamEvent)
          (xml:setAttribute item 'type  (get-start-stop music)))
        ((TieEvent)
          (xml:setAttribute child-item 'type  (get-start-stop music)))
        ((SlurEvent)
          (xml:setAttribute child-item 'type  (get-start-stop music))
          (xml:setAttribute child-item 'number 1 ))
        ((PhrasingSlurEvent)
          (xml:setAttribute child-item 'type  (get-start-stop music))
          (xml:setAttribute child-item 'number 2 ))
        ((ArticulationEvent)
          (cond ((equal? articulation-type "stopped")
              (set! child-item (xml:createElement doc 'technical)))
          )
          (let (( articulation (assoc-get articulation-type art-types)))
            (case articulation
              ((fermata)
                (set! child-item (xml:createElement doc 'fermata )))
              (else
                (cond (articulation 
                  (let ((sub-item (xml:createElement doc
                            (assoc-get articulation-type art-types))))
                        (xml:appendChild child-item sub-item)
                )))))))
        ((HyphenEvent)
          (xml:appendChild item (xml:createTextNode doc 'begin))
          (if should-append (xml:appendChild parent item)) '())
      )
      (cond ((not (null? child-item ))
        (xml:appendChild item child-item)
        (if should-append (xml:appendChild parent item)) '()))))))

(define add-part-context-element (lambda (music element doc)
  (assert (music-item? music)
          (xml:document? doc)
          report: '@add-part-context-element)
  ;; todo: null? condition should be removed when module is stable
  (assert (or (null? element)(xml:element? element)))
  (if (null? element)
    (warning "element added before context is present")
    (let* ((context (music-item:context music))
      (attr (music-item:part-context context)))
      (cond ((not (xml:element? attr))
        (set! attr (xml:createElement doc 'attributes))
        (music-item:set-part-context! context attr)))
      (xml:appendChild attr element)))))

(define get-part-context-element (lambda (music element)
  (assert (music-item? music)
          (symbol? element)
          report: '@get-part-context-element)
  (let* ((context (music-item:context music))
      (attr (music-item:part-context context)))
    (cond ((xml:element? attr)
      (xml:get-child-by-name attr element))
    (else '())))))

(define get-part-context-elements-copy (lambda (music)
  (assert (music-item? music)
          report: '@get-part-context-elements-copy)
  (let* ((context (music-item:context music))
      (attr (music-item:part-context context)))
    (cond ((xml:element? attr) (xml:cloneNode attr #t))
      (else '())))))

(define extract-part-context-element (lambda (music doc)
  (assert (music-item? music)
          (xml:document? doc)
          report: '@extract-part-context-element)
  (let* ((context (music-item:context music))
      (attr (music-item:part-context context)))
    (cond ((xml:element? attr)
      (let ((new-attr (xml:createElement doc 'attributes)))
        (music-item:set-part-context! context new-attr))))
    attr)))

(define bar->musicxml (lambda (value doc)
  (let ((barline-element (xml:createElement doc 'barline))
      (bar-style-element (xml:createElement doc 'bar-style))
      (repeat-element (xml:createElement doc 'repeat))
      (bar-style '())
      (repeat-direction '())
      (repeat-winged '()))
    (xml:setAttribute barline-element 'location 'middle)
    ; not done with case because of the characters used in value
    (cond ((equal? value ";")(set! bar-style 'dotted))
      ((equal? value ".") (set! bar-style 'heavy))
      ((equal? value ".|") (set! bar-style 'heavy-light))
      ((equal? value "..") (set! bar-style 'heavy-heavy))
      ; not supported, replace by heavy-heavy is questionable
      ((equal? value "|.|") (set! bar-style 'heavy-heavy))
      ((equal? value "|.")  (set! bar-style 'light-heavy))
      ((equal? value "'") (set! bar-style 'tick))
      ((equal? value "!") (set! bar-style 'dashed))
      ((equal? value "|") (set! bar-style 'regular))
      ((equal? value "||") (set! bar-style 'light-light))
      ((equal? value ".|:") (set! repeat-direction 'forward)
                  (set! bar-style 'heavy-light))
      ((equal? value ":|.") (set! repeat-direction 'backward)
                  (set! bar-style 'light-heavy))
      ((equal? value ".[|:")  (set! repeat-direction 'forward)
                  (set! bar-style 'heavy-light)
                  (set! repeat-winged 'curved))
      ((equal? value ":|].") (set! repeat-direction 'backward)
                  (set! bar-style 'light-heavy)
                  (set! repeat-winged 'curved)))

    (cond ((not (null? bar-style))
        (xml:appendChild bar-style-element
            (xml:createTextNode doc bar-style))
        (xml:appendChild barline-element bar-style-element)))

    (cond ((not (null? repeat-direction))
        (cond ((not (null? repeat-winged))
            (xml:setAttribute
                repeat-element 'winged repeat-winged)))
        (xml:setAttribute
            repeat-element 'direction repeat-direction)
        (xml:appendChild barline-element repeat-element)))

    barline-element)))

(define add-note-attributes (lambda (item  music doc)
  (assert (music-item? item)
          (ly:music? music)
          (xml:document? doc)
          report: '@add-note-attributes)
  (let ((name (music-item:name item)))
    (cond ((equal? name 'MultiMeasureRestMusic)
        (add-part-context-element item (make-xml-item doc 'measure-style
          (make-xml-item doc 'multiple-rest
               (car (ly:duration-factor (ly:music-property music 'duration))))
                  doc )))))))

(define pre-process-context-specced-music
      (lambda (name music context status parent item doc)
  (assert (symbol? name)
          (ly:music? music)
          (music-item? context)
          (translation-status? status)
          (music-item? parent)
          (music-item? item)
          (xml:document? doc)
          report: '@pre-process-context-specced-music)
  (let* ((part-name (ly:music-property music 'context-id))
        (ctype (ly:music-property music 'context-type))
        (parent-context (music-item:get-attribute parent 'context-type))
        (create-new (ly:music-property music 'create-new))
        (new-context (and (not (null? create-new)) create-new)))
      (music-item:set-attribute item 'context-type ctype)
      (cond (equal? (music-item:name (music-item:parent item)) 'SimultaneousMusic)
        (music-item:set-attribute item 'parent-context parent-context))
      (cond ((not (null? part-name))
        (translation-status:add-id status item part-name)))
      (cond (new-context
        (let* ((attributes-items (get-part-context-elements-copy item)))
          (music-item:set-part-context! item attributes-items)
          (music-item:set-attribute item 'context-type ctype)
        )
        (case ctype
          ((ChoirStaff StaffGroup PianoStaff
            Staff DrumStaff TabStaff RhytmicStaff Lyrics)
            (music-information:set-staff-nr! status
                (+ (music-information:staff-nr status) 1))
            (music-information:set-voice-nr! status 1)
            (music-information:set-max-voice-nr! status 1))
          ((Voice CueVoice NullVoice)
            (music-information:set-voice-nr! status
                (+ (music-information:max-voice-nr status) 1))
            (music-information:set-max-voice-nr! status
                (music-information:voice-nr status))
            (case ctype
              ((CueVoice) (music-information:set-cue-voice status
                  (music-information:staff-nr status)
                  (music-information:voice-nr status)))
              ((NullVoice) (music-information:set-null-voice status
                (music-information:staff-nr status)
                (music-information:voice-nr status))))))))
      ; must be done after (possible) change of staff and voice-nr,
      (let ((context-spec (translation-status:context-tree status)))
        (translation-status:set-context-tree! status
            (make-music-tree (music-information:staff-nr status)
                ctype context-spec))))))

(define pre-process-event-chord
      (lambda (name music context status parent item doc)
  (assert (symbol? name)
          (ly:music? music)
          (music-item? context)
          (translation-status? status)
          (music-item? parent)
          (music-item? item)
          (xml:document? doc)
          report: '@pre-process-context-event-chord)
  (translation-status:add-branch-to-voice-tree status name)
  (translation-status:set-chord-item! status #t)
  (translation-status:set-chord-start-ticks! status
      (music-information:ticks status))))

(define pre-process-line-break-event
      (lambda (name music context status parent item doc)
  (assert (symbol? name)
          (ly:music? music)
          (music-item? context)
          (translation-status? status)
          (music-item? parent)
          (music-item? item)
          (xml:document? doc)
          report: '@pre-process-line-break-event)
  (let* ((print-element (xml:createElement doc 'print)))
    (xml:setAttribute print-element 'new-system 'yes)
    (music-information:insert-xml-element status print-element))))

(define pre-process-key-change-event
      (lambda (name music context status parent item doc)
  (assert (symbol? name)
          (ly:music? music)
          (music-item? context)
          (translation-status? status)
          (music-item? parent)
          (music-item? item)
          (xml:document? doc)
          report: '@pre-process-key-change-event)
  (let* ((att-item (xml:createElement doc 'key)))
    (music-key->xml-item music att-item doc)
    (add-part-context-element item att-item doc))))

(define pre-process-page-break-event
      (lambda (name music context status parent item doc)
  (assert (symbol? name)
          (ly:music? music)
          (music-item? context)
          (translation-status? status)
          (music-item? parent)
          (music-item? item)
          (xml:document? doc)
          report: '@pre-process-page-break-event)
  (let* ((print-element (xml:createElement doc 'print)))
    (xml:setAttribute print-element 'new-page 'yes)
    (music-information:insert-xml-element status print-element))))

(define pre-process-partial-set
      (lambda (name music context status parent item doc)
  (assert (symbol? name)
          (ly:music? music)
          (music-item? context)
          (translation-status? status)
          (music-item? parent)
          (music-item? item)
          (xml:document? doc)
          report: '@pre-process-partial-set)
  (let* ( (p (music-item:music parent))
    (d (ly:music-property music 'duration))
    (duration (car (ly:duration-factor d)))
    (div (/ (expt 2(ly:duration-log d)) (cdr (ly:duration-factor d))))
    (duration-value (/ (* default-measure-ticks duration) div )))
  (add-part-context-element item
      (make-xml-item doc 'partial duration-value) doc))))

(define pre-process-properties
      (lambda (name music context status parent item doc)
  (assert (symbol? name)
          (ly:music? music)
          (music-item? context)
          (translation-status? status)
          (music-item? parent)
          (music-item? item)
          (xml:document? doc)
          report: '@pre-process-properties)
  (let* ((symbol  (ly:music-property music 'symbol))
          (grob-value   (ly:music-property music 'grob-value))
          (value  (ly:music-property music 'value))
          (once    (ly:music-property music 'once)))
  (cond ((not (null? grob-value))
      (music-item:set-attribute item 'gvalue grob-value )))
  (cond ((not (null? value)) (music-item:set-attribute item 'value value )))
  (cond
      ;; currently only very limited properties can be set
      ;; no support is yet for revert unset and once.
      ;; so let us skip those for now
      ((not (null? once)))
      ((equal? name 'PropertyUnset))
      ((equal? name 'RevertProperty))
      ((not (null? symbol))
        (music-item:set-attribute item 'property symbol)
        (case symbol

          ((clefGlyph)
            (let ((clef (make-xml-item doc 'clef
                (make-xml-item doc 'sign (assoc-get value clef-list 'G)))))
              (add-part-context-element item clef doc)))
          ((clefPosition)
              ; in musicxml clef position is defined relative to
              ; bottom line, and by line, not by half line
              (let ((attribute (get-part-context-element item 'clef)))
                (cond ((xml:element? attribute)
                ; there should only be one line element
                  (let ((items (xml:getElementsByTagName attribute 'line)))
                    (if (null? items)
                      (xml:appendChild attribute
                          (make-xml-item doc 'line (+ (/ value 2) 3)))
                          '()
                          ))))))
          ((clefTransposition)
              (let ((attribute (get-part-context-element item 'clef)))
                (cond ((xml:element? attribute)
                  (cond ((not (equal? value 0))
                      (xml:appendChild attribute
                          (make-xml-item doc 'clef-octave-change (/ value 7)))))))))
          ((whichBar)
            (case (translation-status:current-context-spec status)
              ((Timing)
                (music-information:insert-xml-element status
                  (bar->musicxml value doc)))))
          ;;all known  not yet handled property settings
          ((alternativeNumberingStyle clefTranspositionStyle fontSize
          graceSettings instrumentName middleCClefPosition midiInstrument
          midiMinimumVolume midiMaximumVolume shortInstrumentName stanza
          tempoWholesPerMinute AccidentalSuggestion DotColumn Dots
          DynamicText DynamicLineSpanner Fingering Hairpin LaissezVibrerTie
          LigatureBracket LyricText MultiMeasureRest NoteColumn PhrasingSlur
          RepeatTie Rest Script Slur Stem TextScript Tie TimeSignature
          TupletBracket TrillSpanner ) '())
          (else
          ;; todo: remove when stable.
          ;; If you see this is printed in your output, please report it to:
          ;; lilypond@de-wolff.org
            (cond ((not (null? symbol))
              (print "\nunhandled symbol: ")
              (print symbol))))
        ))))))

(define pre-process-note-event
      (lambda (name music context status parent item doc)
  (assert (symbol? name)
          (ly:music? music)
          (music-item? context)
          (translation-status? status)
          (music-item? parent)
          (music-item? item)
          (xml:document? doc)
          report: '@pre-process-note-event)
  (let * ((parentname (if (null? parent) '()(music-item:name parent))))
    (case name
      ((NoteEvent BassFigureEvent LyricEvent)
        (translation-status:add-branch-to-voice-tree status name)
        (cond ((equal? parentname 'EventChord)
            (music-information:set-ticks! status
                (translation-status:chord-start-ticks status)))))
    )
    (note->xml-item music context status item doc))))

(define pre-process-sequential-music
      (lambda (name music context status parent item doc)
  (assert (symbol? name)
          (ly:music? music)
          (music-item? context)
          (translation-status? status)
          (music-item? parent)
          (music-item? item)
          (xml:document? doc)
          report: '@pre-process-sequential-music)
  (let* ((context-type
      (music-item:get-attribute
          (music-item:context item) 'context-type)))
    (translation-status:add-branch-to-voice-tree status name)
    (case context-type
      ((CueVoice) (music-information:set-cue-voice status
          (music-information:staff-nr status)
          (music-information:voice-nr status)))
      ((NullVoice)
        (music-information:set-null-voice status
            (music-information:staff-nr status)
            (music-information:voice-nr status)))))))

(define pre-process-simultaneous-music
      (lambda (name music context status parent item doc)
  (assert (symbol? name)
          (ly:music? music)
          (music-item? context)
          (translation-status? status)
          (music-item? parent)
          (music-item? item)
          (xml:document? doc)
          report: '@pre-process-simultaneous-music)
  (let ((branch (translation-status:voice-tree status)))
    (music-item:set-attributes! item (music-item:attributes context))
    (cond ((equal? (music-tree:current-event branch) name)
      (music-information:set-ticks! status (music-tree:ticks branch)))
      (else
        ; increment voice-nr, as sequential music will raise it
        (music-information:set-voice-nr! status
            (- (music-information:voice-nr status) 1))))
    (translation-status:set-voice-tree! status
        (make-music-tree (music-information:voice-nr status)
          name branch (music-information:ticks status))))))

(define pre-process-tempo-change-event
      (lambda (name music context status parent item doc)
  (assert (symbol? name)
          (ly:music? music)
          (music-item? context)
          (translation-status? status)
          (music-item? parent)
          (music-item? item)
          (xml:document? doc)
          report: '@pre-process-tempo-change-event)
  (let* ((text (ly:music-property music 'text))
      (metronome-count (ly:music-property music 'metronome-count))
      (tempo-unit (ly:music-property music 'tempo-unit))) ; duration
      (if (and metronome-count tempo-unit)
        (let* (
              (metronome-note-name (music-duration->notename tempo-unit))
              (metronome-note-dots (music-duration->dots tempo-unit))
              (direction-element (xml:createElement doc 'direction))
              (direction-type-element (xml:createElement doc 'direction-type))
              (voice-element (xml:createElement doc 'voice))
              (metronome-element (xml:createElement doc 'metronome))
              (beat-unit-element (xml:createElement doc 'beat-unit))
              (per-minute-element (xml:createElement doc 'per-minute))
              (metronome-note-text  (xml:createTextNode doc metronome-note-name))
              (per-minute-text (xml:createTextNode doc metronome-count))
              )
          (xml:appendChild beat-unit-element metronome-note-text)
          (xml:appendChild per-minute-element per-minute-text)
          (xml:appendChild metronome-element beat-unit-element)
          (xml:appendChild metronome-element per-minute-element)
          (xml:appendChild direction-type-element metronome-element)
          (xml:appendChild direction-element direction-type-element)
          (music-information:insert-xml-element status direction-element)

          (music-item:set-attribute item 'text text)
          (music-item:set-attribute item 'metronome-count metronome-count)
          (music-item:set-attribute item 'tempo-unit tempo-unit))
          '()
          ))))

(define pre-process-time-signature-music
      (lambda (name music context status parent item doc)
  (assert (symbol? name)
          (ly:music? music)
          (music-item? context)
          (translation-status? status)
          (music-item? parent)
          (music-item? item)
          (xml:document? doc)
          report: '@pre-process-time-signature-music)
  (let* ((att-item (xml:createElement doc 'time)))
    (music-time->xml-item music att-item doc)
    (add-part-context-element item att-item doc))))


(define pre-process-volta-repeated-music
      (lambda (name music context status parent item doc)
  (assert (symbol? name)
          (ly:music? music)
          (music-item? context)
          (translation-status? status)
          (music-item? parent)
          (music-item? item)
          (xml:document? doc)
          report: '@pre-process-volta-repeated-music)
  (let ((barline-element (xml:createElement doc 'barline))
      (repeat-element (xml:createElement doc 'repeat)))
      (xml:setAttribute repeat-element 'direction 'forward)
      (xml:setAttribute barline-element 'location 'middle)
      (xml:appendChild barline-element repeat-element)
    (music-information:insert-xml-element status barline-element))))

(define post-process-bass-figure-event
      (lambda (name music context status parent item doc)
  (assert (symbol? name)
          (ly:music? music)
          (music-item? context)
          (translation-status? status)
          (music-item? parent)
          (music-item? item)
          (xml:document? doc)
          report: '@post-process-bass-figure-event)
  (translation-status:remove-branch-from-voice-tree status)))

(define post-process-context-specced-music
      (lambda (name music context status parent item doc)
  (assert (symbol? name)
          (ly:music? music)
          (music-item? context)
          (translation-status? status)
          (music-item? parent)
          (music-item? item)
          (xml:document? doc)
          report: '@post-process-context-specced-music)
  (translation-status:set-context-tree! status
      (music-tree:previous (translation-status:context-tree status)))))

(define post-process-event-chord
      (lambda (name music context status parent item doc)
  (assert (symbol? name)
          (ly:music? music)
          (music-item? context)
          (translation-status? status)
          (music-item? parent)
          (music-item? item)
          (xml:document? doc)
          report: '@post-process-event-chord)
  (letrec ((ticks 0)
        (position (music-information:ticks status))
        (first-item (car (music-item:children item)))
        (other-notes (cdr (music-item:children item)))
        (insert-chord-elements (lambda (lst position doc)
            (cond ((null? lst)'())
                ((not (pair? lst)) '())
                (else
                  (let* ((item (car lst))
                        (chord (xml:createElement doc 'chord))
                        (note (if (null? item)
                            '()
                            (music-item:xml-content item))))
                    (music-item:set-attribute item 'stop-position position)
                    (cond ((xml:element? note) (xml:appendChild note chord))))
                    (insert-chord-elements (cdr lst) position doc))))))
    (translation-status:remove-branch-from-voice-tree status)
    (translation-status:set-chord-item! status #f)
    (let* ((first-note (music-item:xml-content first-item)))
      (cond ((xml:element? first-note)
          (let ((ticks (get-item-duration first-note)))
            (music-information:append-ticks status ticks)
            (set! position (music-information:ticks status))
            (music-item:set-attribute item 'stop-position position)
            (music-item:set-attribute first-item 'stop-position position)
            (insert-chord-elements other-notes position doc))))))))

(define post-process-lyric-event
      (lambda (name music context status parent item doc)
  (assert (symbol? name)
          (ly:music? music)
          (music-item? context)
          (translation-status? status)
          (music-item? parent)
          (music-item? item)
          (xml:document? doc)
          report: '@post-process-lyric-event)
  (let* ((lyric (music-item:xml-content item))
      (syllabic (cond ((xml:element? lyric)
          (xml:get-child-by-name lyric 'syllabic))(else '())))
      (syllabic-status (translation-status:syllabic-status status)))
    (translation-status:remove-branch-from-voice-tree status)
    (if (xml:element? lyric)
      ;; adapt syllabic status,
      ;; as syllabic single, middle and end are not in events
      (if syllabic-status
        (if (null? syllabic)
          (begin
              (xml:appendChild lyric (make-xml-item doc 'syllabic 'end))
              (translation-status:set-syllabic-status! status #f))
          (let ((new-item (make-xml-item doc 'syllabic 'middle)))
            (xml:replaceChild lyric new-item syllabic)))
        (begin
          (if (null? syllabic)
            (xml:appendChild lyric (make-xml-item doc 'syllabic 'single))
            (translation-status:set-syllabic-status! status #t))))
      '() ))))

(define post-process-note-event
      (lambda (name music context status parent item doc)
  (assert (symbol? name)
          (ly:music? music)
          (music-item? context)
          (translation-status? status)
          (music-item? parent)
          (music-item? item)
          (xml:document? doc)
          report: '@post-process-note-event)
  (translation-status:remove-branch-from-voice-tree status)
  (let* ((note (music-item:xml-content item))
      (notations (cond ((xml:element? note)
          (xml:get-child-by-name note 'notations))(else '())))
      (tie-element (cond ((xml:element? notations)
          (xml:get-child-by-name notations 'tied)) (else '())))
      (tie-type (cond ((xml:element? tie-element)
          (xml:getAttribute tie-element 'type)) (else '())))
      (tie-status (translation-status:tie-status status))
      (beam-element (cond ((xml:element? note)
          (xml:get-child-by-name note 'beam)) (else '())))
      (beam-type (cond ((xml:element? beam-element)
          (xml:getAttribute beam-element 'type)) (else '())))
      (beam-status (translation-status:beam-status status)))
    ;; adapt tie status, as tie continue and tie stop are not in events
    ;; todo: resolve chord ties
    (if tie-status
      (if (null? tie-type)
        (begin
          (cond ((not (xml:element? notations))
              (set! notations (xml:appendChild note
                  (xml:createElement doc 'notations)))))
            (xml:appendChild notations (make-xml-item doc 'tied
                (acons 'type 'stop '())))
            (translation-status:set-tie-status! status #f)
            )
          (xml:setAttribute tie-element 'type 'continue))
      (cond ((not (null? tie-type))
        (translation-status:set-tie-status! status #t)))
    )
    (if beam-status
      (if (null? beam-type)
          (cond ((xml:element? note)
            (xml:appendChild note (make-xml-item doc 'beam
                (acons 'type 'continue '())))))
        (if (equal? beam-type 'stop)
          (translation-status:set-beam-status! status #f)
          '()))
      (if (equal? beam-type 'start)
        (translation-status:set-beam-status! status #t)
        '() )))))

(define post-process-sequential-music
      (lambda (name music context status parent item doc)
  (assert (symbol? name)
          (ly:music? music)
          (music-item? context)
          (translation-status? status)
          (music-item? parent)
          (music-item? item)
          (xml:document? doc)
          report: '@post-process-sequential-music)
        (translation-status:remove-branch-from-voice-tree status)))

(define post-process-simultaneous-music
      (lambda (name music context status parent item doc)
  (assert (symbol? name)
          (ly:music? music)
          (music-item? context)
          (translation-status? status)
          (music-item? parent)
          (music-item? item)
          (xml:document? doc)
          report: '@post-process-simultaneous-music)
        (let ((ticks (music-tree:max-ticks (translation-status:voice-tree status)))
            (branch (music-tree:previous (translation-status:voice-tree status))))
          (translation-status:set-voice-tree! status branch)
          (music-information:set-voice-nr! status (music-tree:item-nr branch))
          (music-information:set-ticks! status ticks))))


(define post-process-time-scaled-music
      (lambda (name music context status parent item doc)
  (assert (symbol? name)
          (ly:music? music)
          (music-item? context)
          (translation-status? status)
          (music-item? parent)
          (music-item? item)
          (xml:document? doc)
          report: '@post-process-time-scaled-music)
        (letrec ((normal-notes (ly:music-property music 'numerator))
            (actual-notes (ly:music-property music 'denominator))
            (insert-time-modifications (lambda (actual normal lst)
                (for-each (lambda (node)
                    (let ((xml-node (music-item:xml-content node))
                        (actual-element (make-xml-item doc 'actual-notes actual))
                        (normal-element (make-xml-item doc 'normal-notes normal))
                        (time-element (xml:createElement doc 'time-modification)))
                      (if (xml:element? xml-node)
                        (case (music-item:name node)
                          ((BassFigureEvent LyricEvent MultiMeasureRestMusic
                               NoteEvent RestEvent SkipEvent)
                            (xml:appendChild time-element actual-element)
                            (xml:appendChild time-element normal-element)
                            (xml:appendChild xml-node time-element)))
                        (insert-time-modifications actual normal
                            (music-item:children node)))))
              lst))))
          (insert-time-modifications actual-notes normal-notes
              (music-item:children item)))))

(define post-process-volta-repeated-music
      (lambda (name music context status parent item doc)
  (assert (symbol? name)
          (ly:music? music)
          (music-item? context)
          (translation-status? status)
          (music-item? parent)
          (music-item? item)
          (xml:document? doc)
          report: '@post-process-volta-repeated-music)
  (letrec ((repeat-count (ly:music-property music 'repeat-count))
      (items (length (music-item:children item)))
      (set-endings (lambda (item count first)
        (let* ((begin-barline-element (xml:createElement doc 'barline))
          (end-barline-element (xml:createElement doc 'barline))
          (begin-ending-element (xml:createElement doc 'ending))
          (end-ending-element (xml:createElement doc 'ending))
          (repeat-element (xml:createElement doc 'repeat))
          (start-position (music-item:get-attribute item 'start-position))
          (stop-position (music-item:get-attribute item 'stop-position))
          (ending-number count))
          ; barline location attribute will be changes later,
          ; if they are at a measure boundry
        (xml:appendChild begin-barline-element begin-ending-element)
        (xml:setAttribute begin-barline-element 'location 'middle)
        (xml:setAttribute begin-ending-element 'number ending-number)
        (xml:setAttribute begin-ending-element 'type 'start)
        (music-information:insert-xml-element@
              status begin-barline-element start-position)
        (xml:appendChild end-barline-element end-ending-element)
        (xml:setAttribute repeat-element 'direction 'backward)
        (cond (first
          (xml:appendChild end-barline-element repeat-element)
          (if (equal? count 1)
            '()
            (xml:appendChild begin-ending-element
                 (xml:createTextNode doc (format "1, ~a" count)))
          )))
        (xml:setAttribute end-barline-element 'location 'middle)
        (xml:setAttribute end-ending-element 'number ending-number)
        (xml:setAttribute end-ending-element 'type 'stop)
        (music-information:insert-xml-element@
              status end-barline-element stop-position)
        )))
      (create-endings (lambda (count lst)
          (cond ((null? lst) '())
            ((not (pair? lst))
              (error "invalid list"))
            ((not (music-item? (car lst)))
              (error "invalid list item"))
            ((null? (cdr lst))
              ; last ending of the reverse list is first ending
              (set-endings (car lst) count #t))
            (else
              (set-endings (car lst) count #f)
              (create-endings (1- count) (cdr lst)))))))
    (music-item:set-attribute item 'repeat-count repeat-count)
    (music-item:set-attribute item 'items items)
    (cond
      ((equal? repeat-count 0)
        ;invalid situation
        (warning "can not handle repeat with volta 0"))
      ((equal? items 1)
        (let* ((barline-element (xml:createElement doc 'barline))
            (repeat-element (xml:createElement doc 'repeat)))
            ; situation without endings
            (xml:setAttribute repeat-element 'direction 'backward)
            (xml:setAttribute repeat-element 'times repeat-count)
            (xml:appendChild barline-element repeat-element)
            (music-information:insert-xml-element status barline-element)))
      ((<= items (1+ repeat-count))
          ; no ending in for first part,
          ; handle endings in reverse order, for easy of counting
         (create-endings repeat-count
              (reverse (cdr (music-item:children item)))))
      (else
        ;invalid situation
        (warning "can not handle repeat with more endings then volta"))))))


(define music-children->music-item (lambda (music context status parent)
  (assert (ly:music? music)
          (music-item? context)
          (translation-status? status)
          (music-item? parent)
          report: '@music-children->music-item)
  (let* ((e (ly:music-property music 'element))
      (es (ly:music-property music 'elements)))
   (cond ((ly:music? e) (music->music-item e context status parent )))
   (cond ((pair? es)
    (for-each (lambda (n) (music->music-item n context status parent)) es))))))

(define note->xml-item (lambda (music context status item doc)
  (assert (ly:music? music)
          (music-item? context)
          (translation-status? status)
          (music-item? item)
          (xml:document? doc)
          report: '@note->xml-item)
  (let* ((element-names
        '((BassFigureEvent . figured-bass)
        (LyricEvent . lyric)
        (MultiMeasureRestMusic . note)
        (NoteEvent . note)
        (RestEvent . note)
        (SkipEvent . note))
      )
    (lilyname (ly:music-property music 'name))
    (name (assoc-get lilyname element-names))
    (element (xml:createElement doc name))
    (arts (ly:music-property music 'articulations))
    (text (ly:music-property music 'text))
    (attributes (extract-part-context-element item doc))
    (direction-item (xml:createElement doc 'direction))
    (p (ly:music-property music 'pitch))
        (d (ly:music-property music 'duration))
    )
      (music-item:set-xml-content! item element)
      (cond ((ly:duration? d) (music-duration->xml-items d doc element)))
      (cond ((ly:pitch? p) (music-pitch->xml-item p element doc)))
      (cond ((not (null? text))
          (xml:appendChild  element (make-xml-item doc 'text text))))
      (cond ((not (null? arts))
          (for-each (lambda (n)
            (articulations->xml-item n context element doc)
            (direction-articulations->xml-item n context direction-item doc))
          arts)))
      (if (or (equal? lilyname 'RestEvent)
              (equal? lilyname 'SkipEvent)
              (equal? lilyname 'MultiMeasureRestMusic))
        (xml:appendChild element (xml:createElement doc 'rest))'())


      (cond ((and (xml:element? attributes) (xml:hasChildNodes  attributes))
        (music-information:insert-xml-element status attributes)))
      (cond ((and (xml:element? direction-item)
                  (xml:hasChildNodes  direction-item))
        (music-information:insert-xml-element status direction-item)))
      (cond ((and (xml:element? element) (xml:hasChildNodes  element))
        (music-information:insert-xml-element status element)))
      (music-information:collect-time-signatures status attributes element
        ; time info from lyrics is not related to music time
        ; and chord notes should not influence timing
            (or (equal? lilyname 'LyricEvent) (translation-status:chord-item status))))))

(define pre-process-table
      `((ContextSpeccedMusic . , pre-process-context-specced-music)
      (EventChord . , pre-process-event-chord)
      (BassFigureEvent  . , pre-process-note-event)
      (LyricEvent  . , pre-process-note-event)
      (MultiMeasureRestMusic . , pre-process-note-event)
      (NoteEvent  . , pre-process-note-event)
      (RestEvent  . , pre-process-note-event)
      (SkipEvent . , pre-process-note-event)
      (LineBreakEvent . , pre-process-line-break-event)
      (KeyChangeEvent . , pre-process-key-change-event)
      (PageBreakEvent . , pre-process-page-break-event)
      (PartialSet . , pre-process-partial-set)
      (OverrideProperty  . , pre-process-properties)
      (PropertySet  . , pre-process-properties)
      (PropertyUnset  . , pre-process-properties)
      (RevertProperty . , pre-process-properties)
      (SequentialMusic . , pre-process-sequential-music)
      (SimultaneousMusic . , pre-process-simultaneous-music)
      (TempoChangeEvent . , pre-process-tempo-change-event)
      (TimeSignatureMusic . , pre-process-time-signature-music)
      (VoltaRepeatedMusic . , pre-process-volta-repeated-music)))

(define post-process-table
      `((BassFigureEvent .  , post-process-bass-figure-event)
      (ContextSpeccedMusic .  , post-process-context-specced-music)
      (EventChord .  , post-process-event-chord)
      (LyricEvent . , post-process-lyric-event)
      (NoteEvent .  , post-process-note-event)
      (SequentialMusic .  , post-process-sequential-music)
      (SimultaneousMusic .  , post-process-simultaneous-music)
      (TimeScaledMusic .  , post-process-time-scaled-music)
      (VoltaRepeatedMusic .  , post-process-volta-repeated-music)))

(define music->music-item (lambda (music context status parent)
  (assert (ly:music? music)
          (music-item? context)
          (translation-status? status)
          (music-item? parent)
          report: '@music->music-item)
  (let* ((name (ly:music-property music 'name))
      (item (make-music-item name music context parent))
      (associated-context (ly:music-property music 'associated-context))
      (childs-repeat-count 1)
      (doc (music-information:document status))
      (create-new (ly:music-property music 'create-new))
      (new-context (and (not (null? create-new)) create-new))
    )
    (cond ((not (null? associated-context))
      (add-part-context-element item
          (make-xml-item doc 'associated-context associated-context) doc)
    ))
    (music-item:set-attribute item 'start-position
        (music-information:ticks status))
    (case name
      ((GraceMusic) ; do not use grace notes for now
        (set! childs-repeat-count 0)
      )
      ((UnfoldedRepeatedMusic)
        (set! childs-repeat-count (ly:music-property music 'repeat-count))
        )
    )
    (let ((pre-process (assoc-get name pre-process-table)))
      (cond (pre-process
          (pre-process name music context status parent item doc))))
    (cond (new-context
        (set! context item)))
    (do ((i 1 (1+ i))) ((> i childs-repeat-count))
      (music-children->music-item music context status item))
    (music-item:set-attribute item 'stop-position
        (music-information:ticks status))
    (let ((post-process (assoc-get name post-process-table)))
      (cond (post-process
          (post-process name music context status parent item doc))))
       item)))

;; -----------------------------------------------------------------------
;;      C o m b i n e    c o l l e c t e d   i n f o r m a t i o n
;; -----------------------------------------------------------------------

(define create-measures-for-part (lambda (status part doc)
  (assert (music-information? status)
          (xml:element? part)
          (xml:document? doc)
          report: '@create-measures-for-part)
  (let* ((result '())
      (position 0)
      (measure-nr 1)
      (max-pos (music-information:max status))
      (measure-ticks default-measure-ticks)
      (ts (assoc-get position (music-information:time-signatures status)))
    )
    (cond ((not (time-signature? ts))
      (let ((ts-containment
              (music-information:get-ts@-or-before status position)))
          (cond ((pair? ts-containment)
            (set! ts (cdr ts-containment))
                (cond ((time-signature? ts)
             (let ((measure (xml:createElement doc 'measure))
                (attributes (xml:createElement doc 'attributes))
                (start-pos (car ts-containment)))
              (set! measure-ticks(time-signature:get-measure-ticks ts))
              (xml:appendChild part measure)
              (xml:appendChild measure attributes)
              (xml:setAttribute measure 'number 0)
              (xml:setUserData measure 'measure-ticks
                   (+ start-pos measure-ticks))
              (set! result (assoc-set! result position measure ))
              (set! position  (+ start-pos measure-ticks))
              )
            )))
            (else print (format "\nno ts at ~a" position))
            ))))
    (cond ((time-signature? ts)
      (set! measure-ticks(time-signature:get-measure-ticks ts))))
    (while (< position max-pos)
      (let ((measure (xml:createElement doc 'measure))
          (attributes (xml:createElement doc 'attributes))
          (ts (assoc-get position (music-information:time-signatures status))))
        (cond ((time-signature? ts)
          (xml:appendChild attributes (time-signature->xml-item ts doc))
          (set! measure-ticks(time-signature:get-measure-ticks ts))))
        (xml:setAttribute measure 'number measure-nr)
        (xml:appendChild part measure)
        (xml:appendChild measure attributes)
        (xml:setUserData measure 'measure-ticks measure-ticks)
        (xml:setUserData measure 'position position)
        (set! result (assoc-set! result position measure))
        (set! position (+ measure-ticks position))
        (set! measure-nr (1+ measure-nr))))
    result)))

(define append-lyric-to-voice (lambda (lyric-list voice-list)
  (assert (list? lyric-list)
          (list? voice-list)
          report: '@append-lyric-to-voice)
  (letrec ((get-first-element (lambda (lst name)
          (cond ((or (null? lst) (not (pair? lst))
                (not (pair? (car lst)))(not (xml:element? (cdar lst))))'())
            ((equal? (xml:nodeName (cdar lst)) name) lst)
            (else (get-first-element (cdr lst) name)))))
      (get-next-element (lambda (lst name)
          (cond ((or (null? lst) (not (pair? lst))) '())
          (else (get-first-element (cdr lst) name)))))
      (get-first-lyric (lambda (lst) (get-first-element lst 'lyric)))
      (get-first-voice (lambda (lst)
        (let ((element (get-first-element lst 'note)))
          (cond ((null? element ) '())
          ((and (null? (xml:get-child-by-name (cdar element) 'rest))
            (null? (xml:get-child-by-name (cdar element) 'chord))) element)
          (else (get-first-voice (cdr lst)))))))
      (get-next-lyric (lambda (lst) (get-next-element lst 'lyric)))
      (get-next-voice (lambda (lst)
          (cond ((or (null? lst) (not (pair? lst))) '())
          (else (get-first-voice (cdr lst)))))))
    (let* ((lyric-container (get-first-lyric lyric-list))
        (voice-container (get-first-voice voice-list))
        (verse (if (pair? voice-container)
            (xml:getUserData (cdar voice-container) 'verse-nr)'())))
      (if (null? verse)(set! verse 1)(set! verse (1+ verse)))
      (if (pair? voice-container)
          (xml:setUserData (cdar voice-container) 'verse-nr verse)
          '())
      (while (and (pair? lyric-container) (pair? voice-container))
        (let* ((lyric-element (cdar lyric-container))
            (voice-element (cdar voice-container))
            (notation-element
                (xml:get-child-by-name voice-element 'notations))
            (slur-element
                (if (xml:element? notation-element)
                    (xml:get-child-by-name notation-element 'slur) '()))
            (slur-type
                (if (xml:element? slur-element)
                    (xml:getAttribute slur-element 'type) '()))
            (slur-number
                (if (xml:element? slur-element)
                    (xml:getAttribute slur-element 'number) '()))
            (tied-element
                (if (xml:element? notation-element)
                    (xml:get-child-by-name notation-element 'tied) '()))
            (tied-type
                (if (xml:element? tied-element)
                    (xml:getAttribute tied-element 'type) '())))
          (xml:setAttribute lyric-element 'number verse)
          (xml:appendChild voice-element lyric-element)
          ; now skip ties and slurs:
          (cond ((and (not (null? tied-type)) (equal? tied-type 'start))
              ;skip until tied stop
              (while (or (not (equal? tied-type 'stop))
                    (not (pair? voice-container)))
                (set! voice-container (get-next-voice voice-container))
                (if (pair? voice-container)
                  (begin
                    (set! voice-element (cdar voice-container))
                    (set! notation-element
                      (xml:get-child-by-name voice-element 'notations))
                    (set! tied-element
                      (if (xml:element? notation-element)
                        (xml:get-child-by-name notation-element 'tied)
                        '()))
                    (set! tied-type
                      (if (xml:element? tied-element)
                        (xml:getAttribute tied-element 'type)
                        '()))
                  )
                  '())))
            ((and (not (null? slur-type))
                  (not (null? slur-number))
                  (equal? slur-type 'start)
                  (equal? slur-number 1))
              ;skip until slur stop
              (while
                  (or (not
                        (and (equal? slur-type 'stop)
                            (equal? slur-number 1)))
                      (not (pair? voice-container)))
                (set! voice-container (get-next-voice voice-container))
                (if (pair? voice-container)
                  (begin
                    (set! voice-element (cdar voice-container))
                    (set! notation-element
                        (xml:get-child-by-name voice-element 'notations))
                    (set! slur-element
                      (if (xml:element? notation-element)
                        (xml:get-child-by-name notation-element 'slur)
                        '()))
                    (set! slur-type
                      (if (xml:element? slur-element)
                        (xml:getAttribute slur-element 'type)
                        '()))
                    (set! slur-number
                      (if (xml:element? slur-element)
                        (xml:getAttribute slur-element 'number)
                        '()))
                  )
                  '())))
          )
          (set! lyric-container (get-next-lyric lyric-container))
          (set! voice-container (get-next-voice voice-container))
        ))))))



(define merge-attributes (lambda (first second)
  (assert (xml:element? first)
          (xml:element? second)
          report: '@merge-attributes)
  (for-each (lambda (node)
    (let* ((node-name (xml:nodeName node)))
      (cond ((null? (xml:get-child-by-name first node-name))
        (xml:appendChild first node)))))
  (xml:childNodes second))))

; return pair (name remaining)
(define rest-for-ticks (lambda (size)
  (assert (integer? size)
          report: '@rest-for-ticks)
  (cond
    ((>= size ticks-per-whole)
      (cons 'whole (- size ticks-per-whole)))
    ((>= size (/ ticks-per-whole 2))
      (cons 'half (- size (/ ticks-per-whole 2))))
    ((>= size (/ ticks-per-whole 4))
      (cons 'quarter (- size (/ ticks-per-whole 4))))
    ((>= size (/ ticks-per-whole 8))
      (cons 'eighth (- size (/ ticks-per-whole 8))))
    ((>= size (/ ticks-per-whole 16))
        (cons '16th (- size (/ ticks-per-whole 16))))
    ((>= size (/ ticks-per-whole 32))
        (cons '32th (- size (/ ticks-per-whole 32))))
    ((>= size (/ ticks-per-whole 64))
        (cons '64th (- size (/ ticks-per-whole 64))))
    (else (cons  '() 0)))))

; return pair (dots remaining)
(define dots-for-rest (lambda (rest-size remaining-size)
  (assert (integer? rest-size)
          (integer? remaining-size)
          report: '@dots-for-rest)
  (let ((dot-size (/ rest-size 2))
        (dots 0))
  (while (remaining-size >= dot-size)
    (set! remaining-size (- remaining-size dot-size))
    (set! dot-size (/ dot-size 2))
    (set! dots (1+ dots))
    (if (< remaining-size 3) (break) '())
  )
  (cons dots remaining-size))))



(define insert-dummy-rest (lambda (size cur-voice-nr measure doc)
  (assert (integer? size)
          (integer? cur-voice-nr)
          (xml:element? measure)
          (xml:document? doc)
          report: '@insert-dummy-rest)
  (while (> size 0)
    (let* ((result  (rest-for-ticks size))
        (rest-size (- size (cdr result)))
        (rest-type (car result))
        (note-element (xml:createElement doc 'note)))
      (if (null? rest-type) (break)
      (begin
        (set! size (cdr result))
        (xml:appendChild note-element (xml:createElement doc 'rest))
        (xml:appendChild note-element (make-xml-item doc 'duration rest-size))
        (xml:appendChild note-element (make-xml-item doc 'type rest-type))
        (xml:appendChild note-element (make-xml-item doc 'voice cur-voice-nr))
        (xml:appendChild measure note-element)))))))

(define append-element-to-measure (lambda
      (element cur-voice-nr new-measure-for-voice measure status doc)
  (assert (xml:element? element)
          (integer? cur-voice-nr)
          (boolean? new-measure-for-voice)
          (xml:element? measure)
          (music-information? status)
          (xml:document? doc)
          report: '@append-element-to-measure)
  (let* ((attibutes (xml:get-child-by-name measure 'attributes))
      (measure-position (xml:getUserData measure 'position))
      (measure-ticks (xml:getUserData measure 'measure-ticks))
      (duration-element (xml:get-child-by-name element 'duration))
      (duration
        (if (xml:element? duration-element)
          (xml:element-value duration-element)
          0))
      (element-name (xml:nodeName element))
      (is-attributes-element (equal? (xml:nodeName element) 'attributes))
      (position (xml:getUserData element 'position))
      (repeat-element (xml:get-child-by-name element 'repeat))
      (rest-element (xml:get-child-by-name element 'rest))
      (staff-nr (xml:getUserData element 'staff-nr))
      (voice-nr (xml:getUserData element 'voice-nr))
      )
    ; always create backup element, remove the first later
    (cond (new-measure-for-voice
        (let* ((backup-element (xml:createElement doc 'backup))
            (duration-element (xml:createElement doc 'duration))
            (duration-text (xml:createTextNode doc measure-ticks)))
          (xml:appendChild duration-element duration-text)
          (xml:appendChild backup-element duration-element)
          (xml:appendChild measure backup-element)
          ; first element of voice not at start measure: insert rests
          (cond ((not (or (equal? position 0)
                          (equal? position measure-position)))
              (case (xml:nodeName element)
                ((note)
                  (insert-dummy-rest (- position measure-position)
                      cur-voice-nr measure doc)
                )))))))
    (if (null? measure-position)(set! measure-position 0)'())
    (if (null? duration)(set! duration 0)'())
    ;check for rests beyond barline. For now cutoff at barline
    (cond ((and (xml:element? rest-element)
                (> (+ duration position)(+ measure-position measure-ticks)))
        (let* ((new-duration (- (+ measure-position measure-ticks) position))
            (type-element (xml:get-child-by-name element 'type))
            (new-rest-info (rest-for-ticks new-duration))
            (replace-text (xml:createTextNode doc new-duration))
            (replace-type-text
              (xml:createTextNode doc (car new-rest-info))))
            (xml:createTextNode doc new-duration)

          (if (and (xml:element? duration-element)
              (xml:hasChildNodes duration-element))
            (xml:replaceChild duration-element replace-text
                (xml:firstChild duration-element))
            '())
          (if (and (xml:element? type-element)
                    (xml:hasChildNodes type-element))
            (xml:replaceChild type-element replace-type-text
                (xml:firstChild type-element))
            '()))))
    ;insert the element
    (cond
        ; handle barlines at the start of measure
        ((and (equal? (xml:nodeName element) 'barline)
              (equal? position measure-position))
          (let ((ending-element (xml:get-child-by-name element 'ending)))
          ; repeat backward will get the location attribute left
          ; and is placed at the end of the previous meeasure
          (cond ((and (not (null? (xml:previousSibling measure)))
                  (or
                    (and
                      (xml:element? repeat-element)
                        (equal?
                            (xml:getAttribute repeat-element 'direction)
                            'backward))
                    (and
                      (xml:element? ending-element)
                        (equal?
                            (xml:getAttribute ending-element 'type)
                            'stop))))
            (xml:setAttribute element 'location 'right)
            (xml:appendChild (xml:previousSibling measure) element))
          ; other barlines at the start of the measure will get the location
          ; attribute right
          ; and is placed as the first elements in measure
          (else
            (xml:setAttribute element 'location 'left)
            (xml:insertBefore measure element (xml:firstChild measure)))
          )))
        ((and (equal? (xml:nodeName element) 'barline)
              (equal? position (+ measure-position measure-ticks)))
          (xml:setAttribute element 'location 'right)
          (xml:appendChild measure element))
        (is-attributes-element
          (merge-attributes attibutes element))
        ((equal? (xml:nodeName element) 'barline)
          (xml:appendChild measure element))
        (else
          (case (xml:nodeName element)
            ; not all elements should have a voice attached
            ;; todo remove duplicate non-voice attached elements from measurement
            ((attributes backup bookmark grouping harmony link print sound )'())
          (else
            (xml:appendChild element (make-xml-item doc 'voice cur-voice-nr))))
            (xml:appendChild measure element))))))

(define allocate-elements-to-measures (lambda (status root doc)
  (assert (music-information? status)
          (xml:element? root)
          (xml:document? doc)
          report: '@allocate-elements-to-measures)
  (let ((last-valid-part-name #f))
    (for-each (lambda (staff-container)
      (let* ((staff (cdr staff-container))
          (staff-nr (car staff-container))
          (cur-voice-nr -1)
          (name (part-list:id staff)))
          (cond ((part-list:has-notes staff)
              (let* ((last-valid-voice-name #f)
                  (part (xml:createElement doc 'part))
                  (cur-measure '())
                  (prev-voice 1)
                  (measures (create-measures-for-part status part doc))
                  (get-measure (lambda (ticks lst)
                    (let* ((item (find (lambda (n) (<= (car n) ticks)) lst)))
                        (cond ((pair? item) (cdr item))
                          (else '())))))
                  )
                (xml:setUserData part 'staff-nr staff-nr)
                (cond (name (xml:setAttribute part 'id name))
                  (else
                    (cond (last-valid-part-name
                      (xml:setAttribute
                          part 'id last-valid-part-name)))))
                (set! last-valid-part-name #f)
                (set! cur-voice-nr 1)
                (xml:appendChild root part)
                (for-each (lambda (voice-container)
                    (let* ((voice (cdr voice-container))
                      (voice-id (voice-list:id voice)))
                    (cond ((null? (voice-list:collected-xml-nodes voice))
                          (cond (voice-id
                            (set! last-valid-voice-name voice-id)
                            (voice-list:set-id! voice #f))))
                        ((voice-list:figured-bass voice))
                        ((voice-list:cue-voice voice))
                        ((voice-list:null-voice voice))
                      (else
                        (cond ((not voice-id)
                           (voice-list:set-id! voice last-valid-voice-name)))
                          (for-each (lambda (element-container)
                            (let* ((element (cdr element-container))
                              (position (xml:getUserData element 'position))
                              (measure (get-measure position measures))
                              (new-measure-for-voice
                                  (or (not (equal? measure cur-measure))
                                      (not (equal? prev-voice voice-id)))))
;                            (cond (new-measure-for-voice
;                                (print (format "\nnew measure. cur-voice-nr: ~a staff-nr: ~a" cur-voice-nr staff-nr))
;                                (print element)
;                            ))
                            (append-element-to-measure
                                element cur-voice-nr new-measure-for-voice
                                measure status doc)
                            (set! cur-measure measure)
                            (set! prev-voice voice-id)
                          ))
                          (voice-list:collected-xml-nodes voice)
                        )
                        (set! cur-voice-nr (+ cur-voice-nr 1)))
                  )))
                  (part-list:voices staff)
                )
              )
            )
            ((part-list:has-lyrics staff)
              (for-each (lambda (lyric-container)
                  (cond ((and (pair? lyric-container)
                              (voice-list? (cdr lyric-container))
                              (pair? (voice-list:collected-xml-nodes
                                (cdr lyric-container)))
                                (pair? (car (voice-list:collected-xml-nodes
                                    (cdr lyric-container)))))
                      (let* ((lyric-list (voice-list:collected-xml-nodes
                                (cdr lyric-container)))
                          (attributes (cdar lyric-list))
                          (associated-element
                              (xml:get-child-by-name attributes
                                'associated-context))
                          (associated (xml:element-value associated-element))
                          (associated-voice
                            (music-information:get-voice status associated)))
                        ; we need associated-context,
                        ; else we cannot connect with lyric
                        (cond ((not (null? associated-voice))
                          (append-lyric-to-voice (cdr lyric-list)
                          (voice-list:collected-xml-nodes associated-voice))
                      ))))))
                (part-list:voices staff))
            )
            (else (set! last-valid-part-name (part-list:id staff)))
          )
        )
      )
      (music-information:staves status)))))


;; -----------------------------------------------------------------------------
;;      P a r t s     t o    c l e a n    t h e   g e n e r a t e d   x m l
;; -----------------------------------------------------------------------------


(define remove-items-and-below (lambda (item  pname)
  (assert (xml:element? item)
          (symbol? pname)
          report: '@remove-items-and-below)
(begin
  (if (equal? (music-item:name item) pname)
    (xml:for-all-items  (lambda (n) (xml:set-name n #f)) item)
    (for-each
      (lambda (n) (remove-items-and-below n pname))
      (xml:element-children item))))))

;; for now: remove figured bass items
(define handle-figured-bass (lambda (item)
  (assert (xml:element? item)
          report: '@handle-figured-bass)
  (remove-items-and-below item 'figured-bass)))

(define reduce-staff-context (lambda (info staff-context)
  (assert (music-information? info)
          (music-tree? staff-context)
          report: '@reduce-staff-context)
  (letrec ((top (make <music-tree>))
      (find-or-create-related-context
        (lambda (old new lst)
          (let ((item-nr (music-tree:item-nr old))
              (cur-event (music-tree:current-event old)))
          (cond ((null? lst) (make-music-tree-copy old new '()))
            ((and
              (equal? (music-tree:item-nr (car lst)) item-nr)
              (equal? (music-tree:current-event (car lst)) cur-event))
                (car lst))
            (else (find-or-create-related-context old new (cdr lst)))))))
      (reduce-context
        (lambda (new old lst)
          (case (music-tree:current-event old)
            ((StaffGroup ChoirStaff PianoStaff)
              (set! new (find-or-create-related-context old new
                      (music-tree:branches new))))
            ((Staff)
              (let* ((staff-nr (music-tree:item-nr old))
                  (staff (assoc-get staff-nr lst)))
                (if (and staff
                        (part-list:has-notes staff)
                        (not (equal? staff-nr (music-tree:item-nr new))))
                  (find-or-create-related-context old new
                      (music-tree:branches new))
                      '()))))
          (for-each (lambda (n) (reduce-context new n lst))
                (music-tree:branches old))))
      (remove-empty-staff-groups
          (lambda (lst)
            (cond ((null? lst) '())
            ((equal? (music-tree:current-event (car lst)) 'Staff)
              (cons (car lst) (remove-empty-staff-groups (cdr lst))))
            ((null? (music-tree:branches (car lst))) (cdr lst))
            (else
              (music-tree:set-branches! (car lst)
                  (remove-empty-staff-groups (music-tree:branches (car lst))))
              (cons (car lst) (remove-empty-staff-groups (cdr lst))))))))
  (reduce-context top staff-context (music-information:staves info))
  (music-tree:set-branches! top (remove-empty-staff-groups
      (music-tree:branches top)))
  top)))

(define create-staff-id (lambda (staff)
  (assert (integer? staff)
          report: '@create-staff-id)
  (string-append "id" (number->string  staff))))

(define create-header-score-part (lambda (header item doc)
  (assert (xml:element? header)
          (xml:element? item)
          (xml:document? doc)
          report: '@create-header-score-part)
  (if (equal? (xml:nodeName item) 'part )
    (let ((name (xml:getAttribute item 'id))
        (id (create-staff-id (xml:getUserData item 'staff-nr))))
        ;; todo try to extract name from voices
      (cond ((null? name) (set! name "")))
      (xml:setAttribute item 'id  id)
      (let ((score-part (xml:createElement doc  'score-part ))
          (part-element (xml:createElement doc  'part-name ))
          (part-name (xml:createTextNode doc  name)))
        (xml:setAttribute score-part 'id  id)
        (xml:appendChild score-part part-element)
        (xml:appendChild part-element part-name)
        (xml:appendChild header score-part))))))

(define update-header (lambda (item information staff-context doc)
  (assert (xml:element? item)
          (music-information? information)
          (music-tree? staff-context)
          (xml:document? doc)
          report: '@update-header)
  (letrec ((header-item (xml:get-child-by-name item 'part-list))
      (context (reduce-staff-context information staff-context))
      (first-staff-item-has-value (lambda (lst v)
        (cond ((null? lst) #f)
          ((not (equal?  (music-tree:current-event (car lst)) 'Staff)) #f)
          (else (equal? (music-tree:item-nr (car lst)) v)))))
      (last-staff-item-has-value (lambda (lst v)
        (cond ((null? lst) #f)
          ((not (equal?  (music-tree:current-event (car lst)) 'Staff)) #f)
          ((and (null? (cdr lst)) (equal? (music-tree:item-nr (car lst)) v)))
          (else (last-staff-item-has-value (cdr lst) v)))))
      (insert-group-start (lambda (staff-nr header-item doc lst)
        (cond ((null? lst) '())
          (else
            (let ((item (car lst)))
              (cond ((first-staff-item-has-value
                      (music-tree:branches item)  staff-nr)
                  (let ((element (xml:createElement doc 'part-group)))
                    (xml:setAttribute element 'type 'start)
                    (xml:setAttribute element 'number
                            (music-tree:item-nr item))
                    (case (music-tree:current-event item)
                      ((PianoStaff)
                        (xml:appendChild element
                              (make-xml-item doc 'group-symbol 'brace))
                        (xml:appendChild element

                              (make-xml-item doc 'group-barline 'yes)))
                      ((StaffGroup)
                        (xml:appendChild element
                              (make-xml-item doc 'group-symbol 'bracket)))
                      ((ChoirStaff)
                        (xml:appendChild element
                              (make-xml-item doc 'group-symbol 'bracket))
                        (xml:appendChild element
                              (make-xml-item doc 'group-barline 'no)))

                    )
                    (xml:appendChild header-item element)))
              (else
                (insert-group-start staff-nr header-item doc
                      (music-tree:branches item))))
            (insert-group-start staff-nr header-item doc (cdr lst)))))))
      (insert-group-stop (lambda (staff-nr header-item doc lst)
        (cond ((null? lst) '())
          (else
            (let ((item (car lst)))
              (cond ((last-staff-item-has-value
                        (music-tree:branches item) staff-nr)
                  (let ((element (xml:createElement doc 'part-group)))
                    (xml:setAttribute element 'type 'stop)
                    (xml:setAttribute element 'number
                          (music-tree:item-nr item))
                    (xml:appendChild header-item element)))
              (else
                (insert-group-stop staff-nr header-item doc
                      (music-tree:branches item))))
            (insert-group-stop staff-nr header-item doc (cdr lst))))))))
    (for-each (lambda (node)
      (let ((staff-nr (xml:getUserData node 'staff-nr))
        (context-lst (music-tree:branches context)))
        (insert-group-start staff-nr header-item doc context-lst)
        (create-header-score-part header-item node doc)
        (insert-group-stop staff-nr header-item doc context-lst)))
    (xml:xml:get-children-by-name item 'part)))))

(define has-measure-attribute (lambda (item attr)
  (assert (xml:element? item)
          (symbol? attr)
          report: '@has-measure-attribute)
  (let *((measure-attribute (xml:get-child-by-name item 'attributes)))
  (begin
    (cond ((not (xml:element? measure-attribute)) #f)
        ((xml:element? (xml:get-child-by-name measure-attribute attr)) #t)
        (else #f))))))

(define get-measure-attribute (lambda (item attr)
  (assert (xml:element? item)
          (symbol? attr)
          report: '@get-measure-attribute)
  (let *((measure-attribute (xml:get-child-by-name item 'attributes)))
    (if (xml:element measure-attribute)
      (xml:get-child-by-name measure-attribute attr)
      '()))))

(define remove-measure-attribute (lambda (item attr)
  (assert (xml:element? item)
          (symbol? attr)
          report: '@remove-measure-attribute)
  (let *((measure-attribute (xml:get-child-by-name item 'attributes)))
    (cond ((not (xml:element measure-attribute)) '())
      (let ((attribute (xml:get-child-by-name measure-attribute attr)))
        (if (xml:element attribute)
          (xml:soft-clear attribute)
          '()))))))

(define get-measure-time (lambda (item)
  (assert (xml:element? item)
          report: '@get-measure-time)
  (let ((measure-attribute (xml:get-child-by-name item 'attributes)))
    (if (xml:element? measure-attribute)
      (music-xml->ticks measure-attribute)
      default-measure-ticks))))

(define reposition-end-barlines (lambda (item)
  (assert (xml:element? item)
          report: '@reposition-end-barlines)
  ; make sure all bar-elements with attribute location
  ; is right are the last in the measure
  (for-each
    (lambda (node)
      (if (and (equal? (xml:getAttribute node 'location) 'right)
                (xml:node? (xml:nextSibling node)))
        (let ((parent (xml:parentNode node)))
          (xml:removeChild parent node)
          (xml:appendChild parent node))
        '()))
  (xml:xml:get-children-by-name item 'barline))))

;; remove first backup item, from each element
(define remove-first-backup (lambda (item)
  (assert (xml:element? item)
          report: '@remove-first-backup)
  (letrec ((lst (xml:childNodes item))
    (remove-first-backup-item (lambda (lst)
      (cond ((null? lst) #f)
        ((not (xml:element? (car lst))) #f)
        ((equal? (xml:nodeName (car lst)) 'backup)
          (xml:removeChild (xml:parentNode (car lst)) (car lst)) #t)
        (else
          (for-each (lambda (node)
            (if (xml:element? node)
              (remove-first-backup-item (xml:childNodes node))
              '()))
            (xml:childNodes (car lst)))
          (remove-first-backup-item (cdr lst)))))))
    (remove-first-backup-item lst))))

;; remove all empty items, that shouldn't be empty
(define remove-empty-items (lambda (item)
  (assert (xml:element? item)
          report: '@remove-empty-items)
  (letrec ((non-empty-list
    ;; order is important:
      ;; first remove empty direction-type
      ;; so parent direction becomes empty
      ;; a list of items that can be removed if empty
      ;; second arg means attributes should also be empty
    '(
      (attributes . , #t)
      (notations . , #f)
      (direction-type . , #f)
      (direction . , #f)
    ))
    (remove-named-empty-items (lambda (skip-name skip-attr item)
      (cond ((xml:element? item)
        (if (and (equal? (xml:nodeName item) skip-name)
            (not (or (xml:hasChildNodes item)
                  (and (xml:hasAttributes item) skip-attr))))
          (let ((parent (xml:parentNode item)))
            (xml:removeChild (xml:parentNode item) item)
          )
          '()
        )
        (for-each (lambda (n)
            (remove-named-empty-items skip-name skip-attr n))
            (xml:childNodes item))))
          )))
    (for-each (lambda (skip) (
          remove-named-empty-items (car skip) (cdr skip) item)) non-empty-list))))

;; order the content of items that require a fixed ordering
(define order-items (lambda (element doc)
; each list in item order start with an element (tag) name,
; followed by the children in the expected order
  (define item-order
  '(
    ( attributes  divisions key time staves part-symbol instruments
        clef staff-details transpose directive measure-style )
    (note cue grace chord pitch rest unpitched duration tie instrument
        footnote level voice type dot accidental time-modification stem
        notehead notehead-text staff beam notations lyric play)
    (lyric extent humming laughing elision syllabic text)
      ))
  (assert (xml:element? element)
          (xml:document? doc)
          report: '@order-items)
  (letrec ((order-items (lambda (element item-name doc item-lst)
      (cond ((xml:element? element)
        (for-each (lambda (n) (order-item n element doc item-name item-lst))
        (xml:childNodes element))))))
    (order-item (lambda (check-item parent-item doc item-name item-lst)
      (cond ((xml:element? check-item)
        (cond ((equal? (xml:nodeName check-item) item-name)
          (let ((new-item (xml:createElement doc item-name))
              (attributes (xml:attributes check-item)))
              (cond ((list? attributes)
                (for-each (lambda (attr)
                  (let* ((name (xml:nodeName attr))
                      (value (xml:nodeValue attr)))
                    (xml:setAttribute new-item name value)))
                attributes)))
            (for-each (lambda (test-name)
              (let ((elements
                      (xml:getElementsByTagName check-item test-name)))
                (for-each (lambda (node)
                  (cond ((equal? (xml:parentNode node) check-item)
                      (xml:appendChild new-item node))))
                  elements)
              ))
            item-lst)
            (xml:replaceChild parent-item new-item check-item)
            ))
        (else
          (for-each
              (lambda (n)
                (order-item n check-item doc item-name item-lst))
              (xml:childNodes check-item)))))))))
    (for-each (lambda (lst)
      (let* ((item-name (car lst))
          (item-list (cdr lst)))
          (order-items element item-name doc item-list)))
      item-order))))

;; todo: the functioncalls in this function should be moved to cleanuptree
;;       after those are implemented and tested
(define not-cleanuptree (lambda (item information doc)
  (assert (xml:element? item))
  (assert (music-information? information))
  (assert (xml:document? doc))
  (ly:progress  "handle figured-bass\n")
  (handle-figured-bass item)
  (ly:progress  "hide nullvoice\n")
  (handle-nullvoice item)))

(define cleanuptree (lambda (item information staff-context doc)
  (assert (xml:element? item)
          (music-tree? staff-context)
          (music-information? information)
          (xml:document? doc)
          report: '@cleanuptree)
  (ly:progress  "remove first backup elements\n")
  (remove-first-backup item)
  (ly:progress  "reposition end barlines\n")
  (reposition-end-barlines item)
  (ly:progress  "remove empty items\n")
  (remove-empty-items item)
  (ly:progress  "order items\n")
  (order-items item doc)
  (ly:progress  "update header\n")
  (update-header item information staff-context doc)
  item))

;; --------------------------------------------------------------------------
;;            P u t t i n g   i t   a l l   t o g e t h e r
;; --------------------------------------------------------------------------

(define create-default-encoding (lambda (doc)
  (assert (xml:document? doc)
          report: '@create-default-encoding)
  (let* ((encoding-element (xml:createElement doc 'encoding))
      (sw-element-1 (xml:createElement doc 'software))
      (sw-element-2 (xml:createElement doc 'software))
      (version (call-with-output-string
            (lambda (port) (display (ly:version) port))))
      (sw-text-1 (xml:createTextNode doc
        (string-append "Lilypond " version " musicxml encoder")))
      (sw-text-2
        (xml:createTextNode doc "written by Jaap de Wolff (info@jasoon.nl)"))
      (date-element (xml:createElement doc 'encoding-date))
      (date-text (xml:createTextNode doc
          (strftime "%Y-%m-%d" (localtime (current-time))))))
    (xml:appendChild sw-element-1 sw-text-1)
    (xml:appendChild sw-element-2 sw-text-2)
    (xml:appendChild date-element date-text)
    (xml:appendChild encoding-element sw-element-1)
    (xml:appendChild encoding-element sw-element-2)
    (xml:appendChild encoding-element date-element)
    encoding-element)))


(define markup->textNode (lambda (self modules markup)
  (assert (xml:document? self)
          (list? modules)
          report: '@markup->textNode)
  (cond ((or (string? markup) (integer? markup) (symbol? markup))
    (xml:createTextNode self markup))
  (else
    ;; todo: use markup instead of extract content
    (let ((content (markup->string markup modules)))
      (if (pair? content)
        (xml:createTextNode self (car content))
        (xml:createTextNode self "markup command, which we not (yet) can parse")
      ))))))

(define add-element-from-header-with-attribute-and-text
  (lambda (modules parent doc header-field element-name attribute-name)
  (assert (list? modules)
          (xml:element? parent)
          (xml:document? doc)
          (symbol? header-field)
          (symbol? element-name)
          (symbol? attribute-name)
          report: '@add-element-from-header-with-attribute-and-text)
  (let* ((lookup (ly:modules-lookup modules header-field))
      )

    (if lookup
      (let* ((element (xml:createElement doc element-name))
          (text-node (markup->textNode doc modules lookup )))
        (xml:setAttribute element attribute-name header-field)
        (xml:appendChild element text-node)
        (xml:appendChild parent element)) '()))))

(define add-element-from-header-with-text
  (lambda (modules parent doc header-field element-name)
  (assert (list? modules)
          (xml:element? parent)
          (xml:document? doc)
          (symbol? header-field)
          (symbol? element-name)
          report: '@add-element-from-header-with-text)
  (let* ((lookup (ly:modules-lookup modules header-field))
      )

    (if lookup
      (let* ((element (xml:createElement doc element-name))
          (text-node (markup->textNode doc modules lookup )))
        (xml:appendChild element text-node)
        (xml:appendChild parent element)) '()))))

(define create-doc-identification-elements (lambda (modules root doc)
  (assert (list? modules)
          (xml:element? root)
          (xml:document? doc)
          report: '@create-doc-identification-elements)
  (if (null? modules)
; no modules, we can not get information fromn it
    (let* ((identification-element (xml:createElement doc 'identification))
          (encoding-element (create-default-encoding doc)))
      (xml:appendChild identification-element encoding-element)
      (xml:appendChild root identification-element))
; get all possible lilypond header items
    (let* ((identification-element (xml:createElement doc 'identification))
        (work-element (xml:createElement doc 'work))
        (misc-element (xml:createElement doc 'miscellaneous))
        (encoding-element (create-default-encoding doc))
        )
      (add-element-from-header-with-attribute-and-text
          modules identification-element doc 'arranger 'creator 'type)
      (add-element-from-header-with-attribute-and-text
          modules identification-element doc 'composer 'creator 'type)
      (add-element-from-header-with-attribute-and-text
          modules identification-element doc 'dedication 'creator 'type)
      (add-element-from-header-with-attribute-and-text
          modules identification-element doc 'maintainer 'creator 'type)
      (add-element-from-header-with-attribute-and-text
          modules identification-element doc 'meter 'creator 'type)
      (add-element-from-header-with-attribute-and-text
          modules identification-element doc 'poet 'creator 'type)
      (add-element-from-header-with-text
          modules identification-element doc 'copyright 'rights)
      (add-element-from-header-with-text
          modules encoding-element doc 'enteredby 'encoder)
      (xml:appendChild identification-element encoding-element)
      (add-element-from-header-with-text
          modules identification-element doc 'source 'source)
      ;all not yet used fields goes to misc
      (add-element-from-header-with-attribute-and-text
          modules misc-element doc
          'breakbefore 'miscellaneous-field 'name)
      (add-element-from-header-with-attribute-and-text
          modules misc-element doc 'date 'miscellaneous-field 'name)
      (add-element-from-header-with-attribute-and-text
          modules misc-element doc 'footer 'miscellaneous-field 'name)
      (add-element-from-header-with-attribute-and-text
          modules misc-element doc 'instrument 'miscellaneous-field 'name)
      (add-element-from-header-with-attribute-and-text
          modules misc-element doc
          'lastupdated 'miscellaneous-field 'name)
      (add-element-from-header-with-attribute-and-text
          modules misc-element doc
          'maintainerEmail 'miscellaneous-field 'name)
      (add-element-from-header-with-attribute-and-text
          modules misc-element doc
          'maintainerWeb 'miscellaneous-field 'name)
      (add-element-from-header-with-attribute-and-text
          modules misc-element doc
          'moreInfo 'miscellaneous-field 'name)
      (add-element-from-header-with-attribute-and-text
          modules misc-element doc 'piece 'miscellaneous-field 'name)
      (add-element-from-header-with-attribute-and-text
          modules misc-element doc 'style 'miscellaneous-field 'name)
      (add-element-from-header-with-attribute-and-text
          modules misc-element doc 'subtitle 'miscellaneous-field 'name)
      (add-element-from-header-with-attribute-and-text
          modules misc-element doc
          'subsubtitle 'miscellaneous-field 'name)
      (if (xml:hasChildNodes misc-element)
        (xml:appendChild identification-element misc-element) '())
      (add-element-from-header-with-text
          modules work-element doc 'opus 'work-number)
      (add-element-from-header-with-text
          modules work-element doc 'title 'work-title)
      (if (xml:hasChildNodes work-element)
        (xml:appendChild root work-element) '())
      (if (xml:hasChildNodes identification-element)
        (xml:appendChild root identification-element) '())))))

; as each context is derived from its parent context,
; we can define the divisions element for the top-context,
; and all staffs will get this in their attributes

(define make-first-music-item (lambda (doc)
  (assert (xml:document? doc)
          report: '@make-first-music-item)
  (let* ((result (make-empty-music-item)))
    (music-item:set-context! result result)
    (add-part-context-element result
        (make-xml-item doc 'divisions (/ ticks-per-whole 4)) doc)
    result)))


(define write-item (lambda (item doc info port)
  (assert (music-item? item)
          (xml:document? doc)
          (music-information? info)
          (output-port? port)
          report: '@write-item)
  (xml:formatDocument doc)
  (xml:write doc port)))

(define create-empty-document (lambda (type)
  (assert (symbol? type)
          report: '@create-empty-document)
  (let* ((attributes-types
      '((xsd .
          (( xmlns:xsi . http://www.w3.org/2001/XMLSchema-instance )
          ( xsi:schemaLocation . "http://www.musicxml.org/xsd musicxml.xsd" )))
      (local-xsd .
        (( xmlns:xsi . http://www.w3.org/2001/XMLSchema-instance )
        ( xsi:noNamespaceSchemaLocation . musicxml.xsd )))))
      (dtd-types
      '((dtd . ((name . score-partwise)
          ( public-id . "-//Recordare//DTD MusicXML Partwise//EN" )
          ( system-id . "http://www.musicxml.org/dtds/partwise.dtd" )))
      (local-dtd . (( name . score-partwise)
          ( system-id . "partwise.dtd")))))
      (doc-type-values (assoc-get type dtd-types '()))
      (doc-type (if (null? doc-type-values) '()
            (xml:createDocumentType
              (assoc-get 'name doc-type-values)
              (assoc-get 'public-id doc-type-values "")
              (assoc-get 'system-id doc-type-values ""))))
      (document (xml:createDocument '() 'score-partwise doc-type))
      (top-item (xml:documentElement document))
      (attributes (assoc-get type attributes-types '())))
    (for-each
        (lambda (attr)
            (xml:setAttribute top-item (car attr) (cdr attr)))
      attributes)
    document)))

(define* (music->musicxml score port type #:optional (write write-item) (cleanup cleanuptree))
  (assert (ly:score? score)
          (output-port? port)
          (symbol? type)
          report: '@music->musicxml)
  (let* ((document (create-empty-document type))
      (music (ly:score-music score))
      (modules (ly:score-output-defs score))
      (header (ly:score-header score))
      (def-header (ly:parser-lookup '$defaultheader))
      (top-item (xml:documentElement document))
      (first-music-item (make-first-music-item document))
      (generation-status (make-translation-status document))
      (generated-music
          (music->music-item music first-music-item
            generation-status first-music-item))
      )
    (if (module? def-header) (set! modules (cons def-header modules)) '())
    (if (module? header)  (set! modules (cons header modules)) '())
    (ly:progress "\nwriting musicxml to \"" )
    (ly:progress (port-filename port))
    (ly:progress "\"\n")
    (create-doc-identification-elements modules top-item document)
    (xml:appendChild top-item (xml:createElement document 'part-list))
    (allocate-elements-to-measures generation-status top-item document)
    (cleanup top-item generation-status
        (translation-status:context-tree generation-status) document)
    (write generated-music document generation-status port)
    music
  )
  (ly:progress "\ndone writing musicxml\n" )
)

(define-public (ly:score-or-music? x)
  (or (ly:music? x) (ly:score? x)))

(define*-public (write-musicxml music #:optional
    (port (current-output-port)) (type 'dtd))
  "Dump Music XML to @var{port}."
  (assert (ly:score-or-music? music)
          (output-port? port)
          (symbol? type)
          report: '@write-musicxml)
  (start-stack 1
  (let ((score (if (ly:music? music)(ly:make-score music) music)))
    (music->musicxml score port type))))
