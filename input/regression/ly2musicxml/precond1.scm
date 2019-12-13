; get the target name from the command line
(define target-name (cadr (program-arguments)))
; find position of dot
(define dot-pos (string-index target-name #\.))
; create the script base name
(define base-name (substring target-name 0 dot-pos))


(setenv "XML_LIBRARY_DEBUG" "true" )

(use-modules (scm xml-library))

(catch 'assertion-failure
    (lambda ()
        (define node (xml:createElement '() '()))
        '())
    (lambda (key args)
        (define info-port (open-output-file (format #f "~a.info" base-name)))
        (define xpath-port (open-output-file (format #f "~a.xpath" base-name)))
        (define xml-port (open-output-file (format #f "~a.xml" base-name)))
        (display "caught expected exception" info-port)
        (display "<checked/>" xml-port)
        ; just full xpath process, because simple coming here means the test is ok
        (display "count(/checked) = 1" xpath-port)
        (close-output-port info-port)
        (close-output-port xml-port)
        (close-output-port xpath-port)
        (quit 0)
    )
)
(quit 1)
