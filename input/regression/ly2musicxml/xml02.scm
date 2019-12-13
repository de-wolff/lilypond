;; test createElement, documentElement and appendChild

; get the target name from the command line
(define target-name (cadr (program-arguments)))
; find position of dot
(define dot-pos (string-index target-name #\.))
; create the script base name
(define base-name (substring target-name 0 dot-pos))
; and create output files based on this name
(define xpath-port (open-output-file (format #f "~a.xpath" base-name)))
(define info-port (open-output-file (format #f "~a.info" base-name)))
(display "testing xml:createElement and xml:appendChild " info-port)
(close-output-port info-port)

(setenv "XML_LIBRARY_DEBUG" "true" )
(use-modules (scm xml-library))
(define doc (xml:createDocument '() 'root '()))
; check root element
(display "count(/root)=1" xpath-port)

(define root (xml:documentElement doc))
(define node (xml:createElement doc 'demo))
(xml:appendChild root node)
; check root element
(display " and count(/root/demo)=1" xpath-port)

(define xml-port (open-output-file (format #f "~a.xml" base-name)))
(xml:formatDocument doc)
(xml:write doc xml-port)
(close-output-port xml-port)

; expect 2 element2 in document
(display " and count(//*)=2" xpath-port)
; expect 0 attributes in document
(display " and count(//@*)=0" xpath-port)
(close-output-port xpath-port)