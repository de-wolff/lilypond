;; test xml:nodeName, xml:firstChild and xml:lastChild

; get the target name from the command line
(define target-name (cadr (program-arguments)))
; find position of dot
(define dot-pos (string-index target-name #\.))
; create the script base name
(define base-name (substring target-name 0 dot-pos))
; and create output files based on this name
(define xpath-port (open-output-file (format #f "~a.xpath" base-name)))
(define info-port (open-output-file (format #f "~a.info" base-name)))
(display "testing xml:nodeName, xml:firstChild and xml:lastChild " info-port)
(close-output-port info-port)

(setenv "XML_LIBRARY_DEBUG" "true" )
(use-modules (scm xml-library))

(define doc (xml:createDocument '() 'root '()))
(define root (xml:documentElement doc))
; check root element
(display "count(/root)=1" xpath-port)

(define node1 (xml:createElement doc 'first))
(define node2 (xml:createElement doc 'middle))
(define node3 (xml:createElement doc 'last))

(xml:appendChild root node1)
(display " and count(/root/first)=1" xpath-port)
(xml:appendChild root node2)
(display " and count(/root/middle)=1" xpath-port)
(xml:appendChild root node3)
(display " and count(/root/last)=1" xpath-port)
(xml:setAttribute root 'first-child (xml:nodeName (xml:firstChild root)))
(display " and /root/@first-child='first'" xpath-port)
(xml:setAttribute root 'last-child (xml:nodeName (xml:lastChild root)))
(display " and /root/@last-child='last'" xpath-port)

; check node

(define xml-port (open-output-file (format #f "~a.xml" base-name)))
(xml:formatDocument doc)
(xml:write doc xml-port)
(close-output-port xml-port)

; expect 4 elements in document
(display " and count(//*)=4" xpath-port)
; expect 2 attributes in document
(display " and count(//@*)=2" xpath-port)
(close-output-port xpath-port)