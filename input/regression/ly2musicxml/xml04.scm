;; test setAttribute

; get the target name from the command line
(define target-name (cadr (program-arguments)))
; find position of dot
(define dot-pos (string-index target-name #\.))
; create the script base name
(define base-name (substring target-name 0 dot-pos))
; and create output files based on this name
(define xpath-port (open-output-file (format #f "~a.xpath" base-name)))
(define info-port (open-output-file (format #f "~a.info" base-name)))
(display "testing xml:setAttribute" info-port)
(close-output-port info-port)

(setenv "XML_LIBRARY_DEBUG" "true" )
(use-modules (scm xml-library))

(define doc (xml:createDocument '() 'root '()))
(define root (xml:documentElement doc))
; check root element
(display "count(/root)=1" xpath-port)

(define node (xml:createElement doc 'demo))
(define node1 (xml:createElement doc "demo2"))
(define node2 (xml:createElement doc 'n124))

(xml:appendChild node node1)
; check node
(display " and count(/root/demo/demo2)=1" xpath-port)

(xml:insertBefore node node2 node1)
; check position of new node
(display " and name(/root/demo/*[1])='n124'" xpath-port)

(xml:appendChild root node)
; check node
(display " and count(/root/demo)=1" xpath-port)

(xml:setAttribute node1 "node" 'demo2)
; check attribute
(display " and string(/root/demo/demo2/@node)='demo2'" xpath-port)

(xml:setAttribute node2 'node 123)
; check attribute
(display " and string(/root/demo/n124/@node)='123'" xpath-port)


(xml:setAttribute node 'node 'demo)
; check attribute
(display " and string(/root/demo/@node)='demo'" xpath-port)

(define xml-port (open-output-file (format #f "~a.xml" base-name)))
(xml:formatDocument doc)
(xml:write doc xml-port)
(close-output-port xml-port)

; expect 4 elements in document
(display " and count(//*)=4" xpath-port)
; expect 3 attributes in document
(display " and count(//@*)=3" xpath-port)
(close-output-port xpath-port)