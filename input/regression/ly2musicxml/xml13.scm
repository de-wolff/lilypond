;; test xml:value (get)

; get the target name from the command line
(define target-name (cadr (program-arguments)))
; find position of dot
(define dot-pos (string-index target-name #\.))
; create the script base name
(define base-name (substring target-name 0 dot-pos))
; and create output files based on this name
(define xpath-port (open-output-file (format #f "~a.xpath" base-name)))
(define info-port (open-output-file (format #f "~a.info" base-name)))
(display "testing xml:value (get)" info-port)
(close-output-port info-port)

(setenv "XML_LIBRARY_DEBUG" "true" )
(use-modules (scm xml-library))
(define doc (xml:createDocument '() 'root '()))
; check root element
(display "count(/root)=1" xpath-port)

(define root (xml:documentElement doc))
(define node (xml:createElement doc 'node))
(xml:setAttribute node 'test 'value )
(xml:appendChild root node)
(display "and count(root/node)=1" xpath-port)
(xml:setAttribute root 'value (xml:value (xml:getAttributeNode node 'test)))
(display "and /root/@value='value'" xpath-port)


(define xml-port (open-output-file (format #f "~a.xml" base-name)))
(xml:formatDocument doc)
(xml:write doc xml-port)
(close-output-port xml-port)

; expect 6 elements in document
(display " and count(//*)=2" xpath-port)
; expect 0 attributes in document
(display " and count(//@*)=2" xpath-port)
(close-output-port xpath-port)