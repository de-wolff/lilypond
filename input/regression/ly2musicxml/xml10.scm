;; test createAttribute setAttributeNode and value (only setting)

; get the target name from the command line
(define target-name (cadr (program-arguments)))
; find position of dot
(define dot-pos (string-index target-name #\.))
; create the script base name
(define base-name (substring target-name 0 dot-pos))
; and create output files based on this name
(define xpath-port (open-output-file (format #f "~a.xpath" base-name)))
(define info-port (open-output-file (format #f "~a.info" base-name)))
(display "testing xml:createAttribute xml:setAttributeNode and xml:value (set) " info-port)
(close-output-port info-port)

(setenv "XML_LIBRARY_DEBUG" "true" )
(use-modules (scm xml-library))

(define doc (xml:createDocument '() 'root '()))
(define root (xml:documentElement doc))
; check root element
(display "count(/root)=1" xpath-port)
(define attribute (xml:createAttribute doc "demo"))
(xml:value attribute 'demo)
(xml:setAttributeNode root attribute)

; check attribute
(display " and string(/root/@demo)='demo'" xpath-port)

(define xml-port (open-output-file (format #f "~a.xml" base-name)))
(xml:formatDocument doc)
(xml:write doc xml-port)
(close-output-port xml-port)

; expect 1 elements in document
(display " and count(//*)=1" xpath-port)
; expect 1 attributes in document
(display " and count(//@*)=1" xpath-port)
(close-output-port xpath-port)