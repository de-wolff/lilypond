;; test createCDATASection

; get the target name from the command line
(define target-name (cadr (program-arguments)))
; find position of dot
(define dot-pos (string-index target-name #\.))
; create the script base name
(define base-name (substring target-name 0 dot-pos))
; and create output files based on this name
(define xpath-port (open-output-file (format #f "~a.xpath" base-name)))
(define regex-port (open-output-file (format #f "~a.regex" base-name)))
(define info-port (open-output-file (format #f "~a.info" base-name)))
(display "testing xml:createCDATASection:" info-port)
(close-output-port info-port)

(setenv "XML_LIBRARY_DEBUG" "true" )
(use-modules (scm xml-library))

(define doc (xml:createDocument '() 'root '()))
(define root (xml:documentElement doc))
; check root element
(display "count(/root)=1" xpath-port)

(define node (xml:createCDATASection doc "this is cdata"))
;; there is in xpath no way to distinguish between CDATA and text
;; so we check for the specific CDATA text
(display "\\<root>[[:space:]]*[<]!\\[CDATA.*\\]\\]>[[:space:]]*[<]/root>" regex-port)

(xml:appendChild root node)

; check node
(display " and (//text()='this is cdata')" xpath-port)

(define xml-port (open-output-file (format #f "~a.xml" base-name)))
(xml:formatDocument doc)
(xml:write doc xml-port)
(close-output-port xml-port)

; expect 1 element in document
(display " and count(//*)=1" xpath-port)
; expect 0 attributes in document
(display " and count(//@*)=0" xpath-port)
(close-output-port regex-port)
(close-output-port xpath-port)
