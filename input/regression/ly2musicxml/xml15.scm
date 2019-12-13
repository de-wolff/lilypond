;; test xml:nodeType, xml:getAttributeNode

; get the target name from the command line
(define target-name (cadr (program-arguments)))
; find position of dot
(define dot-pos (string-index target-name #\.))
; create the script base name
(define base-name (substring target-name 0 dot-pos))
; and create output files based on this name
(define xpath-port (open-output-file (format #f "~a.xpath" base-name)))
(define info-port (open-output-file (format #f "~a.info" base-name)))
(display "testing xml:nodeType, xml:getAttributeNode " info-port)
(close-output-port info-port)

(setenv "XML_LIBRARY_DEBUG" "true" )
(use-modules (scm xml-library))

(define doc (xml:createDocument '() 'root '()))
(define root (xml:documentElement doc))
; check root element
(display "count(/root)=1" xpath-port)

(define node-text (xml:createTextNode doc 'text))
(define node-cdata (xml:createCDATASection doc "cdata"))
(define node-comment (xml:createComment doc "comment"))
(define node-document-type (xml:createDocumentType 'root "comment" "2"))

(xml:setAttribute root 'type-document (xml:nodeType doc))
(display " and /root/@type-document='9'" xpath-port)
(xml:setAttribute root 'type-document-type (xml:nodeType node-document-type))
(display " and /root/@type-document-type='10'" xpath-port)
(xml:setAttribute root 'type-text (xml:nodeType node-text))
(display " and /root/@type-text='3'" xpath-port)
(xml:setAttribute root 'type-element (xml:nodeType root))
(display " and /root/@type-element='1'" xpath-port)
(xml:setAttribute root 'type-cdata (xml:nodeType node-cdata))
(display " and /root/@type-cdata='4'" xpath-port)
(xml:setAttribute root 'type-comment (xml:nodeType node-comment))
(display " and /root/@type-comment='8'" xpath-port)
(xml:setAttribute root 'type-attribute (xml:nodeType (xml:getAttributeNode root 'type-cdata)))
(display " and /root/@type-attribute='2'" xpath-port)

; check node

(define xml-port (open-output-file (format #f "~a.xml" base-name)))
(xml:formatDocument doc)
(xml:write doc xml-port)
(close-output-port xml-port)

; expect 1 elements in document
(display " and count(//*)=1" xpath-port)
; expect 7 attributes in document
(display " and count(//@*)=7" xpath-port)
(close-output-port xpath-port)