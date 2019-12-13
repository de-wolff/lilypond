;; test xml:removeChild and xml:childNodes

; get the target name from the command line
(define target-name (cadr (program-arguments)))
; find position of dot
(define dot-pos (string-index target-name #\.))
; create the script base name
(define base-name (substring target-name 0 dot-pos))
; and create output files based on this name
(define xpath-port (open-output-file (format #f "~a.xpath" base-name)))
(define info-port (open-output-file (format #f "~a.info" base-name)))
(display "testing xml:removeChild and xml:childNodes" info-port)
(close-output-port info-port)

(setenv "XML_LIBRARY_DEBUG" "true" )
(use-modules (scm xml-library))
(define doc (xml:createDocument '() 'root '()))
; check root element
(display "count(/root)=1" xpath-port)

(define root (xml:documentElement doc))
(define from-node (xml:createElement doc 'from))
(define to-node (xml:createElement doc 'to))
(define node1 (xml:createElement doc 'node1))
(define node2 (xml:createElement doc 'node2))
(define node3 (xml:createElement doc 'node3))
(xml:appendChild from-node node1)
(xml:appendChild from-node node2)
(xml:appendChild from-node node3)

(define move (lambda (from-node to-node lst)
  (cond ((null? lst) '())
    (else
      (let ((node (car lst)))
        (xml:removeChild from-node node)
        (xml:appendChild to-node node)
        (move from-node to-node (cdr lst)))))))

(move from-node to-node (xml:childNodes from-node))

(xml:appendChild root from-node)
;check from-node is empty
(display "and count(root/from/*)=0" xpath-port)
(xml:appendChild root to-node)
;check if al elements are moved
(display "and count(root/to/node1)=1" xpath-port)
(display "and count(root/to/node2)=1" xpath-port)
(display "and count(root/to/node3)=1" xpath-port)

(define xml-port (open-output-file (format #f "~a.xml" base-name)))
(xml:formatDocument doc)
(xml:write doc xml-port)
(close-output-port xml-port)

; expect 6 elements in document
(display " and count(//*)=6" xpath-port)
; expect 0 attributes in document
(display " and count(//@*)=0" xpath-port)
(close-output-port xpath-port)