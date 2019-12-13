\version "2.18.0"

#(use-modules (scm musicxml))

musicxml-dtd = #'dtd
musicxml-xsd = #'xsd
musicxml-local-dtd = #'local-dtd
musicxml-local-xsd = #'local-xsd

musicxml =
#(define-void-function (parser location type music) ((symbol? 'dtd) ly:score-or-music?)
   (_i "Display the musicxml representation of @var{music} ")
      (call-with-output-file
         (format #f "~a.xml"
            (ly:parser-output-name parser))
         (lambda (port) (write-musicxml music port type))))
