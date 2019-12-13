#!/bin/bash
music=$(<$1)
base=${1%.*}
cat <<END > $2
\version "2.20.0"
\include "musicxml.ly"

%for tests we prefer xsd, as this is more specific
check=\musicxml-xsd

info =
#(define-void-function (parser location value) (string?)
    (define port (open-output-file (format #f "~a.info" (ly:parser-output-name parser))))
    (display value port)
    (close-output-port port)
    (make-music 'SequentialMusic 'elements '()))

xpath =
#(define-void-function (parser location value) (string?)
    (define port (open-output-file (format #f "~a.xpath" (ly:parser-output-name parser))))
    (display value port)
    (close-output-port port)
    (make-music 'SequentialMusic 'elements '()))

$music

\musicxml \check \music
END