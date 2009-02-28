%% Do not edit this file; it is auto-generated from LSR http://lsr.dsi.unimi.it
%% This file is in the public domain.
\version "2.13.0"

\header {
  lsrtags = "text"

  texidoces = "
Las marcas se pueden imprimir al final de la línea actual, en vez de
al comienzo de la línea siguiente.  Esto es útil principalmente cuando
se tiene que añadir una marca al final de la partitura (donde no hay
línea siguiente).

En tales casos, el extremo derecho de la marca se tiene que alinear
con la última línea divisoria, como muestra la segunda línea de este
ejemplo.

"
  doctitlees = "Imprimir marcas al final de la línea o de la partitura"

%% Translation of GIT committish :0364058d18eb91836302a567c18289209d6e9706
  texidocde = "
Zeichen können auch am Ende der aktuellen Zeile ausgegeben werden, anstatt
dass sie auf die folgende Zeile verschoben werden.  Das ist nüztlich,
wenn ein Zeichen am Ende einer Partitur eingefügt werden soll, wo
gar keine nächste Zeile vorhanden ist.

In derartigen Fällen muss die rechte Ecke des Zeichens an dem letzten
Taktstrich ausgerichtet werden, wie die zweite Zeile des Beispiels zeigt.

"
  doctitlede = "Zeichen am Ende einer Zeile oder Partitur setzen"

  texidoc = "
Marks can be printed at the end of the current line, instead of the
beginning of the following line. This is particularly useful when a
mark has to be added at the end of a score -- when there is no next
line.

In such cases, the right end of the mark has to be aligned with the
final bar line, as demonstrated on the second line of this example. 

"
  doctitle = "Printing marks at the end of a line or a score"
} % begin verbatim

\relative c'' {  
  \override Score.RehearsalMark #'break-visibility = #begin-of-line-invisible
  g2 c
  d,2 a'
  \mark \default
  \break
  g2 b,
  c1 \bar "||"
  \override Score.RehearsalMark #'self-alignment-X = #RIGHT  
  \mark "D.C. al Fine"
}
