%% Do not edit this file; it is auto-generated from LSR http://lsr.dsi.unimi.it
%% This file is in the public domain.
\version "2.13.0"

\header {
  lsrtags = "chords, template"

  texidoces = "
¿Quiere preparar una hoja guía de acordes (o «lead sheet») con
melodía y acordes?  ¡No busque más!

"

doctitlees = "Plantilla de pentagrama único con música y acordes"

  texidocde = "
Wollen Sie ein Liedblatt mit Melodie und Akkorden schreiben?  Hier ist 
das richtige Beispiel für Sie!
"

  texidocja = "
旋律とコードを持つリード譜を欲しくはありませんか？他を見る必要はありません！
"

  texidoc = "
Want to prepare a lead sheet with a melody and chords? Look no further!


"
  doctitle = "Single staff template with notes and chords"
} % begin verbatim

melody = \relative c' {
  \clef treble
  \key c \major
  \time 4/4
  
  f4 e8[ c] d4 g
  a2 ~ a
}

harmonies = \chordmode {
  c4:m f:min7 g:maj c:aug
  d2:dim b:sus
}

\score {
  <<
    \new ChordNames {
      \set chordChanges = ##t
      \harmonies
    }
    \new Staff \melody
  >>  
  \layout{ }
  \midi { }
}
