%% Do not edit this file; it is auto-generated from LSR http://lsr.dsi.unimi.it
%% This file is in the public domain.
\version "2.11.61"

\header {
  lsrtags = "rhythms, vocal-music, ancient-notation, contexts-and-engravers"

  texidoces = "
Este tipo de notación se utiliza para el canto de los Salmos, en
que las estrofas no siempre tienen la misma longitud.

"
  doctitlees = "Notación de responsos o salmos"

  texidoc = "
This form of notation is used for the chant of the Psalms, where verses
aren't always the same length.

"
  doctitle = "Chant or psalms notation"
} % begin verbatim
stemOn = { \override Staff.Stem #'transparent = ##f }
stemOff = { \override Staff.Stem #'transparent = ##t }

\score {
  \new Staff \with { \remove "Time_signature_engraver" }
  {
    \key g \minor
    \cadenzaOn
    \stemOff a'\breve bes'4 g'4
    \stemOn a'2 \bar "||"
    \stemOff a'\breve g'4 a'4
    \stemOn f'2 \bar "||"
    \stemOff a'\breve^\markup { \italic flexe }
    \stemOn g'2 \bar "||"
  }
}
