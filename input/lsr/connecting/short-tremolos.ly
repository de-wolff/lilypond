%%  Do not edit this file; it is auto-generated from LSR!
\version "2.11.23"

\header { texidoc = "
Short tremolos (involving eighth notes or shorter durations) can be
obtained; in such a case only one beam is connected to the stems.
" }

\layout { ragged-right = ##t }

\context Staff  \relative c' {
  \repeat "tremolo"  2 { c32 e32 }
  \stemDown
  \repeat "tremolo"  2 { c32 e32 }
}

