%% Do not edit this file; it is auto-generated from LSR http://lsr.dsi.unimi.it
%% This file is in the public domain.
\version "2.11.61"

\header {
  lsrtags = "fretted-strings"

  texidoc = "
A hammer in tablature can be faked with slurs.  

"
  doctitle = "Faking a hammer in tablatures"
} % begin verbatim
\score {
  \new TabStaff {
    \relative c'' {
      c4( d) d( d)
      d2( c)
    }
  }
}

