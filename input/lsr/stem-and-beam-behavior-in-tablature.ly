%% Do not edit this file; it is auto-generated from LSR http://lsr.dsi.unimi.it
%% This file is in the public domain.
\version "2.11.62"

\header {
  lsrtags = "fretted-strings"

  texidoc = "
The direction of stems is controlled the same way in tablature as in
traditional notation. Beams can be made horizontal, as shown in this
example.

"
  doctitle = "Stem and beam behavior in tablature"
} % begin verbatim
\new TabStaff {
  \relative c {
    g16 b d g b d g b
    \stemDown
    \override Beam #'damping = #+inf.0
    g,,16 b d g b d g b
  }
}
