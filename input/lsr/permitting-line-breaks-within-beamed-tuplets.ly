%% Do not edit this file; it is auto-generated from LSR http://lsr.dsi.unimi.it
%% This file is in the public domain.
\version "2.13.0"

\header {
  lsrtags = "rhythms"

 doctitlees = "Permitir saltos de línea dentro de grupos especiales con barra"
 texidoces = "
Este ejemplo artificial muestra cómo se pueden permitir tanto los
saltos de línea manuales como los automáticos dentro de un grupo de
valoración especial unido por una barra.  Observe que estos grupos
sincopados se deben barrar manualmente.

"

%% Translation of GIT committish :<6ce7f350682dfa99af97929be1dec6b9f1cbc01a>
  texidocde = "
Dieses künstliche Beispiel zeigt, wie sowohl automatische als auch
manuelle Zeilenumbrüche innerhalb einer N-tole mit Balken erlaubt
werden können.  Diese unregelmäßige Bebalkung muss allerdings manuell
gesetzt werden.

"
  doctitlede = "Zeilenumbrüche bei N-tolen mit Balken erlauben"

  texidoc = "
This artificial example shows how both manual and automatic line breaks
may be permitted to within a beamed tuplet. Note that such off-beat
tuplets have to be beamed manually.

"
  doctitle = "Permitting line breaks within beamed tuplets"
} % begin verbatim

\layout {
  \context {
    \Voice
    % Permit line breaks within tuplets
    \remove "Forbid_line_break_engraver"
    % Allow beams to be broken at line breaks
    \override Beam #'breakable = ##t
  }
}
\relative c'' {
  a8
  \repeat unfold 5 { \times 2/3 { c[ b a] } }
  % Insert a manual line break within a tuplet
  \times 2/3 { c[ b \bar "" \break a] }
  \repeat unfold 5 { \times 2/3 { c[ b a] } }
  c8
}
