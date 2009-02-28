%% Do not edit this file; it is auto-generated from LSR http://lsr.dsi.unimi.it
%% This file is in the public domain.
\version "2.13.0"

\header {
  lsrtags = "repeats"

  texidoces = "
Las repeticiones de compases completos mayores de dos repeticiones
pueden llevar un contador si se activa la propiedad adecuada, como se
ve en este ejemplo:

"
  doctitlees = "Percent repeat counter"

%% Translation of GIT committish :<0364058d18eb91836302a567c18289209d6e9706>
  texidocde = "
Ganztaktwiederholungen mit mehr als zwei Wiederholungen erhalten einen
Zähler, wenn man die entsprechende Eigenschaft einsetzt:

"
  doctitlede = "Prozent-Wiederholungen zählen"

  texidoc = "
Measure repeats of more than two repeats can get a counter when the
convenient property is switched, as shown in this example:

"
  doctitle = "Percent repeat counter"
} % begin verbatim

\relative c'' {
  \set countPercentRepeats = ##t
  \repeat percent 4 { c1 }
}
