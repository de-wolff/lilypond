\header {

  texidoc = "Scripts avoid stem tremolos even if there is no visible stem."

}
\version "2.7.16"

\layout {raggedright =##t}
{
  \stemDown
  g'1:32_"foo"
  g'1:32_.
  g'1:32_\f
}
