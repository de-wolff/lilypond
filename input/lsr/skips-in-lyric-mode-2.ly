%% Do not edit this file; it is auto-generated from LSR http://lsr.dsi.unimi.it
%% This file is in the public domain.
\version "2.13.0"

\header {
  lsrtags = "rhythms, vocal-music"

  texidoc = "
Although @code{s} skips cannot be used in @code{\\lyricmode} (it is
taken to be a literal \"s\", not a space), double quotes (@code{\"\"})
or underscores (@code{_}) are available.So for example: 

"
  doctitle = "Skips in lyric mode (2)"
} % begin verbatim

<<
  \relative c'' { a4 b c d }
  \new Lyrics \lyricmode { a4 "" _ gap }
>>
