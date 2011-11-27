%% DO NOT EDIT this file manually; it is automatically
%% generated from LSR http://lsr.dsi.unimi.it
%% Make any changes in LSR itself, or in Documentation/snippets/new/ ,
%% and then run scripts/auxiliar/makelsr.py
%%
%% This file is in the public domain.
\version "2.15.20"

\header {
  lsrtags = "editorial-annotations, tweaks-and-overrides"

  texidoc = "
The @code{print-function} can be overridden to draw a box around an
arbitrary grob.

"
  doctitle = "Drawing boxes around grobs"
} % begin verbatim

\relative c'' {
  \override TextScript #'stencil =
    #(make-stencil-boxer 0.1 0.3 ly:text-interface::print)
  c'4^"foo"

  \override Stem #'stencil =
    #(make-stencil-boxer 0.05 0.25 ly:stem::print)
  \override Score.RehearsalMark  #'stencil =
    #(make-stencil-boxer 0.15 0.3 ly:text-interface::print)
  b8

  \revert Stem #'stencil

  \revert Flag #'stencil
  c4. c4
  \mark "F"
  c1
}



