\version "1.7.18"
% regression or delete.  -gp

\score {
  \context Voice \notes \relative c'' {
    \time 2/4
    \times 2/3 { r c,,, c''' }
    \times 2/3 { r c c }
    \times 2/3 { r c r}
    \times 2/3 { r r r}
    \times 2/3 { r c e}
    \times 2/3 { c r e}
    \times 2/3 { r c g}
    \times 2/3 { c r g}
  }
}

%% new-chords-done %%
