\version "1.7.18"
% regression or delete.

\score
{
\context Voice \notes \relative c'' {
\time 2/4
\times 2/3 { c8 c c c c c  }
\property Voice . tupletSpannerDuration = #(ly:make-moment 1 4)
\times 2/3 { c8 c c c c c  }

}

}
%% new-chords-done %%
