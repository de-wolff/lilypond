\version "2.1.26"

\header { texidoc = "@cindex Stem Cross Staff
There is no support for putting chords across staves.
You can get this result by increasing the length of the stem in the
lower stave so it reaches the stem in the upper stave, or vice versa. "
}

stemExtend = \once \override Stem  #'length = #22

%% following reqs 1.7.1 or better.
noFlag = \once \override Stem  #'flag-style = #'no-flag


\score { \notes 
    \context  PianoStaff
        << \new Staff   {
	    \stemDown
	    \stemExtend
	    f'4
	    \stemExtend
	    \noFlag
	    f'8 }
          \new Staff {
	      \clef bass
	      a4 a8 }
	  >>

    \paper { raggedright = ##t}
}

