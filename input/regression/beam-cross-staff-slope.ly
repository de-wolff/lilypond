\version "2.1.22"
\header {
    texidoc = "Cross staff  (kneed) beams don't cause extreme slopes."
}

\score {
    \notes\context PianoStaff <<
    \context Staff = up
    \relative c'' <<
        {
	    \stemDown
            f16( \> d b\! \change Staff = down \stemUp
            \clef treble g ~ < g e>8)
	    
	     e[ \change Staff = up
	    \stemDown
	    e e]
	    \change Staff = down
	    \stemUp
	    e8.[
		\change Staff = up
	    \stemDown
	    e8.]
	    
	} \\
    >>
    \context Staff = down {
        \time 3/8 \clef bass s4.*3 }
    >>
    \paper { raggedright = ##t}
}

