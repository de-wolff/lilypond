
\version "2.1.22"

\header { texidoc = "@cindex text spanner
You can make LilyPond print text spanners. "
}

\score{
    \notes\relative c''{
        \override TextSpanner  #'edge-text = #'("bla" . "blu")
        a \startTextSpan
	b c 
        a \stopTextSpan

        \override TextSpanner  #'dash-period = #2
        \override TextSpanner  #'dash-fraction = #0.0
        a \startTextSpan
	b c 
        a \stopTextSpan

        \revert TextSpanner #'style
        \override TextSpanner  #'style = #'dashed-line
        \override TextSpanner  #'edge-height = #'(1 . -2)
        a \startTextSpan
	b c 
        a \stopTextSpan


        \set Staff.centralCPosition =  #-13

        \override TextSpanner  #'dash-period = #10
        \override TextSpanner  #'dash-fraction = #.5
        \override TextSpanner  #'thickness = #10
        a \startTextSpan
	b c 
        a \stopTextSpan
        \set Staff.centralCPosition =  #-6	
    }
	\paper{ raggedright = ##t}
}

