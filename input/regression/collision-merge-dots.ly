\header {

    texidoc  = "When merging heads, the dots are merged too."
    }
\version "2.1.22"

\score {
\notes {
\relative c'' \new Staff {
    << { d8. e16 } \\ { d8. b16 } >> 
    }
}

\paper { raggedright = ##t }}
