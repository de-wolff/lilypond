
\header {
texidoc = "@cindex Ancient Accidentals
Accidentals are available in different ancient styles. This
file lists all of them.
"
}

\version "2.1.22"

\score { 
  \context Voice \notes\relative c {
    \time 3/4
	\override Staff.Accidental  #'style = #'default
	cisis''^"Accidental style = \#'default" cisis! cisis? |
	cis cis! cis? | 
	c c! c? |
	ces ces! ces? |
	ceses ceses! ceses? |
	\break
	
	\override Staff.Accidental  #'style = #'hufnagel
	cisis^"Accidental style = \#'hufnagel" cisis! cisis? |
	cis cis! cis? | 
	c c! c? |
	ces ces! ces? |
	ceses ceses! ceses? |
	\break
	
	\override Staff.Accidental  #'style = #'medicaea
	cisis^"Accidental style = \#'medicaea" cisis! cisis? |
	cis cis! cis? | 
	c c! c? |
	ces ces! ces? |
	ceses ceses! ceses? |
	\break
	
	\override Staff.Accidental  #'style = #'vaticana
	cisis^"Accidental style = \#'vaticana" cisis! cisis? |
	cis cis! cis? | 
	c c! c? |
	ces ces! ces? |
	ceses ceses! ceses? |
	\break
	
	\override Staff.Accidental  #'style = #'mensural
	cisis^"Accidental style = \#'mensural" cisis! cisis? |
	cis cis! cis? | 
	c c! c? |
	ces ces! ces? |
	ceses ceses! ceses? |
	
  }
}

