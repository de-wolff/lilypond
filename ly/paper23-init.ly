% paper23-init.ly


\version "1.5.49"

paperTwentythree = \paper {
	staffheight = 23.0\pt
	\stylesheet #(make-style-sheet 'paper23)
	\include "params-init.ly"
}

\paper { \paperTwentythree }
