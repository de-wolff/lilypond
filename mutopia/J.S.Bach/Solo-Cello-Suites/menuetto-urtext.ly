% menuetto-urtext.ly
% belongs together with -cello.ly and -viola.ly
% (who is going to make a -violin.ly?)

%{
Well, there are still some scripts in this "urtext".
But merging melodic and scripts doen't work too well yet (see viola_scripts).
%}

menuetto_i_a = \context Staff \notes \relative c {
	\context Voice=i
	<d2\f f a> bes'4 |
	bes8 a bes g a4 |
	<d,4 bes> g f8 e |
	f8( e )d cis b a |
	<d2 f a> bes'!4 |
	bes8 a bes g c!4 | 
	%7
	<a f> <d f, bes,> <e g, g,> |
	<cis2. e, a,> |
	<a2\f e'> d8 e16 f |
	e8 d cis e a, g |
	a4 d cis |
	g'8 f e f d c |
	%13
	bes2 c4 |
	a8 g a f <d'4 e, bes> |
	<c f, a,> bes8 a g a |
	f8 e f a g bes |
	<a2^\trill fis> bes4 |
	c8 bes c a fis4^\trill |
	%19
% dispute
%	<bes,4 g' d'> <c, g' c> < d,8 bes'(> )a |
	<d'4 g, bes,> <c g c,> bes8 a |
	c8 bes a bes g bes |
	d4 cis d |
	g,8 f g e f4 |
	g,8 g' <{e4.^\trill d8 } a4.> ~ |
	<d2. a d,> |
}

menuetto_i_b = \context Staff \notes \relative c {
	\context Voice = ii
	\stemdown
	\skip 2.*1; |
	<e8 c> \skip 8*5; |
	\skip 2.*1; |
	a,8 \skip 8*5; |
	\skip 2.*1; |
	e'8 \skip 8*5; |
	\stemboth
	s2.*2 |
	s2.
	g8 \skip 8*5; |
	f2 e4
	d8 \skip 8*5; |
	g4 () f e
	f8 \skip 8*5; |
	\skip 2.*3; |
	es8 \skip 8*3; d4 |
	\skip 4*2; d4 |
	<d8 g,> \skip 8*5; |
% dispute
%	g2 f4 |
	g4 \skip 4*1; f4 |
	cis8 \skip 8*3; d4 |
	s2.*2
}

% UGH, fix this like in sarabande
menuetto_i_a_voice_urg = \notes{
	\context Voice = i
	\skip 2.*1; \stemup
	\skip 2.*1; \stemboth
	\skip 2.*1; \stemup
	\skip 2.*1; \stemboth
	\skip 2.*1; \stemup
	\skip 2.*1; \stemboth
	\skip 2.*3; \stemup
	\skip 2.*1; \stemup
	\skip 2.*3;
	\skip 4*2; \stemboth
	\skip 4*2; \stemup
	\skip2.*1; \stemup
	\skip 4*2; \stemboth
	\skip2.*1; \stemup
	\skip 2.*1; \stemboth
	\skip 2*1; \stemup
	\skip 4*1;
	\skip 2.*4; \stemboth
	\skip 2.*1;
}

menuetto_i_a_voice_urg_urg = \notes<
	\$menuetto_i_a_voice_urg
	\$menuetto_i_a
>

menuetto_i_b_voice_urg = \notes{
	\context Voice=ii
	\stemdown
	% urg urg, huh?
	\skip 2.*8; \stemdown
}

menuetto_i_b_voice_urg_urg = \notes<
	\$menuetto_i_b_voice_urg
	\$menuetto_i_b
>

menuetto_i = \context Staff \notes<
	\repeat "semi" 2 { \$menuetto_i_a_voice_urg_urg }
	\repeat "semi" 2 { \$menuetto_i_b_voice_urg_urg }
>

menuetto_ii = \context Staff\notes \relative c {
	\context Voice=i
	fis4^\trill d8 e fis g |
	a4 fis, a' |
	g,8 b e4 g |
	d8( cis )b cis a g |
	% ugh, forcing knee
	% Lily's not yet smart enough to decide for herself.
	\stemup fis \stemboth d''( cis b a )g |
	b( a g fis e )d |
	%7
	cis d g4 fis8( g16 )a |
	<\stemdown a,2. \stemup e'> |
	\stemboth
	cis4^\prall e8( d cis )b |
	cis4 g, cis' |
	fis,8()a d4 fis |
	b,8()a g()fis g b |
	%13
	e, d'( cis )b cis()ais |
	d,( b' a! g fis )e |
	g( fis e d cis )d |
	b( cis d e fis )g |
	a( g fis g a )b |
	c4 dis,, c'' |
	%19
	b8()a c( b a )g |
	fis() g a()fis g()e |
	cis4^\trill a8 b cis d |
	e( fis g )b a4 |
	g8()fis e()d e()cis |
	d2.
}

\version "1.1.55";
