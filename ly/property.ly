% property.ly
% list of properties that lily recognises
% and some shorthands (ugh)

%{

SEE THE REFERENCE MANUAL FOR EXPLANATIONS.

%}

\version "1.0.20";

%hmm, (these) abbrevs suck, imo
% i guess they're meant as some form of doco
% that's what i use them for...
stemup =        \property Voice.verticalDirection = \up 
stemboth= 	\property Voice.verticalDirection = \center
stemdown = 	\property Voice.verticalDirection = \down

slurup = \notes {
	s1*0
	\property Voice.slurVerticalDirection = \up 
	}
slurboth= \notes {
	s1*0
	\property Voice.slurVerticalDirection = \center
}
slurdown = \notes { 	
	s1*0
	\property Voice.slurVerticalDirection = \down
}

shifton = \property Voice.horizontalNoteShift = 1
shiftoff = \property Voice.horizontalNoteShift = 0

onevoice = { 	
	\stemboth \shiftoff	
}

voiceone = 
	\context Voice = one  {
	\stemup
}

voicetwo = 
	\context Voice = two {
	\stemdown
}

voicethree = 
	\context Voice = three {
	\stemup

}

voicefour = 
	\context Voice = four {
	\stemdown
	\shifton
}


% ugh, cluttering global namespace...

% ugh2. 
none=0
free=0
normal=1
traditional=2
infinity=10000

beamslopeproportional = 
	\property Score.beamslopedamping = \none

beamslopedamped = 
	\property Score.beamslopedamping = \normal


beamslopezero = 
	\property Score.beamslopedamping = \infinity


% this sucks, you'd want to pass an array, at least
% (or embedded code: you still can't dictate the slope / stemlength)
beamposfree = 
	\property Score.beamquantisation = \none


beamposnormal = 
	\property Score.beamquantisation = \normal


beampostraditional = 
	\property Score.beamquantisation = \traditional


slurnormal = 
	\property Voice.slurdash = ""


slurdotted = 
	\property Voice.slurdash = 1


%{
 for grace note hack, see input/test/grace.ly
%}
tupletoff = {
	\property Voice.tupletVisibility = 0
}
tupleton = {
	\property Voice.tupletVisibility = 3
}
tiny  = {
	\property Voice.fontSize= "-2"
	\tupletoff %urg
}

small  = {
	\property Voice.fontSize= "-1"
}

normalsize = {
	\property Voice.fontSize= "0"
	\tupleton %urg
}

%{
  [urg: try at] temporary grace note hack
  the total visible duration of the grace notes must be half
  the duration of the 'at' note: e.g.:

  \grace b8 \graceat c4 \ecarg
  \grace c16 b16 \graceat c4 \ecarg

grace = {
	\tiny
% it would be so cool not to have to specify these factors each time...
% :-(
	\property Voice.tupletVisibility = 0
	\[1/16
}

graceat = \melodic {
	\normalsize
	\property Voice.tupletVisibility = 0
	\] \[31/32
}

ecarg =  \melodic {
	\property Voice.tupletVisibility = 0
	\]
	\property Voice.tupletVisibility = 3
}
%}

normalkey = {
	\property Staff.keyoctaviation = 1
}

specialkey = {
	\property Staff.keyoctaviation = 0
}

% End the incipit and print a ``normal line start''.
endincipit = \notes{
    \partial 16; s16  % Hack to handle e.g. \bar ".|"; \endincipit
    \property Staff.clefStyle = "fullSizeChanges" 
    \nobreak \bar "";
}

