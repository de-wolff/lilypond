\version "1.7.18"
% delete; covered in refman.  Trust me, I'm a cellist, and I found
% this stuff easily in the refman.  :)   -gp

\header{ texidoc = "
the thumb-script is used in cello music to indicate a note that should
 be played with your thumb."

%% TODO: merge with fingering ?

}




\score { \notes \relative c'' {
		[<<a a'-3(>>8_\thumb <<b b'-3>>-)_\thumb
		<<c c'-3(>>_\thumb <<d d'-3>>-)_\thumb]
	}
	\paper{ 
		linewidth = 80.\mm 

	}
}

	
%% new-chords-done %%
