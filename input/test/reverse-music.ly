
\version "2.1.26"
% possible rename to scheme-something.
\header { texidoc="@cindex Scheme Reverse Music
Simple customised music apply. "
}

music = \notes \relative c'' { c4 d4( e4 f4 }

#(define (reverse-music music)
  (let* ((elements (ly:music-property music 'elements))
         (reversed (reverse elements))
         (e (ly:music-property music 'element))
         (span-dir (ly:music-property music 'span-direction)))

    (ly:music-set-property! music 'elements reversed)

    (if (ly:music? e)
        (ly:music-set-property!
         music 'element
         (reverse-music e)))

    (if (ly:dir? span-dir)
        (ly:music-set-property! music 'span-direction (- span-dir)))

    (map reverse-music reversed)

    music))

\score {
  \context Voice {
    \music
    \apply #reverse-music \music
  }
  \paper { raggedright = ##t}
}


