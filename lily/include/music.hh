/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2017 Han-Wen Nienhuys <hanwen@xs4all.nl>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef MUSIC_HH
#define MUSIC_HH

#include "smobs.hh"
#include "moment.hh"
#include "pitch.hh"
#include "prob.hh"

#define is_mus_type(x) internal_is_music_type (ly_symbol2scm (x))

struct Preinit_Music {
  SCM length_callback_;
  SCM start_callback_;
  Preinit_Music ();
};

class Music : Preinit_Music, public Prob
{
public:
  Music (SCM init);
  Music (Music const &m);
  VIRTUAL_COPY_CONSTRUCTOR (Music, Music);

  Input *origin () const;
  void set_spot (Input);

  bool internal_is_music_type (SCM) const;

  Stream_event *to_event () const;

  DECLARE_SCHEME_CALLBACK (relative_callback, (SCM, SCM));
  Pitch to_relative_octave (Pitch);
  Pitch generic_to_relative_octave (Pitch);
  Moment get_length () const;
  Moment start_mom () const;
  void print () const;

  /// Scale the music in time by #factor#.
  void compress (Moment factor);

  // Broadcast the event in a context's event-source.
  void send_to_context (Context *c);

  DECLARE_SCHEME_CALLBACK (duration_length_callback, (SCM));

protected:
  virtual SCM copy_mutable_properties () const;
  virtual void type_check_assignment (SCM, SCM) const;
  virtual void derived_mark () const;
protected:
  friend SCM ly_extended_make_music (SCM, SCM);
};

Music *make_music_by_name (SCM sym);
SCM music_deep_copy (SCM m);
void set_origin (SCM m, SCM origin);

SCM ly_camel_case_2_lisp_identifier (SCM name_sym);

extern SCM ly_music_p_proc;

#endif /* MUSIC_HH */
