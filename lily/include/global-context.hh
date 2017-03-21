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

#ifndef GLOBAL_CONTEXT_HH
#define GLOBAL_CONTEXT_HH

#include "context.hh"
#include "pqueue.hh"

struct Preinit_Global_context
{
  Output_def *output_def_;
  Preinit_Global_context ();
};

class Global_context : Preinit_Global_context, public Context
{
  PQueue<Moment> extra_mom_pq_;
  virtual void derived_mark () const;

  DECLARE_CLASSNAME (Global_context);

  friend class Output_def;
public:
  Global_context (Output_def *);
  int get_moments_left () const;
  Moment sneaky_insert_extra_moment (Moment);
  void add_moment_to_process (Moment);
  void run_iterator_on_me (Music_iterator *);
  virtual Context *get_score_context () const;

  void apply_finalizations ();
  void add_finalization (SCM);

  void prepare (SCM);
  virtual SCM get_output ();
  virtual Output_def *get_output_def () const;
  virtual Moment now_mom () const;
  virtual Context *get_default_interpreter (const string &context_id = "");

  Moment previous_moment () const;
protected:
  Moment prev_mom_;
  Moment now_mom_;
};

SCM ly_format_output (SCM);

#endif // GLOBAL_CONTEXT_HH
