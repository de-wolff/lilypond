/*
  Simultaneous_music-iterator.cc -- implement Simultaneous_music_iterator

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "context.hh"
#include "warn.hh"
#include "simultaneous-music-iterator.hh"
#include "music-list.hh"
#include "context-def.hh"


Simultaneous_music_iterator::Simultaneous_music_iterator ()
{
  separate_contexts_b_ = false;
  children_list_ = SCM_EOL;
}


void
Simultaneous_music_iterator::derived_mark()const
{
  scm_gc_mark (children_list_);
}

void
Simultaneous_music_iterator::derived_substitute(Context *f,Context *t)
{
  for (SCM s = children_list_; gh_pair_p (s); s = gh_cdr(s))
    unsmob_iterator (gh_car (s))-> substitute_outlet (f,t);
}

void
Simultaneous_music_iterator::construct_children ()
{
  int j = 0;

  SCM i = get_music ()->get_mus_property ("elements");

  children_list_ = SCM_EOL;
  SCM * tail = &children_list_;
  for (; gh_pair_p (i); i = ly_cdr (i), j++)
    {
      Music *mus = unsmob_music (ly_car (i));

      SCM scm_iter = get_static_get_iterator (mus);
      Music_iterator * mi = unsmob_iterator (scm_iter);

      /* if separate_contexts_b_ is set, create a new context with the
	 number number as name */

      SCM name = unsmob_context_def (get_outlet ()->definition_)->get_context_name ();
      Context * t = (j && separate_contexts_b_)
	? get_outlet ()->find_create_context (name, to_string (j), SCM_EOL)
	: get_outlet ();

      if (!t)
	t = get_outlet ();

      mi->init_translator (mus, t);
      mi->construct_children ();

      if (mi->ok ()) 
	{
	  *tail = scm_cons (scm_iter, *tail);
	  tail = SCM_CDRLOC (*tail);
	}
      else
	mi->set_translator (0);
    }
}

void
Simultaneous_music_iterator::process (Moment until)
{
  SCM *proc = &children_list_; 
  while(gh_pair_p (*proc))
    {
      Music_iterator * i = unsmob_iterator (gh_car (*proc));
      if (i->run_always ()
	  || i->pending_moment () == until) 
	{
	  i->process (until);
	}
      if (!i->ok ())
	{
	  i->quit ();
	  *proc = gh_cdr (*proc);
	}
      else
	{
	  proc = SCM_CDRLOC(*proc);
	}
    }
}

Moment
Simultaneous_music_iterator::pending_moment () const
{
  Moment next;
  next.set_infinite (1);
  
  for (SCM s = children_list_; gh_pair_p (s); s = gh_cdr(s))
    {
      Music_iterator * it = unsmob_iterator (gh_car (s));
      next = next <? it->pending_moment ();
    }
  
  return next;
}

bool
Simultaneous_music_iterator::ok () const
{
  bool run_always_ok = false; 
  for (SCM s = children_list_; gh_pair_p (s); s = gh_cdr(s))
    {
      Music_iterator * it = unsmob_iterator (gh_car (s));
      if (!it->run_always ())
	return true;
      else
	run_always_ok =  run_always_ok || it->ok ();
    }
  return run_always_ok;
}

bool
Simultaneous_music_iterator::run_always () const
{
  for (SCM s = children_list_; gh_pair_p (s); s = gh_cdr(s))
    {
      Music_iterator * it = unsmob_iterator (gh_car (s));
      if (it->run_always ())
	return true;
    }
  return false;
}

Music_iterator*
Simultaneous_music_iterator::try_music_in_children (Music *m) const
{
  Music_iterator * b=0;
  for (SCM s = children_list_; !b && gh_pair_p (s); s = gh_cdr(s))
    b =unsmob_iterator (gh_car (s))->try_music (m);
  return b;
}

void
Simultaneous_music_iterator::do_quit ()
{
  for (SCM s = children_list_; gh_pair_p (s); s = gh_cdr(s))
    unsmob_iterator (gh_car (s))->quit();
}


IMPLEMENT_CTOR_CALLBACK (Simultaneous_music_iterator);
