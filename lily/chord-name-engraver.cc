/*
  chord-name-engraver.cc -- implement Chord_name_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1998--2004 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "engraver.hh"
#include "chord-name.hh"
#include "event.hh"
#include "paper-def.hh"
#include "font-interface.hh"
#include "paper-def.hh"
#include "dimensions.hh"
#include "item.hh"
#include "pitch.hh"
#include "protected-scm.hh"
#include "context.hh"
#include "warn.hh"

class Chord_name_engraver : public Engraver 
{
  TRANSLATOR_DECLARATIONS( Chord_name_engraver);
protected:
  virtual void stop_translation_timestep ();
  virtual void process_music ();
  virtual bool try_music (Music *);
  virtual void finalize ();
private:
  void add_note (Music *);
  
  Item* chord_name_;
  Link_array<Music> notes_;
  
  Protected_scm last_chord_;
};

void
Chord_name_engraver::finalize ()
{
  last_chord_ = SCM_EOL; 
}

Chord_name_engraver::Chord_name_engraver ()
{
  chord_name_ = 0;
  last_chord_ = SCM_EOL;
}

void
Chord_name_engraver::add_note (Music * n)
{
  notes_.push (n);
}

void
Chord_name_engraver::process_music ()
{
  if (!notes_.size() )
    return;
  
  SCM bass = SCM_EOL;
  SCM inversion = SCM_EOL;
  SCM pitches = SCM_EOL;

  Music* inversion_event = 0;
  for (int i =0 ; i < notes_.size (); i++)
    {
      Music *n = notes_[i];
      SCM p = n->get_mus_property ("pitch");
      if (!unsmob_pitch (p))
	continue;
      
      if (n->get_mus_property ("inversion") == SCM_BOOL_T)
	{
	  inversion_event = n;
	  inversion = p;
	}
      else if (n->get_mus_property ("bass") == SCM_BOOL_T)
	bass = p;
      else
	pitches = gh_cons (p, pitches);
    }

  if (inversion_event)
    {
      SCM oct = inversion_event->get_mus_property ("octavation");
      if (gh_number_p (oct))
	{
	  Pitch *p = unsmob_pitch (inversion_event->get_mus_property ("pitch"));
	  int octavation =  gh_scm2int (oct);
	  Pitch orig = p->transposed (Pitch (-octavation, 0,0));
	  
	  pitches= gh_cons (orig.smobbed_copy (), pitches);
	}
      else
	programming_error ("Inversion does not have original pitch.");
    }

  pitches = scm_sort_list (pitches, Pitch::less_p_proc);

  SCM name_proc = get_property ("chordNameFunction");
  SCM markup = scm_call_4 (name_proc, pitches, bass, inversion,
			   daddy_context_->self_scm ());

  /*
    Ugh. 
   */
  SCM chord_as_scm = gh_cons (pitches, gh_cons (bass, inversion));
  
  chord_name_ = make_item ("ChordName");
  chord_name_->set_grob_property("text", markup);
  announce_grob(chord_name_, notes_[0]->self_scm ());
  SCM s = get_property ("chordChanges");
  if (to_boolean (s) && gh_pair_p (last_chord_) 
      && gh_equal_p (chord_as_scm, last_chord_))
    chord_name_->set_grob_property ("begin-of-line-visible", SCM_BOOL_T);

  last_chord_ = chord_as_scm;
}

bool
Chord_name_engraver::try_music (Music* m)
{
  /*
    hmm. Should check? 
   */
  if (m->is_mus_type ("note-event"))
    {
      add_note (m);
      return true;
    }
  return false;
}

void
Chord_name_engraver::stop_translation_timestep ()
{
  if (chord_name_)
    {
      typeset_grob (chord_name_);
    }
  chord_name_ = 0;
  notes_.clear ();
}

/*
  The READs description is not strictly accurate:
  which properties are read depend on the chord naming function active.
*/
ENTER_DESCRIPTION(Chord_name_engraver,
/* descr */       "Catch note-events "
"and generate the appropriate chordname.",
/* creats*/       "ChordName",
/* accepts */     "note-event",
/* acks  */      "",
/* reads */       "chordChanges chordNameExceptions chordNameFunction "
"chordNoteNamer chordRootNamer chordNameExceptions majorSevenSymbol",
/* write */       "");
