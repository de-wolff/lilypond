/*
  collision.cc -- implement Collision

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "debug.hh"
#include "collision.hh"
#include "note-column.hh"
#include "note-head.hh"
#include "paper-def.hh"

Collision::Collision()
{
  set_axes (X_AXIS, Y_AXIS);
}

void
Collision::add_column (Note_column* ncol_l)
{
  clash_l_arr_.push (ncol_l);
  add_element (ncol_l);
  add_dependency (ncol_l);
}


/** This complicated routine moves note columns around horizontally to
  ensure that notes don't clash.

  This should be done better, probably.

  TODO: forced hshift
  
  */
void
Collision::do_pre_processing()
{
  Drul_array<Link_array<Note_column> > clash_groups;
  Drul_array<Array<int> > shifts;
  
  for (int i=0; i < clash_l_arr_.size(); i++)
    {
      clash_groups[clash_l_arr_[i]->dir ()].push (clash_l_arr_[i]);
    }

  
  Direction d = UP;
  do
    {
      Array<int> & shift (shifts[d]);
      Link_array<Note_column> & clashes (clash_groups[d]);

      clashes.sort (Note_column::shift_compare);

      for (int i=0; i < clashes.size (); i++)
	{
	  SCM sh
	    = clashes[i]->remove_elt_property (horizontal_shift_scm_sym);

	  if (sh == SCM_BOOL_F)
	    shift.push (0);
	  else
	    shift.push (gh_scm2int (SCM_CDR (sh)));
	}
      
      for (int i=1; i < shift.size (); i++)
	{
	  if (shift[i-1] == shift[i])
	    {
	      warning (_ ("Too many clashing notecolumns. Ignoring them."));
	      return;
	    }
	}
    }
  while ((flip (&d))!= UP);

  Drul_array< Array < Slice > > extents;
  Drul_array< Array < Real > > offsets;
  d = UP;
  do
    {
      for (int i=0; i < clash_groups[d].size (); i++)
	{
	  Slice s(clash_groups[d][i]->head_positions_interval ());
	  s[LEFT] --;
	  s[RIGHT]++;
	  extents[d].push (s);
	  offsets[d].push (d * 0.5 * i);
	}
    }
  while ((flip (&d))!= UP);
  
  do
    {
      for (int i=1; i < clash_groups[d].size (); i++)
	{
	  Slice prev =extents[d][i-1];
	  prev.intersect (extents[d][i]);
	  if (prev.length ()> 0 ||
	      (extents[-d].size () && d * (extents[d][i][-d] - extents[-d][0][d]) < 0))
	    for (int j = i; j <  clash_groups[d].size (); j++)
	      offsets[d][j] += d * 0.5;
	}
    }	
  while ((flip (&d))!= UP);

  /*
    if the up and down version are close, and can not be merged, move
    all of them again. */
  if (extents[UP].size () && extents[DOWN].size ())
    {
      Note_column *cu_l =clash_groups[UP][0];
      Note_column *cd_l =clash_groups[DOWN][0];
      Note_head * nu_l= cu_l->head_l_arr_[0];
      Note_head * nd_l = cd_l->head_l_arr_.top();
      int downpos = 	cd_l->head_positions_interval ()[SMALLER];
      int uppos = 	cu_l->head_positions_interval ()[BIGGER];      
      
      bool merge  =
	downpos == uppos
	&& nu_l->balltype_i_ == nd_l->balltype_i_
	&& nu_l->dots_i_ == nd_l->dots_i_;

      /*
	notes are close, but can not be merged.  Shift
       */
      if (abs(uppos - downpos) < 2 && !merge)
	  do
	  {
	    for (int i=0; i < clash_groups[d].size (); i++)
	      {
		offsets[d][i] -= d * 0.5;
	      }
	  }
	  while ((flip (&d))!= UP);
    }

  Real wid_f = paper_l ()->note_width ();
  do
    {
      for (int i=0; i < clash_groups[d].size (); i++)
	{
	  clash_groups[d][i]->translate_axis (offsets[d][i]*wid_f, X_AXIS);
	}
    }
  while (flip (&d) != UP);
}


void
Collision::do_substitute_element_pointer (Score_element*o_l,Score_element*n_l)
{
  if (o_l)
    {
      clash_l_arr_.substitute (dynamic_cast<Note_column *> (o_l),
			       dynamic_cast <Note_column *> (n_l));

    }
}
