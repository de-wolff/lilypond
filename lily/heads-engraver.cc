/*
  head-grav.cc -- part of GNU LilyPond

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "note-head.hh"
#include "heads-engraver.hh"
#include "paper-def.hh"
#include "musical-request.hh"
#include "dots.hh"
#include "dot-column.hh"

Note_heads_engraver::Note_heads_engraver()
{
}

bool
Note_heads_engraver::do_try_music (Music *req_l) 
{
  if (Note_req * n =dynamic_cast <Note_req *> (req_l))
    {
      note_req_l_arr_.push (n);
      return true;
    }
  return false;
}

void
Note_heads_engraver::do_process_requests()
{
  if (note_p_arr_.size ())
    return ;
  
  for (int i=0; i < note_req_l_arr_.size (); i++)
    {
      Note_head *note_p  = new Note_head;
      Note_req * note_req_l = note_req_l_arr_[i];
      note_p->balltype_i_ = note_req_l->duration_.durlog_i_;
      note_p->dots_i_ = note_req_l->duration_.dots_i_;
      if (note_p->dots_i_)
	{
	  Dots * d = new Dots;
	  note_p->dots_l_ = d;
	  announce_element (Score_element_info (d,0));
	  dot_p_arr_.push (d);
	}

      note_p->steps_i_ = note_req_l->pitch_.steps ();
      //      note_p->position_i_ = note_req_l->pitch_.steps ();

      String noteheadstyle = get_property ("noteheadStyle", 0);
      if (noteheadstyle.length_i ())
        note_p->note_head_type_str_ = noteheadstyle;
  
      Score_element_info itinf (note_p,note_req_l);
      announce_element (itinf);
      note_p_arr_.push (note_p);
    }
}
 
void
Note_heads_engraver::do_pre_move_processing()
{
  for (int i=0; i < note_p_arr_.size (); i++)
    {
      typeset_element (note_p_arr_[i]);
    }
  note_p_arr_.clear ();
  for (int i=0; i < dot_p_arr_.size (); i++)
    {
      typeset_element (dot_p_arr_[i]);
    }
  dot_p_arr_.clear ();
  
  note_req_l_arr_.clear ();
}

void
Note_heads_engraver::do_post_move_processing()
{

}



ADD_THIS_TRANSLATOR(Note_heads_engraver);
