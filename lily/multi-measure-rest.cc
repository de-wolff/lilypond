/*   
  multi-measure-rest.cc --  implement Multi_measure_rest
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--1999 Jan Nieuwenhuizen <janneke@gnu.org>
  
 */

#include "multi-measure-rest.hh"
#include "debug.hh"
#include "paper-def.hh"
#include "paper-column.hh" // urg
#include "bar.hh"
#include "lookup.hh"
#include "rest.hh"
#include "molecule.hh"
#include "misc.hh"


Multi_measure_rest::Multi_measure_rest ()
{
  measures_i_ = 0;
}

void
Multi_measure_rest::do_print () const
{
#ifndef NPRINT
  DOUT << "measures_i_ " << measures_i_;
#endif
}



/*
   [TODO]                                          17
 * variable-sized multi-measure rest symbol: |====| ??
 
 * build 3, 5, 6, 7, 8 symbols (how far, property?)
       from whole, brevis and longa rests

*/
Molecule*
Multi_measure_rest::do_brew_molecule_p () const
{
  Interval sp_iv;
  Direction d = LEFT;
  do
    {
      Item * col = spanned_drul_[d]->column_l ();

      Interval coldim = col->extent (X_AXIS);

      sp_iv[d] = coldim[-d]  ;
    }
  while ((flip (&d)) != LEFT);
  Molecule *mol_p  = new Molecule;
  Real x_off = 0.0;


  //  Dimension_cache * col_ref = spanned_drul_[LEFT]->column_l ()->dim_cache_[X_AXIS];

  Real rx  = spanned_drul_[LEFT]->absolute_coordinate (X_AXIS);
  /*
    we gotta stay clear of sp_iv, so move a bit to the right if
    needed.
   */
  x_off += (sp_iv[LEFT] -  rx) >? 0;

  /*
    center between stuff.
   */
  x_off += sp_iv.length ()/ 2;

  
  Molecule s;
  bool rest_symbol=true;
  SCM alt_symbol_sym =get_elt_property (alt_symbol_scm_sym);
  if (alt_symbol_sym != SCM_BOOL_F)
    {
      s = lookup_l () -> afm_find (ly_scm2string (SCM_CDR(alt_symbol_sym)));
      rest_symbol = false;
    }
  else if (measures_i_ == 1 || measures_i_ == 2 || measures_i_ == 4) 
    {
      s = lookup_l ()->rest (- intlog2(measures_i_), 0, "");
      s.translate_axis (-s.extent ()[X_AXIS].length () / 2, X_AXIS);
    }
  else 
    {
      s = lookup_l ()->rest (-4, 0, "");
    }
  mol_p->add_molecule (s);
  Real interline_f = staff_line_leading_f ();
  if (measures_i_ == 1 && rest_symbol)
    {
      mol_p->translate_axis (interline_f, Y_AXIS);
    }
  else if (measures_i_ > 1)
    {
      Molecule s ( lookup_l ()->text ("number", to_str (measures_i_), paper_l ()));
      s.align_to (X_AXIS, CENTER);
      s.translate_axis (3.0 * interline_f, Y_AXIS);
      mol_p->add_molecule (s);
    }
  mol_p->translate_axis (x_off, X_AXIS);
  return mol_p;
}

void
Multi_measure_rest::do_add_processing ()
{
  if (column_arr_.size ())
    {
      set_bounds (LEFT, column_arr_[0 >? column_arr_.size () - 2]);
      set_bounds (RIGHT, column_arr_[column_arr_.size () - 1]);
    }
}
  
void
Multi_measure_rest::do_post_processing ()
{
  if (!column_arr_.size ())
    set_elt_property (transparent_scm_sym, SCM_BOOL_T);
}


void
Multi_measure_rest::do_substitute_element_pointer (Score_element* o, Score_element* n)
{
  Staff_symbol_referencer::do_substitute_element_pointer (o,n);
  if (Item* c = dynamic_cast <Item*> (o))
    column_arr_.substitute (c, dynamic_cast<Item*> (n));
}
  
void
Multi_measure_rest::add_column (Item* c)
{
  column_arr_.push (c);
  add_dependency (c);
}


Array<Rod>
Multi_measure_rest::get_rods () const
{
  Array<Rod> a;

  if (!(spanned_drul_[LEFT] && spanned_drul_[RIGHT]))
    {
      programming_error ("Multi_measure_rest::get_rods (): I am not spanned!");
      return a;
    }

  Item * l = spanned_drul_[LEFT]->column_l ();
  Item * r = spanned_drul_[RIGHT]->column_l ();
  Item * lb = l->find_prebroken_piece (RIGHT);
  Item * rb = r->find_prebroken_piece (LEFT);      
  
  Item* combinations[4][2]={{l,r}, {lb,r}, {l,rb},{lb,rb}};
  for (int i=0; i < 4; i++)
    {
      Item * l =  combinations[i][0];
      Item *r = combinations[i][1];

      if (!l || !r)
	continue;

      Rod rod;
      rod.item_l_drul_[LEFT] = l;
      rod.item_l_drul_[RIGHT] = r;

	/*
	  should do something more advanced.
	 */
      rod.distance_f_ = l->extent (X_AXIS)[BIGGER] - r->extent (X_AXIS)[SMALLER]
	+ paper_l ()->get_var ("mmrest_x_minimum");
  
      a.push (rod);
    }
  
  return a;
}
