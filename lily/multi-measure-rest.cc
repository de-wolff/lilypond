/*   
  multi-measure-rest.cc --  implement Multi_measure_rest
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2002 Jan Nieuwenhuizen <janneke@gnu.org>
  
 */

#include "multi-measure-rest.hh"
#include "debug.hh"
#include "paper-def.hh"
#include "paper-column.hh" // urg
#include "font-interface.hh"
#include "rest.hh"
#include "molecule.hh"
#include "misc.hh"
#include "spanner.hh"
#include "staff-symbol-referencer.hh"
#include "text-item.hh"
#include "percent-repeat-item.hh"
#include "lookup.hh"

bool
Multi_measure_rest::has_interface (Grob*me)
{
  return me->has_interface (ly_symbol2scm ("multi-measure-rest-interface"));
}

MAKE_SCHEME_CALLBACK (Multi_measure_rest,percent,1);
SCM
Multi_measure_rest::percent (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  Spanner *sp = dynamic_cast<Spanner*> (me);
  
  Molecule r = Percent_repeat_item_interface::x_percent (me, 1,  0.75, 1.6);

  // ugh copy & paste.
  
  Interval sp_iv;
  Direction d = LEFT;
  do
    {
      Item * col = sp->get_bound (d)->column_l ();

      Interval coldim = col->extent (0, X_AXIS);

      sp_iv[d] = coldim[-d]  ;
    }
  while ((flip (&d)) != LEFT);
  Real x_off = 0.0;

  Real rx  = sp->get_bound (LEFT)->relative_coordinate (0, X_AXIS);
  /*
    we gotta stay clear of sp_iv, so move a bit to the right if
    needed.
   */
  x_off += (sp_iv[LEFT] -  rx) >? 0;

  /*
    center between stuff.
   */
  x_off += sp_iv.length ()/ 2;

  r.translate_axis (x_off,X_AXIS);

  
  return r.smobbed_copy ();
}


/*
   [TODO]                                      17
   variable-sized multi-measure rest symbol: |====| ??
*/
MAKE_SCHEME_CALLBACK (Multi_measure_rest,brew_molecule,1);
SCM
Multi_measure_rest::brew_molecule (SCM smob) 
{
  Grob *me = unsmob_grob (smob);
  Spanner * sp = dynamic_cast<Spanner*> (me);

  SCM alist_chain = Font_interface::font_alist_chain (me);

  Interval sp_iv;
  Direction d = LEFT;

  Grob *common = sp->get_bound (LEFT)->common_refpoint (sp->get_bound (RIGHT), X_AXIS);
  do
    {
      Item * col = sp->get_bound (d)->column_l ();

      Interval coldim = col->extent (common, X_AXIS);

      sp_iv[d] = coldim[-d]  ;
    }
  while ((flip (&d)) != LEFT);

  Real space = sp_iv.length();

  Real rx  = sp->get_bound (LEFT)->relative_coordinate (0, X_AXIS);
  /*
    we gotta stay clear of sp_iv, so move a bit to the right if
    needed.
   */
  Real x_off = (sp_iv[LEFT] -  rx) >? 0;


  Molecule mol;
  mol.add_molecule (symbol_molecule (me, space));

  int measures = 0;
  SCM m (me->get_grob_property ("measure-count"));
  if (gh_number_p (m))
    {
      measures = gh_scm2int (m);
    }

  if (measures > 1)
    {
      Molecule s = Text_item::text2molecule (me,
					     ly_str02scm (to_str (measures).ch_C ()),
					     alist_chain);

      
      s.align_to (X_AXIS, CENTER);
      s.translate_axis (3.0, Y_AXIS);

      s.translate_axis (mol.extent (X_AXIS).center (),  X_AXIS);
      mol.add_molecule (s);
    }
  mol.translate_axis (x_off, X_AXIS);
  return mol.smobbed_copy ();
}


Molecule
Multi_measure_rest::symbol_molecule (Grob *me, Real space)
{
  int measures = 0;
  SCM m (me->get_grob_property ("measure-count"));
  if (gh_number_p (m))
    {
      measures = gh_scm2int (m);
    }
  

  SCM limit = me->get_grob_property ("expand-limit");
  if (measures <= 0)
    return Molecule();

  if (measures > gh_scm2int (limit))
    {
      Real padding = 0.15;  
      Molecule s =  big_rest (me, (1.0 - 2*padding) * space);
      s.translate_axis (padding * space,  X_AXIS); 
      return s;
    }

  SCM alist_chain = Font_interface::font_alist_chain (me);

  SCM style_chain =
    Font_interface::add_style (me, ly_symbol2scm ("mmrest-symbol"),
			       alist_chain);

  Real staff_space = Staff_symbol_referencer::staff_space (me);
  Font_metric *musfont
    = Font_interface::get_font (me,style_chain);

  if (measures == 1)
    {
      Molecule s = musfont->find_by_name (Rest::glyph_name (me, 0, ""));

      /*
	ugh.
       */
      if (Staff_symbol_referencer::position_f (me) == 0.0)
	s.translate_axis (staff_space, Y_AXIS);

      s.translate_axis ((space - s.extent (X_AXIS).length ())/2, X_AXIS);
      
      return s ;
    }
  else
    {
      return  church_rest (me, musfont, measures, space);
    }
}



Molecule
Multi_measure_rest::big_rest (Grob *me, Real width)
{
  Real thick = gh_scm2double (me->get_grob_property ("thickness"));
  Real ss = Staff_symbol_referencer::staff_space (me);
  
  Real slt = me->paper_l ()->get_var ("stafflinethickness");
  Real y = slt * thick/2 * ss;
  Box b(Interval (0, width), Interval (-y, y));
  Real ythick = slt * ss;
  
  Molecule m =  Lookup::filledbox (b);
  Molecule yb = Lookup::filledbox (Box (Interval (-ythick, ythick), Interval (-ss, ss)));

  m.add_at_edge (X_AXIS, RIGHT, yb, -ythick);
  m.add_at_edge (X_AXIS, LEFT, yb, -ythick);

  m.align_to (X_AXIS, LEFT);
  
  return m;
}

/*
  Kirchenpause (?)
 */
Molecule
Multi_measure_rest::church_rest (Grob*me, Font_metric *musfont, int measures,
				 Real space)
{
  SCM mols = SCM_EOL; 

  /*
   see Wanske pp. 125
  */
  int l = measures;
  int count = 0;
  Real symbols_width = 0.0;
  while (l)
    {
      int k;
      if (l >= 4)
	{
	  l-=4;
	  k = -2;
	}
      else if (l>= 2)
	{
	  l -= 2;
	  k = -1;
	}
      else
	{
	  k = 0;
	  l --;
	}

      Molecule r (musfont->find_by_name ("rests-" + to_str (k)));
      if (k == 0)
	{
	  Real staff_space = Staff_symbol_referencer::staff_space (me);
	  r.translate_axis (staff_space, Y_AXIS);
	}
      symbols_width += r.extent (X_AXIS).length ();
      mols = gh_cons (r.smobbed_copy (), mols);
      count ++;
    }


  Real outer_padding_factor = 1.5; //     make outer padding this much bigger.
  Real inner_padding = (space - symbols_width) / (2 * outer_padding_factor + (count-1)); 

  Molecule mol; 
  for (SCM  s = mols; gh_pair_p (s); s = gh_cdr(s))
    {
      mol.add_at_edge (X_AXIS, LEFT, *unsmob_molecule (gh_car (s)), inner_padding);
    }
  mol.align_to (X_AXIS, LEFT);
  mol.translate_axis (outer_padding_factor *  inner_padding, X_AXIS);

  return mol;
}

void
Multi_measure_rest::add_column (Grob*me,Item* c)
{
  add_bound_item (dynamic_cast<Spanner*> (me),c);
}


MAKE_SCHEME_CALLBACK (Multi_measure_rest, set_spacing_rods,1);

SCM
Multi_measure_rest::set_spacing_rods (SCM smob)
{
  return SCM_UNSPECIFIED;

  Grob*me = unsmob_grob (smob);

  Spanner*sp = dynamic_cast<Spanner*> (me);
  if (! (sp->get_bound (LEFT) && sp->get_bound (RIGHT)))
    {
      programming_error ("Multi_measure_rest::get_rods (): I am not spanned!");
      return SCM_UNSPECIFIED;
    }

  Item * l = sp->get_bound (LEFT)->column_l ();
  Item * r = sp->get_bound (RIGHT)->column_l ();
  Item * lb = l->find_prebroken_piece (RIGHT);
  Item * rb = r->find_prebroken_piece (LEFT);      
  
  Item* combinations[4][2]={{l,r}, {lb,r}, {l,rb},{lb,rb}};
  Real staff_space = Staff_symbol_referencer::staff_space (me);
  for (int i=0; i < 4; i++)
    {
      Item * l =  combinations[i][0];
      Item *r = combinations[i][1];

      if (!l || !r)
	continue;

      Rod rod;
      rod.item_l_drul_[LEFT] = l;
      rod.item_l_drul_[RIGHT] = r;
      rod.distance_f_ = l->extent (l, X_AXIS)[BIGGER] - r->extent (r, X_AXIS)[SMALLER]
	+ 4.0;			// magic!
  
      rod.add_to_cols ();
    }
  return SCM_UNSPECIFIED;
}



ADD_INTERFACE (Multi_measure_rest,"multi-measure-rest-interface",
  "A rest that spans a whole number of measures.  For typesetting the
numbers, fields from font-interface may be used.

padding is the space between number and rest. Measured in staffspace.

",
  "thickness measure-count expand-limit padding");
