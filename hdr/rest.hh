/*
  rest.hh -- part of LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef REST_HH
#define REST_HH
#include "item.hh"

/// ball at the end of the stem
struct Rest : Item {

    int dots;
    int balltype;

    /****************/


    Rest(int dur,int dots);
    void do_print()const;
    const char * name() const;
    Molecule* brew_molecule_p()const;
};
/**
  takes care of:

  * help lines  
  * proper placing of dots 

  */
#endif 

