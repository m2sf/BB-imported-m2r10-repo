/* Modula-2 R10 Compiler (m2r10c)
 *
 *  m2_productions.h
 *  List of productions
 *
 *  Author: Benjamin Kowarsch
 *
 *  Copyright (C) 2010 B.Kowarsch. All rights reserved.
 *
 *  License:
 *
 *  Permission is hereby granted to review and test this software for the sole
 *  purpose of supporting the effort by the licensor  to implement a reference
 *  compiler for  Modula-2 R10.  It is not permissible under any circumstances
 *  to  use the software  for the purpose  of creating derivative languages or 
 *  dialects.  This permission is valid until 31 December 2010, 24:00h GMT.
 *
 *  Future licensing:
 *
 *  The licensor undertakes  to release  this software  under a BSD-style open
 *  source license  AFTER  the M2R10 language definition has been finalised.
 *  
 */


#ifndef M2_PRODUCTIONS_H
#define M2_PRODUCTIONS_H


// ---------------------------------------------------------------------------
// Enumeration type containing all productions
// ---------------------------------------------------------------------------

#define _add_production(_production, \
    _fi0, _fi1, _fi2, _fi3, _fo0, _fo1, _fo2, _fo3) p ## _production,
typedef enum /* m2_production_t */ {
#include "m2_table_of_productions.h"
M2_NUMBER_OF_PRODUCTIONS
} m2_production_t;
#undef _add_production


// ---------------------------------------------------------------------------
// function:  m2_production_name( production )
// ---------------------------------------------------------------------------
//
// Returns the human readable name for production <production>.
// Returns NULL if the value of <production> is invalid.

const char *m2_production_name(m2_production_t production);


#endif /* M2_PRODUCTIONS_H */


// END OF FILE