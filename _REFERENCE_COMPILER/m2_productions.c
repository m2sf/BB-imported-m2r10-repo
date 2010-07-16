/* Modula-2 R10 Compiler (m2r10c)
 *
 *  m2_productions.c
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

#include <stdlib.h>
#include "common.h"
#include "m2_productions.h"


// ---------------------------------------------------------------------------
// Human readable names of productions
// ---------------------------------------------------------------------------
//
// Only the first macro parameter is used,  all other parameters are ignored.

typedef char *m2_production_name_t;

#define _add_production(_production, \
    _fi0, _fi1, _fi2, _fo0, _fo1, _fo2) \
    static const char _PRODNAM ## _production[] = # _production EMPTY_STRING;

#include "m2_table_of_productions.h"

#undef _add_production


// ---------------------------------------------------------------------------
// Table of human readable production names
// ---------------------------------------------------------------------------
//
// Only the first macro parameter is used,  all other parameters are ignored.
// Each string's address is incremented to skip the preceeding underscore.

#define _add_production(_production, \
    _fi0, _fi1, _fi2, _fo0, _fo1, _fo2) \
    (char *)&_PRODNAM ## _production + 1,

static const m2_production_name_t m2_production_name_str[] = {
#include "m2_table_of_productions.h"
} /* m2_production_name_str */;

#undef _add_production


// ---------------------------------------------------------------------------
// function:  m2_production_name( production )
// ---------------------------------------------------------------------------
//
// Returns the human readable name for production <production>.
// Returns NULL if the value of <production> is invalid.

const char *m2_production_name(m2_production_t production) {
    
    if (production < M2_NUMBER_OF_PRODUCTIONS)
        return m2_production_name_str[production];
    else 
        return NULL;
    
} // end m2_production_name


// END OF FILE
