/* Modula-2 R10 Compiler (m2r10c)
 *
 *  m2_tokenset_literals.h
 *  Tokenset literal generator
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


#include "m2_tokens.h"
#include "m2_tokenset.h"


// --------------------------------------------------------------------------
// Status codes
// --------------------------------------------------------------------------

typedef enum /* objm2_tokenset_literal_status_t */ {
    M2_TOKENSET_LITERAL_STATUS_SUCCESS = 0,
    M2_TOKENSET_LITERAL_STATUS_INVALID_REFERENCE,
    M2_TOKENSET_LITERAL_STATUS_LIBRARY_MISMATCH,
    M2_TOKENSET_LITERAL_STATUS_OUT_OF_RANGE
} m2_tokenset_literal_status_t;


// --------------------------------------------------------------------------
// Size and string type for tokenset literals
// --------------------------------------------------------------------------
//
// tokenset-literal := tuple ( ", " tuple )*
//   
// tuple := "0x" digit digit digit digit digit digit digit digit
//
// digit := "0" .. "9" | "A" .. "F"
//
// length of literal = tuples * 10 + (tuples - 1) * 2
// number of tuples = number of tokenset segments
// storage size = length + 1

#define M2_TOKENSET_LITERAL_LENGTH \
    ((M2_TOKENSET_LITERAL_NUM_OF_TUPLES * 10) \
    + ((M2_TOKENSET_LITERAL_NUM_OF_TUPLES - 1) * 2))

#define M2_TOKENSET_LITERAL_NUM_OF_TUPLES \
    M2_TOKENSET_SEGMENTS_PER_SET

typedef char m2_tokenset_literal_t[M2_TOKENSET_LITERAL_LENGTH + 1];


// ---------------------------------------------------------------------------
// function:  m2_tokenset_to_literal( set, literal, status )
// ---------------------------------------------------------------------------
//
// Passes a C string containing an initialiser literal for tokenset <set> back
// in parameter <literal>.  If <set> is NULL,  an empty string is passed back.
// If <literal> is NULL,  the function returns without action.

void m2_tokenset_to_literal(m2_tokenset_t set,
                    m2_tokenset_literal_t *literal,
             m2_tokenset_literal_status_t *status);


// END OF FILE
