/* Modula-2 R10 Compiler (m2r10c)
 *
 *  m2_tokenset_literals.c
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


#include "ASCII.h"
#include "common.h"
#include "m2_tokenset_literals.h"

#include <stdlib.h> // import NULL


// ---------------------------------------------------------------------------
// Integrity checks
// ---------------------------------------------------------------------------

#if (M2_TOKENSET_BITS_PER_SEGMENT != 32)
#error "invalid value for M2_TOKENSET_BITS_PER_SEGMENT"
#endif

#if (M2_TOKENSET_SEGMENTS_PER_SET != M2_TOKENSET_LITERAL_NUM_OF_TUPLES)
#error "invalid value for M2_TOKENSET_LITERAL_NUM_OF_TUPLES"
#endif


// ---------------------------------------------------------------------------
// Total number of bits in a tokenset
// ---------------------------------------------------------------------------

#define _M2_TOKENSET_TOTAL_BITS \
    (M2_TOKENSET_BITS_PER_SEGMENT * M2_TOKENSET_SEGMENTS_PER_SET)


// ---------------------------------------------------------------------------
// private function:  _base16_digit( value )
// ---------------------------------------------------------------------------
//
// Converts <value> to a base-16 digit  and  returns its ASCII representation.
// If <value> is  greater than 15,  a C string terminator is returned instead.

static char _base16_digit(uint_fast8_t value) {
    switch (value) {
        case  0 :
        case  1 :
        case  2 :
        case  3 :
        case  4 :
        case  5 :
        case  6 :
        case  7 :
        case  8 :
        case  9 :
            return DIGIT_ZERO + (char) value;
        case 10 :
            return UPPERCASE_A;
        case 11 :
            return UPPERCASE_B;
        case 12 :
            return UPPERCASE_C;
        case 13 :
            return UPPERCASE_D;
        case 14 :
            return UPPERCASE_E;
        case 15 :
            return UPPERCASE_F;
        default :
            return CSTRING_TERMINATOR;
    } // end switch
} // end _base16_digit


// ---------------------------------------------------------------------------
// function:  m2_tokenset_to_literal( set, literal )
// ---------------------------------------------------------------------------
//
// Passes a C string containing an initialiser literal for tokenset <set> back
// in parameter <literal>.  If <set> is NULL,  an empty string is passed back.
// If <literal> is NULL,  the function returns without action.

void m2_tokenset_to_literal(m2_tokenset_t set,
                    m2_tokenset_literal_t *literal,
             m2_tokenset_literal_status_t *status) {
    uint_fast8_t index = 0, bit = 0;
    uint_fast8_t bit0, bit1, bit2, bit3;
    char *_literal = (char *) literal;
        
    // literal must not be NULL
    if (literal == NULL) {
        ASSIGN_BY_REF(status, M2_TOKENSET_LITERAL_STATUS_INVALID_REFERENCE);
        return;
    } // end if
    
    // return empty string if set is NULL
    if (set == NULL) {
        ASSIGN_BY_REF(status, M2_TOKENSET_LITERAL_STATUS_INVALID_REFERENCE);
        _literal[0] = CSTRING_TERMINATOR;
        return;
    } // end if
        
#ifdef DEBUG
    int tokens = M2_NUMBER_OF_TOKENS;
    int tokdiv32 = (tokens - 1) / M2_TOKENSET_BITS_PER_SEGMENT;
    int tuples = M2_TOKENSET_LITERAL_NUM_OF_TUPLES;
#endif
        
    while (bit < _M2_TOKENSET_TOTAL_BITS) {
        
        // if first bit in tuple ...
        if ((bit % M2_TOKENSET_BITS_PER_SEGMENT) == 0) {
            
            // add separator comma and whitespace unless first tuple
            if (bit != 0) {
                _literal[index] = COMMA;
                index++;
                _literal[index] = WHITESPACE;
                index++;
            } // end if
            
            // add ' 0x' before each tuple
            _literal[index] = DIGIT_ZERO;
            index++;
            _literal[index] = LOWERCASE_X;
            index++;
        } // end if
        
        if (bit < M2_NUMBER_OF_TOKENS)
            bit0 = m2_tokenset_is_element(set, (m2_token_t) bit);
        else
            bit0 = 0;
        bit++;
        
        if ( bit < M2_NUMBER_OF_TOKENS)
            bit1 = m2_tokenset_is_element(set, (m2_token_t) bit);
        else
            bit1 = 0;
        bit++;
        
        if ( bit < M2_NUMBER_OF_TOKENS)
            bit2 = m2_tokenset_is_element(set, (m2_token_t) bit);
        else
            bit2 = 0;
        bit++;
        
        if ( bit < M2_NUMBER_OF_TOKENS)
            bit3 = m2_tokenset_is_element(set, (m2_token_t) bit);
        else
            bit3 = 0;
        bit++;
                
        // determine the next digit in the tuple
        _literal[index] =
            _base16_digit((bit0 << 3) + (bit1 << 2) + (bit2 << 1) + bit3);
        
        if (_literal[index] == CSTRING_TERMINATOR) {
            ASSIGN_BY_REF(status, M2_TOKENSET_LITERAL_STATUS_OUT_OF_RANGE);
            _literal[0] = CSTRING_TERMINATOR;
            return;
        } // end if
        
        index++;
    } // end while
        
    // terminate the literal
    _literal[index] = CSTRING_TERMINATOR;
    
    ASSIGN_BY_REF(status, M2_TOKENSET_LITERAL_STATUS_SUCCESS);
    return;
} // end m2_tokenset_to_literal


// END OF FILE
