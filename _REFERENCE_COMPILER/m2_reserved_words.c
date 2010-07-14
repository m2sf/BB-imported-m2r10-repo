/* Modula-2 R10 Compiler (m2r10c)
 *
 *  m2_reserved_words.c
 *  Recogniser for rerserved words
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


#include "common.h"
#include "m2_reserved_words.h"


// ---------------------------------------------------------------------------
// Reserved word hash table
// ---------------------------------------------------------------------------

#define _add_reserved_word(_suffix, _hash) _hash,

static const cardinal _m2_reserved_word[] = {
0, // leading null-entry
// insert hashes from master table
#include "m2_table_of_reserved_words.h"
0 // trailing null-entry
} /* end _m2_reserved_word */;

#undef _add_reserved_word


// ---------------------------------------------------------------------------
// Perfect Hash to Token Index mapping table
// ---------------------------------------------------------------------------
//
// This table maps  intermediate  perfect hash codes to alphabetically ordered
// minimal perfect hash codes between  1  and  M2_NUMBER_OF_RESERVED_WORDS, it
// further maps all invalid codes to M2_NUMBER_OF_RESERVED_WORDS + 1.

#define _N M2_NUMBER_OF_RESERVED_WORDS + 1

static const char _m2_perfect_hash_to_index_map[] = {
//            0    1    2    3    4    5    6    7    8    9
/*  00 */    _N,  _N,  _N,  _N,  _N,  _N,  _N,  _N,  _N,  18,
/*  10 */    _N,  13,  _N,  _N,  _N,  17,  _N,  37,  _N,  41,
/*  20 */     1,  _N,  _N,  32,  _N,  _N,  _N,  _N,  _N,   3,
/*  30 */    _N,  _N,  _N,  _N,  38,  _N,  _N,  _N,  _N,  _N,
/*  40 */    _N,  _N,  _N,  _N,  _N,  _N,  _N,  _N,  _N,  _N,
/*  50 */    _N,  _N,  _N,  _N,  _N,   2,  36,  _N,  _N,  16,
/*  60 */    _N,  20,  _N,  _N,  _N,  29,  _N,  _N,  _N,  23,
/*  70 */    _N,   9,  24,  22,  _N,  _N,  _N,  _N,  _N,  _N,
/*  80 */    _N,   4,  _N,  _N,  _N,  _N,  _N,  _N,  _N,  _N,
/*  90 */    _N,  _N,  _N,  _N,  _N,  43,  _N,  _N,  _N,  _N,
/* 100 */    _N,  _N,  _N,  _N,  _N,  _N,  26,  _N,  _N,  _N,
/* 110 */    _N,  _N,  _N,  _N,  _N,  _N,   7,  _N,  _N,  _N,
/* 120 */    _N,  14,  _N,  _N,  _N,  _N,  40,  _N,  _N,  _N,
/* 130 */    _N,   8,  _N,  _N,  _N,  _N,  15,  _N,  _N,  _N,
/* 140 */    _N,  27,  _N,  _N,  _N,  12,  _N,  _N,  _N,  _N,
/* 150 */    _N,   6,  _N,  _N,  _N,  _N,  _N,  35,  _N,  _N,
/* 160 */    _N,  _N,  _N,  _N,  _N,  _N,  _N,  _N,  _N,  19,
/* 170 */    _N,  42,  _N,  _N,  _N,  _N,  _N,  34,  _N,  10,
/* 180 */    _N,  _N,  _N,  28,  _N,  _N,  _N,  _N,  _N,  21,
/* 190 */    _N,  _N,  _N,  _N,  _N,  30,  _N,  25,  _N,  _N,
/* 200 */    _N,   5,  _N,  _N,  _N,  _N,  _N,  _N,  _N,  _N,
/* 210 */    _N,  _N,  _N,  _N,  _N,  _N,  _N,  _N,  _N,  _N,
/* 220 */    _N,  31,  _N,  _N,  _N,  33,  _N,  _N,  _N,  _N,
/* 230 */    _N,  _N,  _N,  _N,  _N,  _N,  44,  _N,  _N,  _N,
/* 240 */    _N,  _N,  _N,  _N,  _N,  _N,  _N,  _N,  _N,  _N,
/* 250 */    _N,  _N,  11,  _N,  _N,  39 // last index is 255
}; // end _m2_perfect_hash_to_index_map

#undef _N


// ---------------------------------------------------------------------------
// function:  m2_stored_hash_for_reserved_word_token(token_index)
// ---------------------------------------------------------------------------
//
// Returns the  stored hash value  for the reserved word  whose token index is
// <token_index>.  Returns zero if <token_index> is greater than the number of
// as defined by M2_NUMBER_OF_RESERVED_WORDS.

cardinal m2_hash_for_reserved_word_token(cardinal token_index) {

    if (token_index > M2_NUMBER_OF_RESERVED_WORDS)
        return 0;
    else
        return _m2_reserved_word[token_index];

} // end m2_stored_hash_for_reserved_word_token


// ---------------------------------------------------------------------------
// function:  m2_token_for_reserved_word_hash(hash)
// ---------------------------------------------------------------------------
//
// Returns the  token index  for the reserved word whose hash value is <hash>.
// Returns zero if <hash> is zero,  returns M2_NUMBER_OF_RESERVED_WORDS + 1 if
// <hash> does not represent any reserved word.

cardinal m2_token_for_reserved_word_hash(cardinal hash) {
    cardinal table_index, token_index;
    
    // calculate perfect hash from storage hash
    table_index = ((hash & 0x1FF) >> ((hash & 0x800) > 0)) +
                  (((hash & 0x400) > 0) << 2);
    
    // fold to fit all values into 255 cells
    if (table_index > 255)
        table_index = (table_index - 256) & 0xFF;

    // retrieve token index from mapping table
    token_index = _m2_perfect_hash_to_index_map[table_index];
    
    // compare storage hash with stored hash
    if (hash == _m2_reserved_word[token_index])
        return token_index;
    else
        return M2_NUMBER_OF_RESERVED_WORDS + 1;
    
} // end m2_token_for_reserved_word_hash


// END OF FILE