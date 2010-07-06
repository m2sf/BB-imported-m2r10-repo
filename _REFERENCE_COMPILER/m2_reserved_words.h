/* Modula-2 R10 Compiler (m2r10c)
 *
 *  m2_reserved_words.h
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

#ifndef M2_RESERVED_WORDS_H
#define M2_RESERVED_WORDS_H

#include "common.h"


// ---------------------------------------------------------------------------
// Total number of reserved words
// ---------------------------------------------------------------------------

#define M2_NUMBER_OF_RESERVED_WORDS  (_max_reserved_word_index_plus_one - 1)


// ---------------------------------------------------------------------------
// Length of longest reserved word
// ---------------------------------------------------------------------------

#define M2_MAX_RESERVED_WORD_LENGTH  14


// ---------------------------------------------------------------------------
// Index values for reserved words
// ---------------------------------------------------------------------------

#define _add_reserved_word(_suffix, _hash) M2_RESERVED_WORD_INDEX ## _suffix,

typedef /* m2_reserved_word_t */ enum {
_min_reserved_word_index_minus_one,
// insert reserved words from master table
#include "m2_table_of_reserved_words.h"
_max_reserved_word_index_plus_one
} m2_reserved_word_t;

#undef _add_reserved_word


// ---------------------------------------------------------------------------
// function:  m2_stored_hash_for_reserved_word_token(token_index)
// ---------------------------------------------------------------------------
//
// Returns the  stored hash value  for the reserved word  whose token index is
// <token_index>.  Returns zero if <token_index> is greater than the number of
// as defined by M2_NUMBER_OF_RESERVED_WORDS.

cardinal m2_hash_for_reserved_word_token(cardinal token_index);


// ---------------------------------------------------------------------------
// function:  m2_token_for_reserved_word_hash(hash)
// ---------------------------------------------------------------------------
//
// Returns the  token index  for the reserved word whose hash value is <hash>.
// Returns zero if <hash> is zero,  returns M2_NUMBER_OF_RESERVED_WORDS + 1 if
// <hash> does not represent any reserved word.

cardinal m2_token_for_reserved_word_hash(cardinal hash);


#endif /* M2_RESERVED_WORDS_H */

// END OF FILE