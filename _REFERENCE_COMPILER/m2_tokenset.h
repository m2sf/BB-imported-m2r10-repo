/*! Modula-2 R10 Compiler (m2r10c)
 *
 *  @file  m2_tokenset.h
 *  @brief M2R10 token set inteface
 *
 *  @b Author: Benjamin Kowarsch
 *
 *  @b Copyright: (C) 2010 B.Kowarsch. All rights reserved.
 *
 *  @b License:
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


#ifndef M2_TOKENSET_H
#define M2_TOKENSET_H

#include "common.h"
#include "m2_tokens.h"
#include "m2_productions.h"


// ----------------------------------------------------------------------------
// Tokenset segmentation
// ----------------------------------------------------------------------------

// IMPLEMENTATION DEPENDENT DATA - DO NOT MODIFY !!!
#define M2_TOKENSET_BITS_PER_SEGMENT 32

// IMPLEMENTATION DEPENDENT DATA - DO NOT MODIFY !!!
#define M2_TOKENSET_SEGMENTS_PER_SET \
    ((M2_NUMBER_OF_TOKENS / M2_TOKENSET_BITS_PER_SEGMENT) + 1)


// ---------------------------------------------------------------------------
// Opaque tokenset handle type
// ---------------------------------------------------------------------------
//
// WARNING: Objects of this opaque type should only be accessed through this
// public interface.  DO NOT EVER attempt to bypass the public interface.
//
// The internal data structure of this opaque type is HIDDEN  and  MAY CHANGE
// at any time WITHOUT NOTICE. Accessing the internal data structure directly
// other than through the  functions  in this public interface is  UNSAFE and
// may result in an inconsistent program state or a crash.

typedef opaque_t m2_tokenset_t;


// ---------------------------------------------------------------------------
// Opaque tokenset iterator handle type
// ---------------------------------------------------------------------------
//
// WARNING: Objects of this opaque type should only be accessed through this
// public interface.  DO NOT EVER attempt to bypass the public interface.
//
// The internal data structure of this opaque type is HIDDEN  and  MAY CHANGE
// at any time WITHOUT NOTICE. Accessing the internal data structure directly
// other than through the  functions  in this public interface is  UNSAFE and
// may result in an inconsistent program state or a crash.

typedef opaque_t m2_tokenset_iterator_t;


// ---------------------------------------------------------------------------
// Empty token set
// ---------------------------------------------------------------------------

#define M2_EMPTY_TOKENSET m2_tokenset_from_list(0)


// ---------------------------------------------------------------------------
// function:  m2_tokenset_from_list( token_list )
// ---------------------------------------------------------------------------
//
// Returns a new token set with the tokens passed as parameters included.  At
// least one token must be passed.  Further tokens  may be passed as variadic
// parameters.  The list of tokens must always be terminated  by passing zero
// as the last argument in the function call.  Values passed in that are out-
// side of the range of defined tokens are ignored.  If zero is passed as the
// only argument,  an empty token set is returned.

m2_tokenset_t m2_tokenset_from_list(m2_token_t first_token, ...);


// ---------------------------------------------------------------------------
// function:  m2_tokenset_is_element( set, token )
// ---------------------------------------------------------------------------
//
// Tests membership of token <token> in token set <set>.  Returns true if the
// token is a member,  token ∈ set,  returns false  otherwise.  If a value is
// passed that is outside the range of defined tokens,  false is returned.

bool m2_tokenset_is_element(m2_tokenset_t set, m2_token_t token);


// ---------------------------------------------------------------------------
// function:  m2_tokenset_is_subset( set1, set2 )
// ---------------------------------------------------------------------------
//
// Returns true if token set <set2> is a subset of token set <set1>,  that is
// if all elements of token set <set2> are also elements of token set <set1>,
// set2 ⊆ set1,  returns false otherwise.

bool m2_tokenset_is_subset(m2_tokenset_t set1, m2_tokenset_t set2);


// ---------------------------------------------------------------------------
// function:  m2_tokenset_is_disjunct( set1, set2 )
// ---------------------------------------------------------------------------
//
// Tests if token sets <set1>  and <set2>  are disjunct,  Returns true if the
// sets are disjunct,  set1 ∩ set2 = {},  returns false otherwise.

bool m2_tokenset_is_disjunct(m2_tokenset_t set1, m2_tokenset_t set2);


// ---------------------------------------------------------------------------
// function:  m2_tokenset_incl( set, token )
// ---------------------------------------------------------------------------
//
// Includes token  <token>  in token set  <set>.  Any value passed in that is
// outside of the range of defined tokens is ignored.

void m2_tokenset_incl(m2_tokenset_t set, m2_token_t token);


// ---------------------------------------------------------------------------
// function:  m2_tokenset_excl( set, token )
// ---------------------------------------------------------------------------
//
// Excludes token  <token>  in token set  <set>.  Any value passed in that is
// outside of the range of defined tokens is ignored.

void m2_tokenset_excl(m2_tokenset_t set, m2_token_t token);


// ---------------------------------------------------------------------------
// function:  m2_tokenset_incl_list( set, token_list )
// ---------------------------------------------------------------------------
//
// Includes  the tokens  passed as  variadic parameters  in token set  <set>.
// The list of tokens must be terminated by passing zero as the last argument
// in the function call.  Any value passed in that is outside of the range of
// defined tokens is ignored.

void m2_tokenset_incl_list(m2_tokenset_t set, ...);


// ---------------------------------------------------------------------------
// function:  m2_tokenset_excl_list( set, token_list )
// ---------------------------------------------------------------------------
//
// Excludes the tokens  passed as variadic parameters  from token set  <set>.
// The list of tokens must be terminated by passing zero as the last argument
// in the function call.  Any value passed in that is outside of the range of
// defined tokens is ignored.

void m2_tokenset_excl_list(m2_tokenset_t set, ...);


// ---------------------------------------------------------------------------
// function:  m2_tokenset_union( set1, set2 )
// ---------------------------------------------------------------------------
//
// Returns the union of token sets <set1> and <set2>,  set1 ∪ set2.

m2_tokenset_t m2_tokenset_union(m2_tokenset_t set1, m2_tokenset_t set2);


// ---------------------------------------------------------------------------
// function:  m2_tokenset_intersection( set1, set2 )
// ---------------------------------------------------------------------------
//
// Returns the intersection of token sets <set1> and <set2>,  set1 ∩ set2.

m2_tokenset_t m2_tokenset_intersection(m2_tokenset_t set1,
                                       m2_tokenset_t set2);


// ---------------------------------------------------------------------------
// function:  m2_tokenset_difference( set1, set2 )
// ---------------------------------------------------------------------------
//
// Returns the difference of token sets <set1> and <set2>,  set1 \ set 2.

m2_tokenset_t m2_tokenset_difference(m2_tokenset_t set1, m2_tokenset_t set2);


// ---------------------------------------------------------------------------
// function:  m2_tokenset_first_set( production )
// ---------------------------------------------------------------------------
//
// Returns the FIRST set of production <production>.

m2_tokenset_t m2_tokenset_first_set(m2_production_t production);


// ---------------------------------------------------------------------------
// function:  m2_tokenset_follow_set( production )
// ---------------------------------------------------------------------------
//
// Returns the FOLLOW set of production <production>.

m2_tokenset_t m2_tokenset_follow_set(m2_production_t production);


// ---------------------------------------------------------------------------
// function:  m2_tokenset_dispose( set )
// ---------------------------------------------------------------------------
//
// Disposes of tokenset <set>.

void m2_tokenset_dispose(m2_tokenset_t set);


// ---------------------------------------------------------------------------
// function:  m2_tokenset_iterator( set )
// ---------------------------------------------------------------------------
//
// Returns a new iterator for tokenset <set>.

m2_tokenset_iterator_t m2_tokenset_iterator(m2_tokenset_t set);


// ---------------------------------------------------------------------------
// function:  m2_tokenset_iterator_token_count( iterator )
// ---------------------------------------------------------------------------
//
// Returns the number of tokens in tokenset iterator <iterator>.

cardinal
    m2_tokenset_iterator_token_count(m2_tokenset_iterator_t iterator);


// ---------------------------------------------------------------------------
// function:  m2_tokenset_iterator_token_at_index( iterator, index )
// ---------------------------------------------------------------------------
//
// Returns the token at index <index> in tokenset iterator <iterator>.

m2_token_t
    m2_tokenset_iterator_token_at_index(m2_tokenset_iterator_t iterator,
                                                      cardinal index);


// ---------------------------------------------------------------------------
// function:  m2_tokenset_iterator_for_first_set( production )
// ---------------------------------------------------------------------------
//
// Returns an iterator for the FIRST set of production <production>.

m2_tokenset_iterator_t
    m2_tokenset_iterator_for_first_set(m2_production_t production);


// ---------------------------------------------------------------------------
// function:  m2_tokenset_iterator_follow_set( production )
// ---------------------------------------------------------------------------
//
// Returns an iterator for the FOLLOW set of production <production>.

m2_tokenset_iterator_t
    m2_tokenset_iterator_for_follow_set(m2_production_t production);


// ---------------------------------------------------------------------------
// function:  m2_tokenset_iterator_dispose( iterator )
// ---------------------------------------------------------------------------
//
// Disposes of tokenset iterator <iterator>.

void m2_tokenset_iterator_dispose(m2_tokenset_iterator_t iterator);


#endif /* M2_TOKENSET_H */

// END OF FILE
