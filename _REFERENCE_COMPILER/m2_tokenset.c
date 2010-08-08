/*! Modula-2 R10 Compiler (m2r10c)
 *
 *  @file  m2_tokenset.c
 *  @brief M2R10 token set implementation
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


#include "alloc.h"
#include "m2_tokenset.h"
#include "m2_productions.h"
#include "m2_first_follow.h"
#include "m2_build_params.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>


// ---------------------------------------------------------------------------
// Token set segmentation
// ---------------------------------------------------------------------------

#define _M2_TOKENSET_BITS_PER_SEGMENT 32

#if (M2_TOKENSET_BITS_PER_SEGMENT != _M2_TOKENSET_BITS_PER_SEGMENT)
#error "illegal value for M2_TOKENSET_BITS_PER_SEGMENT"
#endif

#define _M2_TOKENSET_SEGMENTS_PER_SET \
    ((M2_NUMBER_OF_TOKENS / M2_TOKENSET_BITS_PER_SEGMENT) + 1)

#if (M2_TOKENSET_SEGMENTS_PER_SET != _M2_TOKENSET_SEGMENTS_PER_SET)
#error "illegal value for M2_TOKENSET_SEGMENTS_PER_SET"
#endif

#define _M2_TOKENSET_SEGMENT_TYPE uint_fast32_t


// ---------------------------------------------------------------------------
// Token set type
// ---------------------------------------------------------------------------

typedef _M2_TOKENSET_SEGMENT_TYPE
    m2_tokenset_a[_M2_TOKENSET_SEGMENTS_PER_SET];


// ---------------------------------------------------------------------------
// Array of FIRST sets
// ---------------------------------------------------------------------------

#define _add_production(_production, \
    _first0,  _first1,  _first2, \
    _follow0, _follow1, _follow2 ) \
    { _first0, _first1, _first2 },

static const m2_tokenset_a _first_set[] = {
#include "m2_table_of_productions.h"
}; // _first_set

#undef _add_production


// ---------------------------------------------------------------------------
// Array of FOLLOW sets
// ---------------------------------------------------------------------------

#define _add_production(_production, \
    _first0,  _first1,  _first2, \
    _follow0, _follow1, _follow2 ) \
    { _follow0, _follow1, _follow2 },

static const m2_tokenset_a _follow_set[] = {
#include "m2_table_of_productions.h"
}; // _follow_set

#undef _add_production


// ---------------------------------------------------------------------------
// Token set iterator type
// ---------------------------------------------------------------------------

#define _M2_TOKENSET_ITERATOR_BASE_TYPE    uint8_t

typedef _M2_TOKENSET_ITERATOR_BASE_TYPE *m2_tokenset_iterator_a;

#define _TOKEN_COUNT(_iterator) _iterator[0]
#define _TOKEN_AT_INDEX(_iterator, _index) _iterator[_index + 1]


// ---------------------------------------------------------------------------
// Iterators for FIRST sets
// ---------------------------------------------------------------------------

#define _add_production(_production, \
    _fi0, _fi1, _fi2, _fo0, _fo1, _fo2 ) \
    static const _M2_TOKENSET_ITERATOR_BASE_TYPE \
        _FiSI ## _production[] = { FIRST_LIST_for ## _production };

#include "m2_table_of_productions.h"

#undef _add_production


// ---------------------------------------------------------------------------
// Array of FIRST set iterators
// ---------------------------------------------------------------------------

#define _add_production(_production, \
    _fi0,  _fi1,  _fi2, _fo0, _fo1, _fo2 ) \
    (m2_tokenset_iterator_t) &_FiSI ## _production,

static const _M2_TOKENSET_ITERATOR_BASE_TYPE * _first_set_iterator[] = {
#include "m2_table_of_productions.h"
}; // _first_set_iterator

#undef _add_production


// ---------------------------------------------------------------------------
// Iterators for FOLLOW sets
// ---------------------------------------------------------------------------

#define _add_production(_production, \
    _fi0, _fi1, _fi2, _fo0, _fo1, _fo2 ) \
    static const _M2_TOKENSET_ITERATOR_BASE_TYPE \
        _FoSI ## _production[] = { FOLLOW_LIST_for ## _production };

#include "m2_table_of_productions.h"

#undef _add_production


// ---------------------------------------------------------------------------
// Array of FOLLOW set iterators
// ---------------------------------------------------------------------------

#define _add_production(_production, \
    _fi0,  _fi1,  _fi2, _fo0, _fo1, _fo2 ) \
    (m2_tokenset_iterator_t) &_FoSI ## _production,

static const _M2_TOKENSET_ITERATOR_BASE_TYPE * _follow_set_iterator[] = {
#include "m2_table_of_productions.h"
}; // _follow_set_iterator

#undef _add_production


// --------------------------------------------------------------------------
// function:  m2_tokenset_from_list( token_list )
// --------------------------------------------------------------------------
//
// Returns a new token set with the tokens passed as parameters included.  At
// least one token must be passed.  Further tokens  may be passed as variadic
// parameters.  The list of tokens must always be terminated  by passing zero
// as the last argument in the function call.  Values passed in that are out-
// side of the range of defined tokens are ignored.  If zero is passed as the
// only argument,  an empty token set is returned.

m2_tokenset_t m2_tokenset_from_list(m2_token_t first_token, ...) {
    m2_tokenset_a *new_set;
    uint_fast8_t bit, segment = 0;
    m2_token_t token;
    va_list token_list;
    va_start(token_list, first_token);
        
    new_set = (m2_tokenset_a *) ALLOCATE(sizeof(m2_tokenset_a));
    
    if (new_set == NULL) return NULL;

    while (segment < _M2_TOKENSET_SEGMENTS_PER_SET) {
        (*new_set)[segment] = 0;
        segment++;
    } // end while
            
    token = first_token;
    
    while (token != 0) {
        if (token < M2_NUMBER_OF_TOKENS) {
            segment = (uint_fast8_t) (token / _M2_TOKENSET_BITS_PER_SEGMENT);
            bit = (uint_fast8_t) (token % _M2_TOKENSET_BITS_PER_SEGMENT);
            (*new_set)[segment] = (*new_set)[segment] | (1 << bit);
        } // end if
        token = va_arg(token_list, m2_token_t); // next token
    } // end while
    
    return (m2_tokenset_t) new_set;
} // end m2_tokenset_from_list


// --------------------------------------------------------------------------
// function:  m2_tokenset_is_element( set, token )
// --------------------------------------------------------------------------
//
// Tests membership of token <token> in token set <set>.  Returns true if the
// token is a member,  token ∈ set,  returns false  otherwise.  If a value is
// passed that is outside the range of defined tokens,  false is returned.

bool m2_tokenset_is_element(m2_tokenset_t set, m2_token_t token) {
    m2_tokenset_a *_set = (m2_tokenset_a *) set;
    uint_fast8_t segment, bit;
    
    if (token >= M2_NUMBER_OF_TOKENS)
        return false;
    
    segment = (uint_fast8_t) (token / _M2_TOKENSET_BITS_PER_SEGMENT);
    bit = (uint_fast8_t) (token % _M2_TOKENSET_BITS_PER_SEGMENT);
    
    return ((*_set)[segment] & (1 << bit)) != 0;
} // end m2_tokenset_is_element


// --------------------------------------------------------------------------
// function:  m2_tokenset_is_subset( set1, set2 )
// --------------------------------------------------------------------------
//
// Returns true if token set <set2> is a subset of token set <set1>,  that is
// if all elements of token set <set2> are also elements of token set <set1>,
// set2 ⊆ set1,  returns false otherwise.

bool m2_tokenset_is_subset(m2_tokenset_t set1, m2_tokenset_t set2) {
    m2_tokenset_a *_set1 = (m2_tokenset_a *) set1;
    m2_tokenset_a *_set2 = (m2_tokenset_a *) set2;
    uint_fast8_t segment = 0;
    
    while (segment < _M2_TOKENSET_SEGMENTS_PER_SET) {
        if ((((*_set1)[segment] & (*_set2)[segment]) ^ (*_set2)[segment]) == 0)
            segment++;
        else
            return false;
    } // end while
    
    return true;
} // end m2_tokenset_is_subset


// --------------------------------------------------------------------------
// function:  m2_tokenset_is_disjunct( set1, set2 )
// --------------------------------------------------------------------------
//
// Tests if token sets <set1>  and <set2>  are disjunct,  Returns true if the
// sets are disjunct,  set1 ∩ set2 = {},  returns false otherwise.

bool m2_tokenset_is_disjunct(m2_tokenset_t set1, m2_tokenset_t set2) {
    m2_tokenset_a *_set1 = (m2_tokenset_a *) set1;
    m2_tokenset_a *_set2 = (m2_tokenset_a *) set2;
    uint_fast8_t segment = 0;
    
    while (segment < _M2_TOKENSET_SEGMENTS_PER_SET) {
        if (((*_set1)[segment] & (*_set2)[segment]) == 0)
            segment++;
        else
            return false;
    } // end while
   
    return true;
} // end m2_tokenset_is_disjunct


// --------------------------------------------------------------------------
// function:  m2_tokenset_incl( set, token )
// --------------------------------------------------------------------------
//
// Includes token  <token>  in token set  <set>.  Any value passed in that is
// outside of the range of defined tokens is ignored.

void m2_tokenset_incl(m2_tokenset_t set, m2_token_t token) {
    m2_tokenset_a *_set = (m2_tokenset_a *) set;
    uint_fast8_t segment, bit;
        
    if (token >= M2_NUMBER_OF_TOKENS)
        return;
    
    segment = (uint_fast8_t) (token / _M2_TOKENSET_BITS_PER_SEGMENT);
    bit = (uint_fast8_t) (token % _M2_TOKENSET_BITS_PER_SEGMENT);
    (*_set)[segment] = (*_set)[segment] | (1 << bit);
    
    return;
} // end m2_tokenset_incl


// --------------------------------------------------------------------------
// function:  m2_tokenset_excl( set, token )
// --------------------------------------------------------------------------
//
// Excludes token  <token>  in token set  <set>.  Any value passed in that is
// outside of the range of defined tokens is ignored.

void m2_tokenset_excl(m2_tokenset_t set, m2_token_t token) {
    m2_tokenset_a (*_set) = (m2_tokenset_a *) set;
    uint_fast8_t segment, bit;
    
    if (token >= M2_NUMBER_OF_TOKENS)
        return;
    
    segment = (uint_fast8_t) (token / _M2_TOKENSET_BITS_PER_SEGMENT);
    bit = (uint_fast8_t) (token % _M2_TOKENSET_BITS_PER_SEGMENT);
    (*_set)[segment] = (*_set)[segment] & ~(1 << bit);
    
    return;
} // end m2_tokenset_excl


// --------------------------------------------------------------------------
// function:  m2_tokenset_incl_list( set, token_list )
// --------------------------------------------------------------------------
//
// Includes  the tokens  passed as  variadic parameters  in token set  <set>.
// The list of tokens must be terminated by passing zero as the last argument
// in the function call.  Any value passed in that is outside of the range of
// defined tokens is ignored.

void m2_tokenset_incl_list(m2_tokenset_t set, ...) {
    m2_tokenset_a *_set = (m2_tokenset_a *) set;
    uint_fast8_t segment, bit;
    m2_token_t token;
    va_list token_list;
    va_start(token_list, set);
    
    token = va_arg(token_list, m2_token_t);
    
    while (token != 0) {
        if (token < M2_NUMBER_OF_TOKENS) {
            segment = (uint_fast8_t) (token / _M2_TOKENSET_BITS_PER_SEGMENT);
            bit = (uint_fast8_t) (token % _M2_TOKENSET_BITS_PER_SEGMENT);
            (*_set)[segment] = (*_set)[segment] | (1 << bit);
        } // end if
        token = va_arg(token_list, m2_token_t); // next token
    } // end while
    
    return;
} // end m2_tokenset_incl_list


// --------------------------------------------------------------------------
// function:  m2_tokenset_excl_list( set, token_list )
// --------------------------------------------------------------------------
//
// Excludes the tokens  passed as variadic parameters  from token set  <set>.
// The list of tokens must be terminated by passing zero as the last argument
// in the function call.  Any value passed in that is outside of the range of
// defined tokens is ignored.

void m2_tokenset_excl_list(m2_tokenset_t set, ...) {
    m2_tokenset_a *_set = (m2_tokenset_a *) set;
    uint_fast8_t segment, bit;
    m2_token_t token;
    va_list token_list;
    va_start(token_list, set);
    
    token = va_arg(token_list, m2_token_t);
    
    while (token != 0) {
        if (token < M2_NUMBER_OF_TOKENS) {
            segment = (uint_fast8_t) (token / _M2_TOKENSET_BITS_PER_SEGMENT);
            bit = (uint_fast8_t) (token % _M2_TOKENSET_BITS_PER_SEGMENT);
            (*_set)[segment] = (*_set)[segment] & ~(1 << bit);
        } // end if
        token = va_arg(token_list, m2_token_t); // next token
    } // end while
    
    return;
} // end m2_tokenset_excl_list


// --------------------------------------------------------------------------
// function:  m2_tokenset_union( set1, set2 )
// --------------------------------------------------------------------------
//
// Returns the union of token sets <set1> and <set2>,  set1 ∪ set2.

m2_tokenset_t m2_tokenset_union(m2_tokenset_t set1,
                                m2_tokenset_t set2) {
    m2_tokenset_a *_set1 = (m2_tokenset_a *) set1;
    m2_tokenset_a *_set2 = (m2_tokenset_a *) set2;
    m2_tokenset_a *new_set;
    uint_fast8_t segment = 0;
    
    new_set = (m2_tokenset_a *) ALLOCATE(sizeof(m2_tokenset_a));
    
    if (new_set == NULL) return NULL;
    
    while (segment < _M2_TOKENSET_SEGMENTS_PER_SET) {
        (*new_set)[segment] = (*_set1)[segment] | (*_set2)[segment]; 
        segment++;
    } // end while
    
    return (m2_tokenset_t) new_set;
} // end m2_tokenset_union


// --------------------------------------------------------------------------
// function:  m2_tokenset_intersection( set1, set2 )
// --------------------------------------------------------------------------
//
// Returns the intersection of token sets <set1> and <set2>,  set1 ∩ set2.

m2_tokenset_t m2_tokenset_intersection(m2_tokenset_t set1, 
                                       m2_tokenset_t set2) {
    m2_tokenset_a *_set1 = (m2_tokenset_a *) set1;
    m2_tokenset_a *_set2 = (m2_tokenset_a *) set2;
    m2_tokenset_a *new_set;
    uint_fast8_t segment = 0;
    
    new_set = (m2_tokenset_a *) ALLOCATE(sizeof(m2_tokenset_a));
    
    if (new_set == NULL) return NULL;
    
    while (segment < _M2_TOKENSET_SEGMENTS_PER_SET) {
        *new_set[segment] = *_set1[segment] & *_set2[segment]; 
        segment++;
    } // end while
    
    return (m2_tokenset_t) new_set;
} // end m2_tokenset_intersection


// --------------------------------------------------------------------------
// function:  m2_tokenset_difference( set1, set2 )
// --------------------------------------------------------------------------
//
// Returns the difference of token sets <set1> and <set2>,  set1 \ set 2.

m2_tokenset_t m2_tokenset_difference(m2_tokenset_t set1,
                                     m2_tokenset_t set2) {
    m2_tokenset_a *_set1 = (m2_tokenset_a *) set1;
    m2_tokenset_a *_set2 = (m2_tokenset_a *) set2;
    m2_tokenset_a *new_set;
    uint_fast32_t segment = 0;
    
    new_set = (m2_tokenset_a *) ALLOCATE(sizeof(m2_tokenset_a));
    
    if (new_set == NULL) return NULL;
    
    while (segment < _M2_TOKENSET_SEGMENTS_PER_SET) {
        (*new_set)[segment] =
            ((*_set1)[segment] | (*_set2)[segment]) & (~ (*_set2)[segment]);
        segment++;
    } // end while
    
    return (m2_tokenset_t) new_set;
} // end m2_tokenset_difference


// ---------------------------------------------------------------------------
// function:  m2_tokenset_first_set( production )
// ---------------------------------------------------------------------------
//
// Returns the FIRST set of production <production>.

m2_tokenset_t m2_tokenset_first_set(m2_production_t production) {
    
    if (production < M2_NUMBER_OF_PRODUCTIONS)
        return (const m2_tokenset_t) &_first_set[production];
    else
        return NULL;
    
} // end m2_tokenset_first_set


// ---------------------------------------------------------------------------
// function:  m2_tokenset_follow_set( production )
// ---------------------------------------------------------------------------
//
// Returns the FOLLOW set of production <production>.

m2_tokenset_t
    m2_tokenset_follow_set(m2_production_t production) {
    
    if (production < M2_NUMBER_OF_PRODUCTIONS)
        return (const m2_tokenset_t) &_follow_set[production];
    else
        return NULL;
    
} // end m2_tokenset_follow_set


// ---------------------------------------------------------------------------
// function:  m2_tokenset_dispose( set )
// ---------------------------------------------------------------------------
//
// Disposes of tokenset <set>.

void m2_tokenset_dispose(m2_tokenset_t set) {
    
    if (set == NULL) return;
    
    DEALLOCATE(set);
} // end m2_tokenset_dispose


// ---------------------------------------------------------------------------
// function:  m2_tokenset_iterator( set )
// ---------------------------------------------------------------------------
//
// Returns a new iterator for tokenset <set>.

m2_tokenset_iterator_t m2_tokenset_iterator(m2_tokenset_t set) {
    m2_tokenset_a *_set = (m2_tokenset_a *) set;
    m2_token_t token_list[M2_NUMBER_OF_TOKENS];
    m2_token_t token = 0;
    cardinal index = 0;
    _M2_TOKENSET_ITERATOR_BASE_TYPE *new_iterator;
    uint_fast8_t bit, segment;
    
    if (set == NULL) return NULL;
    
    bit = 0;
    segment = 0;
    while (segment < _M2_TOKENSET_SEGMENTS_PER_SET) {
        while (bit < _M2_TOKENSET_BITS_PER_SEGMENT) {
            if (((*_set)[segment] & (1 << bit)) != 0) {
                token_list[index] = token;
                index++;
            } // end if
            token++;
            bit++;
        } // end while
        bit = 0;
        segment++;
    } // end while
    
    new_iterator = (_M2_TOKENSET_ITERATOR_BASE_TYPE *)
        ALLOCATE((index + 1) * sizeof(_M2_TOKENSET_ITERATOR_BASE_TYPE));
    
    if (new_iterator == NULL) return NULL;
    
    _TOKEN_COUNT(new_iterator) = index;
    
    index = 0;
    while (index < _TOKEN_COUNT(new_iterator)) {
        _TOKEN_AT_INDEX(new_iterator, index) = token_list[index];
        index++;
    } // end while
    
    return (m2_tokenset_iterator_t) new_iterator;
} // end m2_tokenset_iterator


// ---------------------------------------------------------------------------
// function:  m2_tokenset_iterator_token_count( iterator )
// ---------------------------------------------------------------------------
//
// Returns the number of tokens in tokenset iterator <iterator>.

cardinal m2_tokenset_iterator_token_count(m2_tokenset_iterator_t iterator) {
    m2_tokenset_iterator_a _iterator = (m2_tokenset_iterator_a) iterator;
    
    if (iterator == NULL) return 0;
        
    return _TOKEN_COUNT(_iterator);
} // end m2_tokenset_iterator_token_count


// ---------------------------------------------------------------------------
// function:  m2_tokenset_iterator_token_at_index( iterator, index )
// ---------------------------------------------------------------------------
//
// Returns the token at index <index> in tokenset iterator <iterator>.

m2_token_t
    m2_tokenset_iterator_token_at_index(m2_tokenset_iterator_t iterator,
                                                      cardinal index) {
    m2_tokenset_iterator_a _iterator = (m2_tokenset_iterator_a) iterator;
    
    if ((iterator == NULL) || (index >= _TOKEN_COUNT(_iterator))) return 0;
        
    return _TOKEN_AT_INDEX(_iterator, index);
} // end m2_tokenset_iterator_token_at_index


// ---------------------------------------------------------------------------
// function:  m2_tokenset_iterator_for_first_set( production )
// ---------------------------------------------------------------------------
//
// Returns an iterator for the FIRST set of production <production>.

m2_tokenset_iterator_t
    m2_tokenset_iterator_for_first_set(m2_production_t production) {
    
    if (production < M2_NUMBER_OF_PRODUCTIONS)
        return (m2_tokenset_iterator_t) _first_set_iterator[production];
    else
        return NULL;
    
} // end m2_tokenset_iterator_for_first_set


// ---------------------------------------------------------------------------
// function:  m2_tokenset_iterator_follow_set( production )
// ---------------------------------------------------------------------------
//
// Returns an iterator for the FOLLOW set of production <production>.

m2_tokenset_iterator_t
    m2_tokenset_iterator_for_follow_set(m2_production_t production) {
    
    if (production < M2_NUMBER_OF_PRODUCTIONS)
        return (m2_tokenset_iterator_t) _follow_set_iterator[production];
    else
        return NULL;
    
} // end m2_tokenset_iterator_for_follow_set


// ---------------------------------------------------------------------------
// function:  m2_tokenset_iterator_dispose( iterator )
// ---------------------------------------------------------------------------
//
// Disposes of tokenset iterator <iterator>.

void m2_tokenset_iterator_dispose(m2_tokenset_iterator_t iterator) {
    
    if (iterator == NULL) return;
    
    DEALLOCATE(iterator);
} // end m2_tokenset_iterator_dispose


// END OF FILE
