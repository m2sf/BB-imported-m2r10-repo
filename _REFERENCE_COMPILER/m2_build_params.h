/* Modula-2 R10 Compiler (m2r10c)
 *
 *  m2_build_params.h
 *  User definable build parameters for the compiler
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


#ifndef M2_BUILD_PARAMS_H
#define M2_BUILD_PARAMS_H

#include "common.h"


// ===========================================================================
// P U B L I C   S E C T I O N  -  U S E R   D E F I N A B L E   V A L U E S
// ===========================================================================


// build version
#define BUILD_VERSION "(0000)"


// ---------------------------------------------------------------------------
// User definable lexical parameters
// ---------------------------------------------------------------------------

// maximum length allowed for identifiers
#define M2_MAX_IDENT_LENGTH    32

// maximum length allowed for number literals
#define M2_MAX_NUM_LENGTH      32

// maximum length allowed for string literals
#define M2_MAX_STRING_LENGTH   1024

// maximum length allowed for tokenised comments
#define M2_MAX_COMMENT_LENGTH  2048


// ---------------------------------------------------------------------------
// User definable symbol table parameters
// ---------------------------------------------------------------------------

// minimum number of scope sub-tables
#define M2_MIN_SCOPES_IN_SYMBOL_TABLE  20

// default capacity of a scope's sub-table
#define M2_DEFAULT_SCOPE_CAPACITY  23


// ===========================================================================
// P R I V A T E   S E C T I O N  -  D O   N O T   M O D I F Y ! ! !
// ===========================================================================

// ---------------------------------------------------------------------------
// Parameter integrity checks
// ---------------------------------------------------------------------------

#if (M2_MAX_IDENT_LENGTH < 8)
#error "M2_MAX_IDENT_LENGTH must not be less than 8"
#elif (M2_MAX_IDENT_LENGTH < 16)
#warning "unreasonably low value for M2_MAX_IDENT_LENGTH"
#elif (M2_MAX_IDENT_LENGTH > 80)
#warning "unreasonably high value for M2_MAX_IDENT_LENGTH"
#endif

#if (M2_MAX_NUM_LENGTH < 4)
#error "M2_MAX_NUM_LENGTH must not be less than 4"
#elif (M2_MAX_NUM_LENGTH < 8)
#warning "unreasonably low value for M2_MAX_NUM_LENGTH"
#elif (M2_MAX_NUM_LENGTH > 80)
#warning "unreasonably high value for M2_MAX_NUM_LENGTH"
#endif

#if (M2_MAX_STRING_LENGTH < 10)
#error "M2_MAX_STRING_LENGTH must not be less than 10"
#elif (M2_MAX_STRING_LENGTH < 80)
#warning "unreasonably low value for M2_MAX_STRING_LENGTH"
#elif (M2_MAX_STRING_LENGTH > 4096)
#warning "unreasonably high value for M2_MAX_STRING_LENGTH"
#endif

#if (M2_MAX_COMMENT_LENGTH < 10)
#error "M2_MAX_COMMENT_LENGTH must not be less than 10"
#elif (M2_MAX_COMMENT_LENGTH < 80)
#warning "unreasonably low value for M2_MAX_COMMENT_LENGTH"
#elif (M2_MAX_COMMENT_LENGTH > 4096)
#warning "unreasonably high value for M2_MAX_COMMENT_LENGTH"
#endif

#if (M2_MIN_SCOPES_IN_SYMBOL_TABLE < 2)
#error "M2_MIN_SCOPES_IN_SYMBOL_TABLE must not be less than 2"
#elif (M2_MIN_SCOPES_IN_SYMBOL_TABLE < 8)
#warning "unreasonably low value for M2_MIN_SCOPES_IN_SYMBOL_TABLE"
#elif (M2_MIN_SCOPES_IN_SYMBOL_TABLE > 40)
#warning "unreasonably high value for M2_MIN_SCOPES_IN_SYMBOL_TABLE"
#endif

#if (M2_DEFAULT_SCOPE_CAPACITY < 5)
#error "M2_DEFAULT_SCOPE_CAPACITY must not be less than 5"
#elif (M2_DEFAULT_SCOPE_CAPACITY < 11)
#warning "unreasonably low value for M2_DEFAULT_SCOPE_CAPACITY"
#elif (M2_DEFAULT_SCOPE_CAPACITY > 211)
#warning "unreasonably high value for M2_DEFAULT_SCOPE_CAPACITY"
#endif


// ---------------------------------------------------------------------------
// Index type for identifier lexeme iteration
// ---------------------------------------------------------------------------

#if (M2_MAX_IDENT_LENGTH >= 0xffff)
#error "illegal value for OBJM2_MAX_IDENT_LENGTH"
#elif (M2_MAX_IDENT_LENGTH >= 0xff)
typedef uint16_t ident_index_t;
#else
typedef uint8_t ident_index_t;
#endif


// ---------------------------------------------------------------------------
// Index type for numeric literal lexeme iteration
// ---------------------------------------------------------------------------

#if (M2_MAX_NUM_LENGTH >= 0xffff)
#error "illegal value for M2_MAX_NUM_LENGTH"
#elif (M2_MAX_NUM_LENGTH >= 0xff)
typedef uint16_t num_index_t;
#else
typedef uint8_t num_index_t;
#endif


// ---------------------------------------------------------------------------
// Index type for string literal lexeme iteration
// ---------------------------------------------------------------------------

#if (M2_MAX_STRING_LENGTH >= 0xffff)
#error "illegal value for M2_MAX_STRING_LENGTH"
#elif (M2_MAX_STRING_LENGTH >= 0xff)
typedef uint16_t string_index_t;
#else
typedef uint8_t string_index_t;
#endif


// ---------------------------------------------------------------------------
// Index type for comment lexeme iteration
// ---------------------------------------------------------------------------

#if (M2_MAX_COMMENT_LENGTH >= 0xffff)
#error "illegal value for M2_MAX_COMMENT_LENGTH"
#elif (M2_MAX_COMMENT_LENGTH >= 0xff)
typedef uint16_t comment_index_t;
#else
typedef uint8_t comment_index_t;
#endif

#endif /* M2_BUILD_PARAMS_H */


// END OF FILE