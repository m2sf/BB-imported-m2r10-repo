/* Modula-2 R10 Compiler (m2r10c)
 *
 *  m2_tokens.c
 *  Implementation of lexer tokens
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
#include "m2_tokens.h"


// ---------------------------------------------------------------------------
// Token identifier strings for debugging
// ---------------------------------------------------------------------------

typedef char *m2_token_identifier_t;

#define _add_token(_suffix, _desc) \
static const char _TOKID ## _suffix[] = "TOKEN" # _suffix EMPTY_STRING;

#define _add_reserved_word(_suffix, _hash) \
static const char _TOKID ## _suffix[] = "TOKEN" # _suffix EMPTY_STRING;

static const char _TOKID_ILLEGAL_CHARACTER[] = "TOKEN_ILLEGAL_CHARACTER\0";
#include "m2_table_of_reserved_words.h"
#include "m2_table_of_tokens.h"

#undef _add_token
#undef _add_reserved_word


// ---------------------------------------------------------------------------
// Human readable names of tokens for error messages
// ---------------------------------------------------------------------------

typedef char *m2_token_name_t;

#define _add_token(_suffix, _desc) \
static const char _TOKNAM ## _suffix[] = _desc EMPTY_STRING;

#define _add_reserved_word(_suffix, _hash) \
static const char _TOKNAM ## _suffix[] = # _suffix EMPTY_STRING;

static const char _TOKNAM_ILLEGAL_CHARACTER[] = "illegal character\0";
#include "m2_table_of_reserved_words.h"
#include "m2_table_of_tokens.h"

#undef _add_token
#undef _add_reserved_word


// ---------------------------------------------------------------------------
// Table of token identifier strings
// ---------------------------------------------------------------------------

#define _add_token(_suffix, _desc) (char *)&_TOKID ## _suffix,
#define _add_reserved_word(_suffix, _hash) (char *)&_TOKID ## _suffix,

static const m2_token_identifier_t m2_token_identifier_str[] = {
(char *)&_TOKNAM_ILLEGAL_CHARACTER,
#include "m2_table_of_reserved_words.h"
#include "m2_table_of_tokens.h"
} /* m2_token_identifier_str */;

#undef _add_token
#undef _add_reserved_word


// ---------------------------------------------------------------------------
// Table of human readable token names
// ---------------------------------------------------------------------------

#define _add_token(_suffix, _desc) (char *)&_TOKNAM ## _suffix,
#define _add_reserved_word(_suffix, _hash) (char *)&_TOKNAM ## _suffix+1,
// to skip preceeding underscore each keyword string address is incremented

static const m2_token_name_t m2_token_name_str[] = {
(char *)&_TOKNAM_ILLEGAL_CHARACTER,
#include "m2_table_of_reserved_words.h"
#include "m2_table_of_tokens.h"
} /* m2_token_name_str */;

#undef _add_token
#undef _add_reserved_word



// ---------------------------------------------------------------------------
// function:  m2_token_identifier(token)
// ---------------------------------------------------------------------------
//
// Returns the identifier string for token <token>.
// Returns NULL if the value of <token> is invalid.

const char *m2_token_identifier(m2_token_t token) {
    
    if (token < M2_NUMBER_OF_TOKENS)
        return m2_token_identifier_str[token];
    else 
        return NULL;
    
} // end m2_token_identifier


// ---------------------------------------------------------------------------
// function:  m2_token_name(token)
// ---------------------------------------------------------------------------
//
// Returns the human readable name for token <token>.
// Returns NULL if the value of <token> is invalid.

const char *m2_token_name(m2_token_t token) {

    if (token < M2_NUMBER_OF_TOKENS)
        return m2_token_name_str[token];
    else 
        return NULL;
    
} // end m2_token_name


// END OF FILE