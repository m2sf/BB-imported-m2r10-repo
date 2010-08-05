/*! Modula-2 R10 Compiler (m2r10c)
 *
 *  @file  m2_tokens.h
 *  @brief Definition of lexer tokens
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


#ifndef M2_TOKENS_H
#define M2_TOKENS_H


// ---------------------------------------------------------------------------
// Total number of tokens
// ---------------------------------------------------------------------------

#define M2_NUMBER_OF_TOKENS  _M2_NUMBER_OF_TOKENS


// ---------------------------------------------------------------------------
// Enumeration type containing all tokens
// ---------------------------------------------------------------------------

#define _add_token(_suffix, _desc) TOKEN ## _suffix,
#define _add_reserved_word(_suffix, _hash) TOKEN ## _suffix,

typedef /* m2_token_t */ enum {
// first entry for illegal input
TOKEN_ILLEGAL_CHARACTER = 0,
// insert reserved word tokens from reserved word master table
#include "m2_table_of_reserved_words.h"
// insert all other tokens from token master table
#include "m2_table_of_tokens.h"
// final entry to obtain total number of tokens
_M2_NUMBER_OF_TOKENS
} m2_token_t;

#undef _add_token
#undef _add_reserved_word


// --------------------------------------------------------------------------
// Token aliases for convenience
// --------------------------------------------------------------------------

#define TOKEN_ILLEGAL_CHAR   TOKEN_ILLEGAL_CHARACTER
#define TOKEN_IDENT          TOKEN_IDENTIFIER
#define TOKEN_NUM_LITERAL    TOKEN_NUMERIC_LITERAL
#define TOKEN_STR_LITERAL    TOKEN_STRING_LITERAL
#define TOKEN_LPAREN         TOKEN_OPENING_PARENTHESIS
#define TOKEN_RPAREN         TOKEN_CLOSING_PARENTHESIS
#define TOKEN_LBRACKET       TOKEN_OPENING_BRACKET
#define TOKEN_RBRACKET       TOKEN_CLOSING_BRACKET
#define TOKEN_LBRACE         TOKEN_OPENING_BRACE
#define TOKEN_RBRACE         TOKEN_CLOSING_BRACE


// ---------------------------------------------------------------------------
// function:  m2_token_identifier(token)
// ---------------------------------------------------------------------------
//
// Returns the identifier string for token <token>.
// Returns NULL if the value of <token> is invalid.

const char *m2_token_identifier(m2_token_t token);


// ---------------------------------------------------------------------------
// function:  m2_token_name(token)
// ---------------------------------------------------------------------------
//
// Returns the human readable name for token <token>.
// Returns NULL if the value of <token> is invalid.

const char *m2_token_name(m2_token_t token);


#endif /* M2_TOKENS_H */

// END OF FILE