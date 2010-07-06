/* Modula-2 R10 Compiler (m2r10c)
 *
 *  m2_token_table.h
 *  Master table of non-identifier tokens
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

// This file  represents the TOKEN MASTER TABLE for the M2R10 compiler,  which
// is used to derive other token related tables and  thereby keep those tables
// in synchronisation.  Consequently,  any changes,  additions  or removals of
// tokens must be made in this file, not anywhere else.  Failing to do so will
// result in an incorrect compiler as tables will no longer be synchronised.
//
// How to use this master table:
//
// The following code  will generate a declaration for an enumeration of token
// identifiers prefixed with TOKEN ...
//
//  #define _add_token(_suffix, _desc) TOKEN ## _suffix,
//  enum m2_token_t {
//  #include "m2_token_table.h"
//  } /* m2_token_t */;
//  #undef _add_token
//
// Arguments for the _add_token macro:
//
//  1st: token identifier suffix, must be preceeded by an underscore
//       used to construct token identifiers for use by the lexer and parser
//
//  2nd: human readable name, double quoted string describing the token
//       used as substrings within error messages and warnings

_add_token( _IDENTIFIER,      "identifier" )
_add_token( _NUMERIC_LITERAL, "numeric literal" )
_add_token( _STRING_LITERAL,  "string literal" )

_add_token( _NOT_EQUAL_OP,         "\"#\"" )
_add_token( _ASTERISK_OP,          "\"*\"" )
_add_token( _PLUS_OP,              "\"+\"" )
_add_token( _INCREMENT_OP,         "\"++\"" )
_add_token( _COMMA,                "\",\"" )
_add_token( _MINUS_OP,             "\"-\"" )
_add_token( _DECREMENT_OP,         "\"--\"" )
_add_token( _DOT,                  "\".\"" )
_add_token( _RANGE_OP,             "\"..\"" )
_add_token( _SLASH_OP,             "\"/\"" )
_add_token( _COLON,                "\":\"" )
_add_token( _TYPE_CONVERSION_OP,   "\"::\"" )
_add_token( _ASSIGN_OP,            "\":=\"" )
_add_token( _SEMICOLON,            "\";\"" )
_add_token( _LESS_OP,              "\"<\"" )
_add_token( _LESS_OR_EQUAL_OP,     "\"<=\"" )
_add_token( _EQUAL_OP,             "\"=\"" )
_add_token( _GREATER_OP,           "\">\"" )
_add_token( _GREATER_OR_EQUAL_OP,  "\">=\"" )
_add_token( _POINTER_DEREF_OP,     "\"^\"" )
_add_token( _CASE_LABEL_SEPARATOR, "\"|\"" )

_add_token( _OPENING_PARENTHESIS,  "\"(\"" )
_add_token( _CLOSING_PARENTHESIS,  "\")\"" )
_add_token( _OPENING_BRACKET,      "\"[\"" )
_add_token( _CLOSING_BRACKET,      "\"]\"" )
_add_token( _OPENING_BRACE,        "\"{\"" )
_add_token( _CLOSING_BRACE,        "\"}\"" )

_add_token( _STORAGE_PSEUDO_OP,    "\"!\"" )
_add_token( _REMOVAL_PSEUDO_OP,    "\"~\"" )
_add_token( _RETRIEVAL_PSEUDO_OP,  "\"?\"" )

_add_token( _START_PRAGMA,    "start of pragma" )
_add_token( _END_PRAGMA,      "end of pragma" )

_add_token( _EOF_MARKER,      "end-of-file marker" )

// END OF FILE