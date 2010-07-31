/* Modula-2 R10 Compiler (m2r10c)
 *
 *  m2_pragma_parser.h
 *  Pragma parser interface
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

#ifndef M2_PRAGMA_PARSER_H
#define M2_PRAGMA_PARSER_H


// --------------------------------------------------------------------------
// Embedded library imports
// --------------------------------------------------------------------------

// FROM common IMPORT opaque_t;
#include "common.h"


// --------------------------------------------------------------------------
// Project library imports
// --------------------------------------------------------------------------

// FROM m2_symbol_table IMPORT m2_symtab_t;
#include "m2_tokens.h"

// FROM m2_parser IMPORT m2_parset_t;
#include "m2_parser.h"


// --------------------------------------------------------------------------
// Opaque pragma parser handle type
// --------------------------------------------------------------------------
//
// WARNING: Objects of this opaque type should only be accessed through this
// public interface.  DO NOT EVER attempt to bypass the public interface.
//
// The internal data structure of this opaque type is HIDDEN  and  MAY CHANGE
// at any time WITHOUT NOTICE. Accessing the internal data structure directly
// other than through the  functions  in this public interface is  UNSAFE and
// may result in an inconsistent program state or a crash.

typedef opaque_t m2_pragma_parser_t;


// --------------------------------------------------------------------------
// Status codes
// --------------------------------------------------------------------------

typedef /* m2_pragma_parser_status_t */ enum {
    
    // operation completed successfully
    M2_PRAGMA_PARSER_STATUS_SUCCESS = 0,
    
    // invalid pragma parser object passed
    M2_PRAGMA_PARSER_STATUS_INVALID_REFERENCE,
    
    // invalid parser object passed
    M2_PRAGMA_PARSER_STATUS_INVALID_PARSER_REFERENCE,

    // invalid handler object passed
    M2_PRAGMA_PARSER_STATUS_INVALID_HANDLER_REFERENCE,

    // unable to allocate memory
    M2_PRAGMA_PARSER_STATUS_ALLOCATION_FAILED,
    
    // one or more pragma syntax errors encountered
    M2_PRAGMA_PARSER_STATUS_SYNTAX_ERRORS_FOUND,
    
    // no errors encountered but warnings reported
    M2_PRAGMA_PARSER_STATUS_WARNINGS_REPORTED
} m2_pragma_parser_status_t;


// --------------------------------------------------------------------------
// Pragma condition evaluation handler type
// --------------------------------------------------------------------------
//
// A handler of this type  must be installed  in a pragma parser object.  The
// pragma parser calls the installed handler  to evaluate boolean expressions
// within a pragma.

typedef bool (*m2_pragma_cond_handler_f)(m2_parser_t);


// --------------------------------------------------------------------------
// Pragma expression evaluation handler type
// --------------------------------------------------------------------------
//
// A handler of this type  must be installed  in a pragma parser object.  The
// pragma parser calls the installed handler  to evaluate integer expressions
// within a pragma.

typedef long int (*m2_pragma_expr_handler_f)(m2_parser_t);


// --------------------------------------------------------------------------
// Pragma getsym handler type
// --------------------------------------------------------------------------
//
// A handler of this type  must be installed  in a pragma parser object.  The
// pragma parser calls the installed handler to read the next symbol within a
// pragma.

typedef m2_token_t (*m2_pragma_getsym_handler_f)(m2_parser_t);


// --------------------------------------------------------------------------
// Pragma lookahead handler type
// --------------------------------------------------------------------------
//
// A handler of this type  must be installed  in a pragma parser object.  The
// pragma parser calls the installed handler  to obtain the current lookahead
// symbol within a pragma.

typedef m2_token_t (*m2_pragma_lookahead_handler_f)(m2_parser_t);


// ---------------------------------------------------------------------------
// function:  m2_new_pragma_parser(parser, handlers ... , status)
// ---------------------------------------------------------------------------
//
// Creates and returns a new pragma parser object associated with a parser and
// several handlers.  The status of the operation  is passed back  in <status>
// unless NULL is passed in for <status>.
//
// Returns NULL if the pragma parser object could not be created.

m2_pragma_parser_t m2_new_pragma_parser(m2_parser_t parser,
                           m2_pragma_cond_handler_f cond_handler,
                           m2_pragma_expr_handler_f expr_handler,
                         m2_pragma_getsym_handler_f getsym_handler,
                      m2_pragma_lookahead_handler_f lookahead_handler,
                          m2_pragma_parser_status_t *status);


// ---------------------------------------------------------------------------
// function:  m2_parse_pragma(pragma_parser, status)
// ---------------------------------------------------------------------------
//
// Parses  the pragma  at the current position  of the parser  associated with
// pragma parser <pragma_parser>.  The status of the operation  is passed back
// in <status> unless NULL is passed in for <status>.

void m2_parse_pragma(m2_pragma_parser_t pragma_parser,
                     m2_parser_status_t *status);


// ---------------------------------------------------------------------------
// function:  m2_dispose_parser(pragma_parser, status)
// ---------------------------------------------------------------------------
//
// Deallocates  pragma parser object <pragma_parser>.  The status of the oper-
// ation  is  passed back in <status> unless NULL is passed in for <status>.

void m2_dispose_pragma_parser(m2_pragma_parser_t pragma_parser,
                              m2_parser_status_t *status);


#endif /* M2_PRAGMA_PARSER_H */

// END OF FILE
