/*! Modula-2 R10 Compiler (m2r10c)
 *
 *  @file  m2_pragma_parser.c
 *  @brief Pragma parser implementation
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

#include <stddef.h>
#include "m2_pragma_parser.h"


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
                          m2_pragma_parser_status_t *status) {
    
    // TO DO
    
    return NULL;
} // end m2_new_pragma_parser


// ---------------------------------------------------------------------------
// function:  m2_parse_pragma(pragma_parser, status)
// ---------------------------------------------------------------------------
//
// Parses  the pragma  at the current position  of the parser  associated with
// pragma parser <pragma_parser>.  The status of the operation  is passed back
// in <status> unless NULL is passed in for <status>.

void m2_parse_pragma(m2_pragma_parser_t pragma_parser,
                     m2_parser_status_t *status) {
    
    // TO DO
    
    return;
} // end m2_parse_pragma


// ---------------------------------------------------------------------------
// function:  m2_dispose_parser(pragma_parser, status)
// ---------------------------------------------------------------------------
//
// Deallocates  pragma parser object <pragma_parser>.  The status of the oper-
// ation  is  passed back in <status> unless NULL is passed in for <status>.

void m2_dispose_pragma_parser(m2_pragma_parser_t pragma_parser,
                              m2_parser_status_t *status) {
    
    // TO DO
    
    return;
} // end m2_dispose_pragma_parser
