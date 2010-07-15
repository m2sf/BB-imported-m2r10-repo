/* Modula-2 R10 Compiler (m2r10c)
 *
 *  m2_parser.h
 *  Parser interface
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

#ifndef M2_PARSER_H
#define M2_PARSER_H

// --------------------------------------------------------------------------
// Opaque lexer handle type
// --------------------------------------------------------------------------
//
// WARNING: Objects of this opaque type should only be accessed through this
// public interface.  DO NOT EVER attempt to bypass the public interface.
//
// The internal data structure of this opaque type is HIDDEN  and  MAY CHANGE
// at any time WITHOUT NOTICE. Accessing the internal data structure directly
// other than through the  functions  in this public interface is  UNSAFE and
// may result in an inconsistent program state or a crash.

typedef opaque_t m2_parser_t;


// --------------------------------------------------------------------------
// Status codes
// --------------------------------------------------------------------------

typedef /* m2_parser_status_t */ enum {
    
    // operation completed successfully
    M2_PARSER_STATUS_SUCCESS = 0,
    
    // invalid pointer to parser object passed
    M2_PARSER_STATUS_INVALID_REFERENCE,
    
    // unable to allocate memory
    M2_PARSER_STATUS_ALLOCATION_FAILED,
    
    // one or more syntax errors encountered
    M2_PARSER_STATUS_SYNTAX_ERRORS_FOUND
} m2_parser_status_t;


// --------------------------------------------------------------------------
// function:  m2_new_parser(infile, lextab, status)
// --------------------------------------------------------------------------
//
// Creates  and returns  a  new  parser object  associated  with  source file 
// <infile> and lexeme table <lextab>.  The status of the operation is passed
// back in <status> unless NULL is passed in for <status>.
//
// Returns NULL if the parser object could not be created.

m2_parser_t m2_new_parser(FILE *infile,
                    kvs_table_t lextab,
             m2_parser_status_t *status);


// ---------------------------------------------------------------------------
// function:  m2_dispose_parser(lexer, status)
// ---------------------------------------------------------------------------
//
// Deallocates  lexer object <lexer>.  The function does  not  close the input
// stream  and  it  does  not  deallocate the lexeme table associated with the
// lexer object.  The  status  of  the  operation  is  passed back in <status>
// unless NULL is passed in for <status>.

void m2_dispose_parser(m2_parser_t parser, m2_parser_status_t *status);


#endif /* M2_PARSER_H */

// END OF FILE
