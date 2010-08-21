/*! Modula-2 R10 Compiler (m2r10c)
 *
 *  @file  m2_lexer.h
 *  @brief Lexer interface
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


#ifndef M2_LEXER_H
#define M2_LEXER_H


// ---------------------------------------------------------------------------
// Embedded library imports
// ---------------------------------------------------------------------------

// FROM common IMPORT opaque_t, cardinal;
#include "common.h"

// FROM KVS IMPORT kvs_table_t;
#include "KVS.h"

// ---------------------------------------------------------------------------
// Project library imports
// ---------------------------------------------------------------------------

// FROM m2_tokens IMPORT m2_token_t;
#include "m2_tokens.h"

// FROM m2_fileio IMPORT m2_file_t;
#include "m2_fileio.h"

// FROM m2_notifications IMPORT m2_notification_f;
#include "m2_notifications.h"


// ---------------------------------------------------------------------------
// Opaque lexer handle type
// ---------------------------------------------------------------------------
//
// WARNING: Objects of this opaque type should only be accessed through this
// public interface.  DO NOT EVER attempt to bypass the public interface.
//
// The internal data structure of this opaque type is HIDDEN  and  MAY CHANGE
// at any time WITHOUT NOTICE. Accessing the internal data structure directly
// other than through the  functions  in this public interface is  UNSAFE and
// may result in an inconsistent program state or a crash.

typedef opaque_t m2_lexer_t;


// ---------------------------------------------------------------------------
// Status codes
// ---------------------------------------------------------------------------

typedef /* m2_lexer_status_t */ enum {
    
    // operation completed successfully
    M2_LEXER_STATUS_SUCCESS = 0,
    
    // invalid pointer to lexer object passed
    M2_LEXER_STATUS_INVALID_REFERENCE,
    
    // invalid notification handler passed
    M2_LEXER_STATUS_INVALID_HANDLER,
    
    // unable to allocate memory
    M2_LEXER_STATUS_ALLOCATION_FAILED,
    
    // illegal character found
    M2_LEXER_STATUS_ILLEGAL_CHARACTER,
    
    // literal exceeds maximum length
    M2_LEXER_STATUS_LITERAL_TOO_LONG,

    // numeric literal is malformed
    M2_LEXER_STATUS_MALFORMED_NUMBER,

    // string literal is missing delimiting quotation
    M2_LEXER_STATUS_STRING_NOT_DELIMITED,
    
    // nested comment exceeds maximum nesting limit
    M2_LEXER_STATUS_COMMENT_NESTING_LIMIT_REACHED,
    
    // EOF found while processing comment
    M2_LEXER_STATUS_EOF_REACHED_WITHIN_COMMENT
} m2_lexer_status_t;


// ---------------------------------------------------------------------------
// function:  m2_new_lexer(infile, lextab, handler, status)
// ---------------------------------------------------------------------------
//
// Creates  and  returns  a  new  lexer object  associated  with  source file 
// <infile> and lexeme table <lextab>.  The status of the operation is passed
// back in <status> unless NULL is passed in for <status>.
//
// Returns NULL if the lexer object could not be created.

m2_lexer_t m2_new_lexer(m2_file_t infile,
                      kvs_table_t lextab,
                m2_notification_f handler,
                m2_lexer_status_t *status);


// ---------------------------------------------------------------------------
// function:  m2_lexer_getsym(lexer, lexeme, status)
// ---------------------------------------------------------------------------
//
// Reads one symbol from the input stream of lexer <lexer>, returns its token,
// and passes a key for its lexeme back in <lexeme> unless  NULL  is passed in
// for <lexeme>.  The  status  of  the  operation  is  passed back in <status>
// unless NULL is passed in for <status>.

m2_token_t m2_lexer_getsym(m2_lexer_t lexer,
                             cardinal *lexeme,
                    m2_lexer_status_t *status);


// ---------------------------------------------------------------------------
// function:  m2_lexer_getpos(lexer, row, col, status)
// ---------------------------------------------------------------------------
//
// Obtains the position of the last symbol read from the input stream.  Passes
// the row back in <row>  unless NULL is passed in for <row>,  and the coloumn
// back in <col>  unless  NULL  is  passed  in  for <col>.  The status  of the
// operation is passed back in <status> unless NULL is passed in for <status>.

void m2_lexer_getpos(m2_lexer_t lexer,
                       cardinal *row,
                       cardinal *col,
              m2_lexer_status_t *status);


// ---------------------------------------------------------------------------
// function:  m2_offending_char(lexer, row, col, status)
// ---------------------------------------------------------------------------
//
// Returns the offending character of the last read operation  and  passes its
// position  back  in  <row>  and <col>.  If no error occurred during the last
// read operation then ASCII NUL is returned  and zero is passed pack in <row>
// and <col>.  This function should only be called  after a preceeding call to
// function objm2_lexer_getsym()  returned an error indicating that an illegal
// or unexcpected character was found.  The status of the operation  is passed
// back in <status> unless NULL is passed in for <status>.

char m2_offending_char(m2_lexer_t lexer,
                         cardinal *row,
                         cardinal *col,
                m2_lexer_status_t *status);


// ---------------------------------------------------------------------------
// function:  m2_dispose_lexer(lexer, status)
// ---------------------------------------------------------------------------
//
// Deallocates  lexer object <lexer>.  The function does  not  close the input
// stream  and  it  does  not  deallocate the lexeme table associated with the
// lexer object.  The  status  of  the  operation  is  passed back in <status>
// unless NULL is passed in for <status>.

void m2_dispose_lexer(m2_lexer_t lexer,
               m2_lexer_status_t *status);


#endif /* M2_LEXER_H */

// END OF FILE
