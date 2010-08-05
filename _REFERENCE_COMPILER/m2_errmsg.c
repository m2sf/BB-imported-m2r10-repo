/*! Modula-2 R10 Compiler (m2r10c)
 *
 *  @file  m2_errmsg.c
 *  @brief Error message implementation
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

#include "m2_errmsg.h"


// --------------------------------------------------------------------------
// Error messages
// --------------------------------------------------------------------------

static const char *_error_message[] = {
    "option not yet implemented.",
    "invalid option.",
    "memory allocation failed.",
    "source file not specified."
}; // end _error_message


// --------------------------------------------------------------------------
// function:  error_message()
// --------------------------------------------------------------------------
//
// Returns the error message associated with <error>.  If the value passed in
// for <error> is invalid,  the function will return NULL.

const char *error_message(m2_err_t error) {
    
    // bail out if error number is invalid
    if (error >= NUMBER_OF_ERROR_MESSAGES)
        return NULL;
    
    // return error message
    return _error_message[error];
} // end error_message


// END OF FILE
