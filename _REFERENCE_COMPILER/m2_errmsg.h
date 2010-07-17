/* Modula-2 R10 Compiler (m2r10c)
 *
 *  m2_errmsg.h
 *  Error message interface
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

#ifndef M2_ERRMSG_H
#define M2_ERRMSG_H


// --------------------------------------------------------------------------
// Error type
// --------------------------------------------------------------------------

typedef enum /* m2_err_t */ {
    ERR_OPT_NOT_IMPLEMENTED,
    ERR_OPT_INVALID,
    ERR_ALLOC_FAILED,
    ERR_SRC_NOT_SPECIFIED,
    
    NUMBER_OF_ERROR_MESSAGES
} m2_err_t;


// --------------------------------------------------------------------------
// function:  error_message()
// --------------------------------------------------------------------------
//
// Returns the error message associated with <error>.  If the value passed in
// for <error> is invalid,  the function will return NULL.

const char *error_message(m2_err_t error);


#endif /* M2_ERRMSG_H */

// END OF FILE
