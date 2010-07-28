/* Modula-2 R10 Compiler (m2r10c)
 *
 *  m2_fileio.h
 *  File IO interface
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


#include "stddef.h"
#include "m2_fileio.h"


// ---------------------------------------------------------------------------
// function:  m2_open_sourcefile(filename, status)
// ---------------------------------------------------------------------------
//
// Creates  and  returns  a new  file IO object  associated  with  source file
// <filename>.  The status of the operation is  passed back in <status> unless
// NULL is passed in for <status>.
//
// Returns NULL if the file IO object could not be created.

m2_fileio_t m2_open_sourcefile(m2_filename_t filename,
                               m2_fileio_status_t *status) {
    
    // TO DO
    
    return NULL;
} // end m2_open_sourcefile


// ---------------------------------------------------------------------------
// function:  m2_new_outfile(filename, status)
// ---------------------------------------------------------------------------
//
// Creates  and  returns  a new  file IO object  associated  with  output file
// <filename>.  The status of the operation is  passed back in <status> unless
// NULL is passed in for <status>.
//
// Returns NULL if the file IO object could not be created.

m2_fileio_t m2_new_outfile(m2_filename_t filename,
                           m2_fileio_status_t *status) {
    
    // TO DO
    
    return NULL;
} // end m2_new_outfile


// ---------------------------------------------------------------------------
// function:  m2_new_outfile(filename, status)
// ---------------------------------------------------------------------------
//
// Closes the file  associated with  file IO object <file>.  The status of the
// operation is passed back in <status> unless NULL is passed in for <status>.
//
// Returns NULL if the file IO object could not be created.

void m2_close_file(m2_fileio_t file, m2_fileio_status_t *status) {
    
    // TO DO
    
    return;
} // end m2_close_file


// END OF FILE
