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

#ifndef M2_FILEIO_H
#define M2_FILEIO_H


// ---------------------------------------------------------------------------
// Embedded library imports
// ---------------------------------------------------------------------------

// FROM common IMPORT opaque_t;
#include "common.h"

// ---------------------------------------------------------------------------
// Project library imports
// ---------------------------------------------------------------------------

// FROM m2_filenames IMPORT m2_filename_t;
#include "m2_filenames.h"


// ---------------------------------------------------------------------------
// Opaque code generator handle type
// ---------------------------------------------------------------------------
//
// WARNING: Objects of this opaque type should only be accessed through this
// public interface.  DO NOT EVER attempt to bypass the public interface.
//
// The internal data structure of this opaque type is HIDDEN  and  MAY CHANGE
// at any time WITHOUT NOTICE. Accessing the internal data structure directly
// other than through the  functions  in this public interface is  UNSAFE and
// may result in an inconsistent program state or a crash.

typedef opaque_t m2_file_t;


// ---------------------------------------------------------------------------
// Status codes
// ---------------------------------------------------------------------------

typedef /* m2_fileio_status_t */ enum {
    
    // operation completed successfully
    M2_FILEIO_STATUS_SUCCESS = 0,
    
    // invalid file IO object passed
    M2_FILEIO_STATUS_INVALID_REFERENCE,
    
    // unable to allocate memory
    M2_FILEIO_STATUS_ALLOCATION_FAILED
    
} m2_fileio_status_t;


// ---------------------------------------------------------------------------
// function:  m2_open_sourcefile(filename, status)
// ---------------------------------------------------------------------------
//
// Creates  and  returns  a new  file IO object  associated  with  source file
// <filename>.  The status of the operation is  passed back in <status> unless
// NULL is passed in for <status>.
//
// Returns NULL if the file IO object could not be created.

m2_file_t m2_open_sourcefile(m2_filename_t filename,
                        m2_fileio_status_t *status);


// ---------------------------------------------------------------------------
// function:  m2_new_outfile(filename, status)
// ---------------------------------------------------------------------------
//
// Creates  and  returns  a new  file IO object  associated  with  output file
// <filename>.  The status of the operation is  passed back in <status> unless
// NULL is passed in for <status>.
//
// Returns NULL if the file IO object could not be created.

m2_file_t m2_new_outfile(m2_filename_t filename,
                    m2_fileio_status_t *status);


// ---------------------------------------------------------------------------
// function:  m2_fileio_read(file, codepoint)
// ---------------------------------------------------------------------------
//
// Reads one octet of data at the current position of <file>, passes it back
// in <codepoint> and advances the read/write position of <file> by one.

void m2_fileio_read(m2_file_t file, int *codepoint);


// ---------------------------------------------------------------------------
// function:  m2_fileio_lookahead(file, codepoint)
// ---------------------------------------------------------------------------
//
// Passes in <codepoint> the octet  that is going to be read next  by function
// m2_fileio_read().  This function does not update the read/write position of
// <file>.

void m2_fileio_lookahead(m2_file_t file, int *codepoint);


// ---------------------------------------------------------------------------
// function:  m2_fileio_write(file, codepoint)
// ---------------------------------------------------------------------------
//
// Writes one octet of data  passed in <codepoint>  at the current position of
// <file> and advances the read/write position of <file> by one.

void m2_fileio_write(m2_file_t file, octet_t codepoint);


// ---------------------------------------------------------------------------
// function:  m2_fileio_getpos(file, line, col)
// ---------------------------------------------------------------------------
//
// Obtains the  current read/write position  of file <file>.  The line counter
// is passed back in <line> and the coloumn counter is passed back in <col>.

void m2_fileio_getpos(m2_file_t file, cardinal *line, cardinal *col);


// ---------------------------------------------------------------------------
// function:  m2_fileio_eof(file)
// ---------------------------------------------------------------------------
//
// Returns true if <file> has reached end-of-file status, otherwise false.

bool m2_fileio_eof(m2_file_t file);


// ---------------------------------------------------------------------------
// function:  m2_close_file(file, status)
// ---------------------------------------------------------------------------
//
// Closes the file  associated with  file IO object <file>.  The status of the
// operation is passed back in <status> unless NULL is passed in for <status>.
//
// Returns NULL if the file IO object could not be created.

void m2_close_file(m2_file_t file, m2_fileio_status_t *status);


#endif /* M2_FILEIO_H */

// END OF FILE
