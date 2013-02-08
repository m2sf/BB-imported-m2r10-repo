/*! Modula-2 R10 Compiler (m2r10c)
 *
 *  @file  m2_fileio.h
 *  @brief File IO interface
 *
 *  @b Author: Benjamin Kowarsch
 *
 *  @b Copyright: (C) 2010-2013 B.Kowarsch. All rights reserved.
 *
 *  @b License:
 *
 *  Permission is hereby granted to review and test this software for the sole
 *  purpose of supporting the effort by the licensor  to implement a reference
 *  compiler for  Modula-2 R10.  It is not permissible under any circumstances
 *  to  use the software  for the purpose  of creating derivative languages or 
 *  dialects.  This permission is valid until 31 December 2013, 24:00h GMT.
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
    
    // file with invalid file type passed
    M2_FILEIO_STATUS_INVALID_FILE_TYPE,
    
    // unable to allocate memory
    M2_FILEIO_STATUS_ALLOCATION_FAILED
    
} m2_fileio_status_t;


// ---------------------------------------------------------------------------
// File position type
// ---------------------------------------------------------------------------

typedef /* m2_file_pos_t */ struct {
    uint16_t line;
    uint16_t col;
} m2_file_pos_t;


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
// function:  m2_fileio_file_type(file)
// ---------------------------------------------------------------------------
//
// Returns the file type of <file> or FILE_TYPE_UNKNOWN if <file> is NULL.

m2_file_type_t m2_fileio_file_type(m2_file_t file);


// ---------------------------------------------------------------------------
// function:  m2_fileio_read(file)
// ---------------------------------------------------------------------------
//
// Reads  one octet  of data  at the  current position  of <file>,  returns it
// and advances the read/write position of <file> by one.

int m2_fileio_read(m2_file_t file);


// ---------------------------------------------------------------------------
// function:  m2_fileio_lookahead(file)
// ---------------------------------------------------------------------------
//
// Returns the lookahead character in the input stream of <file>  and returns
// it without incrementing the file pointer  and  without changing the file's
// coloumn and line counters.

int m2_fileio_lookahead(m2_file_t file);


// ---------------------------------------------------------------------------
// function:  m2_fileio_lookahead2(file)
// ---------------------------------------------------------------------------
//
// Returns the  second  lookahead character in the input stream of <file> and
// returns it without incrementing the file pointer  and without changing the
// file's coloumn and line counters.

int m2_fileio_lookahead2(m2_file_t file);


// ---------------------------------------------------------------------------
// function:  m2_fileio_write(file, codepoint)
// ---------------------------------------------------------------------------
//
// Writes one octet of data  passed in <codepoint>  at the current position of
// <file> and advances the read/write position of <file> by one.

void m2_fileio_write(m2_file_t file, octet_t codepoint);


// ---------------------------------------------------------------------------
// function:  m2_fileio_filename(file)
// ---------------------------------------------------------------------------
//
// Returns a pointer  to the  stored filename  of file descriptor <file>.  The
// pointer is returned as a pointer to an  immutable  string.  Returns NULL if
// the file descriptor is NULL.

const char *m2_fileio_filename(m2_file_t file);
    

// ---------------------------------------------------------------------------
// function:  m2_fileio_getpos(file, position)
// ---------------------------------------------------------------------------
//
// Obtains the current read/write position of file <file>  and  passes it back
// in <position>.  No data is passed back if <file> is NULL.

void m2_fileio_getpos(m2_file_t file, m2_file_pos_t *position);


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
// Closes the file  associated with  file IO object <file>.

void m2_close_file(m2_file_t file);


#endif /* M2_FILEIO_H */

// END OF FILE
