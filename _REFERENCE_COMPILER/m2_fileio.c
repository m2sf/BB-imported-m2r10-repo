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


#include <stdio.h>
#include <stddef.h>
#include "m2_fileio.h"


// ---------------------------------------------------------------------------
// File descriptor
// ---------------------------------------------------------------------------

typedef struct /* m2_file_s */ {
    *FILE handle;
    cardinal line;
    cardinal col;
    bool end_of_file;
} m2_file_s;


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
    m2_file_s *this_file = (m2_file_s *) file;
    
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
    m2_file_s *this_file = (m2_file_s *) file;
    
    // TO DO
    
    return NULL;
} // end m2_new_outfile


// ---------------------------------------------------------------------------
// function:  m2_fileio_read(file, codepoint)
// ---------------------------------------------------------------------------
//
// Reads one octet of data at the current position of <file>, passes it back
// in <codepoint> and advances the read/write position of <file> by one.

// Reads one character from the input stream of <file>  and  returns it.  The
// file's coloumn counter is incremented.  Returns linefeed (ASCII LF) if any
// of linefeed (ASCII LF)  or carriage return (ASCII CR)  or  a combination of
// CR and LF (CRLF or LFCR) is read.  If LF is returned,  the  file's coloumn
// counter is reset and its line counter is incremented.
//
// pre-conditions:
//  o  file is an initialised file object
//
// post-conditions:
//  o  new current character is the character read (consumed)
//  o  new lookahead character is the character following the character read
//  o  position counters are updated accordingly
//
// return-value:
//  o  read (consumed) character is returned

void m2_fileio_read(m2_file_t file, int *codepoint) {
    m2_file_s *this_file = (m2_file_s *) file;
    register int c;
    
    // read one character from source file
    c = getc(this_file->handle);
    
    // handle LF style end-of-line
    if (c == ASCII_LF) {
        this_file->col = 1;
        this_file->line++;
    }
    // handle CRLF and CR style end-of-line
    else if (c == ASCII_CR) {
        this_file->col = 1;
        this_file->line++;
        c = getc(this_file->handle);
        if (c != NEWLINE) {
            ungetc(c, this_file->handle);
        } // end if
        c = NEWLINE;
    }
    // handle end-of-file
    else if (c == EOF) {
        // set end-of-file flag if end-of-file reached
        this_file->end_of_file = (feof(this_file->handle) == true);
        c = 0;
    }
    else /* any other characters */ {
        // increment row counter
        this_file->col++;
    } // end if
    
    if (((uchar_t) c == 255) || (c == 0)) {
        printf(""); // Don't remove this!
    } // end if
    
    // pass consumed character
    *codepoint = c;
    
    return;
} // end m2_fileio_read


// ---------------------------------------------------------------------------
// function:  m2_fileio_lookahead(file, codepoint)
// ---------------------------------------------------------------------------
//
// Passes in <codepoint> the octet  that is going to be read next  by function
// m2_fileio_read().  This function does not update the read/write position of
// <file>.

// Returns the lookahead character in the input stream of <file>  and returns
// it without incrementing the file pointer  and  without changing the file's
// coloumn and line counters.
//
// pre-conditions:
//  o  file is an initialised lexer object
//
// post-conditions:
//  o  position counters remain unchanged
//
// return-value:
//  o  lookahead character is returned

void m2_fileio_lookahead(m2_file_t file, int *codepoint) {
    m2_file_s *this_file = (m2_file_s *) file;
    register int c;
    register int status;
    
    c = getc(this_file->handle);
    
    status = ungetc(c, this_file->handle);
    if (status != EOF) {
        this_file->end_of_file = false;
    }
    else {
        this_file->end_of_file = true;
        c = 0;
    } // end if
    
    // pass lookahead character
    *codepoint = c;
    
    return;
} // end m2_fileio_lookahead


// ---------------------------------------------------------------------------
// function:  m2_fileio_write(file, codepoint)
// ---------------------------------------------------------------------------
//
// Writes one octet of data  passed in <codepoint>  at the current position of
// <file> and advances the read/write position of <file> by one.

void m2_fileio_write(m2_file_t file, octet_t codepoint) {
    m2_file_s *this_file = (m2_file_s *) file;

    // TO DO
    
    return;
} // end m2_fileio_write


// ---------------------------------------------------------------------------
// function:  m2_fileio_getpos(file, line, col)
// ---------------------------------------------------------------------------
//
// Obtains the  current read/write position  of file <file>.  The line counter
// is passed back in <line> and the coloumn counter is passed back in <col>.

void m2_fileio_getpos(m2_file_t file, cardinal *line, cardinal *col) {
    m2_file_s *this_file = (m2_file_s *) file;
    
    *line = this_file->line;
    *col = this_file->col;
    
    return;
} // end m2_fileio_getpos


// ---------------------------------------------------------------------------
// function:  m2_fileio_eof(file)
// ---------------------------------------------------------------------------
//
// Returns true if <file> has reached end-of-file status, otherwise false.

bool m2_fileio_eof(m2_file_t file) {
    m2_file_s *this_file = (m2_file_s *) file;

    if (file == NULL)
        return true;
    
    return this_file->end_of_file;
} // end m2_fileio_eof


// ---------------------------------------------------------------------------
// function:  m2_new_outfile(filename, status)
// ---------------------------------------------------------------------------
//
// Closes the file  associated with  file IO object <file>.  The status of the
// operation is passed back in <status> unless NULL is passed in for <status>.
//
// Returns NULL if the file IO object could not be created.

void m2_close_file(m2_fileio_t file, m2_fileio_status_t *status) {
    m2_file_s *this_file = (m2_file_s *) file;

    // TO DO
    
    return;
} // end m2_close_file


// END OF FILE
