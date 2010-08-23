/*! Modula-2 R10 Compiler (m2r10c)
 *
 *  @file  m2_fileio.c
 *  @brief File IO interface
 *
 *  @b Author: Benjamin Kowarsch, Roel Messiant
 *
 *  @b Copyright: (C) 2010 B.Kowarsch, R.Messiant. All rights reserved.
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


// ---------------------------------------------------------------------------
// C standard library imports
// ---------------------------------------------------------------------------

#include <stdio.h>
#include <stddef.h>

// ---------------------------------------------------------------------------
// Project imports
// ---------------------------------------------------------------------------

#include "m2_fileio.h"
#include "alloc.h"
#include "ASCII.h"


// ---------------------------------------------------------------------------
// File descriptor
// ---------------------------------------------------------------------------

typedef struct /* m2_file_s */ {
    FILE *handle;
    m2_file_type_t type;
    m2_file_pos_t position;
    bool end_of_file;
    char filename[];
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

m2_file_t m2_open_sourcefile(m2_filename_t filename,
                             m2_fileio_status_t *status) {
    m2_file_s *new_file;
    m2_file_type_t file_type;
    m2_filename_status_t filename_status;
    char pathname[m2_path_string_length(filename) + 1];
    
    // bail out if filename is invalid
    if (filename == NULL) {
        ASSIGN_BY_REF(status, M2_FILEIO_STATUS_INVALID_REFERENCE);
        return NULL;
    } // end if
    
    // get the file type
    file_type = m2_file_type(filename);
    
    // bail out if the file type does not represent a source file
    if ((file_type != FILE_TYPE_DEF) && (file_type != FILE_TYPE_MOD)) {
        ASSIGN_BY_REF(status, M2_FILEIO_STATUS_INVALID_FILE_TYPE);
        return NULL;
    } // end if
        
    // allocate memory for a file descriptor
    new_file = ALLOCATE(sizeof(m2_file_s) +
                        m2_filename_string_length(filename) + 1);
    
    // bail out if allocation failed
    if (new_file == NULL) {
        ASSIGN_BY_REF(status, M2_FILEIO_STATUS_ALLOCATION_FAILED);
        return NULL;
    } // end if
    
    // copy filename with extension into file descriptor
    m2_copy_filename_string(filename, new_file->filename);
    
    // get the full file path
    m2_copy_path_string(filename, &pathname);
    
    // bail out if getting the file path failed
    if (filename_status != M2_FILENAME_STATUS_SUCCESS) {
        DEALLOCATE(new_file);
        ASSIGN_BY_REF(status, M2_FILEIO_STATUS_ALLOCATION_FAILED);
        return NULL;
    } // end if
    
    // open the file for reading
    new_file->handle = fopen(pathname, "r");
    
    // bail out if opening the file failed
    if (new_file->handle == NULL) {
        DEALLOCATE(new_file);
        ASSIGN_BY_REF(status, M2_FILEIO_STATUS_ALLOCATION_FAILED);
        return NULL;
    } // end if
        
    // initialise the file descriptor
    new_file->type = file_type;
    new_file->position.line = 1;
    new_file->position.col = 1;
    new_file->end_of_file = false;
    
    // pass status and new file descriptor back to caller
    ASSIGN_BY_REF(status, M2_FILEIO_STATUS_SUCCESS);
    return (m2_file_t) new_file;
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

m2_file_t m2_new_outfile(m2_filename_t filename,
                         m2_fileio_status_t *status) {
    m2_file_s *new_file;
    m2_file_type_t file_type;
    m2_filename_status_t filename_status;
    char pathname[m2_path_string_length(filename) + 1];

    // bail out if filename is invalid
    if (filename == NULL) {
        ASSIGN_BY_REF(status, M2_FILEIO_STATUS_INVALID_REFERENCE);
        return NULL;
    } // end if
    
    // get the file type
    file_type = m2_file_type(filename);
    
    // bail out if the file type represents a source file
    if ((file_type == FILE_TYPE_DEF) || (file_type == FILE_TYPE_MOD)) {
        ASSIGN_BY_REF(status, M2_FILEIO_STATUS_INVALID_FILE_TYPE);
        return NULL;
    } // end if
    
    // allocate memory for a file descriptor
    new_file = ALLOCATE(sizeof(m2_file_s) +
                        m2_filename_string_length(filename) + 1);
    
    // bail out if allocation failed
    if (new_file == NULL) {
        ASSIGN_BY_REF(status, M2_FILEIO_STATUS_ALLOCATION_FAILED);
        return NULL;
    } // end if
    
    // get the full file path
    m2_copy_path_string(filename, &pathname);
        
    // open the file for writing
    new_file->handle = fopen(pathname, "w");
    
    // bail out if opening the file failed.
    if (new_file->handle == NULL) {
        DEALLOCATE(new_file);
        ASSIGN_BY_REF(status, M2_FILEIO_STATUS_ALLOCATION_FAILED);
        return NULL;
    } // end if
        
    // initialise the file descriptor
    new_file->type = file_type;
    new_file->position.line = 1;
    new_file->position.col = 1;
    new_file->end_of_file = false;
    
    // pass status and new file descriptor back to caller
    ASSIGN_BY_REF(status, M2_FILEIO_STATUS_SUCCESS);
    return (m2_file_t) new_file;
} // end m2_new_outfile


// ---------------------------------------------------------------------------
// function:  m2_fileio_file_type(file)
// ---------------------------------------------------------------------------
//
// Returns the file type of <file> or FILE_TYPE_UNKNOWN if <file> is NULL.

m2_file_type_t m2_fileio_file_type(m2_file_t file) {
    m2_file_s *this_file = (m2_file_s *) file;
    
    if (file == NULL)
        return FILE_TYPE_UNKNOWN;
    
    return this_file->type;
} // end m2_fileio_file_type


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

int m2_fileio_read(m2_file_t file) {
    m2_file_s *this_file = (m2_file_s *) file;
    register int c;
    
    // read one character from source file
    c = getc(this_file->handle);
    
    // handle LF style end-of-line
    if (c == ASCII_LF) {
        this_file->position.col = 1;
        this_file->position.line++;
    }
    // handle CRLF and CR style end-of-line
    else if (c == ASCII_CR) {
        this_file->position.col = 1;
        this_file->position.line++;
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
        this_file->position.col++;
    } // end if
    
    if (((uchar_t) c == 255) || (c == 0)) {
        printf(""); // Don't remove this!
    } // end if
    
    // pass consumed character
    return c;
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

int m2_fileio_lookahead(m2_file_t file) {
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
    return c;
} // end m2_fileio_lookahead


// ---------------------------------------------------------------------------
// function:  m2_fileio_write(file, codepoint)
// ---------------------------------------------------------------------------
//
// Writes one octet of data  passed in <codepoint>  at the current position of
// <file> and advances the read/write position of <file> by one.

void m2_fileio_write(m2_file_t file, octet_t codepoint) {
    m2_file_s *this_file = (m2_file_s *) file;
    int written;
    
    // Write one character to the output file
    written = fputc(codepoint, this_file->handle);
    
    // Bail out on error.
    if (written == EOF)
        return;
    
    // Handle LF style line-feed.
    if (codepoint == ASCII_LF) {
        // Start a new line.
        this_file->position.col = 1;
        this_file->position.line++;
    }
    // Handle any other character.
    else {
        // Move to the next column.
        this_file->position.col++;
    } // end if
} // end m2_fileio_write


// ---------------------------------------------------------------------------
// function:  m2_fileio_filename(file)
// ---------------------------------------------------------------------------
//
// Returns a pointer  to the  stored filename  of file descriptor <file>.  The
// pointer is returned as a pointer to an  immutable  string.  Returns NULL if
// the file descriptor is NULL.

const char *m2_fileio_filename(m2_file_t file) {
    m2_file_s *this_file = (m2_file_s *) file;
    
    // bail out if file descriptor is NULL
    if (file == NULL)
        return NULL;
    
    return (const char *) this_file->filename;
} // end m2_fileio_filename


// ---------------------------------------------------------------------------
// function:  m2_fileio_getpos(file, position)
// ---------------------------------------------------------------------------
//
// Obtains the current read/write position of file <file>  and  passes it back
// in <position>.  No data is passed back if <file> is NULL.

void m2_fileio_getpos(m2_file_t file, m2_file_pos_t *position) {
    m2_file_s *this_file = (m2_file_s *) file;
    
    if (file != NULL)
        ASSIGN_BY_REF(position, this_file->position);
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
// function:  m2_close_file(file, status)
// ---------------------------------------------------------------------------
//
// Closes the file  associated with  file IO object <file>.

void m2_close_file(m2_file_t file) {
    m2_file_s *this_file = (m2_file_s *) file;
    
    // Bail out if file descriptor is NULL.
    if (this_file == NULL)
        return;
    
    // Close the file handle.
    fclose(this_file->handle);
    
    // Deallocate the file descriptor.
    DEALLOCATE(this_file);
} // end m2_close_file


// END OF FILE
