/* Modula-2 R10 Compiler (m2r10c)
 *
 *  m2_fileio_test.c
 *  File IO interface tests
 *
 *  Author: Roel Messiant
 *
 *  Copyright (C) 2010 R.Messiant. All rights reserved.
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

#include <stddef.h>

#include "../driver.h"
#include "../../_REFERENCE_COMPILER/m2_fileio.h"
#include "../../_REFERENCE_COMPILER/ASCII.h"


// --------------------------------------------------------------------------
// File information type
// --------------------------------------------------------------------------

typedef struct /* file_info_t */
{
    char *path;
    m2_filenaming_t naming;
    int lines;
    int columns;
    char *data;
} file_info_t;


// ---------------------------------------------------------------------------
// File information
// ---------------------------------------------------------------------------

static const file_info_t _file_info[] = {
    // For POSIX
    { "./empty.def", POSIX_FILENAMING,
      1, 1, "" },
    { "./fourlines.mod", POSIX_FILENAMING,
      4, 11, "1010101010\n0101010101\n1010101010\n0101010101" }
}; /* _file_info */


// --------------------------------------------------------------------------
// function:  m2_fileposition_test()
// --------------------------------------------------------------------------
//
// Tests if reading or writing <codepoint> from <file> correctly updated the
// file position from (<previous_line>, <previous_column>) to the current one.

static void m2_fileposition_test(m2_file_t file, octet_t codepoint,
                                 cardinal prev_line, cardinal prev_column)
{
    cardinal line;
    cardinal column;
    
    // Get the current file position.
    m2_fileio_getpos(file, &line, &column);
    
    if (codepoint == ASCII_NUL)
    {
        // Check if the EOF is as expected.
        assert_true(m2_fileio_eof(file) == true);
        assert_true(line == prev_line);
        assert_true(column == prev_column);
    }
    else
    if (codepoint == ASCII_LF)
    {
        // Check if a new line has been started in the file position.
        assert_true(m2_fileio_eof(file) == false);
        assert_true(line == prev_line + 1);
        assert_true(column == 1);
    }
    else
    {
        // Check if one column has been advanced in the file position.
        assert_true(m2_fileio_eof(file) == false);
        assert_true(line == prev_line);
        assert_true(column == prev_column + 1);
    }
}

// --------------------------------------------------------------------------
// function:  m2_writefile_test()
// --------------------------------------------------------------------------
//
// Tests the interaction between m2_file_t and the following functions:
// - m2_fileio_write()
// - m2_fileio_getpos()

static void m2_writefile_test(m2_file_t file, const file_info_t *info)
{
    int i;
    cardinal line;
    cardinal column;
    
    // Test the initial file position.
    m2_fileio_getpos(file, &line, &column);
    assert_true(line == 1);
    assert_true(column == 1);
    
    // Write all data to the file.
    for (i = 0; info->data[i] != CSTRING_TERMINATOR; i++)
    {
        // Write a character.
        m2_fileio_write(file, info->data[i]);
        
        // Check if the file position was advanced properly.
        m2_fileposition_test(file, info->data[i], line, column);
        
        // Remember this new file position.
        m2_fileio_getpos(file, &line, &column);
    }
    
    // Test if the file pointer ended up as expected.
    assert_true(line == info->lines);
    assert_true(column == info->columns);
}


// --------------------------------------------------------------------------
// function:  m2_new_outfile_test()
// --------------------------------------------------------------------------
//
// Tests the m2_new_outfile() function.

static void m2_new_outfile_test(void)
{
    int i;
    m2_filename_t filename;
    m2_file_t file;
    m2_fileio_status_t status;
    
    // Check all file information for the expected associated behaviour.
    for (i = 0; i < sizeof(_file_info) / sizeof(_file_info[0]); i++)
    {
        // Allocate the filename descriptor.
        filename = m2_new_filename_from_path(
            _file_info[i].path, _file_info[i].naming, NULL
        );
        
        // Make sure the allocation succeeded.
        assert_false(filename == NULL);
        if (filename == NULL)
            continue;
        
        // Allocate the file descriptor.
        file = m2_new_outfile(filename, &status);
        
        // Make sure the allocation succeeded.
        assert_true(status == M2_FILEIO_STATUS_SUCCESS);
        
        if (file != NULL)
        {
            // Test the file descriptor.
            m2_writefile_test(file, &_file_info[i]);
            
            // Clean up the file descriptor.
            m2_close_file(file);
        }
        
        // Clean up the filename descriptor.
        m2_dispose_filename(filename);
    }
}


// --------------------------------------------------------------------------
// function:  m2_readfile_test()
// --------------------------------------------------------------------------
//
// Tests the interaction between m2_file_t and the following functions:
// - m2_fileio_read()
// - m2_fileio_lookahead()
// - m2_fileio_getpos()
// - m2_fileio_eof()

static void m2_readfile_test(m2_file_t file, const file_info_t *info)
{
    int i;
    cardinal line;
    cardinal column;
    int codepoint;
    
    // Test the initial file position.
    m2_fileio_getpos(file, &line, &column);
    assert_true(line == 1);
    assert_true(column == 1);
    
    // Test if looking ahead works.
    m2_fileio_lookahead(file, &codepoint);
    m2_fileio_getpos(file, &line, &column);
    assert_true(line == 1);
    assert_true(column == 1);
    
    for (i = 0; m2_fileio_eof(file) == false; i++)
    {
        // Read a character.
        m2_fileio_read(file, &codepoint);
        
        // Test if this character was expected.
        assert_equal(codepoint, info->data[i]);
        
        // Check if the file position was advanced properly.
        m2_fileposition_test(file, info->data[i], line, column);
        
        // Remember this new file position.
        m2_fileio_getpos(file, &line, &column);
    }
    
    // Test if the file pointer ended up as expected.
    assert_true(line == info->lines);
    assert_true(column == info->columns);
}


// --------------------------------------------------------------------------
// function:  m2_open_sourcefile_test()
// --------------------------------------------------------------------------
//
// Tests the m2_open_sourcefile() function.

static void m2_open_sourcefile_test(void)
{
    int i;
    m2_filename_t filename;
    m2_file_t file;
    m2_fileio_status_t status;
    
    // Check all file information for the expected associated behaviour.
    for (i = 0; i < sizeof(_file_info) / sizeof(_file_info[0]); i++)
    {
        // Allocate the filename descriptor.
        filename = m2_new_filename_from_path(
            _file_info[i].path, _file_info[i].naming, NULL
        );
        
        // Make sure the allocation succeeded.
        assert_false(filename == NULL);
        if (filename == NULL)
            continue;
        
        // Allocate the file descriptor.
        file = m2_open_sourcefile(filename, &status);
        
        // Make sure the allocation succeeded.
        assert_true(status == M2_FILEIO_STATUS_SUCCESS);
        
        if (file != NULL)
        {
            // Test the file descriptor.
            m2_readfile_test(file, &_file_info[i]);
            
            // Clean up the file descriptor.
            m2_close_file(file);
        }
        
        // Clean up the filename descriptor.
        m2_dispose_filename(filename);
    }
}


// --------------------------------------------------------------------------
// function:  collect_tests()
// --------------------------------------------------------------------------
//
// User-supplied function that registers test cases to be run.

void
collect_tests(void)
{
    // Add all of our test cases.
    add_test(m2_new_outfile_test);
    add_test(m2_open_sourcefile_test);
}


// END OF FILE
