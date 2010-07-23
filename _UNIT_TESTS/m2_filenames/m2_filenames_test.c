/* Modula-2 R10 Compiler (m2r10c)
 *
 *  m2_filenames_test.c
 *  Portable filename handling interface tests
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

// TO DO: Add more test data.
// TO DO: Add tests for m2_working_directory.

#include <stddef.h>
#include <stdlib.h>

#include "../driver.h"
#include "../../_REFERENCE_COMPILER/m2_filenames.h"


// ---------------------------------------------------------------------------
// Filename validity information type
// ---------------------------------------------------------------------------

typedef struct /* filename_validity_info_t */ {
    char *name;
    
    bool valid;
} filename_validity_info_t;


// ---------------------------------------------------------------------------
// Filename validity information
// ---------------------------------------------------------------------------

static const filename_validity_info_t _validity_info[] = {
    // Valid with a single letter
    { "a", true },
    { "Z", true },
    { "_", true },
    { "$", true },
    
    // Invalid with a single letter
    { "1", false },
    { "&", false },
    { "-", false },
    { " ", false },
    
    // Valid with multiple letters
    { "azerty", true },    
    { "az3rty", true },
    { "a_Z_e_$_r_T_y", true},
    
    // Invalid with multiple letters
    { "4z3rty", false },
    { "Meet M2R10", false }
}; /* _validity_info */


// ---------------------------------------------------------------------------
// Target/file type information type
// ---------------------------------------------------------------------------

typedef struct /* target_file_type_info_t */ {
    m2_target_t target;
    m2_file_type_t source;
    m2_file_type_t output;
} target_file_type_info_t;


// --------------------------------------------------------------------------
// Target/file type information
// --------------------------------------------------------------------------

static const target_file_type_info_t _type_info[] = {
    // For C99
    { M2_TARGET_C99, FILE_TYPE_UNKNOWN, FILE_TYPE_UNKNOWN },
    
    // For LLVM
    { M2_TARGET_LLVM, FILE_TYPE_UNKNOWN, FILE_TYPE_UNKNOWN }
};


// --------------------------------------------------------------------------
// Filename information type
// --------------------------------------------------------------------------

typedef struct /* filename_info_t */
{
    char *dir;
    char *file;
    m2_file_type_t type;
    m2_filenaming_t naming;
    char *path;
    
    m2_filename_status_t expect_status;
    char *expect_dir;
    char *expect_file;
    char *expect_ext;
    char *expect_path;
} filename_info_t;


// ---------------------------------------------------------------------------
// Filename information
// ---------------------------------------------------------------------------

static const filename_info_t _filename_info[] = {
    // For POSIX
    { "dir", "file", FILE_TYPE_MOD, POSIX_FILENAMING, "dir/file.mod",
      M2_FILENAME_STATUS_SUCCESS, "dir/", "file", "mod", "dir/file.mod" },
    { "dir/", "file", FILE_TYPE_DEF, POSIX_FILENAMING, "dir/file.def",
      M2_FILENAME_STATUS_SUCCESS, "dir/", "file", "def", "dir/file.def" },
    
    // For MSDOS
    { "dir", "file", FILE_TYPE_MOD, MSDOS_FILENAMING, "dir\\file.mod",
      M2_FILENAME_STATUS_SUCCESS, "dir\\", "file", "mod", "dir\\file.mod" },
    { "dir\\", "file", FILE_TYPE_DEF, MSDOS_FILENAMING, "dir\\file.def",
      M2_FILENAME_STATUS_SUCCESS, "dir\\", "file", "def", "dir\\file.def" }
}; /* _filename_info */


// --------------------------------------------------------------------------
// function:  m2_working_directory_test()
// --------------------------------------------------------------------------
//
// Tests the m2_working_directory() function.

static void m2_working_directory_test(void)
{
}


// --------------------------------------------------------------------------
// function:  m2_is_valid_filename_string_test()
// --------------------------------------------------------------------------
//
// Tests the m2_is_valid_filename_string() function.

static void m2_is_valid_filename_string_test(void)
{
    int i;
    
    // Check all filenames for their expected validity.
    for (i = 0; i < sizeof(_validity_info) / sizeof(_validity_info[0]); i++)
    {
        assert_equal(
            m2_is_valid_filename_string(_validity_info[i].name),
            _validity_info[i].valid
        );
    }
}


// --------------------------------------------------------------------------
// function:  m2_outfile_type_for_test()
// --------------------------------------------------------------------------
//
// Tests the m2_outfile_type_for() function.

static void m2_outfile_type_for_test(void)
{
    int i;
    
    // Check all source and target types for their expected output file type.
    for (i = 0; i < sizeof(_type_info) / sizeof(_type_info[0]); i++)
    {
        assert_equal(
            m2_outfile_type_for(_type_info[i].target, _type_info[i].source),
            _type_info[i].output
        );
    }
}


// --------------------------------------------------------------------------
// function:  m2_filename_t_test()
// --------------------------------------------------------------------------
//
// Tests the interaction between m2_filename_t and the following functions:
// - m2_directory_string()
// - m2_filename_string()
// - m2_file_ext_string()
// - m2_file_type()
// - m2_filenaming()
// - m2_path_from_filename()

static void m2_filename_t_test(m2_filename_t filename,
                               const filename_info_t *info)
{
    m2_filename_status_t status;
    const char *path;
    
    // Check if the directory is as expected.
    assert_same_string(m2_directory_string(filename), info->expect_dir);
    
    // Check if the filename is as expected.
    assert_same_string(m2_filename_string(filename), info->expect_file);
    
    // Check if the extension is as expected.
    assert_same_string(m2_file_ext_string(filename), info->expect_ext);
    
    // Check if the file type is as expected.
    assert_equal(m2_file_type(filename), info->type);
    
    // Check if the filenaming is as expected.
    assert_equal(m2_filenaming(filename), info->naming);
    
    // Get the filepath.
    path = m2_path_from_filename(filename, &status);
    
    // Compare with our expectation.
    assert_equal(status, info->expect_status);
    
    // Check if more tests are applicable.
    if (path != NULL)
    {
        // Check if the path is as expected.
        assert_same_string(path, info->expect_path);
        
        // Clean up.
        free(path);
    }
}


// --------------------------------------------------------------------------
// function:  m2_new_filename_test()
// --------------------------------------------------------------------------
//
// Tests the m2_new_filename() function.

static void m2_new_filename_test(void)
{
    int i;
    m2_filename_t filename;
    m2_filename_status_t status;
    
    // Check all filename information for the expected associated behaviour.
    for (i = 0; i < sizeof(_filename_info) / sizeof(_filename_info[0]); i++)
    {
        // Allocate a filename descriptor.
        filename = m2_new_filename(
            _filename_info[i].dir, _filename_info[i].file,
            _filename_info[i].type, _filename_info[i].naming, &status
        );
        
        // Compare with our expectation.
        assert_equal(status, _filename_info[i].expect_status);
        
        if (filename == NULL)
        {
            // No descriptor should mean failure.
            assert_false(status == M2_FILENAME_STATUS_SUCCESS);
        }
        else
        {
            // A descriptor should mean success.
            assert_true(status == M2_FILENAME_STATUS_SUCCESS);
            
            // Check the filename descriptor.
            m2_filename_t_test(filename, &_filename_info[i]);
            
            // Clean up.
            m2_dispose_filename(filename);
        }
    }
}


// --------------------------------------------------------------------------
// function:  m2_new_filename_from_path_test()
// --------------------------------------------------------------------------
//
// Tests the m2_new_filename_from_path() function.

static void m2_new_filename_from_path_test(void)
{
    int i;
    m2_filename_t filename;
    m2_filename_status_t status;
    
    // Check all filename information for the expected associated behaviour.
    for (i = 0; i < sizeof(_filename_info) / sizeof(_filename_info[0]); i++)
    {
        // Allocate a filename descriptor.
        filename = m2_new_filename_from_path(
            _filename_info[i].path, _filename_info[i].naming, &status
        );
        
        // Compare with our expectation.
        assert_equal(status, _filename_info[i].expect_status);
        
        if (filename == NULL)
        {
            // No descriptor should mean failure.
            assert_false(status == M2_FILENAME_STATUS_SUCCESS);
        }
        else
        {
            // A descriptor should mean success.
            assert_true(status == M2_FILENAME_STATUS_SUCCESS);
            
            // Check the filename descriptor.
            m2_filename_t_test(filename, &_filename_info[i]);
            
            // Clean up.
            m2_dispose_filename(filename);
        }
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
    add_test(m2_is_valid_filename_string_test);
    add_test(m2_outfile_type_for_test);
    add_test(m2_new_filename_test);
    add_test(m2_new_filename_from_path_test);
}


// END OF FILE
