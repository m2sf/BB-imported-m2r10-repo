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
    cardinal expected_path_length;
    cardinal expected_dir_length;
    cardinal expected_file_length;
    cardinal expected_base_length;
    cardinal expected_ext_length;
    char *expected_path;
    char *expected_dir;
    char *expected_file;
    char *expected_base;
    char *expected_ext;
} filename_info_t;


// ---------------------------------------------------------------------------
// Filename information
// ---------------------------------------------------------------------------

static const filename_info_t _filename_info[] = {
    // For POSIX
    { "dir", "file", FILE_TYPE_MOD, POSIX_FILENAMING, "dir/file.mod",
      M2_FILENAME_STATUS_SUCCESS, 12, 4, 8, 4, 3,
      "dir/file.mod", "dir/", "file.mod", "file", "mod" },
    
    { "dir/", "file", FILE_TYPE_DEF, POSIX_FILENAMING, "dir/file.def",
      M2_FILENAME_STATUS_SUCCESS, 12, 4, 8, 4, 3,
      "dir/file.def", "dir/", "file.def", "file", "def" },
    
    { "dir", "1337", FILE_TYPE_MOD, POSIX_FILENAMING, "dir/3.mod",
      M2_FILENAME_STATUS_INVALID_FILENAME, 0, 0, 0, 0, 0,
      NULL, NULL, NULL, NULL },
    
    // For MSDOS
    { "dir", "file", FILE_TYPE_MOD, MSDOS_FILENAMING, "dir\\file.mod",
      M2_FILENAME_STATUS_SUCCESS, 12, 4, 8, 4, 3,
      "dir\\file.mod", "dir\\", "file.mod", "file", "mod" },
    
    { "dir\\", "file", FILE_TYPE_DEF, MSDOS_FILENAMING, "dir\\file.def",
      M2_FILENAME_STATUS_SUCCESS, 12, 4, 8, 4, 3,
      "dir\\file.def", "dir\\", "file.def", "file", "def" },
    
    { "dir", "1337", FILE_TYPE_MOD, MSDOS_FILENAMING, "dir\\3.mod",
      M2_FILENAME_STATUS_INVALID_FILENAME, 0, 0, 0, 0, 0,
      NULL, NULL, NULL, NULL },
    
    // For OpenVMS
    { "[dir]", "file;3", FILE_TYPE_MOD, OPENVMS_FILENAMING, "[dir]file.mod;3",
      M2_FILENAME_STATUS_SUCCESS, 13, 5, 8, 4, 3,
      "[dir]file.mod", "[dir]", "file.mod", "file", "mod" },
    
    { "[dir]", "file;3", FILE_TYPE_DEF, OPENVMS_FILENAMING, "[dir]file.def;3",
      M2_FILENAME_STATUS_SUCCESS, 13, 5, 8, 4, 3,
      "[dir]file.def", "[dir]", "file.def", "file", "def" },
    
    { "[dir]", "1337;3", FILE_TYPE_MOD, OPENVMS_FILENAMING, "[dir]1337.mod;3",
      M2_FILENAME_STATUS_INVALID_FILENAME, 0, 0, 0, 0, 0,
      NULL, NULL, NULL, NULL }
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
// - m2_path_string_length()
// - m2_directory_string_length()
// - m2_filename_string_length()
// - m2_basename_string_length()
// - m2_file_ext_string_length()
// - m2_copy_path_string()
// - m2_copy_directory_string()
// - m2_copy_filename_string()
// - m2_copy_basename_string()
// - m2_copy_file_ext_string()
// - m2_file_type()
// - m2_filenaming()

static void m2_filename_t_test(m2_filename_t filename,
                               const filename_info_t *info)
{
    cardinal length;
    char *buffer;
    
    // Get the path.
    length = m2_path_string_length(filename);
    buffer = malloc(length + 1);
    m2_copy_path_string(filename, buffer);
    
    // Check if the path is as expected.
    assert_true(length == info->expected_path_length);
    assert_same_string(buffer, info->expected_path);
    free(buffer);
    
    // Get the directory.
    length = m2_directory_string_length(filename);
    buffer = malloc(length + 1);
    m2_copy_directory_string(filename, buffer);
    
    // Check if the directory is as expected.
    assert_true(length == info->expected_dir_length);
    assert_same_string(buffer, info->expected_dir);
    free(buffer);
    
    // Get the filename.
    length = m2_filename_string_length(filename);
    buffer = malloc(length + 1);
    m2_copy_filename_string(filename, buffer);
    
    // Check if the filename is as expected.
    assert_true(length == info->expected_file_length);
    assert_same_string(buffer, info->expected_file);
    free(buffer);
    
    // Get the basename.
    length = m2_basename_string_length(filename);
    buffer = malloc(length + 1);
    m2_copy_basename_string(filename, buffer);
    
    // Check if the basename is as expected.
    assert_true(length == info->expected_base_length);
    assert_same_string(buffer, info->expected_base);
    free(buffer);
    
    // Get the file extension.
    length = m2_file_ext_string_length(filename);
    buffer = malloc(length + 1);
    m2_copy_file_ext_string(filename, buffer);
    
    // Check if the file extension is as expected.
    assert_true(length == info->expected_ext_length);
    assert_same_string(buffer, info->expected_ext);
    free(buffer);
    
    // Check if the file type is as expected.
    assert_equal(m2_file_type(filename), info->type);
    
    // Check if the filenaming is as expected.
    assert_equal(m2_filenaming(filename), info->naming);
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
// function:  m2_new_filename_from_filename_test()
// --------------------------------------------------------------------------
//
// Tests the m2_new_filename_from_filename() function.

static void m2_new_filename_from_filename_test(void)
{
    int i;
    m2_filename_t from_filename;
    m2_filename_t filename;
    m2_filename_status_t status;
    
    // Check all filename information for the expected associated behaviour.
    for (i = 0; i < sizeof(_filename_info) / sizeof(_filename_info[0]); i++)
    {
        // Allocate a filename descriptor.
        from_filename = m2_new_filename(
            _filename_info[i].dir, _filename_info[i].file,
            _filename_info[i].type, _filename_info[i].naming, NULL
        );
        
        // Allocate a filename descriptor based on the previous one.
        filename = m2_new_filename_from_filename(
            from_filename, _filename_info[i].type, &status
        );
        
        if (from_filename == NULL)
        {
            // No descriptor should mean failure.
            assert_true(filename == NULL);
            assert_true(status == M2_FILENAME_STATUS_INVALID_REFERENCE);
        }
        else
        {
            // A descriptor should mean success.
            assert_true(status == M2_FILENAME_STATUS_SUCCESS);
            
            // Check the filename descriptor.
            m2_filename_t_test(filename, &_filename_info[i]);
            
            // Clean up.
            m2_dispose_filename(filename);
            m2_dispose_filename(from_filename);
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
    add_test(m2_new_filename_from_filename_test);
    add_test(m2_new_filename_from_path_test);
}


// END OF FILE
