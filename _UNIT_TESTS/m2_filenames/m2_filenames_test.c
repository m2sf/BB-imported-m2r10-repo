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
// TO DO: Add tests for m2_new_filename_from_path.
// TO DO: Add tests for m2_path_from_filename.

#include <stddef.h>

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
    char *name;
    char *directory;
    m2_file_type_t type;
    m2_filenaming_t naming;
    
    m2_filename_status_t status;
    char *ext;
} filename_info_t;


// ---------------------------------------------------------------------------
// Filename information
// ---------------------------------------------------------------------------

static const filename_info_t _filename_info[] = {
    { "filename", "directory", FILE_TYPE_MOD, DEFAULT_FILENAMING,
      M2_FILENAME_STATUS_SUCCESS, "mod" }
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
// function:  m2_new_filename_test()
// --------------------------------------------------------------------------
//
// Tests the m2_new_filename() function.

static void m2_new_filename_test(void)
{
    int i;
    m2_filename_t filename;
    m2_filename_status_t status;
    
    // Check all source file types and for their expected output file type.
    for (i = 0; i < sizeof(_filename_info) / sizeof(_filename_info[0]); i++)
    {
        // Allocate a filename descriptor.
        filename = m2_new_filename(
            _filename_info[i].directory, _filename_info[i].name,
            _filename_info[i].type, _filename_info[i].naming, &status
        );
        
        // Compare with our expectation.
        assert_equal(status, _filename_info[i].status);
        
        // Check if more tests are applicable.
        if (status != M2_FILENAME_STATUS_SUCCESS)
        {
            // Failure should mean no descriptor.
            assert_true(filename == NULL);
            
            // Clean up if needed.
            if (filename != NULL)
                m2_dispose_filename(filename);
            continue;
        }
        
        // Success should mean there is a descriptor.
        assert_true(filename != NULL);
        if (filename == NULL)
            continue;
        
        // Check if the directory is as expected.
        assert_same_string(
            m2_directory_string(filename),
            _filename_info[i].directory
        );
        
        // Check if the filename is as expected.
        assert_same_string(
            m2_filename_string(filename),
            _filename_info[i].name
        );
        
        // Check if the extension is as expected.
        assert_same_string(
            m2_file_ext_string(filename),
            _filename_info[i].ext
        );
        
        // Check if the file type is as expected.
        assert_equal(m2_file_type(filename), _filename_info[i].type);
        
        // Check if the filenaming is as expected.
        assert_equal(m2_filenaming(filename), _filename_info[i].naming);
        
        // Clean up.
        m2_dispose_filename(filename);
    }
}


// --------------------------------------------------------------------------
// function:  m2_new_filename_from_path_test()
// --------------------------------------------------------------------------
//
// Tests the m2_new_filename_from_path() function.

static void m2_new_filename_from_path_test(void)
{
}


// --------------------------------------------------------------------------
// function:  m2_path_from_filename_test()
// --------------------------------------------------------------------------
//
// Tests the m2_path_from_filename() function.

static void m2_path_from_filename_test(void)
{
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
}


// END OF FILE
