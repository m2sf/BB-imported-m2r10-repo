/* Modula-2 R10 Compiler (m2r10c)
 *
 *  m2_lexer_test.c
 *  Lexer interface tests
 *
 *  Author: Eric Streit, Roel Messiant
 *
 *  Copyright (C) 2010 E.Streit, R.Messiant. All rights reserved.
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

// ---------------------------------------------------------------------------
// imports
// ---------------------------------------------------------------------------

#include <stdlib.h>

#include "../driver.h"
#include "../../_REFERENCE_COMPILER/KVS.h"
#include "../../_REFERENCE_COMPILER/m2_lexer.h"
#include "../../_REFERENCE_COMPILER/m2_symbol_table.h"


// ---------------------------------------------------------------------------
// Lexer input information type
// ---------------------------------------------------------------------------

typedef struct /* lexer_input_info_t */ {
    char *file;
    int good;
    int bad;
    int lines;
    int columns;
} lexer_input_info_t;


// ---------------------------------------------------------------------------
// Lexer input information
// ---------------------------------------------------------------------------

static const lexer_input_info_t _input_info[] = {
    { "comment.tst", 0, 2, 29, 1 },
    { "special.tst", 32, 0, 22, 1 },
    { "numeric.tst", 17, 11, 30, 1 },
    { "string.tst", 5, 6, 32, 1 },
    { "identifier.tst", 13, 0, 18, 1 },
    { "reserved.tst", 44, 0, 27, 1 }
}; /* _input_info */


// ---------------------------------------------------------------------------
// function:  m2_lexer_t_test()
// ---------------------------------------------------------------------------
//
// Tests the interaction between m2_lexer_t and the following functions:
// - m2_lexer_getpos()
// - m2_lexer_getsym()

static void m2_lexer_t_test(m2_lexer_t lexer, const lexer_input_info_t *info)
{
    int i;
    cardinal line;
    cardinal column;
    m2_lexer_status_t status;
    m2_token_t token;
    
    // Test the initial lexer position.
    m2_lexer_getpos(lexer, &line, &column, &status);
    assert_true(status == M2_LEXER_STATUS_SUCCESS);
    assert_true(line == 1);
    assert_true(column == 1);
    
    // Read the expected amount of good lexemes.
    for (i = 0; i < info->good; i++)
    {
        // Read the next lexeme.
        token = m2_lexer_getsym(lexer, NULL, &status);
        
        // Make sure nothing went wrong.
        assert_true(status == M2_LEXER_STATUS_SUCCESS);
    }
    
    // Read the expected amount of bad lexemes.
    for (i = 0; i < info->bad; i++)
    {
        // Read the next lexeme.
        token = m2_lexer_getsym(lexer, NULL, &status);
        
        // Make sure a lexing error happened.
        assert_false(status == M2_LEXER_STATUS_SUCCESS);
        assert_false(status == M2_LEXER_STATUS_INVALID_REFERENCE);
        assert_false(status == M2_LEXER_STATUS_ALLOCATION_FAILED);
    }
    
    // Make sure the next token is EOF.
    token = m2_lexer_getsym(lexer, NULL, &status);
    assert_true(status == M2_LEXER_STATUS_SUCCESS);
    assert_true(token == TOKEN_EOF_MARKER);
    
    // Test the final lexer position.
    m2_lexer_getpos(lexer, &line, &column, &status);
    assert_true(status == M2_LEXER_STATUS_SUCCESS);
    assert_true(line == info->lines);
    assert_true(column == info->columns);
}


// ---------------------------------------------------------------------------
// function:  m2_new_lexer_test()
// ---------------------------------------------------------------------------
//
// Tests the m2_new_lexer() function.

static void m2_new_lexer_test(void)
{
    int i;
    m2_filename_t filename;
    m2_file_t file;
    kvs_table_t table;
    m2_lexer_t lexer;
    m2_lexer_status_t status;
    
    // Check all input information for the expected associated behaviour.
    for (i = 0; i < sizeof(_input_info) / sizeof(_input_info[0]); i++)
    {
        // Allocate the filename descriptor.
        filename = m2_new_filename_from_path(
            _input_info[i].file, POSIX_FILENAMING, NULL
        );
        
        // Make sure allocating the filename descriptor succeeded.
        assert_false(filename == NULL);
        if (filename == NULL)
            continue;
        
        // Open the input file and clean up the filename.
        file = m2_open_sourcefile(filename, NULL);
        m2_dispose_filename(filename);
        
        // Make sure opening the file succeeded.
        assert_false(file == NULL);
        if (file == NULL)
            continue;
        
        // Create a symbol table.
        table = kvs_new_table(0, NULL);
        
        // Make sure creating the symbol table succeeded.
        assert_false(table == NULL);
        if (table == NULL)
        {
            m2_close_file(file);
            continue;
        }
        
        // Create a lexer.
        lexer = m2_new_lexer(file, table, &status);
        
        // Make sure creating the lexer succeeded.
        assert_true(status == M2_LEXER_STATUS_SUCCESS);
        
        if (lexer != NULL)
        {
            // Test the lexer.
            m2_lexer_t_test(lexer, &_input_info[i]);
            
            // Clean up the lexer.
            m2_dispose_lexer(lexer, &status);
            
            // Make sure disposing the lexer succeeded.
            assert_true(status == M2_LEXER_STATUS_SUCCESS);
        }
        
        // Clean up the symbol table.
        kvs_dispose_table(table, NULL);
    }
} // end main


// --------------------------------------------------------------------------
// function:  collect_tests()
// --------------------------------------------------------------------------
//
// User-supplied function that registers test cases to be run.

void
collect_tests(void)
{
    // Add all of our test cases.
    add_test(m2_new_lexer_test);
}


// END OF FILE
