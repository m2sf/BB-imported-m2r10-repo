/* Modula-2 R10 Compiler (m2r10c)
 *
 *  driver.c
 *  Test driver implementation
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "driver.h"


// --------------------------------------------------------------------------
// Exit codes
// --------------------------------------------------------------------------

#ifndef EXIT_SUCCESS
#define EXIT_SUCCESS 0
#endif

#ifndef EXIT_FAILURE
#define EXIT_FAILURE 1
#endif


// ---------------------------------------------------------------------------
// Test cases type
// ---------------------------------------------------------------------------

typedef struct /* test_cases_t */
{
    unsigned int total;
    unsigned int used;
    test_case_t *cases;
} test_cases_t;


// ---------------------------------------------------------------------------
// Test cases
// ---------------------------------------------------------------------------

test_cases_t tests;


// ---------------------------------------------------------------------------
// Test statistics type
// ---------------------------------------------------------------------------

typedef struct /* test_statistics_t */
{
    unsigned int total;
    unsigned int success;
    unsigned int failed;
} test_statistics_t;


// ---------------------------------------------------------------------------
// Test statistics
// ---------------------------------------------------------------------------

test_statistics_t statistics;


// --------------------------------------------------------------------------
// function:  initialize_test_environment()
// --------------------------------------------------------------------------
//
// Initialises the registered test cases and test statistics.

void
initialize_test_environment(void)
{
    // Allocate space for 32 test cases.
    tests.total = 32;
    tests.used = 0;
    tests.cases = malloc(32 * sizeof(test_case_t));
    
    // Initialize the test statistics.
    statistics.total = 0;
    statistics.success = 0;
    statistics.failed = 0;
}


// --------------------------------------------------------------------------
// function:  run_tests()
// --------------------------------------------------------------------------
//
// Runs all test cases that have been registered.

void
run_tests(void)
{
    unsigned i;
    
    // Run all test cases.
    for (i = 0; i < tests.used; i++)
        tests.cases[i]();
}


// --------------------------------------------------------------------------
// function:  generate_report()
// --------------------------------------------------------------------------
//
// Generates a report containing results of tests that have been run.

void
generate_report(void)
{
    // Generate the report.
    printf("Tests run: %u\n", statistics.total);
    printf("Tests succeeded: %u\n", statistics.success);
    printf("Tests failed: %u\n", statistics.failed);
}


// --------------------------------------------------------------------------
// function:  add_test(test)
// --------------------------------------------------------------------------
//
// Adds <test> to the list of test cases to be run.

void
add_test(test_case_t test)
{
    // Grow the buffer if needed.
    if (tests.used == tests.total);
    {
        tests.total *= 2;
        tests.cases = realloc(tests.cases, tests.total * sizeof(test_case_t));
    }
    
    // Add the test case to the buffer.
    tests.cases[tests.used] = test;
    tests.used++;
}


// --------------------------------------------------------------------------
// function:  assert_true(file, line, condition)
// --------------------------------------------------------------------------
//
// Checks if <condition> is true, and prints <file> and <line> if not.

#undef assert_true

void
assert_true(const char *file, int line, int condition)
{
    // Register the test has run.
    statistics.total++;
    
    if (condition == 0)
    {
        // Register the failure.
        statistics.failed++;
        
        // Print the file and line, if any.
        if (file != NULL)
            printf("%s:%d\n", file, line);
    }
    else
        // Register the success
        statistics.success++;
}


// --------------------------------------------------------------------------
// function:  assert_false(file, line, condition)
// --------------------------------------------------------------------------
//
// Checks if <condition> is false, and prints <file> and <line> if not.

#undef assert_false

void
assert_false(const char *file, int line, int condition)
{
    // Forward the call.
    assert_true(file, line, condition == 0);
}


// --------------------------------------------------------------------------
// function:  assert_same_string(file, line, a, b)
// --------------------------------------------------------------------------
//
// Checks if <a> and <b> are identical, and prints <file> and <line> if not.

#undef assert_same_string

void
assert_same_string(const char *file, int line,
                   const char *a, const char *b)
{
    // Compare and forward the call.
    assert_true(file, line, strcmp(a, b) == 0);
}


// --------------------------------------------------------------------------
// function:  main()
// --------------------------------------------------------------------------
//
// Collects and runs the tests.

int
main (void)
{
    // Initialise the test environment.
    initialize_test_environment();

    // Collect all tests cases to run.
    collect_tests();
    
    // Run all tests cases.
    run_tests();

    // Generate a test report.
    generate_report();
    
    return EXIT_SUCCESS;
}

// END OF FILE
