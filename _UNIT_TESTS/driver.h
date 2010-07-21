/* Modula-2 R10 Compiler (m2r10c)
 *
 *  driver.h
 *  Test driver interface
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

#ifndef M2_TESTDRIVER_H
#define M2_TESTDRIVER_H


// --------------------------------------------------------------------------
// Test case type
// --------------------------------------------------------------------------

typedef void (*test_case_t)(void);


// --------------------------------------------------------------------------
// function:  collect_tests()
// --------------------------------------------------------------------------
//
// User-supplied function that registers test cases to be run.

void collect_tests(void);


// --------------------------------------------------------------------------
// function:  add_test(test)
// --------------------------------------------------------------------------
//
// Adds <test> to the list of test cases to be run.

void add_test(test_case_t test);


// --------------------------------------------------------------------------
// function:  assert_true(file, line, condition)
// --------------------------------------------------------------------------
//
// Checks if <condition> is true, and prints a message if not.

void assert_true(const char *file, int line, int condition);


// --------------------------------------------------------------------------
// function:  assert_false(file, line, condition)
// --------------------------------------------------------------------------
//
// Checks if <condition> is false, and prints a message if not.

void assert_false(const char *file, int line, int condition);


// --------------------------------------------------------------------------
// macro:  assert_same_string(file, line, got, expect)
// --------------------------------------------------------------------------
//
// Checks if <got> and <expect> are identical, and prints a message if not.

void assert_same_string(const char *file, int line,
                        const char *got, const char *expect);


// --------------------------------------------------------------------------
// macro:  assert_true(condition)
// --------------------------------------------------------------------------
//
// Checks if <condition> is true, and prints a message if not.

#define assert_true(condition)  assert_true(__FILE__, __LINE__, condition)


// --------------------------------------------------------------------------
// macro:  assert_false(condition)
// --------------------------------------------------------------------------
//
// Checks if <condition> is true, and prints a message if not.

#define assert_false(condition)  assert_false(__FILE__, __LINE__, condition)


// --------------------------------------------------------------------------
// macro:  assert_equal(a, b)
// --------------------------------------------------------------------------
//
// Checks if <a> equals <b>, and prints a message if not.

#define assert_equal(a, b)  assert_true((a) == (b))


// --------------------------------------------------------------------------
// macro:  assert_same_string(a, b)
// --------------------------------------------------------------------------
//
// Checks if <a> and <b> are identical, and prints a message if not.

#define assert_same_string(a, b)  assert_same_string(__FILE__, __LINE__, a, b)


#endif /* M2_TESTDRIVER_H */

// END OF FILE
