/* Modula-2 R10 Compiler (m2r10c)
 *
 *  KVS_test.c
 *  Key Value Storage interface tests
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

#include <stdlib.h>
#include <stddef.h>

#include "../driver.h"
#include "../../_REFERENCE_COMPILER/KVS.h"


// --------------------------------------------------------------------------
// Table information type
// --------------------------------------------------------------------------

typedef struct /* table_info_t */
{
    char *value;
    size_t size;
} table_info_t;


// ---------------------------------------------------------------------------
// Table information
// ---------------------------------------------------------------------------

static const table_info_t _table_info[] = {
    { "", 1 },
    { "a", 2 },
    { "ab", 3 },
    { "abc", 4 },
    { "abcd", 5 },
    { "abcde", 6 },
    { "abcdef", 7 },
    { "abcdefg", 8 },
    { "abcdefgh", 9 },
    { "abcdefghi", 10 },
    { "abcdefghij", 11 },
    { "abcdefghijk", 12 },
    { "abcdefghijkl", 13 },
    { "abcdefghijklm", 14 },
    { "abcdefghijklmn", 15 },
    { "abcdefghijklmno", 16 },
    { "abcdefghijklmnop", 17 },
    { "abcdefghijklmnopq", 18 },
    { "abcdefghijklmnopqr", 19 },
    { "abcdefghijklmnopqrs", 20 },
    { "abcdefghijklmnopqrst", 21 },
    { "abcdefghijklmnopqrstu", 22 },
    { "abcdefghijklmnopqrstuv", 23 },
    { "abcdefghijklmnopqrstuvw", 24 },
    { "abcdefghijklmnopqrstuvwx", 25 },
    { "abcdefghijklmnopqrstuvwxy", 26 },
    { "abcdefghijklmnopqrstuvwxyz", 27 }
}; /* _table_info */


// --------------------------------------------------------------------------
// function:  kvs_table_t_test()
// --------------------------------------------------------------------------
//
// Tests the interaction between kvs_table_t and the following functions:
// - kvs_store_value()
// - kve_store_reference()
// - kvs_entry_exists()
// - kvs_get_entry()
// - kvs_value_for_key()
// - kvs_reference_for_key
// - kvs_reference_count_for_key()
// - kvs_size_for_key()
// - kvs_data_for_key_is_null_terminated()
// - kvs_release_entry()
// - kvs_remove_entry()
// - kvs_number_of_entries()

static void kvs_table_t_test(kvs_table_t table, bool by_value, bool terminated)
{
    int i;
    int j;
    int ref_count;
    cardinal size;
    bool exists;
    bool is_terminated;
    char *value;
    kvs_status_t status;
    
    // Test if the table is initially empty.
    assert_true(kvs_number_of_entries(table) == 0);
    
    // Check adding values to the table for the expected associated behaviour.
    for (i = 0; i < sizeof(_table_info) / sizeof(_table_info[0]); i++)
    {
        // Have the storage figure out the size if null-terminated.
        if (terminated == true)
            size = 0;
        else
            size = _table_info[i].size;
         
        if (by_value == false)
        {
            // Copy value if storage is by reference.
            value = malloc(_table_info[i].size);
            for (j = 0; j < _table_info[i].size; j++)
                value[j] = _table_info[i].value[j];
            
            // Store the value by reference.
            kvs_store_reference(
                table, i + 1, value, size, terminated, &status
            );
        }
        else
            // Store the value by value.
            kvs_store_value(
                table, i + 1, _table_info[i].value, size, terminated, &status
            );
        
        assert_true(status == KVS_STATUS_SUCCESS);
        
        // An entry with the specified key should exist.
        exists = kvs_entry_exists(table, i + 1, &status);
        
        assert_true(status == KVS_STATUS_SUCCESS);
        assert_true(exists == true);
        
        // Attempt to add the value using the same key again.
        if (by_value == false)
            kvs_store_reference(
                table, i + 1, value, size, terminated, &status
            );
        else
            kvs_store_value(
                table, i + 1, _table_info[i].value, size, terminated, &status
            );
        
        assert_true(status == KVS_STATUS_KEY_NOT_UNIQUE);
        
        // The size of the value for the key should match ours.
        size = kvs_size_for_key(table, i + 1, &status);
        
        assert_true(status == KVS_STATUS_SUCCESS);
        assert_true(size == _table_info[i].size);
        
        // The null-termination of the data should match ours.
        is_terminated = kvs_data_for_key_is_null_terminated(
            table, i + 1, &status
        );
        
        assert_true(status == KVS_STATUS_SUCCESS);
        assert_true(is_terminated == terminated);
    }
    
    // Test if the expected amount of entries is in the table.
    assert_equal(
        kvs_number_of_entries(table),
        sizeof(_table_info) / sizeof(_table_info[0])
    );
    
    // Check retrieving values for the expected associated behaviour.
    for (i = 0; i < sizeof(_table_info) / sizeof(_table_info[0]); i++)
    {
        // There should be one reference to the key.
        ref_count = kvs_reference_count_for_key(table, i + 1, &status);
        
        assert_true(status = KVS_STATUS_SUCCESS);
        assert_true(ref_count == 1);
        
        // Retrieve the entry by copying.
        value = kvs_get_entry(
            table, true, i + 1, &size, &is_terminated, &status
        );
        
        assert_true(status == KVS_STATUS_SUCCESS);
        assert_true(size == _table_info[i].size);
        assert_true(is_terminated == terminated);
        assert_same_string(value, _table_info[i].value);
        
        free(value);
        
        // Retrieve the value of the entry.
        value = kvs_value_for_key(table, i + 1, &status);
        
        assert_true(status == KVS_STATUS_SUCCESS);
        assert_same_string(value, _table_info[i].value);
        
        free(value);
        
        // There should be one reference to the key.
        ref_count = kvs_reference_count_for_key(table, i + 1, &status);
        
        assert_true(status = KVS_STATUS_SUCCESS);
        assert_true(ref_count == 1);
        
        // Retrieve the entry by reference.
        value = kvs_get_entry(
            table, false, i + 1, &size, &is_terminated, &status
        );
        
        assert_true(status == KVS_STATUS_SUCCESS);
        assert_true(size == _table_info[i].size);
        assert_true(is_terminated == terminated);
        assert_same_string(value, _table_info[i].value);
        
        // Retrieve a reference to the entry.
        value = kvs_reference_for_key(table, i + 1, &status);
        
        assert_true(status == KVS_STATUS_SUCCESS);
        assert_same_string(value, _table_info[i].value);
        
        // There should be three references to the key.
        ref_count = kvs_reference_count_for_key(table, i + 1, &status);
        
        assert_true(status = KVS_STATUS_SUCCESS);
        assert_true(ref_count == 3);
        
        // Release two references to the key.
        kvs_release_entry(table, i + 1, &status);
        kvs_release_entry(table, i + 1, &status);
        
        // There should be one reference to the key.
        ref_count = kvs_reference_count_for_key(table, i + 1, &status);
        
        assert_true(status = KVS_STATUS_SUCCESS);
        assert_true(ref_count == 1);
    }
    
    // Check removing entries for the expected associated behaviour.
    for (i = 0; i < sizeof(_table_info) / sizeof(_table_info[0]); i++)
    {
        // Remove an existing key from the table.
        kvs_remove_entry(table, i + 1, &status);
        
        assert_true(status == KVS_STATUS_SUCCESS);
        
        // Attempt to remove the value with the same key again.
        kvs_remove_entry(table, i + 1, &status);
        
        assert_true(status == KVS_STATUS_ENTRY_NOT_FOUND);
    }
    
    // Test if the table is empty again.
    assert_true(kvs_number_of_entries(table) == 0);
}


// --------------------------------------------------------------------------
// function:  kvs_new_table_test()
// --------------------------------------------------------------------------
//
// Tests the kvs_new_table() function.

static void kvs_new_table_test(void)
{
    int buckets;
    kvs_table_t table;
    kvs_status_t status;
    
    // Check tables of with various bucket counts
    for (buckets = 0; buckets < 10; buckets++)
    {
        // Allocate a KVS table.
        table = kvs_new_table(buckets, &status);
        
        // Make sure the allocation succeeded.
        assert_true(status == KVS_STATUS_SUCCESS);
        if (table == NULL)
            continue;
        
        // Make sure the amount of buckets is as requested.
        if (buckets == 0)
            assert_true(kvs_number_of_buckets(table) == KVS_DEFAULT_TABLE_SIZE);
        else
            assert_true(kvs_number_of_buckets(table) == buckets);
        
        // Test the table.
        kvs_table_t_test(table, true, true);
        kvs_table_t_test(table, true, false);
        kvs_table_t_test(table, false, true);
        kvs_table_t_test(table, false, false);
        
        // Clean up the table.
        kvs_dispose_table(table, &status);
        
        // Make sure disposing the table succeeded.
        assert_true(status == KVS_STATUS_SUCCESS);
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
    add_test(kvs_new_table_test);
}


// END OF FILE
