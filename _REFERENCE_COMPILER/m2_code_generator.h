/* Modula-2 R10 Compiler (m2r10c)
 *
 *  m2_code_generator.h
 *  Code generator interface
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

#ifndef M2_CODEGEN_H
#define M2_CODEGEN_H


// ---------------------------------------------------------------------------
// C standard library imports
// ---------------------------------------------------------------------------

// FROM stdio IMPORT FILE;
#include <stdio.h>

// ---------------------------------------------------------------------------
// Embedded library imports
// ---------------------------------------------------------------------------

// FROM common IMPORT opaque_t;
#include "common.h"

// FROM KVS IMPORT kvs_table_t;
#include "KVS.h"

// ---------------------------------------------------------------------------
// Project library imports
// ---------------------------------------------------------------------------

// FROM m2_notifications IMPORT m2_notification_f;
#include "m2_notifications.h"

// FROM m2_symbol_table IMPORT m2_symtab_t;
#include "m2_symbol_table.h"

// FROM m2_ast IMPORT m2_ast_node_t;
#include "m2_ast.h"


// ---------------------------------------------------------------------------
// Opaque code generator handle type
// ---------------------------------------------------------------------------
//
// WARNING: Objects of this opaque type should only be accessed through this
// public interface.  DO NOT EVER attempt to bypass the public interface.
//
// The internal data structure of this opaque type is HIDDEN  and  MAY CHANGE
// at any time WITHOUT NOTICE. Accessing the internal data structure directly
// other than through the  functions  in this public interface is  UNSAFE and
// may result in an inconsistent program state or a crash.

typedef opaque_t m2_codegen_t;


// ---------------------------------------------------------------------------
// Status codes
// ---------------------------------------------------------------------------

typedef /* m2_codegen_status_t */ enum {
    
    // operation completed successfully
    M2_CODEGEN_STATUS_SUCCESS = 0,
    
    // invalid code generator object passed
    M2_CODEGEN_STATUS_INVALID_REFERENCE,
        
    // unable to allocate memory
    M2_CODEGEN_STATUS_ALLOCATION_FAILED
    
} m2_codegen_status_t;


// ---------------------------------------------------------------------------
// Target codes
// ---------------------------------------------------------------------------

typedef /* m2_codegen_target_t */ enum {
    M2_CODEGEN_TARGET_AST,
    M2_CODEGEN_TARGET_C99,
    M2_CODEGEN_TARGET_LLVM    
} m2_codegen_target_t;


// ---------------------------------------------------------------------------
// function:  m2_new_codegen(outfile, lextab, symtab, ast, handler, status)
// ---------------------------------------------------------------------------
//
// Creates  and returns a  new  code generator object  associated with  output
// file <outfile>  and  lexeme table <lextab>.  The status of the operation is
// passed back in <status> unless NULL is passed in for <status>.
//
// Returns NULL if the code generator object could not be created.

m2_codegen_t m2_new_codegen(FILE *outfile,
                     kvs_table_t lextab,
                     m2_symtab_t symtab,
                   m2_ast_node_t ast,
               m2_notification_f handler,
             m2_codegen_status_t *status);


// ---------------------------------------------------------------------------
// function:  m2_gen_code(cgen, target, status)
// ---------------------------------------------------------------------------
//
// Generates output for target <target>  and writes it  to the file associated
// with code generator <cgen>.  The status of the operation  is passed back in
// <status>  unless NULL is passed in for <status>.

void m2_gen_code(m2_codegen_t cgen,
          m2_codegen_target_t target,
          m2_codegen_status_t *status);


// ---------------------------------------------------------------------------
// function:  m2_dispose_codegen(cgen, status)
// ---------------------------------------------------------------------------
//
// Deallocates code generator object <cgen>.  The  status  of the operation is
// passed back in <status> unless NULL is passed in for <status>.

void m2_dispose_codegen(m2_codegen_t cgen, m2_codegen_status_t *status);


#endif /* M2_CODEGEN_H */

// END OF FILE
