/* Modula-2 R10 Compiler (m2r10c)
 *
 *  m2_code_generator.c
 *  Code generator implementation
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


#include "m2_code_generator.h"


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
                            m2_codegen_status_t *status) {
    
    // TO DO
    
    return NULL;
} // end m2_new_codegen


// ---------------------------------------------------------------------------
// function:  m2_gen_code(cgen, target, status)
// ---------------------------------------------------------------------------
//
// Generates output for target <target>  and writes it  to the file associated
// with code generator <cgen>.  The status of the operation  is passed back in
// <status>  unless NULL is passed in for <status>.

void m2_gen_code(m2_codegen_t cgen,
                 m2_codegen_target_t target,
                 m2_codegen_status_t *status) {
    
    // TO DO
    
    return;
} // end m2_gen_code


// ---------------------------------------------------------------------------
// function:  m2_dispose_codegen(cgen, status)
// ---------------------------------------------------------------------------
//
// Deallocates code generator object <cgen>.  The  status  of the operation is
// passed back in <status> unless NULL is passed in for <status>.

void m2_dispose_codegen(m2_codegen_t cgen, m2_codegen_status_t *status) {
    
    // TO DO
    
    return;
} // end m2_dispose_codegen


// END OF FILE
