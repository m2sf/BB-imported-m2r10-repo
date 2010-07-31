/* Modula-2 R10 Compiler (m2r10c)
 *
 *  m2_symbol_table.h
 *  Symbol table interface
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

#ifndef M2_SYMTAB_H
#define M2_SYMTAB_H


#include "common.h"


// ---------------------------------------------------------------------------
// Opaque SYMTAB handle type
// ---------------------------------------------------------------------------
//
// WARNING:  Objects of this opaque type should  only be accessed through this
// public interface.  DO NOT EVER attempt to bypass the public interface.
//
// The internal data structure of this opaque type is  HIDDEN  and  MAY CHANGE
// at any time WITHOUT NOTICE.  Accessing the internal data structure directly
// other than  through the  functions  in this public interface is  UNSAFE and
// may result in an inconsistent program state or a crash.

typedef opaque_t m2_symtab_t;


// ---------------------------------------------------------------------------
// Status codes
// ---------------------------------------------------------------------------

typedef /* m2_symtab_status_t */ enum {
    M2_SYMTAB_STATUS_UNDEFINED = -1,
    M2_SYMTAB_STATUS_SUCCESS = 1,
    M2_SYMTAB_STATUS_UNABLE_TO_ALLOCATE
} m2_symtab_status_t;


// ---------------------------------------------------------------------------
// Symbol table scope selectors
// ---------------------------------------------------------------------------

typedef /* m2_symtab_scope_t */ enum {
    M2_SYMTAB_ANY_SCOPE,     // search in all scopes
    M2_SYMTAB_LOCAL_SCOPE,   // search in all local scopes
    M2_SYMTAB_GLOBAL_SCOPE,  // search only in the global scope
    M2_SYMTAB_CURRENT_SCOPE  // search only in the current scope
} m2_symtab_scope_t;


// ---------------------------------------------------------------------------
// Symbol types
// ---------------------------------------------------------------------------

typedef /* m2_symbol_type_t */ enum {
    M2_SYMTYPE_MODULE,
    M2_SYMTYPE_CONST,
    M2_SYMTYPE_TYPE,
    M2_SYMTYPE_VAR,
    M2_SYMTYPE_PROC
} m2_symbol_type_t;


// ---------------------------------------------------------------------------
// function:  m2_new_symtab(status)
// ---------------------------------------------------------------------------
//
// Returns a  new  symbol table.  Passes  the status  of the operation back in
// <status> unless NULL is passed for <status>.

m2_symtab_t m2_new_symtab(m2_symtab_status_t *status);


// ---------------------------------------------------------------------------
// function:  m2_symtab_new_scope(symtab, status)
// ---------------------------------------------------------------------------
//
// Adds a  new local scope  to symbol table <symtab>  within the current scope
// and sets the current scope  to the  new scope.  The status of the operation
// is passed back in <status> unless NULL is passed in for <status>.

void m2_symtab_new_scope(m2_symtab_t symtab, m2_symtab_status_t *status);


// ---------------------------------------------------------------------------
// function:  m2_symtab_insert(symtab, lexkey, index, symtype, status)
// ---------------------------------------------------------------------------
//
// Inserts the symbol whose lexeme key is <lexkey> into the  current scope  of
// symbol table <symtab>.  The  symbol type  is  recorded  as  <symtype>.  The
// operation fails  if a symbol with the same lexeme key is already present in
// the  current scope.  The status of the operation is passed back in <status>
// unless NULL is passed in for <status>.

void m2_symtab_insert(m2_symtab_t symtab,
                         cardinal lexkey,
                 m2_symbol_type_t symtype,
               m2_symtab_status_t *status);


// ---------------------------------------------------------------------------
// function:  m2_symtab_lookup(symtab, filter, lexkey, index, symtype, status)
// ---------------------------------------------------------------------------
//
// Searches symbol table <symtab>  for a symbol  with the lexeme  whose key is
// <lexkey>.  The search is limited to scopes that match the search scope fil-
// ter <scope_filter>.  If the search scope filter matches more than one scope
// then the search is carried out from inner scopes (closest to current scope)
// to outer scopes (closest to global scope).
//
// If the symbol is found:
//  o  true is returned
//  o  the index of the scope where the symbol was found is passed back
//     in <scope_index>  unless NULL is passed in for <scope_index>
//  o  the symbol type of the symbol found is passed back
//     in <symtype> unless NULL is passed in for <symtype>
//
// If the symbol is not found:
//  o  false is returned
//  o  no value is passed back in <scope_index>
//  o  no value is passed back in <symtype>
//
// The status of the operation is passed back in <status>  unless NULL is pas-
// sed in for <status>.

bool m2_symtab_lookup(m2_symtab_t symtab,
                m2_symtab_scope_t scope_filter,
                         cardinal lexkey,
                         cardinal *scope_index,
                 m2_symbol_type_t *symtype,
               m2_symtab_status_t *status);



// TO DO : Accessors and mutators for const, type, var and proc symbols.


// ---------------------------------------------------------------------------
// function:  m2_symtab_get_type_of_var(symtab, lexkey)
// ---------------------------------------------------------------------------
//
// Returns  the key of the type identifier of the type of the variable that is
// located in the scope with index <scope_index> and whose key is <lexkey>.

cardinal m2_symtab_get_type_of_var(m2_symtab_t symtab,
                                      cardinal scope_index,
                                      cardinal lexkey,
                            m2_symtab_status_t *status);


// ---------------------------------------------------------------------------
// function:  m2_symtab_remove_scope(symtab, status)
// ---------------------------------------------------------------------------
//
// Removes the  current scope  from symbol table <symtab> and sets the current
// scope to the surrounding scope.  The status of the operation is passed back
// in <status> unless NULL is passed in for <status>.

void m2_symtab_remove_scope(m2_symtab_t symtab, m2_symtab_status_t *status);


// ---------------------------------------------------------------------------
// function:  m2_dispose_symtab(symtab, status)
// ---------------------------------------------------------------------------
//
// Deallocates  symbol table object  <symtab>.  The status of the operation is
// passed back in <status> unless NULL is passed in for <status>.

void m2_dispose_symtab(m2_symtab_t symtab, m2_symtab_status_t *status);


#endif /* M2_SYMTAB_H */

// END OF FILE