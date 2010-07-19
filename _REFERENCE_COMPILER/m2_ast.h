/* Modula-2 R10 Compiler (m2r10c)
 *
 *  m2_ast.h
 *  Abstract Syntax Tree interface
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

#ifndef M2_AST_H
#define M2_AST_H


#include "common.h"
#include "m2_tokens.h"


// ---------------------------------------------------------------------------
// Opaque AST node handle type
// ---------------------------------------------------------------------------
//
// WARNING:  Objects of this opaque type should  only be accessed through this
// public interface.  DO NOT EVER attempt to bypass the public interface.
//
// The internal data structure of this opaque type is  HIDDEN  and  MAY CHANGE
// at any time WITHOUT NOTICE.  Accessing the internal data structure directly
// other than  through the  functions  in this public interface is  UNSAFE and
// may result in an inconsistent program state or a crash.

typedef opaque_t m2_ast_node_t;


// ---------------------------------------------------------------------------
// Status codes
// ---------------------------------------------------------------------------

typedef /* m2_ast_status_t */ enum {
    M2_AST_STATUS_UNDEFINED = -1,
    M2_AST_STATUS_SUCCESS = 1,
    M2_AST_STATUS_UNABLE_TO_ALLOCATE
} m2_ast_status_t;


// ---------------------------------------------------------------------------
// function:  m2_ast_new_node(token, lexeme, status)
// ---------------------------------------------------------------------------
//
// Returns a new AST node for token with lexeme.

m2_ast_node_t m2_ast_new_node(m2_token_t token,
                                cardinal lexeme,
                         m2_ast_status_t *status);


// ---------------------------------------------------------------------------
// function:  m2_ast_add_child(parent, child, status)
// ---------------------------------------------------------------------------
//
// Adds node child to node parent. Returns void.

void m2_ast_add_child(m2_ast_node_t parent,
                      m2_ast_node_t child,
                    m2_ast_status_t *status);

// ---------------------------------------------------------------------------
// function:  m2_ast_token(node, status)
// ---------------------------------------------------------------------------
//
// Returns the token of node.

m2_token_t m2_ast_token(m2_ast_node_t node, m2_ast_status_t *status);


// ---------------------------------------------------------------------------
// function:  m2_ast_lexeme(node, status)
// ---------------------------------------------------------------------------
//
// Returns the lexeme of node.

cardinal m2_ast_lexeme(m2_ast_node_t node, m2_ast_status_t *status);


// ---------------------------------------------------------------------------
// function:  m2_ast_parent(node, status)
// ---------------------------------------------------------------------------
//
// Returns the parent of node.

m2_ast_node_t m2_ast_parent(m2_ast_node_t node, m2_ast_status_t *status);


// ---------------------------------------------------------------------------
// function:  m2_ast_child(node, index, status)
// ---------------------------------------------------------------------------
//
// Returns the child of node at index.

m2_ast_node_t m2_ast_child(m2_ast_node_t node,
                                cardinal index,
                         m2_ast_status_t *status);


// ---------------------------------------------------------------------------
// function:  m2_ast_child_count(node, status)
// ---------------------------------------------------------------------------
//
// Returns the number of children of node.

cardinal m2_ast_child_count(m2_ast_node_t node, m2_ast_status_t *status);


// ---------------------------------------------------------------------------
// function:  m2_ast_is_parent(node, child, status)
// ---------------------------------------------------------------------------
//
// Returns true if node is the parent of child, false otherwise.

bool m2_ast_is_parent(m2_ast_node_t node,
                      m2_ast_node_t child,
                    m2_ast_status_t *status);


#endif /* M2_AST_H */

// END OF FILE