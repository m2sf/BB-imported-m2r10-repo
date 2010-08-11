/* Modula-2 R10 FIRST and FOLLOW set initialiser generator utility
 *
 *  gen_first_and_follow_sets.c
 *
 *  This utility program generates  FIRST and FOLLOW set initialisers  for use
 *  within the  Modula-2 R10 compiler (m2r10c).  Any changes relating to FIRST
 *  and FOLLOW sets  should be made within the sources of this utility program
 *  and not within the sources of the compiler. 
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

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#include "m2_tokens.h"
#include "m2_tokenset.h"
#include "m2_tokenset_literals.h"


// ---------------------------------------------------------------------------
// Enumeration type defining output modes
// ---------------------------------------------------------------------------

typedef enum { FIRST, FOLLOW } set_type_t;


// ---------------------------------------------------------------------------
// Arrays for FIRST and FOLLOW sets
// ---------------------------------------------------------------------------

m2_tokenset_t _FIRST[M2_NUMBER_OF_PRODUCTIONS];
m2_tokenset_t _FOLLOW[M2_NUMBER_OF_PRODUCTIONS];


// ---------------------------------------------------------------------------
// private function:  init_first_sets()
// ---------------------------------------------------------------------------
//
// Initialises the array of FIRST sets.

static void init_first_sets() {
    
    _FIRST[p_compilation_unit] =
    m2_tokenset_from_list(TOKEN_MODULE,
                          TOKEN_PROTOTYPE,
                          TOKEN_DEFINITION,
                          TOKEN_IMPLEMENTATION, 0);
    
    _FIRST[p_prototype] =
    m2_tokenset_from_list(TOKEN_PROTOTYPE, 0);
    
    _FIRST[p_program_module] =
    m2_tokenset_from_list(TOKEN_MODULE, 0);
    
    _FIRST[p_definition_of_module] =
    m2_tokenset_from_list(TOKEN_DEFINITION, 0);
    
    _FIRST[p_implementation_of_module] =
    m2_tokenset_from_list(TOKEN_IMPLEMENTATION, 0);
    
    _FIRST[p_required_binding] =
    m2_tokenset_from_list(TOKEN_PROCEDURE, 0);

    _FIRST[p_bindable_operator] =
    m2_tokenset_from_list(TOKEN_DIV,
                          TOKEN_MOD,
                          TOKEN_IN,
                          TOKEN_FOR,
                          TOKEN_ASSIGN_OP,
                          TOKEN_RETRIEVAL_PSEUDO_OP,
                          TOKEN_STORAGE_PSEUDO_OP,
                          TOKEN_REMOVAL_PSEUDO_OP,
                          TOKEN_PLUS_OP,
                          TOKEN_MINUS_OP,
                          TOKEN_ASTERISK_OP,
                          TOKEN_SLASH_OP,
                          TOKEN_EQUAL_OP,
                          TOKEN_LESS_OP,
                          TOKEN_GREATER_OP, 0);

    _FIRST[p_import_list] =
    m2_tokenset_from_list(TOKEN_FROM,
                          TOKEN_IMPORT, 0);
 
    _FIRST[p_block] =
    m2_tokenset_from_list(TOKEN_CONST,
                          TOKEN_TYPE,
                          TOKEN_VAR,
                          TOKEN_PROCEDURE,
                          TOKEN_BEGIN, 0);

    _FIRST[p_declaration] =
    m2_tokenset_from_list(TOKEN_CONST,
                          TOKEN_TYPE,
                          TOKEN_VAR,
                          TOKEN_PROCEDURE, 0);
    
    _FIRST[p_definition] =
    m2_tokenset_from_list(TOKEN_CONST,
                          TOKEN_TYPE,
                          TOKEN_VAR,
                          TOKEN_PROCEDURE, 0);
    
    _FIRST[p_const_declaration] =
    m2_tokenset_from_list(TOKEN_IDENTIFIER, 0);
        
    _FIRST[p_type] =
    m2_tokenset_from_list(TOKEN_ALIAS,
                          TOKEN_LBRACKET,
                          TOKEN_IDENTIFIER,
                          TOKEN_LPAREN,
                          TOKEN_ARRAY,
                          TOKEN_ASSOCIATIVE,
                          TOKEN_RECORD,
                          TOKEN_SET,
                          TOKEN_POINTER,
                          TOKEN_PROCEDURE, 0);

    _FIRST[p_range] =
    m2_tokenset_from_list(TOKEN_LBRACKET, 0);

    _FIRST[p_enumeration_type] =
    m2_tokenset_from_list(TOKEN_LPAREN, 0);
    
    _FIRST[p_array_type] =
    m2_tokenset_from_list(TOKEN_ARRAY,
                          TOKEN_ASSOCIATIVE, 0);
    
    _FIRST[p_record_type] =
    m2_tokenset_from_list(TOKEN_RECORD, 0);
    
    _FIRST[p_field_list_sequence] =
    m2_tokenset_from_list(TOKEN_IDENTIFIER, 0);
    
    _FIRST[p_field_list] =
    m2_tokenset_from_list(TOKEN_IDENTIFIER, 0);
        
    _FIRST[p_set_type] =
    m2_tokenset_from_list(TOKEN_SET, 0);
    
    _FIRST[p_pointer_type] =
    m2_tokenset_from_list(TOKEN_POINTER, 0);
    
    _FIRST[p_procedure_type] =
    m2_tokenset_from_list(TOKEN_PROCEDURE, 0);
    
    _FIRST[p_formal_type_list] =
    m2_tokenset_from_list(TOKEN_CONST,
                          TOKEN_VAR,
                          TOKEN_CAST,
                          TOKEN_ARRAY,
                          TOKEN_IDENTIFIER,
                          TOKEN_VARIADIC, 0);
    
    _FIRST[p_formal_type] =
    m2_tokenset_from_list(TOKEN_CONST,
                          TOKEN_VAR,
                          TOKEN_CAST,
                          TOKEN_ARRAY,
                          TOKEN_IDENTIFIER,
                          TOKEN_VARIADIC, 0);
    
    _FIRST[p_attributed_formal_type] =
    m2_tokenset_from_list(TOKEN_CONST,
                          TOKEN_VAR,
                          TOKEN_CAST,
                          TOKEN_ARRAY,
                          TOKEN_IDENTIFIER, 0);    
 
    _FIRST[p_simple_formal_type] =
    m2_tokenset_from_list(TOKEN_CAST,
                          TOKEN_ARRAY,
                          TOKEN_IDENTIFIER, 0);    

    _FIRST[p_variadic_formal_type] =
    m2_tokenset_from_list(TOKEN_VARIADIC, 0);

    _FIRST[p_variable_declaration] =
    m2_tokenset_from_list(TOKEN_IDENTIFIER, 0);
    
    _FIRST[p_procedure_declaration] =
    m2_tokenset_from_list(TOKEN_PROCEDURE, 0);
    
    _FIRST[p_procedure_header] =
    m2_tokenset_from_list(TOKEN_PROCEDURE, 0);
    
    _FIRST[p_formal_param_list] =
    m2_tokenset_from_list(TOKEN_CONST,
                          TOKEN_VAR,
                          TOKEN_IDENTIFIER,
                          TOKEN_VARIADIC, 0);
    
    _FIRST[p_formal_params] =
    m2_tokenset_from_list(TOKEN_CONST,
                          TOKEN_VAR,
                          TOKEN_IDENTIFIER,
                          TOKEN_VARIADIC, 0);

    _FIRST[p_simple_formal_params] =
    m2_tokenset_from_list(TOKEN_CONST,
                          TOKEN_VAR,
                          TOKEN_IDENTIFIER, 0);

    _FIRST[p_variadic_formal_params] =
    m2_tokenset_from_list(TOKEN_VARIADIC, 0);
    
    _FIRST[p_statement] =
    m2_tokenset_from_list(TOKEN_IDENTIFIER,
                          TOKEN_IF,
                          TOKEN_CASE,
                          TOKEN_WHILE,
                          TOKEN_REPEAT,
                          TOKEN_LOOP,
                          TOKEN_FOR,
                          TOKEN_RETURN,
                          TOKEN_EXIT, 0);
    
    _FIRST[p_statement_sequence] =
    m2_tokenset_from_list(TOKEN_IDENTIFIER,
                          TOKEN_IF,
                          TOKEN_CASE,
                          TOKEN_WHILE,
                          TOKEN_REPEAT,
                          TOKEN_LOOP,
                          TOKEN_FOR,
                          TOKEN_RETURN,
                          TOKEN_EXIT, 0);
    
    _FIRST[p_assignment_or_procedure_call] =
    m2_tokenset_from_list(TOKEN_IDENTIFIER, 0);
    
    _FIRST[p_if_statement] =
    m2_tokenset_from_list(TOKEN_IF, 0);
    
    _FIRST[p_case_statement] =
    m2_tokenset_from_list(TOKEN_CASE, 0);
    
    _FIRST[p_case] =
    m2_tokenset_from_list(TOKEN_PLUS_OP,
                          TOKEN_MINUS_OP,
                          TOKEN_NUMERIC_LITERAL,
                          TOKEN_STRING_LITERAL,
                          TOKEN_IDENTIFIER,
                          TOKEN_LBRACE,
                          TOKEN_LPAREN,
                          TOKEN_CAST,
                          TOKEN_NOT, 0);
    
    _FIRST[p_case_labels] =
    m2_tokenset_from_list(TOKEN_PLUS_OP,
                          TOKEN_MINUS_OP,
                          TOKEN_NUMERIC_LITERAL,
                          TOKEN_STRING_LITERAL,
                          TOKEN_IDENTIFIER,
                          TOKEN_LBRACE,
                          TOKEN_LPAREN,
                          TOKEN_CAST,
                          TOKEN_NOT, 0);
    
    _FIRST[p_while_statement] =
    m2_tokenset_from_list(TOKEN_WHILE, 0);
    
    _FIRST[p_repeat_statement] =
    m2_tokenset_from_list(TOKEN_REPEAT, 0);
    
    _FIRST[p_loop_statement] =
    m2_tokenset_from_list(TOKEN_LOOP, 0);
    
    _FIRST[p_for_statement] =
    m2_tokenset_from_list(TOKEN_FOR, 0);
    
    _FIRST[p_const_expression] =
    m2_tokenset_from_list(TOKEN_PLUS_OP,
                          TOKEN_MINUS_OP,
                          TOKEN_NUMERIC_LITERAL,
                          TOKEN_STRING_LITERAL,
                          TOKEN_IDENTIFIER,
                          TOKEN_LBRACE,
                          TOKEN_LPAREN,
                          TOKEN_CAST,
                          TOKEN_NOT, 0);
    
    _FIRST[p_relation] =
    m2_tokenset_from_list(TOKEN_EQUAL_OP,
                          TOKEN_NOT_EQUAL_OP,
                          TOKEN_LESS_OP,
                          TOKEN_LESS_OR_EQUAL_OP,
                          TOKEN_GREATER_OP,
                          TOKEN_GREATER_OR_EQUAL_OP,
                          TOKEN_IN, 0);
    
    _FIRST[p_simple_const_expr] =
    m2_tokenset_from_list(TOKEN_PLUS_OP,
                          TOKEN_MINUS_OP,
                          TOKEN_NUMERIC_LITERAL,
                          TOKEN_STRING_LITERAL,
                          TOKEN_IDENTIFIER,
                          TOKEN_LBRACE,
                          TOKEN_LPAREN,
                          TOKEN_CAST,
                          TOKEN_NOT, 0);
    
    _FIRST[p_add_operator] =
    m2_tokenset_from_list(TOKEN_PLUS_OP,
                          TOKEN_MINUS_OP,
                          TOKEN_OR, 0);
    
    _FIRST[p_const_term] =
    m2_tokenset_from_list(TOKEN_NUMERIC_LITERAL,
                          TOKEN_STRING_LITERAL,
                          TOKEN_IDENTIFIER,
                          TOKEN_LBRACE,
                          TOKEN_LPAREN,
                          TOKEN_CAST,
                          TOKEN_NOT, 0);
    
    _FIRST[p_mul_operator] =
    m2_tokenset_from_list(TOKEN_ASTERISK_OP,
                          TOKEN_SLASH_OP,
                          TOKEN_DIV,
                          TOKEN_MOD,
                          TOKEN_AND, 0);
    
    _FIRST[p_const_factor] =
    m2_tokenset_from_list(TOKEN_NUMERIC_LITERAL,
                          TOKEN_STRING_LITERAL,
                          TOKEN_IDENTIFIER,
                          TOKEN_LBRACE,
                          TOKEN_LPAREN,
                          TOKEN_CAST,
                          TOKEN_NOT, 0);
    
    _FIRST[p_designator] =
    m2_tokenset_from_list(TOKEN_IDENTIFIER, 0);
    
    _FIRST[p_designator_tail] =
    m2_tokenset_from_list(TOKEN_LBRACKET,
                          TOKEN_POINTER_DEREF_OP, 0);
    
    _FIRST[p_expression_list] =
    m2_tokenset_from_list(TOKEN_PLUS_OP,
                          TOKEN_MINUS_OP,
                          TOKEN_NUMERIC_LITERAL,
                          TOKEN_STRING_LITERAL,
                          TOKEN_LBRACE,
                          TOKEN_IDENTIFIER,
                          TOKEN_LPAREN,
                          TOKEN_CAST,
                          TOKEN_NOT, 0);
    
    _FIRST[p_expression] =
    m2_tokenset_from_list(TOKEN_PLUS_OP,
                          TOKEN_MINUS_OP,
                          TOKEN_NUMERIC_LITERAL,
                          TOKEN_STRING_LITERAL,
                          TOKEN_LBRACE,
                          TOKEN_IDENTIFIER,
                          TOKEN_LPAREN,
                          TOKEN_CAST,
                          TOKEN_NOT, 0);
    
    _FIRST[p_simple_expression] =
    m2_tokenset_from_list(TOKEN_PLUS_OP,
                          TOKEN_MINUS_OP,
                          TOKEN_NUMERIC_LITERAL,
                          TOKEN_STRING_LITERAL,
                          TOKEN_LBRACE,
                          TOKEN_IDENTIFIER,
                          TOKEN_LPAREN,
                          TOKEN_CAST,
                          TOKEN_NOT, 0);
    
    _FIRST[p_term] =
    m2_tokenset_from_list(TOKEN_NUMERIC_LITERAL,
                          TOKEN_STRING_LITERAL,
                          TOKEN_LBRACE,
                          TOKEN_IDENTIFIER,
                          TOKEN_LPAREN,
                          TOKEN_CAST,
                          TOKEN_NOT, 0);
    
    _FIRST[p_factor] =
    m2_tokenset_from_list(TOKEN_NUMERIC_LITERAL,
                          TOKEN_STRING_LITERAL,
                          TOKEN_LBRACE,
                          TOKEN_IDENTIFIER,
                          TOKEN_LPAREN,
                          TOKEN_CAST,
                          TOKEN_NOT, 0);
    
    _FIRST[p_designator_or_procedure_call] =
    m2_tokenset_from_list(TOKEN_IDENTIFIER, 0);
    
    _FIRST[p_actual_parameters] =
    m2_tokenset_from_list(TOKEN_LPAREN, 0);
    
    _FIRST[p_const_structured_value] =
    m2_tokenset_from_list(TOKEN_LBRACE, 0);

    _FIRST[p_const_value_component] =
    m2_tokenset_from_list(TOKEN_PLUS_OP,
                          TOKEN_MINUS_OP,
                          TOKEN_NUMERIC_LITERAL,
                          TOKEN_STRING_LITERAL,
                          TOKEN_IDENTIFIER,
                          TOKEN_LBRACE,
                          TOKEN_LPAREN,
                          TOKEN_CAST,
                          TOKEN_NOT, 0);
    
    _FIRST[p_structured_value] =
    m2_tokenset_from_list(TOKEN_LBRACE, 0);
 
    _FIRST[p_value_component] =
    m2_tokenset_from_list(TOKEN_PLUS_OP,
                          TOKEN_MINUS_OP,
                          TOKEN_NUMERIC_LITERAL,
                          TOKEN_STRING_LITERAL,
                          TOKEN_LBRACE,
                          TOKEN_IDENTIFIER,
                          TOKEN_LPAREN,
                          TOKEN_CAST,
                          TOKEN_NOT, 0);    

    _FIRST[p_qualident] =
    m2_tokenset_from_list(TOKEN_IDENTIFIER, 0);
    
    _FIRST[p_ident_list] =
    m2_tokenset_from_list(TOKEN_IDENTIFIER, 0);
    
} // end init_first_sets


// ---------------------------------------------------------------------------
// private function:  init_follow_sets()
// ---------------------------------------------------------------------------
//
// Initialises the array of FOLLOW sets.

static void init_follow_sets() {

    _FOLLOW[p_compilation_unit] =
    m2_tokenset_from_list(TOKEN_EOF_MARKER, 0);

    _FOLLOW[p_prototype] =
    m2_tokenset_from_list(TOKEN_EOF_MARKER, 0);

    _FOLLOW[p_program_module] =
    m2_tokenset_from_list(TOKEN_EOF_MARKER, 0);
    
    _FOLLOW[p_definition_of_module] =
    m2_tokenset_from_list(TOKEN_EOF_MARKER, 0);

    _FOLLOW[p_implementation_of_module] =
    m2_tokenset_from_list(TOKEN_EOF_MARKER, 0);
    
    _FOLLOW[p_required_binding] =
    m2_tokenset_from_list(TOKEN_CONST,
                          TOKEN_PROCEDURE,
                          TOKEN_END, 0);

    _FOLLOW[p_bindable_operator] =
    m2_tokenset_from_list(TOKEN_RBRACKET, 0);

    _FOLLOW[p_import_list] =
    m2_tokenset_from_list(TOKEN_CONST,
                          TOKEN_TYPE,
                          TOKEN_VAR,
                          TOKEN_PROCEDURE,
                          TOKEN_BEGIN,
                          TOKEN_END, 0);
    
    _FOLLOW[p_block] =
    m2_tokenset_from_list(TOKEN_IDENTIFIER, 0);
    
    _FOLLOW[p_declaration] =
    m2_tokenset_from_list(TOKEN_BEGIN,
                          TOKEN_END, 0);
    
    _FOLLOW[p_definition] =
    m2_tokenset_from_list(TOKEN_END, 0);
    
    _FOLLOW[p_const_declaration] =
    m2_tokenset_from_list(TOKEN_SEMICOLON, 0);
        
    _FOLLOW[p_type] =
    m2_tokenset_from_list(TOKEN_SEMICOLON, 0);

    _FOLLOW[p_range] =
    m2_tokenset_from_list(TOKEN_OF, 0);

    _FOLLOW[p_enumeration_type] =
    m2_tokenset_from_list(TOKEN_SEMICOLON, 0);
    
    _FOLLOW[p_array_type] =
    m2_tokenset_from_list(TOKEN_SEMICOLON, 0);
    
    _FOLLOW[p_record_type] =
    m2_tokenset_from_list(TOKEN_SEMICOLON, 0);
    
    _FOLLOW[p_field_list_sequence] =
    m2_tokenset_from_list(TOKEN_END, 0);
    
    _FOLLOW[p_field_list] =
    m2_tokenset_from_list(TOKEN_SEMICOLON,
                          TOKEN_END, 0);
        
    _FOLLOW[p_set_type] =
    m2_tokenset_from_list(TOKEN_SEMICOLON, 0);
    
    _FOLLOW[p_pointer_type] =
    m2_tokenset_from_list(TOKEN_SEMICOLON, 0);
    
    _FOLLOW[p_procedure_type] =
    m2_tokenset_from_list(TOKEN_SEMICOLON, 0);
    
    _FOLLOW[p_formal_type_list] =
    m2_tokenset_from_list(TOKEN_COLON,
                          TOKEN_SEMICOLON, 0);
    
    _FOLLOW[p_formal_type] =
    m2_tokenset_from_list(TOKEN_COMMA,
                          TOKEN_COLON,
                          TOKEN_SEMICOLON, 0);
    
    _FOLLOW[p_attributed_formal_type] =
    m2_tokenset_from_list(TOKEN_COMMA,
                          TOKEN_COLON,
                          TOKEN_SEMICOLON, 0);

    _FOLLOW[p_simple_formal_type] =
    m2_tokenset_from_list(TOKEN_COMMA,
                          TOKEN_COLON,
                          TOKEN_SEMICOLON, 0);

    _FOLLOW[p_variadic_formal_type] =
    m2_tokenset_from_list(TOKEN_COMMA,
                          TOKEN_COLON,
                          TOKEN_SEMICOLON, 0);
    
    
    _FOLLOW[p_variable_declaration] =
    m2_tokenset_from_list(TOKEN_SEMICOLON, 0);
    
    _FOLLOW[p_procedure_declaration] =
    m2_tokenset_from_list(TOKEN_SEMICOLON, 0);
    
    _FOLLOW[p_procedure_header] =
    m2_tokenset_from_list(TOKEN_SEMICOLON, 0);
    
    _FOLLOW[p_formal_param_list] =
    m2_tokenset_from_list(TOKEN_RPAREN, 0);
    
    _FOLLOW[p_formal_params] =
    m2_tokenset_from_list(TOKEN_SEMICOLON,
                          TOKEN_RPAREN, 0);

    _FOLLOW[p_simple_formal_params] =
    m2_tokenset_from_list(TOKEN_SEMICOLON,
                          TOKEN_RPAREN, 0);

    _FOLLOW[p_variadic_formal_params] =
    m2_tokenset_from_list(TOKEN_SEMICOLON,
                          TOKEN_RPAREN, 0);
        
    _FOLLOW[p_statement] =
    m2_tokenset_from_list(TOKEN_SEMICOLON,
                          TOKEN_ELSE,
                          TOKEN_CASE_LABEL_SEPARATOR,
                          TOKEN_UNTIL,
                          TOKEN_END, 0);
    
    _FOLLOW[p_statement_sequence] =
    m2_tokenset_from_list(TOKEN_ELSIF,
                          TOKEN_ELSE,
                          TOKEN_CASE_LABEL_SEPARATOR,
                          TOKEN_UNTIL,
                          TOKEN_END, 0);
        
    _FOLLOW[p_assignment_or_procedure_call] =
    m2_tokenset_from_list(TOKEN_SEMICOLON,
                          TOKEN_ELSE,
                          TOKEN_CASE_LABEL_SEPARATOR,
                          TOKEN_UNTIL,
                          TOKEN_END, 0);
    
    _FOLLOW[p_if_statement] =
    m2_tokenset_from_list(TOKEN_SEMICOLON,
                          TOKEN_ELSE,
                          TOKEN_CASE_LABEL_SEPARATOR,
                          TOKEN_UNTIL,
                          TOKEN_END, 0);
    
    _FOLLOW[p_case_statement] =
    m2_tokenset_from_list(TOKEN_SEMICOLON,
                          TOKEN_ELSE,
                          TOKEN_CASE_LABEL_SEPARATOR,
                          TOKEN_UNTIL,
                          TOKEN_END, 0);
    
    _FOLLOW[p_case] =
    m2_tokenset_from_list(TOKEN_ELSE,
                          TOKEN_CASE_LABEL_SEPARATOR,
                          TOKEN_END, 0);
        
    _FOLLOW[p_case_labels] =
    m2_tokenset_from_list(TOKEN_COMMA,
                          TOKEN_COLON, 0);
    
    _FOLLOW[p_while_statement] =
    m2_tokenset_from_list(TOKEN_SEMICOLON,
                          TOKEN_ELSE,
                          TOKEN_CASE_LABEL_SEPARATOR,
                          TOKEN_UNTIL,
                          TOKEN_END, 0);
    
    _FOLLOW[p_repeat_statement] =
    m2_tokenset_from_list(TOKEN_SEMICOLON,
                          TOKEN_ELSE,
                          TOKEN_CASE_LABEL_SEPARATOR,
                          TOKEN_UNTIL,
                          TOKEN_END, 0);
    
    _FOLLOW[p_loop_statement] =
    m2_tokenset_from_list(TOKEN_SEMICOLON,
                          TOKEN_ELSE,
                          TOKEN_CASE_LABEL_SEPARATOR,
                          TOKEN_UNTIL,
                          TOKEN_END, 0);
    
    _FOLLOW[p_for_statement] =
    m2_tokenset_from_list(TOKEN_SEMICOLON,
                          TOKEN_ELSE,
                          TOKEN_CASE_LABEL_SEPARATOR,
                          TOKEN_UNTIL,
                          TOKEN_END, 0);
    
    _FOLLOW[p_const_expression] =
    m2_tokenset_from_list(TOKEN_RBRACKET,
                          TOKEN_SEMICOLON,
                          TOKEN_COMMA,
                          TOKEN_OF,
                          TOKEN_COLON,
                          TOKEN_RANGE_OP,
                          TOKEN_DO,
                          TOKEN_RPAREN,
                          TOKEN_BY,
                          TOKEN_LBRACE,
                          TOKEN_RBRACE, 0);
    
    _FOLLOW[p_relation] =
    m2_tokenset_from_list(TOKEN_PLUS_OP,
                          TOKEN_MINUS_OP,
                          TOKEN_NUMERIC_LITERAL,
                          TOKEN_STRING_LITERAL,
                          TOKEN_IDENTIFIER,
                          TOKEN_LPAREN,
                          TOKEN_NOT, 0);
    
    _FOLLOW[p_simple_const_expr] =
    m2_tokenset_from_list(TOKEN_EQUAL_OP,
                          TOKEN_NOT_EQUAL_OP,
                          TOKEN_LESS_OP,
                          TOKEN_LESS_OR_EQUAL_OP,
                          TOKEN_GREATER_OP,
                          TOKEN_GREATER_OR_EQUAL_OP,
                          TOKEN_IN,
                          TOKEN_TYPE_CONVERSION_OP, 0);
    
    _FOLLOW[p_add_operator] =
    m2_tokenset_from_list(TOKEN_NUMERIC_LITERAL,
                          TOKEN_STRING_LITERAL,
                          TOKEN_IDENTIFIER,
                          TOKEN_LPAREN,
                          TOKEN_NOT, 0);
    
    _FOLLOW[p_const_term] =
    m2_tokenset_from_list(TOKEN_PLUS_OP,
                          TOKEN_MINUS_OP,
                          TOKEN_OR,
                          TOKEN_EQUAL_OP,
                          TOKEN_NOT_EQUAL_OP,
                          TOKEN_LESS_OP,
                          TOKEN_LESS_OR_EQUAL_OP,
                          TOKEN_GREATER_OP,
                          TOKEN_GREATER_OR_EQUAL_OP,
                          TOKEN_IN,
                          TOKEN_TYPE_CONVERSION_OP, 0);
    
    _FOLLOW[p_mul_operator] =
    m2_tokenset_from_list(TOKEN_NUMERIC_LITERAL,
                          TOKEN_STRING_LITERAL,
                          TOKEN_IDENTIFIER,
                          TOKEN_LPAREN,
                          TOKEN_NOT, 0);
    
    _FOLLOW[p_const_factor] =
    m2_tokenset_from_list(TOKEN_PLUS_OP,
                          TOKEN_MINUS_OP,
                          TOKEN_OR,
                          TOKEN_ASTERISK_OP,
                          TOKEN_SLASH_OP,
                          TOKEN_DIV,
                          TOKEN_MOD,
                          TOKEN_AND,
                          TOKEN_EQUAL_OP,
                          TOKEN_NOT_EQUAL_OP,
                          TOKEN_LESS_OP,
                          TOKEN_LESS_OR_EQUAL_OP,
                          TOKEN_GREATER_OP,
                          TOKEN_GREATER_OR_EQUAL_OP,
                          TOKEN_IN,
                          TOKEN_TYPE_CONVERSION_OP, 0);
    
    _FOLLOW[p_designator] =
    m2_tokenset_from_list(TOKEN_SEMICOLON,
                          TOKEN_ASSIGN_OP,
                          TOKEN_INCREMENT_OP,
                          TOKEN_DECREMENT_OP,
                          TOKEN_LPAREN,
                          TOKEN_END, 0);
    
    _FOLLOW[p_designator_tail] =
    m2_tokenset_from_list(TOKEN_SEMICOLON,
                          TOKEN_ASSIGN_OP,
                          TOKEN_INCREMENT_OP,
                          TOKEN_DECREMENT_OP,
                          TOKEN_LPAREN,
                          TOKEN_END, 0);
    
    _FOLLOW[p_expression_list] =
    m2_tokenset_from_list(TOKEN_RPAREN,
                          TOKEN_RBRACKET, 0);
    
    _FOLLOW[p_expression] =
    m2_tokenset_from_list(TOKEN_COMMA,
                          TOKEN_RPAREN,
                          TOKEN_RBRACKET,
                          TOKEN_SEMICOLON,
                          TOKEN_THEN,
                          TOKEN_ELSIF,
                          TOKEN_ELSE,
                          TOKEN_CASE_LABEL_SEPARATOR,
                          TOKEN_OF,
                          TOKEN_DO,
                          TOKEN_UNTIL,
                          TOKEN_TO,
                          TOKEN_END, 0);
    
    _FOLLOW[p_simple_expression] =
    m2_tokenset_from_list(TOKEN_PLUS_OP,
                          TOKEN_MINUS_OP,
                          TOKEN_NUMERIC_LITERAL,
                          TOKEN_STRING_LITERAL,
                          TOKEN_IDENTIFIER,
                          TOKEN_NOT,
                          TOKEN_TYPE_CONVERSION_OP,
                          TOKEN_COMMA,
                          TOKEN_RPAREN,
                          TOKEN_RBRACKET,
                          TOKEN_SEMICOLON,
                          TOKEN_THEN,
                          TOKEN_ELSIF,
                          TOKEN_ELSE,
                          TOKEN_CASE_LABEL_SEPARATOR,
                          TOKEN_OF,
                          TOKEN_DO,
                          TOKEN_UNTIL,
                          TOKEN_TO,
                          TOKEN_END, 0);
    
    _FOLLOW[p_term] =
    m2_tokenset_from_list(TOKEN_PLUS_OP,
                          TOKEN_MINUS_OP,
                          TOKEN_OR,
                          TOKEN_NUMERIC_LITERAL,
                          TOKEN_STRING_LITERAL,
                          TOKEN_IDENTIFIER,
                          TOKEN_NOT,
                          TOKEN_TYPE_CONVERSION_OP,
                          TOKEN_COMMA,
                          TOKEN_RPAREN,
                          TOKEN_RBRACKET,
                          TOKEN_SEMICOLON,
                          TOKEN_THEN,
                          TOKEN_ELSIF,
                          TOKEN_ELSE,
                          TOKEN_CASE_LABEL_SEPARATOR,
                          TOKEN_OF,
                          TOKEN_DO,
                          TOKEN_UNTIL,
                          TOKEN_TO,
                          TOKEN_END, 0);
    
    _FOLLOW[p_factor] =
    m2_tokenset_from_list(TOKEN_PLUS_OP,
                          TOKEN_MINUS_OP,
                          TOKEN_OR,
                          TOKEN_NUMERIC_LITERAL,
                          TOKEN_STRING_LITERAL,
                          TOKEN_IDENTIFIER,
                          TOKEN_NOT,
                          TOKEN_TYPE_CONVERSION_OP,
                          TOKEN_COMMA,
                          TOKEN_RPAREN,
                          TOKEN_RBRACKET,
                          TOKEN_SEMICOLON,
                          TOKEN_THEN,
                          TOKEN_ELSIF,
                          TOKEN_ELSE,
                          TOKEN_CASE_LABEL_SEPARATOR,
                          TOKEN_OF,
                          TOKEN_DO,
                          TOKEN_UNTIL,
                          TOKEN_TO,
                          TOKEN_END, 0);
    
    _FOLLOW[p_designator_or_procedure_call] =
    m2_tokenset_from_list(TOKEN_PLUS_OP,
                          TOKEN_MINUS_OP,
                          TOKEN_OR,
                          TOKEN_NUMERIC_LITERAL,
                          TOKEN_STRING_LITERAL,
                          TOKEN_IDENTIFIER,
                          TOKEN_NOT,
                          TOKEN_TYPE_CONVERSION_OP,
                          TOKEN_COMMA,
                          TOKEN_RPAREN,
                          TOKEN_RBRACKET,
                          TOKEN_SEMICOLON,
                          TOKEN_THEN,
                          TOKEN_ELSIF,
                          TOKEN_ELSE,
                          TOKEN_CASE_LABEL_SEPARATOR,
                          TOKEN_OF,
                          TOKEN_DO,
                          TOKEN_UNTIL,
                          TOKEN_TO,
                          TOKEN_END, 0);
    
    _FOLLOW[p_actual_parameters] =
    m2_tokenset_from_list(TOKEN_PLUS_OP,
                          TOKEN_MINUS_OP,
                          TOKEN_OR,
                          TOKEN_NUMERIC_LITERAL,
                          TOKEN_STRING_LITERAL,
                          TOKEN_IDENTIFIER,
                          TOKEN_NOT,
                          TOKEN_TYPE_CONVERSION_OP,
                          TOKEN_COMMA,
                          TOKEN_RPAREN,
                          TOKEN_RBRACKET,
                          TOKEN_SEMICOLON,
                          TOKEN_THEN,
                          TOKEN_ELSIF,
                          TOKEN_ELSE,
                          TOKEN_CASE_LABEL_SEPARATOR,
                          TOKEN_OF,
                          TOKEN_DO,
                          TOKEN_UNTIL,
                          TOKEN_TO,
                          TOKEN_END, 0);
    
    _FOLLOW[p_const_structured_value] =
    m2_tokenset_from_list(TOKEN_COMMA,
                          TOKEN_RBRACE,
                          TOKEN_SEMICOLON,
                          TOKEN_ELSE,
                          TOKEN_CASE_LABEL_SEPARATOR,
                          TOKEN_UNTIL,
                          TOKEN_END, 0);
    
    _FOLLOW[p_const_value_component] =
    m2_tokenset_from_list(TOKEN_COMMA,
                          TOKEN_RBRACE, 0);
    
    _FOLLOW[p_structured_value] =
    m2_tokenset_from_list(TOKEN_COMMA,
                          TOKEN_RBRACE,
                          TOKEN_SEMICOLON,
                          TOKEN_ELSE,
                          TOKEN_CASE_LABEL_SEPARATOR,
                          TOKEN_UNTIL,
                          TOKEN_END, 0);
    
    _FOLLOW[p_value_component] =
    m2_tokenset_from_list(TOKEN_COMMA,
                          TOKEN_RBRACE, 0);
    
    _FOLLOW[p_qualident] =
    m2_tokenset_from_list(TOKEN_COMMA,
                          TOKEN_SEMICOLON,
                          TOKEN_ASSIGN_OP,
                          TOKEN_INCREMENT_OP,
                          TOKEN_DECREMENT_OP,
                          TOKEN_LPAREN,
                          TOKEN_RPAREN,
                          TOKEN_LBRACKET,
                          TOKEN_RBRACKET,
                          TOKEN_POINTER_DEREF_OP,
                          TOKEN_PLUS_OP,
                          TOKEN_MINUS_OP,
                          TOKEN_OR,
                          TOKEN_ASTERISK_OP,
                          TOKEN_SLASH_OP,
                          TOKEN_DIV,
                          TOKEN_MOD,
                          TOKEN_AND,
                          TOKEN_EQUAL_OP,
                          TOKEN_NOT_EQUAL_OP,
                          TOKEN_LESS_OP,
                          TOKEN_LESS_OR_EQUAL_OP,
                          TOKEN_GREATER_OP,
                          TOKEN_GREATER_OR_EQUAL_OP,
                          TOKEN_IN,
                          TOKEN_TYPE_CONVERSION_OP,
                          TOKEN_THEN,
                          TOKEN_ELSIF,
                          TOKEN_ELSE,
                          TOKEN_CASE_LABEL_SEPARATOR,
                          TOKEN_OF,
                          TOKEN_DO,
                          TOKEN_UNTIL,
                          TOKEN_TO,
                          TOKEN_END, 0);
    
    _FOLLOW[p_ident_list] =
    m2_tokenset_from_list(TOKEN_COLON,
                          TOKEN_SEMICOLON,
                          TOKEN_RPAREN,
                          TOKEN_END, 0);
    
} // end init_follow_sets


// ---------------------------------------------------------------------------
// private function:  generate_production_table()
// ---------------------------------------------------------------------------
//
// Generates the production master table for file objm2_production_table.h

void generate_production_table() {
    unsigned int prod_index;
    m2_tokenset_literal_t tuples;
    m2_tokenset_literal_status_t status;
    
    printf("\nNumber of productions: %i", M2_NUMBER_OF_PRODUCTIONS);
    printf("\nNumber of tokens     : %i", M2_NUMBER_OF_TOKENS);
    printf("\n");
    
    printf("\n// Productions\n");

    prod_index = 0;
    while (prod_index < M2_NUMBER_OF_PRODUCTIONS) {
        
        printf("\n// #%i", prod_index+1);
        
        printf("\n_add_production( _%s,\n",
               m2_production_name(prod_index));
        
        m2_tokenset_to_literal(_FIRST[prod_index], &tuples, &status);
        printf("                %s,   // FIRST set\n", tuples);
        
        m2_tokenset_to_literal(_FOLLOW[prod_index], &tuples, &status);
        printf("                %s )  // FOLLOW set\n", tuples);
        
        prod_index++;
    } // end while
    
} // end generate_production_table


// ---------------------------------------------------------------------------
// private function:  generate_set_lists( mode )
// ---------------------------------------------------------------------------
//
// Generates FIRST and FOLLOW set list macros for file m2_first_follow.h

void generate_set_lists(set_type_t mode) {
    
    unsigned int prod_index, token_count, token_index;
    const char *prod_name, *token_name, *token_identifier;
    m2_token_t token;
    m2_tokenset_iterator_t iterator;
    
    if (mode == FIRST)
        printf("\n\n// FIRST set lists\n");
    else
        printf("\n\n// FOLLOW set lists\n");
    
    prod_index = 0;
    while (prod_index < M2_NUMBER_OF_PRODUCTIONS) {
        
        prod_name = m2_production_name(prod_index);
        if (mode == FIRST) {
            printf("\n#define FIRST_LIST_for_%s \\\n", prod_name);
            iterator = m2_tokenset_iterator(_FIRST[prod_index]);
        }
        else {
            printf("\n#define FOLLOW_LIST_for_%s \\\n", prod_name);
            iterator = m2_tokenset_iterator(_FOLLOW[prod_index]);
        } // end if

        token_count = m2_tokenset_iterator_token_count(iterator);
        printf("    %i, \\\n", token_count);
        
        token_index = 0;
        while (token_index < token_count) {
            token = m2_tokenset_iterator_token_at_index(iterator,
                                                        token_index);
            token_name = m2_token_name(token);
            token_identifier = m2_token_identifier(token);
            
            printf("    %s", token_identifier);
            if (token_index + 1 != token_count)
                printf(", \\");
            
            printf("\n");
            
            token_index++;
        } // end while
                
        prod_index++;
    } // end while
        
} // end generate_set_lists


// ---------------------------------------------------------------------------
// main function
// ---------------------------------------------------------------------------
//
// Initialises FIRST and FOLLOW set arrays,  generates production master table
// and FIRST and FOLLOW set lists,  writing the generated data to stdout.

int main (int argc, const char **argv) {
    
    init_first_sets();

    init_follow_sets();
    
    generate_production_table();
    
    generate_set_lists(FIRST);
    
    generate_set_lists(FOLLOW);
    
    return 0;
} // end main


// END OF FILE
