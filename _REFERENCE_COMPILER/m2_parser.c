/* Modula-2 R10 Compiler (m2r10c)
 *
 *  m2_parser.c
 *  Parser implementation
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

#include "m2_parser.h"
#include "m2_tokens.h"


// ===========================================================================
// P R O D U C T I O N   R U L E S
// ===========================================================================


// --------------------------------------------------------------------------
// start symbol
// --------------------------------------------------------------------------

m2_parse(m2_parser_t *p) {
    m2_token_t token;
    
    token = _lookahead(p);
    
    if (p->source_type == SOURCE_TYPE_MOD) {
        if ((token != TOKEN_IMPLEMENTATION) && (token != TOKEN_MODULE)) {
            // illegal start symbol for source type MOD
            fatal_error(); // abort
        } // end if
    }
    else if (p->source_type == SOURCE_TYPE_DEF) {
        if (token != TOKEN_DEFINITION) && (token != TOKEN_PROTOTYPE)) {
            // illegal start symbol for source type DEF
            fatal_error(); // abort
        } // end if
    }
    else {
        // unknown source type
        fatal_error(); // abort
    } // end if
    
    token = m2_compilation_unit(p);
    
    if (token != TOKEN_EOF_MARKER) {
        // illegal symbol after end of compilation unit
        
        // report error
        
    } // end if
    
    return
} // end m2_parse(p);


// --------------------------------------------------------------------------
// #1 compilation_unit
// --------------------------------------------------------------------------
//  prototype |
//  program_module | definition_of_module | implementation_of_module

m2_token_t m2_compilation_unit(m2_parser_t *p) {
    m2_token_t token;
    
    token = _lookahead(p);
    
    switch(token) {
        case TOKEN_PROTOTYPE :
            token = m2_prototype(p);
            break;
        case TOKEN_MODULE :
            token = m2_program_module(p);
            break;
        case TOKEN_DEFINITION :
            token = m2_definition_of_module(p);
            break;
        case TOKEN_IMPLEMENTATION :
            token = m2_implementation_of_module(p);
            break;
        default :
            // unreachable code
            fatal_error(); // abort
    } // end switch
    
    return token;
} // end m2_compilation_unit


// --------------------------------------------------------------------------
// #2 prototype
// --------------------------------------------------------------------------
//  PROTOTYPE prototypeId ";"
//  TYPE "=" ( RECORD | OPAQUE RECORD? ( ":=" literalType )? ) ";"
//  ( ASSOCIATIVE ";" )? requiredBinding* END prototypeId "."

m2_token_t m2_prototype(m2_parser_t *p) {
    m2_token_t token;
    
    _getsym(p); // consume "PROTOTYPE"
    
    token = _lookahead(p);
    
    match(p, TOKEN_IDENTIFIER); // prototypeId is identifier
    
    
    
    
} // end m2_prototype


// --------------------------------------------------------------------------
// #3 program_module
// --------------------------------------------------------------------------
//

m2_token_t m2_program_module(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_program_module


// --------------------------------------------------------------------------
// #4 definition_of_module
// --------------------------------------------------------------------------
//

m2_token_t m2_definition_of_module(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_definition_of_module


// --------------------------------------------------------------------------
// #5 implementation_of_module
// --------------------------------------------------------------------------
//

m2_token_t m2_implementation_of_module(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_implementation_of_module


// --------------------------------------------------------------------------
// #6 required_binding
// --------------------------------------------------------------------------
//

m2_token_t m2_required_binding(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_required_binding


// --------------------------------------------------------------------------
// #7 bindable_operator
// --------------------------------------------------------------------------
//

m2_token_t m2_bindable_operator(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_bindable_operator


// --------------------------------------------------------------------------
// #8 import_list
// --------------------------------------------------------------------------
//

m2_token_t m2_import_list(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_import_list


// --------------------------------------------------------------------------
// #9 block
// --------------------------------------------------------------------------
//

m2_token_t m2_block(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_block


// --------------------------------------------------------------------------
// #10 declaration
// --------------------------------------------------------------------------
//

m2_token_t m2_declaration(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_declaration


// --------------------------------------------------------------------------
// #11 definition
// --------------------------------------------------------------------------
//

m2_token_t m2_definition(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_definition


// --------------------------------------------------------------------------
// #12 const_declaration
// --------------------------------------------------------------------------
//

m2_token_t m2_const_declaration(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_const_declaration


// --------------------------------------------------------------------------
// #13 type
// --------------------------------------------------------------------------
//

m2_token_t m2_type(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_type


// --------------------------------------------------------------------------
// #14 range
// --------------------------------------------------------------------------
//

m2_token_t m2_range(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_range


// --------------------------------------------------------------------------
// #15 enumeration_type
// --------------------------------------------------------------------------
//

m2_token_t m2_enumeration_type(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_enumeration_type


// --------------------------------------------------------------------------
// #16 array_type
// --------------------------------------------------------------------------
//

m2_token_t m2_array_type(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_array_type


// --------------------------------------------------------------------------
// #17 record_type
// --------------------------------------------------------------------------
//

m2_token_t m2_record_type(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_record_type


// --------------------------------------------------------------------------
// #18 field_list_sequence
// --------------------------------------------------------------------------
//

m2_token_t m2_field_list_sequence(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_field_list_sequence


// --------------------------------------------------------------------------
// #19 field_list
// --------------------------------------------------------------------------
//

m2_token_t m2_field_list(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_field_list


// --------------------------------------------------------------------------
// #20 set_type
// --------------------------------------------------------------------------
//

m2_token_t m2_set_type(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_set_type


// --------------------------------------------------------------------------
// #21 pointer_type
// --------------------------------------------------------------------------
//

m2_token_t m2_pointer_type(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_pointer_type


// --------------------------------------------------------------------------
// #22 procedure_type
// --------------------------------------------------------------------------
//

m2_token_t m2_procedure_type(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_procedure_type


// --------------------------------------------------------------------------
// #23 formal_type_list
// --------------------------------------------------------------------------
//

m2_token_t m2_formal_type_list(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_formal_type_list


// --------------------------------------------------------------------------
// #24 formal_type
// --------------------------------------------------------------------------
//

m2_token_t m2_formal_type(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_formal_type


// --------------------------------------------------------------------------
// #25 attributed_formal_type
// --------------------------------------------------------------------------
//

m2_token_t m2_attributed_formal_type(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_attributed_formal_type


// --------------------------------------------------------------------------
// #26 simple_formal_type
// --------------------------------------------------------------------------
//

m2_token_t m2_simple_formal_type(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_simple_formal_type


// --------------------------------------------------------------------------
// #27 variadic_formal_type
// --------------------------------------------------------------------------
//

m2_token_t m2_variadic_formal_type(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_variadic_formal_type


// --------------------------------------------------------------------------
// #28 variable_declaration
// --------------------------------------------------------------------------
//

m2_token_t m2_variable_declaration(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_variable_declaration


// --------------------------------------------------------------------------
// #29 procedure_declaration
// --------------------------------------------------------------------------
//

m2_token_t m2_procedure_declaration(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_procedure_declaration


// --------------------------------------------------------------------------
// #30 procedure_header
// --------------------------------------------------------------------------
//

m2_token_t m2_procedure_header(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_procedure_header


// --------------------------------------------------------------------------
// #31 formal_param_list
// --------------------------------------------------------------------------
//

m2_token_t m2_formal_param_list(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_formal_param_list


// --------------------------------------------------------------------------
// #32 formal_params
// --------------------------------------------------------------------------
//

m2_token_t m2_formal_params(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_formal_params


// --------------------------------------------------------------------------
// #33 simple_formal_params
// --------------------------------------------------------------------------
//

m2_token_t m2_simple_formal_params(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_simple_formal_params


// --------------------------------------------------------------------------
// #34 variadic_formal_params
// --------------------------------------------------------------------------
//

m2_token_t m2_variadic_formal_params(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_variadic_formal_params


// --------------------------------------------------------------------------
// #35 statement
// --------------------------------------------------------------------------
//

m2_token_t m2_statement(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_statement


// --------------------------------------------------------------------------
// #36 statement_sequence
// --------------------------------------------------------------------------
//

m2_token_t m2_statement_sequence(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_statement_sequence


// --------------------------------------------------------------------------
// #37 assignment_or_procedure_call
// --------------------------------------------------------------------------
//

m2_token_t m2_assignment_or_procedure_call(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_assignment_or_procedure_call


// --------------------------------------------------------------------------
// #38 if_statement
// --------------------------------------------------------------------------
//

m2_token_t m2_if_statement(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_if_statement


// --------------------------------------------------------------------------
// #39 case_statement
// --------------------------------------------------------------------------
//

m2_token_t m2_case_statement(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_case_statement


// --------------------------------------------------------------------------
// #40 case
// --------------------------------------------------------------------------
//

m2_token_t m2_case(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_case


// --------------------------------------------------------------------------
// #41 case_labels
// --------------------------------------------------------------------------
//

m2_token_t m2_case_labels(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_case_labels


// --------------------------------------------------------------------------
// #42 while_statement
// --------------------------------------------------------------------------
//

m2_token_t m2_while_statement(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_while_statement


// --------------------------------------------------------------------------
// #43 repeat_statement
// --------------------------------------------------------------------------
//

m2_token_t m2_repeat_statement(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_repeat_statement


// --------------------------------------------------------------------------
// #44 loop_statement
// --------------------------------------------------------------------------
//

m2_token_t m2_loop_statement(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_loop_statement


// --------------------------------------------------------------------------
// #45 for_statement
// --------------------------------------------------------------------------
//

m2_token_t m2_for_statement(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_for_statement


// --------------------------------------------------------------------------
// #46 const_expression
// --------------------------------------------------------------------------
//

m2_token_t m2_const_expression(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_const_expression


// --------------------------------------------------------------------------
// #47 relation
// --------------------------------------------------------------------------
//

m2_token_t m2_relation(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_relation


// --------------------------------------------------------------------------
// #48 simple_const_expression
// --------------------------------------------------------------------------
//

m2_token_t m2_simple_const_expression(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_simple_const_expression


// --------------------------------------------------------------------------
// #49 add_operator
// --------------------------------------------------------------------------
//

m2_token_t m2_add_operator(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_add_operator


// --------------------------------------------------------------------------
// #50 const_term
// --------------------------------------------------------------------------
//

m2_token_t m2_const_term(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_const_term


// --------------------------------------------------------------------------
// #51 mul_operator
// --------------------------------------------------------------------------
//

m2_token_t m2_mul_operator(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_mul_operator


// --------------------------------------------------------------------------
// #52 const_factor
// --------------------------------------------------------------------------
//

m2_token_t m2_const_factor(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_const_factor


// --------------------------------------------------------------------------
// #53 designator
// --------------------------------------------------------------------------
//

m2_token_t m2_designator(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_designator


// --------------------------------------------------------------------------
// #54 designator_tail
// --------------------------------------------------------------------------
//

m2_token_t m2_designator_tail(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_designator_tail


// --------------------------------------------------------------------------
// #55 expression_list
// --------------------------------------------------------------------------
//

m2_token_t m2_expression_list(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_expression_list


// --------------------------------------------------------------------------
// #56 expression
// --------------------------------------------------------------------------
//

m2_token_t m2_expression(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_expression


// --------------------------------------------------------------------------
// #57 simple_expression
// --------------------------------------------------------------------------
//

m2_token_t m2_simple_expression(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_simple_expression


// --------------------------------------------------------------------------
// #58 term
// --------------------------------------------------------------------------
//

m2_token_t m2_term(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_term


// --------------------------------------------------------------------------
// #59 factor
// --------------------------------------------------------------------------
//

m2_token_t m2_factor(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_factor


// --------------------------------------------------------------------------
// #60 designator_or_procedure_call
// --------------------------------------------------------------------------
//

m2_token_t m2_designator_or_procedure_call(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_designator_or_procedure_call


// --------------------------------------------------------------------------
// #61 actual_params
// --------------------------------------------------------------------------
//

m2_token_t m2_actual_params(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_actual_params


// --------------------------------------------------------------------------
// #62 const_structured_value
// --------------------------------------------------------------------------
//

m2_token_t m2_const_structured_value(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_const_structured_value


// --------------------------------------------------------------------------
// #63 const_value_component
// --------------------------------------------------------------------------
//

m2_token_t m2_const_value_component(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_const_value_component


// --------------------------------------------------------------------------
// #64 structured_value
// --------------------------------------------------------------------------
//

m2_token_t m2_structured_value(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_structured_value


// --------------------------------------------------------------------------
// #65 value_component
// --------------------------------------------------------------------------
//

m2_token_t m2_value_component(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_value_component


// --------------------------------------------------------------------------
// #66 qualident
// --------------------------------------------------------------------------
//

m2_token_t m2_qualident(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_qualident


// --------------------------------------------------------------------------
// #67 ident_list
// --------------------------------------------------------------------------
//

m2_token_t m2_ident_list(m2_parser_t *p) {
    m2_token_t token;
    
    
    return token;
} // end m2_ident_list


// END OF FILE
