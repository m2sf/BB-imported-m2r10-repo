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
#include "m2_tokenset.h"


// ---------------------------------------------------------------------------
// Symbol type
// ---------------------------------------------------------------------------

typedef struct /* m2_sym_s */ {
    m2_token_t token;
    cardinal lexeme;
    m2_lexer_status_t status;
    fpos_t pos;
} m2_sym_s;

#define ZERO_SYMBOL { 0, 0, 0, { 0, 0 } }


// ---------------------------------------------------------------------------
// Parser state
// ---------------------------------------------------------------------------

typedef struct /* m2_parser_s */ {
    char *filename;
    m2_source_type_t source_type;
    m2_lexer_t *lexer;
    m2_sym_s current_sym;
    m2_sym_s lookahead_sym;
    m2_ast_t *ast;
    uint16_t warnings;
    uint16_t errors;
} m2_parser_s;


// ===========================================================================
// P U B L I C   F U N C T I O N S
// ===========================================================================

// --------------------------------------------------------------------------
// function:  m2_new_parser(infile, lextab, status)
// --------------------------------------------------------------------------
//
// Creates  and returns  a  new  parser object  associated  with  source file 
// <infile> and lexeme table <lextab>.  The status of the operation is passed
// back in <status> unless NULL is passed in for <status>.
//
// Returns NULL if the parser object could not be created.

m2_parser_t m2_new_parser(FILE *infile,
                          kvs_table_t lextab,
                          m2_parser_status_t *status) {
    
} // end m2_new_parser


// ---------------------------------------------------------------------------
// function:  m2_dispose_parser(lexer, status)
// ---------------------------------------------------------------------------
//
// Deallocates  lexer object <lexer>.  The function does  not  close the input
// stream  and  it  does  not  deallocate the lexeme table associated with the
// lexer object.  The  status  of  the  operation  is  passed back in <status>
// unless NULL is passed in for <status>.

void m2_dispose_parser(m2_parser_t parser, m2_parser_status_t *status) {
    
} // end m2_dispose_parser


// ===========================================================================
// P R I V A T E   F U N C T I O N S
// ===========================================================================

// ---------------------------------------------------------------------------
// private function:  _getsym( p )
// ---------------------------------------------------------------------------
//
// Reads a new symbol from the input stream,  stores the previous lookahead
// symbol as the current symbol,  stores the new symbol as lookahead symbol,
// then returns the current symbol's token.

static m2_token_t _getsym(m2_parser_s *p) {
    p->current_sym = p->lookahead_sym;
    p->lookahead_sym.token = m2_lexer_getsym(p->lexer,
                                             &p->lookahead_sym.lexeme,
                                             &p->lookahead_sym.status);
    return p->lookahead_sym.token;
} // _getsym


// ---------------------------------------------------------------------------
// private function:  _lookahead( p )
// ---------------------------------------------------------------------------
//
// Returns the token of the current lookahead symbol.  This function does not
// read from the input stream.  Subsequent calls to this function will return
// the  same token  again.  To read  further symbols  from the  input stream,
// function _getsym must be used.

static fmacro m2_token_t _lookahead(m2_parser_s *p) {
    return p->lookahead_sym.token;
} // _lookahead


// ---------------------------------------------------------------------------
// private function:  match_token( p, expected_token, skip_to_tokens )
// ---------------------------------------------------------------------------
//
// Compares the current lookahead token of parser <p> to <expected_token>.  If
// the two match,  the lookahead symbol is read from the input stream.  If the
// the two do not match,  the parser's error counter is incremented,  an error
// message is printed to stderr  and  symbols in the input stream  are skipped
// until the lookahead token matches one of the tokens in <skip_to_tokens>.

static bool match_token(m2_parser_s *p,
                         m2_token_t expected_token,
                      m2_tokenset_t set_of_tokens_to_skip_to) {
    
    // consume lookahead if it matches expected token
    if (p->lookahead_sym.token == expected_token) {
        _getsym(p);
        
        return true;
    }
    else /* syntax error */ {
        // report error
        report_mismatch(p, expected_token, 0);
        
        // skip symbols until the lookahead symbol matches skipset
        while NOT (m2_tokenset_is_element(set_of_tokens_to_skip_to,
                                          p->lookahead_sym.token)) {
            _getsym(p);
        } // end while
        
        // update error count
        p->errors++;
        
        return false;
    } // end if
    
} // end match_token


// ---------------------------------------------------------------------------
// private function:  match_token_in_set( p, expected_tokens, skip_to_tokens )
// ---------------------------------------------------------------------------
//
// Tests the current lookahead token  against  tokenset <expected_tokens>.  If
// the lookahead token  matches  any  of the tokens in <expected_tokens>,  the
// lookahead symbol is read from the input stream.  If it does  not match  any
// of the tokens,  the parser's error counter is incremented, an error message
// is printed to stderr  and symbols in the input stream are skipped until the
// lookahead token matches one of the tokens in <skip_to_tokens>.

static bool match_token_in_set(m2_parser_s *p,
                             m2_tokenset_t set_of_expected_tokens,
                             m2_tokenset_t set_of_tokens_to_skip_to) {
    
    // consume lookahead if it matches any of the expected tokens
    if (m2_tokenset_is_element(set_of_expected_tokens,
                               p->lookahead_sym.token)) {
        // get next symbol
        _getsym(p);
        
        return true;
    }
    else /* syntax error */ {
        // report error
        report_mismatch(p, p->lookahead_sym.token, 0);
        
        // skip symbols until the lookahead symbol matches followset
        while NOT (m2_tokenset_is_element(set_of_tokens_to_skip_to,
                                          p->lookahead_sym.token)) {
            _getsym(p);
        } // end while
        
        // update error count
        p->errors++;
        
        return false;
    } // end if
    
} // end match_token_in_set


// ---------------------------------------------------------------------------
// private function: report_mismatch( p, expected_tokens )
// ---------------------------------------------------------------------------
//
// Reports a mismatch  between encountered symbol  found_sym  and the expected
// tokens passed in as tokenset iterator <expected_tokens>.

static void report_mismatch(m2_parser_s *p,
                 m2_tokenset_iterator_t set_of_expected_tokens) {
    
    cardinal row = 0, col = 0, index, token_count;
    m2_lexer_status_t status;
    m2_token_t token;
    
    if (expected_tokens == NULL) return;
    
    token_count = m2_tokenset_iterator_token_count(set_of_expected_tokens);
    
    if (token_count == 0) return;
    
    token = m2_tokenset_iterator_token_at_index(set_of_expected_tokens, 0);
    m2_lexer_getpos(p->lexer, &row, &col, &status);
    
    printf("syntax error in line %i, col %i : found '%s', expected ",
           row, col, m2_token_name(p->lookahead_sym.token));
    
    index = 0;
    while (index < token_count) {
        token =
          m2_tokenset_iterator_token_at_index(set_of_expected_tokens, index);
        printf("'%s'", m2_token_name(token));
        if (index + 2 < token_count)
            printf(", ");
        else if (index + 2 == token_count)
            printf(" or ");
        else
            printf("\n");
        index++;
    } // end while
    
    return;
} // end report_mismatch


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
        
    switch(_lookahead(p)) {
        case TOKEN_PROTOTYPE :
            m2_prototype(p);
            break;
        case TOKEN_MODULE :
            m2_program_module(p);
            break;
        case TOKEN_DEFINITION :
            m2_definition_of_module(p);
            break;
        case TOKEN_IMPLEMENTATION :
            m2_implementation_of_module(p);
            break;
        default :
            // unreachable code
            fatal_error(); // abort
    } // end switch
    
    return _lookahead(p);
} // end m2_compilation_unit


// --------------------------------------------------------------------------
// #2 prototype
// --------------------------------------------------------------------------
//  PROTOTYPE prototypeId ";"
//  TYPE "=" ( RECORD | OPAQUE RECORD? ( ":=" literalType )? ) ";"
//  ( ASSOCIATIVE ";" )? requiredBinding* END prototypeId "."

m2_token_t m2_prototype(m2_parser_t *p) {
    m2_token_t token;
    
    // PROTOTYPE
    _getsym(p);
    
    // prototypeId
    if (match_token(p, TOKEN_IDENTIFIER, SKIP_TO_TYPE_OR_REQ_BINDING)) {
        _getsym(p);
        
        // ";"
        if (match_token, TOKEN_SEMICOLON, SKIP_TO_TYPE_OR_REQ_BINDING) {
            _getsym(p);
            
        } // end ";"
    } // end prototypeId
    
    // TYPE
    if (match_token(p, TOKEN_TYPE, SKIP_TO_REQ_BINDING)) {
        _getsym(p);
        
        // "="
        if (match_token(p, TOKEN_EQUAL_OP, SKIP_TO_REQ_BINDING)) {
            _getsym(p);
            
            // RECORD | OPAQUE
            if (match_token_in_set(p, FIRST_RECORD_OR_OPAQUE,
                                      SKIP_TO_REQ_BINDING)) {
                _getsym(p);
                
                // ":="
                if (_lookahead(p) == TOKEN_ASSIGN_OP) {
                    _getsym(p);
                    
                    // literalType
                    m2_literal_type(p);
                    
                } // end ":="
                
                // ";"
                if (match_token(p, TOKEN_SEMICOLON, SKIP_TO_REQ_BINDING)) {
                    _getsym(p);

                } // end ";" 
                
            } // end RECORD | OPAQUE
            
        } // end "="
        
    } // end TYPE
    
    // ASSOCIATIVE
    if (_lookahead(p) == TOKEN_ASSOCIATIVE) {
        _getsym(p);
        
        // ";"
        if (match_token(p, TOKEN_SEMICOLON, SKIP_TO_REQ_BINDING)) {
            _getsym(p);
            
        } // end ";" 
        
    } // end ASSOCIATIVE
    
    // requiredBinding*
    while (m2_tokenset_is_element(FIRST_REQ_BINDING, _lookahead(p))) {
        m2_required_binding(p);
           
    } // end requiredBinding

    // END
    if (match_token(p, TOKEN_END, SKIP_TO_DOT_OR_EOF)) {
        _getsym(p);
        
    } // end END 

    // prototypeId
    if (match_token(p, TOKEN_IDENTIFIER, SKIP_TO_DOT_OR_EOF)) {
        _getsym(p);
        
        // check against name in prototype header
        *** TO DO ***
        
    } // end prototypeId
    
    // "."
    if (match_token(p, TOKEN_DOT, SKIP_TO_EOF)) {
        _getsym(p);
        
    } // end "."
    
    return _lookahead(p);
} // end m2_prototype


// --------------------------------------------------------------------------
// #3 program_module
// --------------------------------------------------------------------------
//  MODULE moduleId ( "[" constExpression "]" )? ";"
//  importList* block moduleId "."

m2_token_t m2_program_module(m2_parser_t *p) {
        
    // MODULE
    _getsym(p);
        
    // moduleId
    if (match_token(p, TOKEN_IDENTIFIER, SKIP_TO_IMPORT_OR_BLOCK)) {
        _getsym(p); // consume moduleId
        
        // store module name
        *** TO DO ***
        
        // "["
        if (_lookahead(p) == TOKEN_LBRACKET) {
            _getsym(p);
            
            // constExpression
            if (match_token_in_set(p, FIRST_CONST_EXPRESSION,
                                      SKIP_TO_SEMI_OR_IMPORT_OR_BLOCK)) {
                m2_const_expression(p);
                
                // "]"
                if (match_token(p, TOKEN_RBRACKET,
                                   SKIP_TO_SEMI_OR_IMPORT_OR_BLOCK)) {
                    _getsym(p);
                    
                } // end "]"
                
            } // end constExpression
        } // end "["
        
        // ";"
        if (match_token(p, TOKEN_SEMICOLON, SKIP_TO_IMPORT_OR_BLOCK)) {
            _getsym(p);
        } // end ";"
                
    } // end moduleId
    
    // importList*
    while (m2_tokenset_is_element(FIRST_IMPORT_LIST, _lookahead(p))) {
        m2_import_list(p);
        
    } // end while
    
    // block
    if (match_token_in_set(p, FIRST_BLOCK, SKIP_TO_IDENT_OR_DOT_OR_EOF)) {
        m2_block(p);
    } // end block
    
    // moduleId
    if (match_token(p, TOKEN_IDENTIFIER, SKIP_TO_DOT_OR_EOF)) {
        _getsym(p);
        
        // check against name in module header
        *** TO DO ***
        
    } // end moduleId
    
    // "."
    if (match_token(p, TOKEN_DOT, SKIP_TO_EOF)) {
        _getsym(p);

    } // end "."
    
    return _lookahead(p);
} // end m2_program_module


// --------------------------------------------------------------------------
// #4 definition_of_module
// --------------------------------------------------------------------------
//  DEFINITION MODULE moduleId ( "[" prototypeId "]" )? ";"
//  importList* definition* END moduleId "."

m2_token_t m2_definition_of_module(m2_parser_t *p) {
    
    // DEFINITION
    _getsym(p);
    
    // MODULE
    if (match_token(p, TOKEN_MODULE, SKIP_TO_IDENT)) {
        _getsym(p);
        
    } // end MODULE
    
    // moduleId
    if (match_token(p, TOKEN_IDENTIFIER,
                       SKIP_TO_SEMI_OR_IMPORT_OR_DEFN_OR_END)) {
        _getsym(p);
        
        // store module name
        *** TO DO ***
        
        // "["
        if (_lookahead(p) == TOKEN_LBRACKET) {
            _getsym(p);
            
            // prototypeId
            if (match_token(p, TOKEN_IDENTIFIER, SKIP_TO_RBRACKET)) {
                _getsym(p);
                
            } // end prototypeId
            
            if (match_token(p, TOKEN_RBRACKET,
                               SKIP_TO_SEMI_OR_IMPORT_OR_DEFN_OR_END)) {
                _getsym(p);
                
            } // end "]"
        } // end "["
        
    } // end moduleId
    
    // ";"
    if (match_token(p, TOKEN_SEMICOLON,
                       SKIP_TO_IMPORT_OR_DEFN_OR_END)) {
        _getsym(p);
        
    } // end ";"
    
    // importList*
    while (m2_tokenset_is_element(FIRST_IMPORT_LIST, _lookahead(p))) {
        m2_import_list(p);
        
    } // end importList

    // definition*
    while (m2_tokenset_is_element(FIRST_DEFINITION, _lookahead(p))) {
        m2_definition(p);
        
    } // end definition
    
    // END
    if (match_token(p, TOKEN_END, SKIP_TO_DOT_OR_EOF)) {
        _getsym(p);
        
    } // end END
    
    // moduleId
    if (match_token(p, TOKEN_IDENTIFIER, SKIP_TO_DOT_OR_EOF)) {
        _getsym(p);
        
        // check against name in module header
        *** TO DO ***
        
    } // end moduleId
    
    // "."
    if (match_token(p, TOKEN_DOT, SKIP_TO_EOF)) {
        _getsym(p);
        
    } // end "."
    
    return _lookahead(p);
} // end m2_definition_of_module


// --------------------------------------------------------------------------
// #5 implementation_of_module
// --------------------------------------------------------------------------
//  IMPLEMENTATION programModule

m2_token_t m2_implementation_of_module(m2_parser_t *p) {
    
    // IMPLEMENTATION
    _getsym(p);
    
    // programModule
    if (match_token_in_set(p, FIRST_PROGRAM_MODULE, SKIP_TO_EOF)) {
        m2_program_module(p);
        
    } // end programModule
    
    return _lookahead(p);
} // end m2_implementation_of_module


// --------------------------------------------------------------------------
// #6 required_binding
// --------------------------------------------------------------------------
//  ( CONST "[" bindableIdent "] |
//    PROCEDURE "[" ( bindableOperator | bindableIdent ) "]" ) ";"

m2_token_t m2_required_binding(m2_parser_t *p) {
    
    switch (_lookahead(p)) {
        // CONST
        case TOKEN_CONST :
            _getsym(p);
            
            // "["
            if (match_token(p, TOKEN_LBRACKET, SKIP_TO_IDENT)) {
                _getsym(p);
                
            } // end "["
            
            // bindableIdent
            if (match_token_in_set(p, FIRST_BINDABLE_IDENT,
                                      SKIP_TO_RBRACKET_OR_CONST_OR_PROC)) {
                _getsym(p);
                
            } // end bindableIdent
            
            // "]"
            if (match_token(p, TOKEN_RBRACKET, SKIP_TO_CONST_OR_PROC)) {
                _getsym(p);
                
            } // end "]"
            break;
        
        // PROCEDURE
        case TOKEN_PROCEDURE :
            _getsym(p);
            
            // "["
            if (match_token(p, TOKEN_LBRACKET, SKIP_TO_IDENT)) {
                _getsym(p);
                
            } // end "["
            
            // bindableOperator
            if (m2_tokenset_is_element(FIRST_BINDABLE_OP, _lookahead(p))) {
                m2_bindable_operator(p);
                
            }
            else if (_lookahead(p) == TOKEN_IDENTIFIER) {
                _getsym(p);
                
                // check identifier
                *** TO DO ***
            }
            else {
                // syntax error: expected bindable operator or ident
                
            } // end bindableOperator | bindableIdent
            
            // "]"
            if (match_token(p, TOKEN_RBRACKET, SKIP_TO_CONST_OR_PROC)) {
                _getsym(p);
                
            } // end "]"
            break;
            
        default :
            // unreachable code
            fatal_error(); // abort
    } // switch
    
    // ";"
    if (match_token(p, TOKEN_SEMICOLON, FOLLOW_REQ_BINDING)) {
        _getsym(p);
        
    } // end ";"
    
    return _lookahead(p);
} // end m2_required_binding


// --------------------------------------------------------------------------
// #7 bindable_operator
// --------------------------------------------------------------------------
//  DIV | MOD | IN | FOR |
//  ":=" | "?" | "!" | "~" | "+" | "-" | "*" | "/" | "=" | "<" | ">"

m2_token_t m2_bindable_operator(m2_parser_t *p) {
    
    switch (_lookahead(p)) {
        case TOKEN_DIV :
            _getsym(p);
            break;
        case TOKEN_MOD :
            _getsym(p);
            break;
        case TOKEN_IN :
            _getsym(p);
            break;
        case TOKEN_FOR :
            _getsym(p);
            break;
        case TOKEN_ASSIGN_OP :
            _getsym(p);
            break;
        case TOKEN_RETRIEVAL_PSEUDO_OP :
            _getsym(p);
            break;
        case TOKEN_STORAGE_PSEUDO_OP :
            _getsym(p);
            break;
        case TOKEN_REMOVAL_PSEUDO_OP :
            _getsym(p);
            break;
        case TOKEN_PLUS_OP :
            _getsym(p);
            break;
        case TOKEN_MINUS_OP :
            _getsym(p);
            break;
        case TOKEN_ASTERISK_OP :
            _getsym(p);
            break;
        case TOKEN_SLASH_OP :
            _getsym(p);
            break;
        case TOKEN_EQUAL_OP :
            _getsym(p);
            break;
        case TOKEN_LESS_OP :
            _getsym(p);
            break;
        case TOKEN_GREATER_OP :
            _getsym(p);
            break;
        default :
            // unreachable code
            fatal_error(); // abort
    } // end switch
    
    return _lookahead(p);
} // end m2_bindable_operator


// --------------------------------------------------------------------------
// #8 import_list
// --------------------------------------------------------------------------
//  ( FROM moduleId IMPORT ( identList | "*" ) |
//    IMPORT ident "+"? ( "," ident "+"? )* ) ";"

m2_token_t m2_import_list(m2_parser_t *p) {
    
    switch (_lookahead(p)) {
        
        // FROM
        case TOKEN_FROM :
            _getsym(p);
            
            // moduleId
            if (match_token(p, TOKEN_IDENTIFIER, SKIP_TO_IMPORT)) {
                _getsym(p);
                
            } // end moduleId
            
            // IMPORT
            if (match_token(p, TOKEN_IMPORT,
                               SKIP_TO_SEMI_OR_IMPORT_OR_BLOCK)) {
                _getsym(p);
                
                // identList | "*"
                if (match_token_in_set(p, FIRST_IDENT_OR_ASTERISK,
                                          SKIP_TO_SEMI_OR_IMPORT_OR_BLOCK)) {
                    if (_lookahead(p) == TOKEN_IDENTIFIER) {
                        _getsym(p);
                    }
                    else if (_lookahead(p) == TOKEN_ASTERISK_OP) {
                        _getsym(p);
                    }
                    else {
                        // unreachable code
                        fatal_error(); // abort
                    } // end if
                    
                } // end identList | "*"
                
            } // end IMPORT
            
            break;
            
        // IMPORT
        case TOKEN_IMPORT :
            _getsym(p);
            
            // ident
            if (match_token(p, TOKEN_IDENTIFIER,
                               SKIP_TO_COMMA_OR_SEMI_OR_IMPORT_OR_BLOCK)) {
                _getsym(p);
                
            } // ident
            
            // "+"?
            if (_lookahead(p) == TOKEN_PLUS_OP) {
                _getsym(p);
                
            } // "+"?
            
            // ( "," ident "+"? )*
            while (_lookahead(p) == TOKEN_COMMA) {
                _getsym(p);
                
                // ident
                if (match_token(p, TOKEN_IDENTIFIER,
                                   SKIP_TO_SEMI_OR_IMPORT_OR_BLOCK)) {
                    _getsym(p);
                    
                } // end ident
                
                // "+"?
                if (_lookahead(p) == TOKEN_PLUS_OP) {
                    _getsym(p);
                    
                } // "+"?
                
            } // ( "," ident "+"? )*
            
            break;
            
        default :
            // unreachable code
            fatal_error(); // abort
    } // end switch

    // ";"
    if (match_token(p, TOKEN_SEMICOLON, FOLLOW_IMPORT_LIST)) {
        _getsym(p);
        
    } // end ";"
    
    return _lookahead(p);
} // end m2_import_list


// --------------------------------------------------------------------------
// #9 block
// --------------------------------------------------------------------------
//  definition* ( BEGIN statementSequence )? END

m2_token_t m2_block(m2_parser_t *p) {
    
    // definition*
    while (m2_tokenset_is_element(FIRST_DEFINITION, _lookahead(p))) {
        m2_definition(p);
        
    } // end definition*
    
    // BEGIN
    if (_lookahead(p) == TOKEN_BEGIN) {
        _getsym(p);
        
        // statementSequence
        if (match_token_in_set(p, FIRST_STATEMENT_SEQ, SKIP_TO_END)) {
            m2_statement_sequence(p);
            
        } // end statementSequence
        
    } // end BEGIN
    
    // END
    if (match_token(p, TOKEN_END, SKIP_TO_END)) {
        _getsym(p);
        
    } // end END
    
    return _lookahead(p);
} // end m2_block


// --------------------------------------------------------------------------
// #10 declaration
// --------------------------------------------------------------------------
//  CONST ( constantDeclaration ";" )* |
//  TYPE ( ident "=" type ";" )* |
//  VAR ( variableDeclaration ";" )* |
//  procedureDeclaration ";"

m2_token_t m2_declaration(m2_parser_t *p) {
    
    switch (_lookahead(p)) {
        
        // CONST
        case TOKEN_CONST :
            _getsym(p);
            
            // ( constantDeclaration ";" )*
            while (m2_tokenset_is_element(FIRST_CONST_DECLARATION,
                                          _lookahead(p))) {
                m2_const_declaration(p);
                
                // ";"
                if (match_token(p, TOKEN_SEMICOLON, SKIP_TO_SEMICOLON)) {
                    _getsym(p);
                    
                } // ";"
                
            } // end ( constantDeclaration ";" )*
            
            break;
            
        // TYPE
        case TOKEN_TYPE :
            _getsym(p);
            
            // ( ident "=" type ";" )*
            while (_lookahead(p) == TOKEN_IDENTIFIER) {
                _getsym(p);
                
                // "="
                if (match_token(p, TOKEN_EQUAL_OP, SKIP_TO_TYPE)) {
                    _getsym(p);
                    
                } // end "="
                
                // type
                if (match_token_in_set(p, FIRST_TYPE, SKIP_TO_SEMICOLON)) {
                    m2_type(p);
                    
                } // end type
                
                // ";"
                if (match_token(p, TOKEN_SEMICOLON, SKIP_TO_SEMICOLON)) {
                    _getsym(p);
                    
                } // ";"
                
            } // end ( ident "=" type ";" )*
            
            break;
            
        // VAR
        case TOKEN_VAR :
            _getsym(p);
            
            // ( variableDeclaration ";" )*
            while (m2_tokenset_is_element(FIRST_VAR_DECLARATION,
                                          _lookahead(p))) {
                m2_variable_declaration(p);
                
                // ";"
                if (match_token(p, TOKEN_SEMICOLON, SKIP_TO_SEMICOLON)) {
                    _getsym(p);
                    
                } // ";"
                
            } // end ( variableDeclaration ";" )*
            
            break;
            // PROCEDURE
        case TOKEN_PROCEDURE :            
            m2_procedure_declaration(p);
            
            // ";"
            if (match_token(p, TOKEN_SEMICOLON, SKIP_TO_SEMICOLON)) {
                _getsym(p);
                
            } // ";"

            break;
            
        default :
            // unreachable code
            fatal_error(); // abort
    } // end
    
    return _lookahead(p);
} // end m2_declaration


// --------------------------------------------------------------------------
// #11 definition
// --------------------------------------------------------------------------
//  CONST ( ( "[" ident "]" )? constantDeclaration ";"  )* |
//  TYPE ( ident "=" ( type | OPAQUE recordType? ) ";" )* |
//  VAR ( variableDeclaration ";" )* |
//  procedureHeader ";"

m2_token_t m2_definition(m2_parser_t *p) {
    
    switch (_lookahead(p)) {
            
            // CONST
        case TOKEN_CONST :
            _getsym(p);
            
            break;
            
            // TYPE
        case TOKEN_TYPE :
            _getsym(p);
            
            break;
            
            // VAR
        case TOKEN_VAR :
            _getsym(p);
            
            break;
            
            // PROCEDURE
        case TOKEN_PROCEDURE :
            _getsym(p);
            
            break;
            
        default :
            // unreachable code
            fatal_error(); // abort
    } // end
    
    return _lookahead(p);
} // end m2_definition


// --------------------------------------------------------------------------
// #12 const_declaration
// --------------------------------------------------------------------------
//  ident "=" constExpression

m2_token_t m2_const_declaration(m2_parser_t *p) {
    
    // ident
    _getsym(p);
    
    // "="
    if (match_token(p, TOKEN_EQUAL_OP, FIRST_CONST_EXPRESSION)) {
        _getsym(p);
        
    } // end "="
    
    // constExpression
    if (match_token_in_set(p, FIRST_CONST_EXPRESSION,
                              FOLLOW_CONST_EXPRESSION)) {
        m2_const_expression(p);
        
    } // end constExpression
    
    return _lookahead(p);
} // end m2_const_declaration


// --------------------------------------------------------------------------
// #13 type
// --------------------------------------------------------------------------
//  ( ALIAS | range ) OF namedType | enumerationType |
//  arrayType | recordType | setType | pointerType | procedureType

m2_token_t m2_type(m2_parser_t *p) {
    
    if (_lookahead(p) == TOKEN_ALIAS) {
        _getsym(p);
        
        // OF
        if (match_token(p, TOKEN_OF, SKIP_TO_IDENTIFIER)) {
            _getsym(p);
        
        } // end OF
        
        // namedType
        if (match_token(p, TOKEN_IDENTIFIER, FOLLOW_TYPE)) {
            _getsym(p);
            
        } // end namedType
    }
    else if (m2_tokenset_is_element(FIRST_RANGE, _lookahead(p))) {
        m2_range(p);
        
        // OF
        if (match_token(p, TOKEN_OF, SKIP_TO_IDENTIFIER)) {
            _getsym(p);
            
        } // end OF
        
        // namedType
        if (match_token(p, TOKEN_IDENTIFIER, FOLLOW_TYPE)) {
            _getsym(p);
            
        } // end namedType
    }
    else if (m2_tokenset_is_element(FIRST_ENUMERATION_TYPE, _lookahead(p))) {
        m2_enumeration_type(p);
    }
    else if (m2_tokenset_is_element(FIRST_ARRAY_TYPE, _lookahead(p))) {
        m2_array_type(p);
    }
    else if (m2_tokenset_is_element(FIRST_RECORD_TYPE, _lookahead(p))) {
        m2_record_type(p);
    }
    else if (m2_tokenset_is_element(FIRST_SET_TYPE, _lookahead(p))) {
        m2_set_type(p);
    }
    else if (m2_tokenset_is_element(FIRST_POINTER_TYPE, _lookahead(p))) {
        m2_pointer_type(p);
    }
    else if (m2_tokenset_is_element(FIRST_PROCEDURE_TYPE, _lookahead(p))) {
        m2_procedure_type(p);
    }
    else {
        // unreachable code
        fatal_error(); // abort
    } // end if
    
    return _lookahead(p);
} // end m2_type


// --------------------------------------------------------------------------
// #14 range
// --------------------------------------------------------------------------
//  "[" constExpression ".." constExpression "]"

m2_token_t m2_range(m2_parser_t *p) {
    
    // "["
    _getsym(p);
    
    // constExpression
    if (match_token_in_set(p, FIRST_CONST_EXPRESSION, SKIP_TO_RANGE_OP)) {
        m2_const_expression(p);
    
    } // end constExpression
    
    // ".."
    if (match_token(p, TOKEN_RANGE_OP, FIRST_CONST_EXPRESSION)) {
        _getsym(p);
        
    } // end ".."
    
    // constExpression
    if (match_token_in_set(p, FIRST_CONST_EXPRESSION, SKIP_TO_RANGE_OP)) {
        m2_const_expression(p);
        
    } // end constExpression
    
    return _lookahead(p);
} // end m2_range


// --------------------------------------------------------------------------
// #15 enumeration_type
// --------------------------------------------------------------------------
//  "(" ( ( "+" namedType ) | ident )
//        ( "," ( ( "+" namedType ) | ident ) )* ")"

m2_token_t m2_enumeration_type(m2_parser_t *p) {

    // "("
    _getsym(p);
    
    // ( "+" namedType ) | ident )
    if (match_token_in_set(p, FIRST_ENUMERATION_COMPONENT,
                              SKIP_TO_COMMA_OR_RPAREN)) {
        m2_enumeration_component(p);
        
    } // end ( "+" namedType ) | ident )
    
    // ( "," ( ( "+" namedType ) | ident ) )*
    while (_lookahead(p) == TOKEN_COMMA) {
        _getsym(p);
        
        // ( "+" namedType ) | ident )
        if (match_token_in_set(p, FIRST_ENUMERATION_COMPONENT,
                               SKIP_TO_COMMA_OR_RPAREN)) {
            m2_enumeration_component(p);
            
        } // end ( "+" namedType ) | ident )
        
    } // end ( "," ( ( "+" namedType ) | ident ) )*
    
    // ")"
    if (match_token(p, TOKEN_RPAREN, FOLLOW_ENUMERATION_TYPE)) {
        _getsym(p);
                
    } // end ")"
    
    return _lookahead(p);
} // end m2_enumeration_type

// --------------------------------------------------------------------------
// #15.1 enumeration_component
// --------------------------------------------------------------------------
//  ( "+" namedType | ident )

m2_token_t m2_enumeration_component(m2_parser_t *p) {
    
    // "+"
    if (_lookahead(p) == TOKEN_PLUS_OP) {
        _getsym(p);
        
        // namedType
        if (match_token(p, TOKEN_IDENTIFIER, SKIP_TO_COMMA_OR_RPAREN)) {
            
        } // end namedType
        
    }
    // ident
    else if (_lookahead(p) == TOKEN_IDENTIFIER) {
        _getsym(p);
        
    }
    else {
        // unreachable code
        fatal_error(); // abort
    }
    
    return _lookahead(p);
} // end m2_enumeration_component


// --------------------------------------------------------------------------
// #16 array_type
// --------------------------------------------------------------------------
//  ( ARRAY constComponentCount ( "," constComponentCount )* |
//    ASSOCIATIVE ARRAY ) OF namedType

m2_token_t m2_array_type(m2_parser_t *p) {
    
    // ARRAY
    if (_lookahead(p) == TOKEN_ARRAY) {
        _getsym(p);
        
        // constComponentCount
        if (match_token_in_set(p, FIRST_CONST_EXPRESSION,
                                  SKIP_TO_COMMA_OR_OF)) {
            _getsym(p);
            
        } // end constComponentCount
        
        // ( "," constComponentCount )*
        while (_lookahead(p) == TOKEN_COMMA) {
            _getsym(p);
            
            // constComponentCount
            if (match_token_in_set(p, FIRST_CONST_EXPRESSION,
                                   SKIP_TO_COMMA_OR_OF)) {
                _getsym(p);
                
            } // end constComponentCount
            
        } // end ( "," constComponentCount )*
        
    }
    // ASSOCIATIVE
    else if (_lookahead(p) == TOKEN_ASSOCIATIVE) {
        _getsym(p);
        
        // ARRAY
        if (match_token(p, TOKEN_ARRAY, SKIP_TO_OF_OR_IDENT)) {
            _getsym(p);
            
        } // end ARRAY
        
    }
    else {
        // unreachable code
        fatal_error(); // abort
    } //
    
    // OF
    if (match_token(p, TOKEN_OF, SKIP_TO_IDENT)) {
        _getsym(p);
        
    } // end OF
    
    // namedType
    if (match_token(p, TOKEN_IDENTIFIER, FOLLOW_ARRAY_TYPE)) {
        _getsym(p);
        
    } // end namedType
    
    return _lookahead(p);
} // end m2_array_type


// --------------------------------------------------------------------------
// #17 record_type
// --------------------------------------------------------------------------
//  RECORD ( "(" baseType ")" )? fieldListSequence? END

m2_token_t m2_record_type(m2_parser_t *p) {
    
    // RECORD
    _getsym(p);
    
    // ( "(" baseType ")" )?
    if (_lookahead(p) == TOKEN_LPAREN) {
        _getsym(p);
        
        // baseType
        if (match_token(p, TOKEN_IDENTIFIER, SKIP_TO_RPAREN)) {
            _getsym(p);
            
        } // end baseType
        
        // ")"
        if (match_token(p, TOKEN_RPAREN, FIRST_FIELD_LIST_SEQ)) {
            _getsym(p);
            
        } // end ")"
        
    } // end ( "(" baseType ")" )?
    
    // fieldListSequence?
    if (m2_tokenset_is_element(FIRST_FIELD_LIST_SEQ, _lookahead(p))) {
        m2_field_list_sequence(p);
    
    } // end fieldListSequence?
    
    // END
    if (match_token(p, TOKEN_END, FOLLOW_RECORS_TYPE)) {
        _getsym(p);
        
    } // end END
    
    return _lookahead(p);
} // end m2_record_type


// --------------------------------------------------------------------------
// #18 field_list_sequence
// --------------------------------------------------------------------------
//  fieldList ( ";" fieldList )*

m2_token_t m2_field_list_sequence(m2_parser_t *p) {
    
    // fieldList
    m2_field_list(p);
    
    // ( ";" fieldList )*
    while (_lookahead(p) == TOKEN_SEMICOLON) {
        _getsym(p);
        
        // fieldList
        if (match_token_in_set(p, FIRST_FIELD_LIST, FOLLOW_FIELD_LIST_SEQ)) {
            m2_field_list(p);
            
        } // end fieldList
        
    } // end ( ";" fieldList )*
    
    return _lookahead(p);
} // end m2_field_list_sequence


// --------------------------------------------------------------------------
// #19 field_list
// --------------------------------------------------------------------------
//  ident
//  ( ( "," ident )+ ":" namedType |
//    ":" ( ARRAY determinantField OF )? namedType )

m2_token_t m2_field_list(m2_parser_t *p) {
    
    // ident
    _getsym(p);
    
    // ( "," ident )+ ":" namedType
    if (_lookahead(p) == TOKEN_COMMA) {
        _getsym(p);
        
        // ident
        if (match_token(p, TOKEN_IDENTIFIER, SKIPT_TO_COMMA_OR_IDENT)) {
            _getsym(p);
            
        } // end ident
        
        // ","
        while (_lookahead(p) == TOKEN_COMMA) {
            _getsym(p);
            
            // ident
            if (match_token(p, TOKEN_IDENTIFIER, SKIP_TO_COMMA_OR_IDENT)) {
                _getsym(p);
                
            } // end ident
            
        } // end ","
        
        // ":"
        if (match_token(p, TOKEN_SEMICOLON, SKIP_TO_IDENT)) {
            _getstm(p);
            
        } // end ":"
        
        // namedType
        if (match_token(p, TOKEN_IDENTIFIER, FOLLOW_FIELD_LIST)) {
            _getsym(p);
            
        } // end namedType
        
    }
    // ":" ( ARRAY determinantField OF )? namedType
    else if (_lookahead(p) == TOKEN_COLON) {
        _getsym(p);
        
        // ARRAY
        if (_lookahead(p) == TOKEN_ARRAY) {
            _getsym(p);
            
            // determinantField
            if (match_token(p, TOKEN_IDENTIFIER, SKIP_TO_OF_OR_IDENT)) {
                _getsym(p);
                
            } // end determinantField
            
            // OF
            if (match_token(p, TOKEN_OF, SKIP_TO_IDENT)) {
                _getsym(p);
                
            } // end OF
        
        } // end ARRAY

        // namedType
        if (match_token(p, TOKEN_IDENTIFIER, FOLLOW_FIELD_LIST)) {
            _getsym(p);
            
        } // end namedType
        
    } // end 
    
    return _lookahead(p);
} // end m2_field_list


// --------------------------------------------------------------------------
// #20 set_type
// --------------------------------------------------------------------------
//  SET OF ( namedEnumType | "(" identList ")" )

m2_token_t m2_set_type(m2_parser_t *p) {
    
    // SET
    _getsym(p);
    
    // OF
    if (match_token(p, TOKEN_OF, SKIP_TO_LPAREN_OR_IDENT)) {
        _getsym(p);
        
    } // end OF
    
    // set tail
    if (match_token_in_set(p, FIRST_IDENT_OR_LPAREN, FOLLOW_SET_TYPE)) {
        
        // namedEnumType | "("
        if (_lookahead(p) == TOKEN_IDENTIFIER) {
            _getsym(p);
            
        }
        else if (_lookahead(p) == TOKEN_LPAREN) {
            _getsym(p);
            
            // identList
            if (match_token(p, TOKEN_IDENTIFIER, SKIP_TO_RPAREN)) {
                m2_ident_list(p);
                
            } // end identList
            
            // ")"
            if (match_token(p, TOKEN_RPAREN, FOLLOW_SET_TYPE)) {
                _getsym(p);
                
            } // end ")"
        }
        else {
            // unreachable code
            fatal_error(); // abort
            
        } // namedEnumType | "("
        
    } // end set tail
        
    return _lookahead(p);
} // end m2_set_type


// --------------------------------------------------------------------------
// #21 pointer_type
// --------------------------------------------------------------------------
//  POINTER TO CONST? namedType

m2_token_t m2_pointer_type(m2_parser_t *p) {
    
    // POINTER
    _getsym(p);
    
    // TO
    if (match_token(p, TOKEN_TO, SKIP_TO_CONST_OR_IDENT)) {
        _getsym(p);
        
    } // end TO
    
    // CONST?
    if (_lookahead(p) == TOKEN_CONST) {
        _getsym(p);
        
    } // end CONST?
    
    // namedType
    if (match_token(p, TOKEN_IDENTIFIER, FOLLOW_POINTER_TYPE)) {
        _getsym(p);
        
    } // end namedType
    
    return _lookahead(p);
} // end m2_pointer_type


// --------------------------------------------------------------------------
// #22 procedure_type
// --------------------------------------------------------------------------
//  PROCEDURE ( "(" formalTypeList ")" )? ( ":" returnedType )?

m2_token_t m2_procedure_type(m2_parser_t *p) {
    
    // PROCEDURE
    _getsym(p);
    
    // ( "(" formalTypeList ")" )?
    if (_lookahead(p) == TOKEN_LPAREN) {
        _getsym(p);
        
        // formalTypeList
        if (match_token_in_set(p, FIRST_FORMAL_TYPE_LIST,
                                  FOLLOW_FORMAL_TYPE_LIST)) {
            m2_formal_type_list(p);
            
        } // end formalTypeList
        
        // ")"
        if (match_token(p, TOKEN_RPAREN,
                           SKIP_TO_SEMI_OR_FOLLOW_PROCEDURE_TYPE)) {
            _getsym(p);
            
        } // end ")"
        
    } // end ( "(" formalTypeList ")" )?
    
    // ( ":" returnedType )?
    if (_lookahead(p) == TOKEN_SEMICOLON) {
        _getsym(p);
        
        // returnedType
        if (match_token(p, TOKEN_IDENTIFIER, FOLLOW_PROCEDURE_TYPE)) {
            _getsym(p);
            
        } // end returnedType
        
    } // end ( ":" returnedType )?
    
    return _lookahead(p);
} // end m2_procedure_type


// --------------------------------------------------------------------------
// #23 formal_type_list
// --------------------------------------------------------------------------
//  formalType ( "," formalType )*

m2_token_t m2_formal_type_list(m2_parser_t *p) {
    
    // formalType
    m2_formal_type(p);
    
    // ( "," formalType )*
    while (_lookahead(p) == TOKEN_COMMA) {
        _getsym(p);
        
        // formalType
        if (match_token_in_set(p, FIRST_FORMAL_TYPE, FOLLOW_FORMAL_TYPE)) {
            m2_formal_type(p);
            
        } // end formalType
        
    } // end ( "," formalType )*
    
    return _lookahead(p);
} // end m2_formal_type_list


// --------------------------------------------------------------------------
// #24 formal_type
// --------------------------------------------------------------------------
//  attributedFormalType | variadicFormalType

m2_token_t m2_formal_type(m2_parser_t *p) {
    
    // attributedFormalType | variadicFormalType
    if (m2_tokenset_is_element(p, FIRST_ATTRIBUTED_FORMAL_TYPE)) {
        m2_attributed_formal_type(p);
    }
    else if (m2_tokenset_is_element(p, FIRST_VARIADIC_FORMAL_TYPE)) {
        m2_variadic_formal_type(p);
    }
    else {
        // unreachable code
        fatal_error(); // abort
    } // end attributedFormalType | variadicFormalType
    
    return _lookahead(p);
} // end m2_formal_type


// --------------------------------------------------------------------------
// #25 attributed_formal_type
// --------------------------------------------------------------------------
//  ( CONST | VAR )? simpleFormalType

m2_token_t m2_attributed_formal_type(m2_parser_t *p) {
    
    // ( CONST | VAR )?
    if (_lookahead(p) == TOKEN_CONST) {
        _getsym(p);
        
    }
    else if (_lookahead(p) == TOKEN_VAR) {
        _getsym(p);
        
    } // end ( CONST | VAR )?
    
    // simpleFormalType
    if (match_token_in_set(p, FIRST_SIMPLE_FORMAL_TYPE,
                              FOLLOW_SIMPLE_FORMAL_TYPE)) {
        m2_simple_formal_type(p);
        
    } // end simpleFormalType
    
    return _lookahead(p);
} // end m2_attributed_formal_type


// --------------------------------------------------------------------------
// #26 simple_formal_type
// --------------------------------------------------------------------------
//  ( CAST? ARRAY OF )? namedType

m2_token_t m2_simple_formal_type(m2_parser_t *p) {
    
    // ( CAST ARRAY OF )?
    if (_lookahead(p) == TOKEN_CAST) {
        _getsym(p);
        
        // ARRAY
        if (match_token(p, TOKEN_ARRAY, SKIP_TO_OF_OR_IDENT)) {
            _getsym(p);
            
            // OF
            if (match_token(p, TOKEN_OF, SKIP_TO_IDENT)) {
                _getsym(p);
                
            } // end OF
            
        } // end ARRAY
        
    } // end 
    
    // ( ARRAY OF )?
    else if (_lookahead(p) == TOKEN_ARRAY) {
        _getsym(p);
        
        // OF
        if (match_token(p, TOKEN_OF, SKIP_TO_IDENT)) {
            _getsym(p);
            
        } // end OF
        
    } // end ( CAST? ARRAY OF )?

    // namedType
    if (match_token(p, TOKEN_IDENTIFIER, FOLLOW_SIMPLE_FORMAL_TYPE)) {
        _getsym(p);
        
    } // end namedType
    
    return _lookahead(p);
} // end m2_simple_formal_type


// --------------------------------------------------------------------------
// #27 variadic_formal_type
// --------------------------------------------------------------------------
//  VARIADIC OF
//  ( attributedFormalType |
//    "(" attributedFormalType ( "," attributedFormalType )* ")" )

m2_token_t m2_variadic_formal_type(m2_parser_t *p) {
    
    // VARIADIC
    _getsym(p);
    
    // OF
    if (match_token(p, TOKEN_OF, SKIP_TO_FIRST_ATTRIBUTED_TYPE_OR_LPAREN)) {
        _getsym(p);
        
    } // end OF
    
    // variadic formal type tail
    if (match_token_in_set(p, FIRST_ATTRIBUTED_FORMAL_TYPE_OR_LPAREN,
                              FOLLOW_VARIADIC_FORMAL_TYPE)) {
        
        if (m2_tokenset_is_element(FIRST_ATTRIBUTED_FORMAL_TYPE,
                                   _lookahead(p))) {
            m2_attributed_formal_type(p);
        }
        else if (_lookahead(p) == TOKEN_LPAREN) {
            _getsym(p);
            
            // attributedFormalType
            if (match_token_in_set(p, FIRST_ATTRIBUTED_FORMAL_TYPE,
                            FOLLOW_COMMA_OR_FOLLOW_ATTRIBUTED_FORMAL_TYPE )) {
                m2_attributed_formal_type(p);
            } // end attributedFormalType
            
            // ( "," attributedFormalType )*
            while (_lookahead(p) == TOKEN_COMMA) {
                _getsym(p);
                
                // attributedFormalType
                if (match_token_in_set(p, FIRST_ATTRIBUTED_FORMAL_TYPE,
                            FOLLOW_COMMA_OR_FOLLOW_ATTRIBUTED_FORMAL_TYPE )) {
                    m2_attributed_formal_type(p);
                } // end attributedFormalType
                
            } // end ( "," attributedFormalType )*
            
            // ")"
            if (match_token(p, TOKEN_RPAREN)) {
                _getsym(p);
                
            } // end ")"
        }
        else {
            // unreachable code
            fatal_error(); // abort
        } //
        
    } // end variadic formal type tail
    
    return _lookahead(p);
} // end m2_variadic_formal_type


// --------------------------------------------------------------------------
// #28 variable_declaration
// --------------------------------------------------------------------------
//  ident ( "[" machineAddress "]" | "," identList )?
//  ":" ( ARRAY constComponentCount OF )? namedType

m2_token_t m2_variable_declaration(m2_parser_t *p) {
    
    // ident
    _getsym(p);
    
    // first option
    if (m2_tokenset_is_element(FIRST_LBRACKET_OR_COMMA, _lookahead(p))) {
        
        // ( "[" machineAddress "]" | "," identList )?
        if (_lookahead(p) == TOKEN_LBRACKET) {
            _getsym(p);
            
            // machineAddress
            if (match_token_in_set(p, FIRST_CONST_EXPRESSION,
                                      FOLLOW_CONST_EXPRESSION)) {
                _getsym(p);
            } // machineAddress
            
            // "]"
            if (match_token(p, TOKEN_RBRACKET)) {
                _getsym(p);
                
            } // "]"
            
        }
        else if (_lookahead(p) == TOKEN_COMMA) {
            _getsym(p);
            
            // identList
            if (match_token_in_set(p, FIRST_IDENT_LIST, FOLLOW_IDENT_LIST)) {
                m2_ident_list(p);
                
            } // end identList
        }
        else {
            // unreachable code
            fatal_error(); // abort
            
        } // end ( "[" machineAddress "]" | "," identList )?

    } // end first option
    
    // ":"
    if (match_token(p, TOKEN_SEMICOLON,
                       SKIP_TO_ARRAY_OR_FOLLOW_VARIABLE_DECL)) {
        _getsym(p);
    } // end ":"
    
    // second option
    if (_lookahead(p) == TOKEN_ARRAY) {
        _getsym(p);
        
        // constComponentCount
        if (match_token_in_set(p, FIRST_CONST_EXPRESSION,
                                  SKIP_TO_OF_OR_IDENT)) {
            _getsym(p);
        } // end constComponentCount
        
        // OF
        if (match_token(p, TOKEN_OF, SKIP_TO_IDENT)) {
            _getsym(p);
            
        } // end OF
    }
    // end second option

    // namedType
    if (match_token(p, TOKEN_IDENTIFIER, FOLLOW_VARIABLE_DECLARATION)) {
        _getsym(p);
        
    } // end namedType
    
    return _lookahead(p);
} // end m2_variable_declaration


// --------------------------------------------------------------------------
// #29 procedure_declaration
// --------------------------------------------------------------------------
//  procedureHeader ";" block ident

m2_token_t m2_procedure_declaration(m2_parser_t *p) {
    
    // procedureHeader
    m2_procedure_header(p);
    
    // ";"
    if (match_token(p, TOKEN_SEMICOLON, FIRST_BLOCK)) {
        _getsym(p);
        
    } // end ";"
    
    // block
    if (match_token_in_set(p, FIRST_BLOCK, SKIP_TO_IDENT)) {
        m2_block(p);
        
    } // end block
    
    // ident
    if (match_token(p, TOKEN_IDENTIFIER, FOLLOW_PROCEDURE_DECLARATION)) {
        _getsym(p);
        
    } // end ident
    
    return _lookahead(p);
} // end m2_procedure_declaration


// --------------------------------------------------------------------------
// #30 procedure_header
// --------------------------------------------------------------------------
//  PROCEDURE ( "[" ( "::" | bindableOperator | bindableIdent ) "]" )?
//  ident ( "(" formalParamList ")" )? ( ":" returnedType )?

m2_token_t m2_procedure_header(m2_parser_t *p) {
    
    // PROCEDURE
    _getsym(p);
    
    // ( "[" ( "::" | bindableOperator | bindableIdent ) "]" )?
    if (_lookahead(p) == TOKEN_LBRACKET) {
        
        // "["
        _getsym(p);
        
        // "::" | bindableOperator | bindableIdent
        if (match_token_in_set(p, FIRST_TYPECONV_OR_BINDABLE_OP_OR_IDENT,
                                  SKIP_TO_RBRACKET)) {
            
            // "::"
            if (_lookahead(p) == TOKEN_TYPE_CONVERSION_OP) {
                _getsym(p);
                
            }
            // bindableOperator
            else if (m2_tokenset_is_element(FIRST_BINDABLE_OP,
                                            _lookahead(p))) {
                m2_bindable_operator(p);
                
            }
            // bindableIdent
            else if (_lookahead(p) == TOKEN_IDENTIFIER) {
                _getsym(p);
                
            }
            else {
                // unreachable code
                fatal_error(); // abort
            }
            
        } // end "::" | bindableOperator | bindableIdent
        
    } // end ( "[" ( "::" | bindableOperator | bindableIdent ) "]" )?
    
    // ident
    if (match_token(p, TOKEN_IDENTIFIER, SKIP_TO_LPAREN_OR_COLON)) {
        _getsym(p);
        
    } // end ident
    
    // ( "(" formalParamList ")" )?
    if (_lookahead(p) == TOKEN_LPAREN) {
        _getsym(p);
        
        // formalParamList
        if (match_token_in_set(p, FIRST_FORMAL_PARAM_LIST,
                                  FOLLOW_FORMAL_PARAM_LIST)) {
            m2_formal_param_list(p);
            
        } // end formalParamList
        
        // ")"
        if (match_token(p, TOKEN_RPAREN,
                        SKIP_TO_COLON_OR_IDENT_OR_FOLLOW_PROCEDURE_HEADER)) {
            _getsym(p);
        
        } // end ")"
        
    } // end ( "(" formalParamList ")" )?
    
    // ( ":" returnedType )?
    if (_lookahead(p) == TOKEN_COLON) {
        _getsym(p);
        
        // returnedType
        if (match_token(p, TOKEN_IDENTIFIER, FOLLOW_PROCEDURE_HEADER)) {
            _getsym(p);
            
        } end returnedType
        
    } // end ( ":" returnedType )?
    
    return _lookahead(p);
} // end m2_procedure_header


// --------------------------------------------------------------------------
// #31 formal_param_list
// --------------------------------------------------------------------------
//  formalParams ( ";" formalParams )*

m2_token_t m2_formal_param_list(m2_parser_t *p) {
    
    // formalParams
    m2_formal_params(p);
    
    // ( ";" formalParams )*
    while (_lookahead(p) == TOKEN_SEMICOLON) {
        _getsym(p);
        
        // formalParams
        if (match_token_in_set(p, FIRST_FORMAL_PARAMS,
                                  FOLLOW_FORMAL_PARAMS)) {
            m2_formal_params(p);
        } // emd formalParams
        
    } // end ( ";" formalParams )*
    
    return _lookahead(p);
} // end m2_formal_param_list


// --------------------------------------------------------------------------
// #32 formal_params
// --------------------------------------------------------------------------
//  simpleFormalParams | variadicFormalParams

m2_token_t m2_formal_params(m2_parser_t *p) {
    
    // simpleFormalParams | variadicFormalParams
    if (m2_tokenset_is_element(p, FIRST_SIMPLE_FORMAL_PARAMS)) {
        m2_simple_formal_params(p);
    }
    else if (m2_tokenset_is_element(p, FIRST_VARIADIC_FORMAL_PARAMS)) {
        m2_variadic_formal_params(p);
    }
    else {
        // unreachable code
        fatal_error(); // abort
    } // end simpleFormalParams | variadicFormalParams
    
    return _lookahead(p);
} // end m2_formal_params


// --------------------------------------------------------------------------
// #33 simple_formal_params
// --------------------------------------------------------------------------
//  ( CONST | VAR )? identList ":" simpleFormalType

m2_token_t m2_simple_formal_params(m2_parser_t *p) {
    
    // ( CONST | VAR )?
    if (_lookahead(p) == TOKEN_CONST) {
        _getsym(p);
        
    }
    else if (_lookahead(p) == TOKEN_VAR) {
        _getsym(p);
        
    } // end ( CONST | VAR )?
    
    // identList
    if (match_token_in_set(p, FIRST_IDENT_LIST, FOLLOW_IDENT_LIST)) {
        m2_ident_list(p);
        
    } // end identList
    
    // ":"
    if (match_token(p, TOKEN_SEMICOLON, FIRST_SIMPLE_FORMAL_TYPE)) {
        _getsym(p);
        
    } // end ":"
    
    // simpleFormalType
    if (match_token_in_set(p, FIRST_SIMPLE_FORMAL_TYPE,
                           FOLLOW_SIMPLE_FORMAL_TYPE)) {
        m2_simple_formal_type(p);
        
    } // end simpleFormalType
    
    return _lookahead(p);
} // end m2_simple_formal_params


// --------------------------------------------------------------------------
// #34 variadic_formal_params
// --------------------------------------------------------------------------
//  VARIADIC ( variadicCounter | "[" variadicTerminator "]" )? OF
//  ( simpleFormalType |
//    "(" simpleFormalParams ( ";" simpleFormalParams )* ")" )

m2_token_t m2_variadic_formal_params(m2_parser_t *p) {
    
    // VARIADIC
    _getsym(p);
    
    // ( variadicCounter | "[" variadicTerminator "]" )?
    if (_lookahead(p) == TOKEN_IDENTIFIER) {
        _getsym(p);
        
    }
    else if (_lookahead(p) == TOKEN_LBRACKET) {
        _getsym(p);
        
        // variadicTerminator
        if (match_token_in_set(p, FIRST_CONST_EXPRESSION,
                               SKIP_TO_RBRACKET_OR_OF)) {
            m2_const_expression(p);
            
        } // end variadicTerminator
        
        // "]"
        if (match_token(p, TOKEN_RBRACKET, SKIP_TO_OF)) {
            _getsym(p);
            
        } // end "]"
        
    } // end ( variadicCounter | "[" variadicTerminator "]" )?
    
    // OF
    if (match_token(p, TOKEN_OF, FIRST_SIMPLE_FORMAL_TYPE_OR_LPAREN)) {
        _getsym(p);
        
    } // end OF
    
    // tail
    if (match_token_in_set(p, FIRST_SIMPLE_FORMAL_TYPE_OR_LPAREN,
                              FOLLOW_SIMPLE_FORMAL_TYPE)) {
        
        // simpleFormalType
        if (m2_tokenset_is_element(FIRST_SIMPLE_FORMAL_TYPE, _lookahead(p))) {
            m2_simple_formal_type(p);
            
        }
        // "("
        else if (_lookahead(p) == TOKEN_LPAREN) {
            _getsym(p);
            
            // simpleFormalParams
            if (match_token_in_set(p, FIRST_SIMPLE_FORMAL_PARAMS,
                                      FOLLOW_SIMPLE_FORMAL_PARAMS)) {
                m2_simple_formal_params(p);
            } // end simpleFormalParams
            
            // ( ";" simpleFormalParams )*
            if (_lookahead(p) == TOKEN_SEMICOLON) {
                _getsym(p);
                
                // simpleFormalParams
                if (match_token_in_set(p, FIRST_SIMPLE_FORMAL_PARAMS,
                                       FOLLOW_SIMPLE_FORMAL_PARAMS)) {
                    m2_simple_formal_params(p);
                } // end simpleFormalParams
                
            } // end ( ";" simpleFormalParams )*
            
            // ")"
            if (match_token(p, TOKEN_RPAREN, FOLLOW_VARIADIC_FORMAL_PARAMS)) {
                _getsym(p);
                
            } // end ")"
        }
        else {
            // unreachable code
            fatal_error(); // abort
            
        } // end if
        
    } // end tail
    
    return _lookahead(p);
} // end m2_variadic_formal_params


// --------------------------------------------------------------------------
// #35 statement
// --------------------------------------------------------------------------
//  assignmentOrProcedureCall | ifStatement | caseStatement |
//  whileStatement | repeatStatement | loopStatement |
//  forStatement | RETURN expression? | EXIT 

m2_token_t m2_statement(m2_parser_t *p) {
    
    // assignmentOrProcedureCall
    if (m2_tokenset_is_element(FIRST_ASSIGNMENT_OR_PROCEDURE_CALL,
                               _lookahead(p))) {
        m2_assignment_or_procedure_call(p);
    }
    // ifStatement
    else if (_lookahead(p) == TOKEN_IF) {
        m2_if_statement(p);
    }
    // caseStatement
    else if (_lookahead(p) == TOKEN_CASE) {
        m2_case_statement(p);
    }
    // whileStatement
    else if (_lookahead(p) == TOKEN_WHILE) {
        m2_while_statement(p);
    }
    // repeatStatement
    else if (_lookahead(p) == TOKEN_REPEAT) {
        m2_repeat_statement(p);
    }
    // loopStatement
    else if (_lookahead(p) == TOKEN_LOOP) {
        m2_loop_statement(p);
    }
    // forStatement
    else if (_lookahead(p) == TOKEN_FOR) {
        m2_for_statement(p);
    }
    // RETURN
    else if (_lookahead(p) == TOKEN_RETURN) {
        _getsym(p);
        
        // expression
        if (match_token_in_set(p, FIRST_EXPRESSION, FOLLOW_STATEMENT)) {
            m2_expression(p);
            
        } // end expression
    }
    // EXIT
    else if (_lookahead(p) == TOKEN_EXIT) {
        _getsym(p);
        
    }
    else {
        // unreachable code
        fatal_error(); // abort
        
    } // end if
    
    return _lookahead(p);
} // end m2_statement


// --------------------------------------------------------------------------
// #36 statement_sequence
// --------------------------------------------------------------------------
//  statement ( ";" statement )*

m2_token_t m2_statement_sequence(m2_parser_t *p) {
    
    // statement
    m2_statement(p);
    
    // ( ";" statement )*
    while (_lookahead(p) == TOKEN_SEMICOLON) {
        _getsym(p);
        
        // statement
        if (match_token_in_set(p, FIRST_STATEMENT, FOLLOW_STATEMENT)) {
            m2_statement(p);
            
        } // end statement
        
    } // end ( ";" statement )*
    
    return _lookahead(p);
} // end m2_statement_sequence


// --------------------------------------------------------------------------
// #37 assignment_or_procedure_call
// --------------------------------------------------------------------------
//  designator ( ":=" expression | "++" | "--" | actualParameters )?

m2_token_t m2_assignment_or_procedure_call(m2_parser_t *p) {
    
    // designator
    m2_designator(p);
    
    // ( ":=" expression | "++" | "--" | actualParameters )?
    if (m2_tokenset_is_element(ASSIGN_OR_INC_OR_DEC_OR_LPAREN,
                               _lookahead(p))) {
        
        // ":="
        if (_lookahead(p) == TOKEN_ASSIGN_OP) {
            _getsym(p);
            
            // expression
            if (match_token_in_set(p, FIRST_EXPRESSION,
                                      FOLLOW_ASSIGNMENT_OR_PROCEDURE_CALL)) {
                m2_expression(p);
                
            } // end expression
            
        }
        // "++"
        else if (_lookahead(p) == TOKEN_DECREMENT_OP) {
            _getsym(p);
            
        }
        // "--"
        else if (_lookahead(p) == TOKEN_INCREMENT_OP) {
            _getsym(p);
            
        }
        // actualParameters
        else if (m2_tokenset_is_element(FIRST_ACTUAL_PARAMETERS,
                                        _lookahead(p))) {
            m2_actual_params(p);
            
        }
        else {
            // unreachable code
            fatal_error(); // abort
        } // end if
        
    } // end ( ":=" expression | "++" | "--" | actualParameters )?
    
    return _lookahead(p);
} // end m2_assignment_or_procedure_call


// --------------------------------------------------------------------------
// #38 if_statement
// --------------------------------------------------------------------------
//  IF expression THEN statementSequence
//  ( ELSIF expression THEN statementSequence )*
//  ( ELSE statementSequence )?
//  END

m2_token_t m2_if_statement(m2_parser_t *p) {
    
    // IF
    _getsym(p);
    
    // expression
    if (match_token_in_set(p, FIRST_EXPRESSION, FOLLOW_EXPRESSION)) {
        m2_expression(p);
        
    } // end expression
    
    // THEN
    if (match_token(p, TOKEN_THEN, FIRST_STATEMENT_SEQ)) {
        _getsym(p);
        
    } // end THEN
    
    // statementSequence
    if (match_token_in_set(p, FIRST_STATEMENT_SEQ, FOLLOW_STATEMENT_SEQ)) {
        m2_statement_sequence(p);
        
    } // end statementSequence
    
    // ( ELSIF expression THEN statementSequence )*
    while (_lookahead(p) == TOKEN_ELSIF) {
        _getsym(p);
        
        // expression
        if (match_token_in_set(p, FIRST_EXPRESSION, FOLLOW_EXPRESSION)) {
            m2_expression(p);
            
        } // end expression
        
        // THEN
        if (match_token(p, TOKEN_THEN, FIRST_STATEMENT_SEQ)) {
            _getsym(p);
            
        } // end THEN
        
        // statementSequence
        if (match_token_in_set(p, FIRST_STATEMENT_SEQ, FOLLOW_STATEMENT_SEQ)) {
            m2_statement_sequence(p);
            
        } // end statementSequence
        
    } // end ( ELSIF expression THEN statementSequence )*
    
    // ( ELSE statementSequence )?
    if (_lookahead(p) == TOKEN_ELSE) {
        _getsym(p);
        
        // statementSequence
        if (match_token_in_set(p, FIRST_STATEMENT_SEQ, FOLLOW_STATEMENT_SEQ)) {
            m2_statement_sequence(p);
            
        } // end statementSequence
        
    } // end ( ELSE statementSequence )?
    
    // END
    if (match_token(p, TOKEN_END, FOLLOW_IF_STATEMENT)) {
        _getsym(p);
        
    } // end END
    
    return _lookahead(p);
} // end m2_if_statement


// --------------------------------------------------------------------------
// #39 case_statement
// --------------------------------------------------------------------------
//  CASE expression OF case ( "|" case )* ( ELSE statementSequence )? END

m2_token_t m2_case_statement(m2_parser_t *p) {
    
    // CASE
    _getsym(p);
    
    // expression
    if (match_token_in_set(p, FIRST_EXPRESSION, FOLLOW_EXPRESSION)) {
        m2_expression(p);
        
    } // end expression
    
    // OF
    if (match_token(p, TOKEN_OF, FIRST_CASE)) {
        _getsym(p);
        
    } // end OF
    
    // case
    if (match_token_in_set(p, FIRST_CASE, FOLLOW_CASE)) {
        m2_case(p);
        
    } // end case
    
    // ( "|" case )*
    while (_lookahead(p) == TOKEN_CASE_LABEL_SEPARATOR) {
        getsym(p);
        
        // case
        if (match_token_in_set(p, FIRST_CASE, FOLLOW_CASE)) {
            m2_case(p);
            
        } // end case
        
    } // end ( "|" case )*
    
    // ( ELSE statementSequence )?
    if (_lookahead(p) == TOKEN_ELSE) {
        _getsym(p);
        
        // statementSequence
        if (match_token_in_set(p, FIRST_STATEMENT_SEQ, FOLLOW_STATEMENT_SEQ)) {
            m2_statement_sequence(p);
            
        } // end statementSequence
        
    } // end ( ELSE statementSequence )?
    
    // END
    if (match_token(p, TOKEN_END, FOLLOW_CASE_STATEMENT)) {
        _getsym(p);
        
    } // end END
    
    return _lookahead(p);
} // end m2_case_statement


// --------------------------------------------------------------------------
// #40 case
// --------------------------------------------------------------------------
//  caseLabels ( "," caseLabels )* ":" statementSequence

m2_token_t m2_case(m2_parser_t *p) {
    
    // caseLabels
    m2_case_labels(p);
    
    // ( "," caseLabels )*
    while (_lookahead(p) == TOKEN_COMMA) {
        -getsym(p);
        
        // caseLabels
        if (match_token_in_set(p, FIRST_CASE_LABELS, FOLLOW_CASE_LABELS)) {
            m2_case_labels(p);
            
        } // end caseLabels
        
    } // end ( "," caseLabels )*
    
    return _lookahead(p);
} // end m2_case


// --------------------------------------------------------------------------
// #41 case_labels
// --------------------------------------------------------------------------
//  constExpression ( ".." constExpression )?

m2_token_t m2_case_labels(m2_parser_t *p) {
    
    // constExpression
    m2_const_expression(p);
    
    // ( ".." constExpression )?
    if (_lookahead(p) == TOKEN_RANGE_OP) {
        _getsym(p);
        
        // constExpression
        if (match_token_in_set(p, FIRST_CONST_EXPRESSION,
                               FOLLOW_CONST_EXPRESSION)) {
            m2_const_expression(p);
            
        } // end constExpression
        
    } // end ( ".." constExpression )?
    
    return _lookahead(p);
} // end m2_case_labels


// --------------------------------------------------------------------------
// #42 while_statement
// --------------------------------------------------------------------------
//  WHILE expression DO statementSequence END

m2_token_t m2_while_statement(m2_parser_t *p) {
    
    // WHILE
    _getsym(p);
    
    // expression
    if (match_token_in_set(p, FIRST_EXPRESSION, FOLLOW_EXPRESSION)) {
        m2_expression(p);
        
    } // end expression
    
    // DO
    if (match_token(p, TOKEN_DO, FIRST_STATEMENT_SEQ)) {
        _getsym(p);
        
    } // end DO
    
    // statementSequence
    if (match_token_in_set(p, FIRST_STATEMENT_SEQ, FOLLOW_STATEMENT_SEQ)) {
        m2_statement_sequence(p);
        
    } // end statementSequence
    
    // END
    if (match_token(p, TOKEN_END, FOLLOW_WHILE_STATEMENT)) {
        _getsym(p);
        
    } // end OF
    
    return _lookahead(p);
} // end m2_while_statement


// --------------------------------------------------------------------------
// #43 repeat_statement
// --------------------------------------------------------------------------
//  REPEAT statementSequence UNTIL expression

m2_token_t m2_repeat_statement(m2_parser_t *p) {
    
    // REPEAT
    _getsym(p);
    
    // statementSequence
    if (match_token_in_set(p, FIRST_STATEMENT_SEQ, FOLLOW_STATEMENT_SEQ)) {
        m2_statement_sequence(p);
        
    } // end statementSequence
    
    // UNTIL
    if (match_token(p, TOKEN_UNTIL, FIRST_EXPRESSION)) {
        _getsym(p);
        
    } // end OF
    
    // expression
    if (match_token_in_set(p, FIRST_EXPRESSION, FOLLOW_REPEAT_STATEMENT)) {
        m2_expression(p);
        
    } // end expression
    
    return _lookahead(p);
} // end m2_repeat_statement


// --------------------------------------------------------------------------
// #44 loop_statement
// --------------------------------------------------------------------------
//  LOOP statementSequence END

m2_token_t m2_loop_statement(m2_parser_t *p) {
    
    // LOOP
    _getsym(p);
    
    // statementSequence
    if (match_token_in_set(p, FIRST_STATEMENT_SEQ, FOLLOW_STATEMENT_SEQ)) {
        m2_statement_sequence(p);
        
    } // end statementSequence
    
    // END
    if (match_token(p, TOKEN_END, FOLLOW_LOOP_STATEMENT)) {
        _getsym(p);
        
    } // end END

    return _lookahead(p);
} // end m2_loop_statement


// --------------------------------------------------------------------------
// #45 for_statement
// --------------------------------------------------------------------------
//  FOR controlVariable DESCENDING? IN ( expression | range OF namedType )

m2_token_t m2_for_statement(m2_parser_t *p) {
    
    // FOR
    _getsym(p);
    
    // DESCENDING?
    if (_lookahead(p) == TOKEN_DESCENDING) {
        _getsym(p);
        
    } // end DESCENDING?
    
    // IN
    if (match_token(p, TOKEN_IN, FIRST_EXPRESSION_OR_RANGE)) {
        _getsym(p);
        
    } // end IN
    
    // ( expression | range OF namedType )
    if (match_token_in_set(p, FIRST_EXPRESSION_OR_RANGE,
                              FOLLOW_FOR_STATEMENT)) {
        
        // expression
        if (m2_tokenset_is_element(FIRST_EXPRESSION, _lookahead(p))) {
            m2_expression(p);
        }
        // range
        else if (m2_tokenset_is_element(FIRST_RANGE, _lookahead(p))) {
            m2_range(p);
            
            // OF
            if (match_token(p, TOKEN_OF, SKIP_TO_IDENT)) {
                _getsym(p);
                
            } // end OF
            
            // namedType
            if (match_token(p, TOKEN_IDENTIFIER, FOLLOW_FOR_STATEMENT)) {
                _getsym(p);
                
            } // end namedType
        }
        else {
            // unreachable code
            fatal_error(p); // abort
            
        } // end if
        
    } // end
    
    return _lookahead(p);
} // end m2_for_statement


// --------------------------------------------------------------------------
// #46 const_expression
// --------------------------------------------------------------------------
//  simpleConstExpr ( relation simpleConstExpr )?

m2_token_t m2_const_expression(m2_parser_t *p) {
    
    // simpleConstExpr
    m2_simple_const_expression(p);
    
    // ( relation simpleConstExpr )?
    if (m2_tokenset_is_element(FIRST_RELATION, _lookahead(p))) {
        m2_relation(p);
        
        // simpleConstExpr
        if (match_token_in_set(p, FIRST_SIMPLE_CONST_EXPRESSION,
                                  FOLLOW_SIMPLE_CONST_EXPRESSION)) {
            m2_simple_const_expression(p);
            
        } // end simpleConstExpr
        
    } // end ( relation simpleConstExpr )?
    
    return _lookahead(p);
} // end m2_const_expression


// --------------------------------------------------------------------------
// #47 relation
// --------------------------------------------------------------------------
//  "=" | "#" | "<" | "<=" | ">" | ">=" | "IN"

m2_token_t m2_relation(m2_parser_t *p) {
    
    switch (_lookahead(p)) {
        
        // "="
        case TOKEN_EQUAL_OP :
            _getsym(p);
            
            break;
            
        // "#"
        case TOKEN_NOT_EQUAL_OP :
            _getsym(p);
            
            break;
            
        // "<"
        case TOKEN_LESS_OP :
            _getsym(p);
            
            break;
            
        // "<="
        case TOKEN_LESS_OR_EQUAL_OP :
            _getsym(p);
            
            break;
        
        // ">"
        case TOKEN_GREATER_OP :
            _getsym(p);
            
            break;

        // ">="
        case TOKEN_GREATER_OR_EQUAL_OP :
            _getsym(p);
            
            break;
        
        // "IN"
        case TOKEN_IN :
            _getsym(p);
            
            break;
        
        default : // unreachable code
            fatal_error(); // abort
        
    } // end switch
    
    return _lookahead(p);
} // end m2_relation


// --------------------------------------------------------------------------
// #48 simple_const_expression
// --------------------------------------------------------------------------
//  ( "+" | "-" )? constTerm ( addOperator constTerm )*

m2_token_t m2_simple_const_expression(m2_parser_t *p) {
    
    // ( "+" | "-" )?
    if (_lookahead(p) == TOKEN_PLUS_OP) {
        _getsym(p);
        
    }
    else if (_lookahead(p) == TOKEN_MINUS_OP) {
        _getsym(p);
        
    } // end ( "+" | "-" )?
    
    // constTerm
    if (match_token_in_set(p, FIRST_CONST_TERM, FOLLOW_CONST_TERM)) {
        m2_const_term(p);
        
    } // end constTerm
    
    // ( addOperator constTerm )*
    while (m2_tokenset_is_element(FIRST_ADD_OPERATOR, _lookahead(p))) {
        m2_add_operator(p);
        
        // constTerm
        if (match_token_in_set(p, FIRST_CONST_TERM, FOLLOW_CONST_TERM)) {
            m2_const_term(p);
            
        } // end constTerm
        
    } // end ( addOperator constTerm )*
    
    return _lookahead(p);
} // end m2_simple_const_expression


// --------------------------------------------------------------------------
// #49 add_operator
// --------------------------------------------------------------------------
//  "+" | "-" | OR

m2_token_t m2_add_operator(m2_parser_t *p) {
    
    switch (_lookahead(p)) {
        
        // "+"
        case TOKEN_PLUS_OP :
            _getsym(p);
            
            break;
            
        // "-"
        case TOKEN_MINUS_OP :
            _getsym(p);
            
            break;
            
        // OR
        case TOKEN_OR :
            _getsym(p);
            
            break;
            
        default : // unreachable code
            fatal_error(); // abort
            
    } // end switch
    
    return _lookahead(p);
} // end m2_add_operator


// --------------------------------------------------------------------------
// #50 const_term
// --------------------------------------------------------------------------
//  constFactor ( mulOperator constFactor )*

m2_token_t m2_const_term(m2_parser_t *p) {
    
    // constFactor
    m2_const_factor(p);
    
    // ( mulOperator constFactor )*
    while (m2_tokenset_is_element(FIRST_MUL_OPERATOR, _lookahead(p))) {
        m2_mul_operator(p);
        
        // constFactor
        if (match_token_in_set(p, FIRST_CONST_FACTOR, FOLLOW_CONST_FACTOR)) {
            m2_const_factor(p);
            
        } // end constFactor
        
    } // end ( mulOperator constFactor )*
    
    return _lookahead(p);
} // end m2_const_term


// --------------------------------------------------------------------------
// #51 mul_operator
// --------------------------------------------------------------------------
//  "*" | "/" | DIV | MOD | AND

m2_token_t m2_mul_operator(m2_parser_t *p) {
    
    switch (_lookahead(p)) {
            
            // "*"
        case TOKEN_ASTERISK_OP :
            _getsym(p);
            
            break;
            
            // "/"
        case TOKEN_SLASH_OP :
            _getsym(p);
            
            break;
            
            // DIV
        case TOKEN_DIV :
            _getsym(p);
            
            break;
            
        // MOD
        case TOKEN_MOD :
            _getsym(p);
            
            break;
            
        // AND
        case TOKEN_AND :
            _getsym(p);
            
            break;
            
        default : // unreachable code
            fatal_error(); // abort
            
    } // end switch

    return _lookahead(p);
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
