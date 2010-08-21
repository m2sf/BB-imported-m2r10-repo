/*! Modula-2 R10 Compiler (m2r10c)
 *
 *  @file  m2_parser.c
 *  @brief Parser implementation
 *
 *  @b Author: Benjamin Kowarsch
 *
 *  @b Copyright: (C) 2010 B.Kowarsch. All rights reserved.
 *
 *  @b License:
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


#include "alloc.h"

#include "m2_lexer.h"
#include "m2_tokens.h"
#include "m2_tokenset.h"
#include "m2_ast.h"
#include "m2_symbol_table.h"
#include "m2_filenames.h"
#include "m2_parser.h"


// ---------------------------------------------------------------------------
// Symbol type
// ---------------------------------------------------------------------------

typedef struct /* m2_sym_s */ {
    m2_token_t token;
    cardinal lexeme;
    m2_lexer_status_t status;
    file_pos_t pos;
} m2_sym_s;


// ---------------------------------------------------------------------------
// Zero symbol
// ---------------------------------------------------------------------------

#define ZERO_SYMBOL (m2_sym_s) { 0, 0, 0, { 0, 0 } }


// ---------------------------------------------------------------------------
// Parser state
// ---------------------------------------------------------------------------

typedef struct /* m2_parser_s */ {
    m2_file_t source_file;
    m2_file_type_t source_type;
    m2_lexer_t lexer;
    m2_sym_s current_sym;
    m2_sym_s lookahead_sym;
    kvs_table_t lextab;
    m2_symtab_t symtab;
    m2_ast_node_t ast;
    m2_notification_f handler;
    uint16_t warnings;
    uint16_t errors;
} m2_parser_s;


// ---------------------------------------------------------------------------
// FIRST sets
// ---------------------------------------------------------------------------

static m2_tokenset_t
    FIRST_COMPILATION_UNIT,
    FIRST_PROTOTYPE,
    FIRST_PROGRAM_MODULE,
    FIRST_DEFINITION_OF_MODULE,
    FIRST_IMPLEMENTATION_OF_MODULE,
    FIRST_REQUIRED_BINDING,
    FIRST_BINDABLE_OP,
    FIRST_IMPORT_LIST,
    FIRST_BLOCK,
    FIRST_DECLARATION,
    FIRST_DEFINITION,
    FIRST_CONST_DEFINITION_TAIL,
    FIRST_TYPE_DEFINITION_TAIL,
    FIRST_CONST_DECLARATION,
    FIRST_TYPE,
    FIRST_RANGE,
    FIRST_ENUMERATION_TYPE,
    FIRST_ENUMERATION_COMPONENT,
    FIRST_ARRAY_TYPE,
    FIRST_RECORD_TYPE,
    FIRST_FIELD_LIST_SEQ,
    FIRST_FIELD_LIST,
    FIRST_SET_TYPE,
    FIRST_POINTER_TYPE,
    FIRST_PROCEDURE_TYPE,
    FIRST_FORMAL_TYPE_LIST,
    FIRST_FORMAL_TYPE,
    FIRST_SIMPLE_FORMAL_TYPE,
    FIRST_ATTRIBUTED_FORMAL_TYPE,
    FIRST_CONST_VAR_PREFIXED_FORMAL_TYPE,
    FIRST_VARIADIC_PREFIXED_FORMAL_TYPE,
    FIRST_NON_VARIADIC_FORMAL_TYPE_LIST,
    FIRST_NON_VARIADIC_FORMAL_TYPE,
    FIRST_VARIABLE_DECLARATION,
    FIRST_PROCEDURE_DECLARATION,
    FIRST_PROCEDURE_HEADER,
    FIRST_FORMAL_PARAM_LIST,
    FIRST_FORMAL_PARAMS,
    FIRST_CONST_VAR_PREFIXED_FORMAL_PARAMS,
    FIRST_NON_PREFIXED_FORMAL_PARAMS,
    FIRST_NON_PREFIXED_FORMAL_PARAMS_TAIL,
    FIRST_VARIADIC_FORMAL_PARAMS_TAIL,
    FIRST_VARIADIC_ATTRIBUTE,
    FIRST_NON_VARIADIC_FORMAL_PARAMS_TAIL,
    FIRST_NON_VARIADIC_FORMAL_PARAM_LIST,
    FIRST_NON_VARIADIC_FORMAL_PARAMS,
    FIRST_STATEMENT,
    FIRST_STATEMENT_SEQ,
    FIRST_ASSIGNMENT_OR_PROCEDURE_CALL,
    FIRST_IF_STATEMENT,
    FIRST_CASE_STATEMENT,
    FIRST_CASE,
    FIRST_CASE_LABELS,
    FIRST_WHILE_STATEMENT,
    FIRST_REPEAT_STATEMENT,
    FIRST_LOOP_STATEMENT,
    FIRST_FOR_STATEMENT,
    FIRST_CONST_EXPRESSION,
    FIRST_BOOL_CONST_TERM,
    FIRST_BOOL_CONST_FACTOR,
    FIRST_REL_CONST_EXPRESSION,
    FIRST_RELATION,
    FIRST_SIMPLE_CONST_EXPRESSION,
    FIRST_ADD_OPERATOR,
    FIRST_CONST_TERM,
    FIRST_MUL_OPERATOR,
    FIRST_CONST_FACTOR,
    FIRST_DESIGNATOR,
    FIRST_DESIGNATOR_TAIL,
    FIRST_EXPRESSION_LIST,
    FIRST_EXPRESSION,
    FIRST_BOOL_TERM,
    FIRST_BOOL_FACTOR,
    FIRST_REL_EXPRESSION,
    FIRST_SIMPLE_EXPRESSION,
    FIRST_TERM,
    FIRST_FACTOR,
    FIRST_DESIGNATOR_OR_PROCEDURE_CALL,
    FIRST_ACTUAL_PARAMETERS,
    FIRST_CONST_STRUCTURED_VALUE,
    FIRST_CONST_VALUE_COMPONENT,
    FIRST_STRUCTURED_VALUE,
    FIRST_VALUE_COMPONENT,
    FIRST_QUALIDENT,
    FIRST_IDENT_LIST,
    // intra-production first sets
    FIRST_RECORD_OR_OPAQUE,
    FIRST_BINDABLE_OP_OR_IDENT,
    FIRST_IDENT_OR_ASTERISK,
    FIRST_TYPE_OR_OPAQUE,
    FIRST_LBRACKET_OR_COMMA,
    FIRST_TYPECONV_OR_BINDABLE_OP_OR_IDENT,
    ASSIGN_OR_INC_OR_DEC_OR_LPAREN,
    FIRST_EXPRESSION_OR_RANGE,
    ;

// ---------------------------------------------------------------------------
// FOLLOW sets
// ---------------------------------------------------------------------------

static m2_tokenset_t
    FOLLOW_COMPILATION_UNIT,
    FOLLOW_PROTOTYPE,
    FOLLOW_PROGRAM_MODULE,
    FOLLOW_DEFINITION_OF_MODULE,
    FOLLOW_IMPLEMENTATION_OF_MODULE,
    FOLLOW_REQUIRED_BINDING,
    FOLLOW_BINDABLE_OP,
    FOLLOW_IMPORT_LIST,
    FOLLOW_BLOCK,
    FOLLOW_DECLARATION,
    FOLLOW_DEFINITION,
    FOLLOW_CONST_DEFINITION_TAIL,
    FOLLOW_TYPE_DEFINITION_TAIL,
    FOLLOW_CONST_DECLARATION,
    FOLLOW_TYPE,
    FOLLOW_RANGE,
    FOLLOW_ENUMERATION_TYPE,
    FOLLOW_ENUMERATION_COMPONENT,
    FOLLOW_ARRAY_TYPE,
    FOLLOW_RECORD_TYPE,
    FOLLOW_FIELD_LIST_SEQ,
    FOLLOW_FIELD_LIST,
    FOLLOW_SET_TYPE,
    FOLLOW_POINTER_TYPE,
    FOLLOW_PROCEDURE_TYPE,
    FOLLOW_FORMAL_TYPE_LIST,
    FOLLOW_FORMAL_TYPE,
    FOLLOW_SIMPLE_FORMAL_TYPE,
    FOLLOW_ATTRIBUTED_FORMAL_TYPE,
    FOLLOW_CONST_VAR_PREFIXED_FORMAL_TYPE,
    FOLLOW_VARIADIC_PREFIXED_FORMAL_TYPE,
    FOLLOW_NON_VARIADIC_FORMAL_TYPE_LIST,
    FOLLOW_NON_VARIADIC_FORMAL_TYPE,
    FOLLOW_VARIABLE_DECLARATION,
    FOLLOW_PROCEDURE_DECLARATION,
    FOLLOW_PROCEDURE_HEADER,
    FOLLOW_FORMAL_PARAM_LIST,
    FOLLOW_FORMAL_PARAMS,
    FOLLOW_CONST_VAR_PREFIXED_FORMAL_PARAMS,
    FOLLOW_NON_PREFIXED_FORMAL_PARAMS,
    FOLLOW_NON_PREFIXED_FORMAL_PARAMS_TAIL,
    FOLLOW_VARIADIC_FORMAL_PARAMS_TAIL,
    FOLLOW_VARIADIC_ATTRIBUTE,
    FOLLOW_NON_VARIADIC_FORMAL_PARAMS_TAIL,
    FOLLOW_NON_VARIADIC_FORMAL_PARAM_LIST,
    FOLLOW_NON_VARIADIC_FORMAL_PARAMS,
    FOLLOW_STATEMENT,
    FOLLOW_STATEMENT_SEQ,
    FOLLOW_ASSIGNMENT_OR_PROCEDURE_CALL,
    FOLLOW_IF_STATEMENT,
    FOLLOW_CASE_STATEMENT,
    FOLLOW_CASE,
    FOLLOW_CASE_LABELS,
    FOLLOW_WHILE_STATEMENT,
    FOLLOW_REPEAT_STATEMENT,
    FOLLOW_LOOP_STATEMENT,
    FOLLOW_FOR_STATEMENT,
    FOLLOW_CONST_EXPRESSION,
    FOLLOW_BOOL_CONST_TERM,
    FOLLOW_BOOL_CONST_FACTOR,
    FOLLOW_REL_CONST_EXPRESSION,
    FOLLOW_RELATION,
    FOLLOW_SIMPLE_CONST_EXPRESSION,
    FOLLOW_ADD_OPERATOR,
    FOLLOW_CONST_TERM,
    FOLLOW_MUL_OPERATOR,
    FOLLOW_CONST_FACTOR,
    FOLLOW_DESIGNATOR,
    FOLLOW_DESIGNATOR_TAIL,
    FOLLOW_EXPRESSION_LIST,
    FOLLOW_EXPRESSION,
    FOLLOW_BOOL_TERM,
    FOLLOW_BOOL_FACTOR,
    FOLLOW_REL_EXPRESSION,
    FOLLOW_SIMPLE_EXPRESSION,
    FOLLOW_TERM,
    FOLLOW_FACTOR,
    FOLLOW_DESIGNATOR_OR_PROCEDURE_CALL,
    FOLLOW_ACTUAL_PARAMETERS,
    FOLLOW_CONST_STRUCTURED_VALUE,
    FOLLOW_CONST_VALUE_COMPONENT,
    FOLLOW_STRUCTURED_VALUE,
    FOLLOW_VALUE_COMPONENT,
    FOLLOW_QUALIDENT,
    FOLLOW_IDENT_LIST
    ;

// ---------------------------------------------------------------------------
// Skip sets
// ---------------------------------------------------------------------------

// TO DO : initialisers

static m2_tokenset_t
    SKIP_TO_TYPE_OR_REQ_BINDING,
    SKIP_TO_REQ_BINDING,
    SKIP_TO_DOT_OR_EOF,
    SKIP_TO_EOF,
    SKIP_TO_IMPORT_OR_BLOCK,
    SKIP_TO_SEMI_OR_IMPORT_OR_BLOCK,
    SKIP_TO_IDENT_OR_DOT_OR_EOF,
    SKIP_TO_DOT_OR_EOF,
    SKIP_TO_SEMI_OR_IMPORT_OR_DEFN_OR_END,
    SKIP_TO_RBRACKET,
    SKIP_TO_IMPORT_OR_DEFN_OR_END,
    SKIP_TO_IDENT,
    SKIP_TO_RBRACKET_OR_CONST_OR_PROC,
    SKIP_TO_CONST_OR_PROC,
    SKIP_TO_IMPORT,
    SKIP_TO_SEMI_OR_IMPORT_OR_BLOCK,
    SKIP_TO_COMMA_OR_SEMI_OR_IMPORT_OR_BLOCK,
    SKIP_TO_END,
    SKIP_TO_SEMICOLON,
    SKIP_TO_TYPE,
    SKIP_TO_EQUAL,
    SKIP_TO_IDENT,
    SKIP_TO_RANGE_OP,
    SKIP_TO_COMMA_OR_RPAREN,
    SKIP_TO_COMMA_OR_OF,
    SKIP_TO_OF_OR_IDENT,
    SKIP_TO_RPAREN,
    SKIP_TO_COMMA_OR_IDENT,
    SKIP_TO_LPAREN_OR_IDENT,
    SKIP_TO_CONST_OR_IDENT,
    SKIP_TO_SEMI_OR_FOLLOW_PROCEDURE_TYPE,
    SKIP_TO_COLON,
    SKIP_TO_ARRAY_OR_FOLLOW_VARIABLE_DECL,
    SKIP_TO_LPAREN_OR_COLON,
    SKIP_TO_COLON_OR_IDENT_OR_FOLLOW_PROCEDURE_HEADER,
    SKIP_TO_RBRACKET_OR_OF,
    SKIP_TO_OF,
    SKIP_TO_COMMA,
    SKIP_TO_RBRACKET,
    ;

// ===========================================================================
// P U B L I C   F U N C T I O N S
// ===========================================================================

// ---------------------------------------------------------------------------
// function:  m2_new_parser(infile, lextab, symtab, ast, handler, status)
// ---------------------------------------------------------------------------
//
// Creates  and returns  a  new  parser object  associated  with  source file 
// <infile> and lexeme table <lextab>.  The status of the operation is passed
// back in <status> unless NULL is passed in for <status>.
//
// Returns NULL if the parser object could not be created.

m2_parser_t m2_new_parser(m2_file_t infile,
                        kvs_table_t lextab,
                        m2_symtab_t symtab,
                      m2_ast_node_t ast,
                  m2_notification_f handler,
                 m2_parser_status_t *status) {
    m2_parser_s *p;
    m2_file_type_t file_type;
    m2_lexer_t new_lexer;
    m2_lexer_status_t lexer_status;
    
    // bail out if infile is NULL
    if (infile == NULL) {
        ASSIGN_BY_REF(status, M2_PARSER_STATUS_INVALID_FILE_REFERENCE);
        return NULL;
    } // end if
    
    // obtain file type of infile
    file_type = m2_fileio_file_type(infile);
    
    // bail out if file type does not represent a source file
    if ((file_type != FILE_TYPE_DEF) && (file_type != FILE_TYPE_MOD)) {
        ASSIGN_BY_REF(status, M2_PARSER_STATUS_INVALID_FILE_TYPE);
        return NULL;
    } // end if
    
    // bail out if lextab is NULL
    if (lextab == NULL) {
        ASSIGN_BY_REF(status, M2_PARSER_STATUS_INVALID_LEXTAB_REFERENCE);
        return NULL;
    } // end if

    // bail out if symtab is NULL
    if (symtab == NULL) {
        ASSIGN_BY_REF(status, M2_PARSER_STATUS_INVALID_SYMTAB_REFERENCE);
        return NULL;
    } // end if

    // bail out if ast is NULL
    if (ast == NULL) {
        ASSIGN_BY_REF(status, M2_PARSER_STATUS_INVALID_AST_REFERENCE);
        return NULL;
    } // end if

    // bail out if handler is NULL
    if (ast == NULL) {
        ASSIGN_BY_REF(status, M2_PARSER_STATUS_INVALID_HANDLER);
        return NULL;
    } // end if
    
    // create a new lexer object
    new_lexer = m2_new_lexer(infile, lextab, &lexer_status);
    
    // bail out if lexer object creation failed
    if (lexer_status != M2_LEXER_STATUS_SUCCESS) {
        ASSIGN_BY_REF(status, M2_PARSER_STATUS_LEXER_ALLOCATION_FAILED);
        return NULL;
    } // end if
    
    // allocate memory for parser state
    p = malloc(sizeof(m2_parser_s));
    
    // bail out if allocation failed
    if (p == NULL) {
        m2_dispose_lexer(new_lexer, NULL);
        ASSIGN_BY_REF(status, M2_PARSER_STATUS_ALLOCATION_FAILED);
        return NULL;
    } // end if
            
    // initialise
    p->source_file = infile;
    p->source_type = file_type;
    p->lexer = new_lexer;
    p->current_sym = ZERO_SYMBOL;
    p->lookahead_sym = ZERO_SYMBOL;
    p->lextab = lextab;
    p->symtab = symtab;
    p->ast = ast;
    p->handler = handler;
    p->warnings = 0;
    p->errors = 0;
    
    ASSIGN_BY_REF(status, M2_PARSER_STATUS_SUCCESS);
    return (m2_parser_t) p;
} // end m2_new_parser


// ---------------------------------------------------------------------------
// function:  m2_parse_file(parser, status)
// ---------------------------------------------------------------------------
//
// Parses the input file  associated with parser object <parser>.  The status
// of the operation is passed back in <status>  unless  NULL is passed in for
// <status>.

void m2_parse_start_symbol(m2_parser_s *p); /* FORWARD */

void m2_parse_file(m2_parser_t parser, m2_parser_status_t *status) {
    m2_parser_s *this_parser = (m2_parser_s *) parser;

    // bail out if parser is NULL
    if (parser == NULL) {
        ASSIGN_BY_REF(status, M2_PARSER_STATUS_INVALID_REFERENCE);
        return;
    } // end if
        
    m2_parse_start_symbol(this_parser);
    
    if (this_parser->errors != 0) {
        ASSIGN_BY_REF(status, M2_PARSER_STATUS_SYNTAX_ERRORS_FOUND);
    }
    else if (this_parser->warnings != 0) {
        ASSIGN_BY_REF(status, M2_PARSER_STATUS_WARNINGS_REPORTED);
    }
    else /* no errors and no warnings */ {
        ASSIGN_BY_REF(status, M2_PARSER_STATUS_SUCCESS);
    } // end if
    
    return;
} // end m2_parse_file


// ---------------------------------------------------------------------------
// function:  m2_value_of_pragma_cond(parser, status)
// ---------------------------------------------------------------------------
//
// Parses and evaluates an  in-pragma constant boolean expression  and returns
// its value.  The expression must be located  within  a pragma,  it must be a
// compile time expression  of type boolean  and the parser's lookahead symbol
// must be the first symbol of the expression.  The status of the operation is
// passed back in <status> unless NULL is passed in for status.

bool m2_value_of_pragma_cond(m2_parser_t parser,
                      m2_parser_status_t *status) {
    
    // TO DO
    
    return false;
} // end m2_value_of_pragma_cond


// ---------------------------------------------------------------------------
// function:  m2_value_of_pragma_expr(parser, status)
// ---------------------------------------------------------------------------
//
// Parses and evaluates an  in-pragma constant integer expression  and returns
// its value.  The expression must be located  within  a pragma,  it must be a
// compile time expression  of type integer  and the parser's lookahead symbol
// must be the first symbol of the expression.  The status of the operation is
// passed back in <status> unless NULL is passed in for status.

long int m2_value_of_pragma_expr(m2_parser_t parser,
                          m2_parser_status_t *status) {
    
    // TO DO
    
    return 0;
} // end m2_value_of_pragma_expr


// ---------------------------------------------------------------------------
// function:  m2_dispose_parser(lexer, status)
// ---------------------------------------------------------------------------
//
// Deallocates  lexer object <lexer>.  The function does  not  close the input
// stream  and  it  does  not  deallocate the lexeme table associated with the
// lexer object.  The  status  of  the  operation  is  passed back in <status>
// unless NULL is passed in for <status>.

void m2_dispose_parser(m2_parser_t parser, m2_parser_status_t *status) {
    m2_parser_s *this_parser = (m2_parser_s *) parser;

    // bail out if parser is NULL
    if (parser == NULL) {
        ASSIGN_BY_REF(status, M2_PARSER_STATUS_INVALID_REFERENCE);
        return;
    } // end if
    
    // deallocate
    m2_dispose_lexer(this_parser->lexer, NULL);
    DEALLOCATE(this_parser);
    
    ASSIGN_BY_REF(status, M2_PARSER_STATUS_SUCCESS);
    return;
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
// the two match,  'true' is returned.  If the two do not match,  the parser's
// error counter is incremented,  an error message is printed to stderr,  sym-
// bols in the input stream  are skipped until the lookahead token matches one
// of the tokens in <skip_to_tokens> and 'false' is returned.

static void report_mismatch(m2_parser_s *p,
    m2_tokenset_iterator_t set_of_expected_tokens); /* FORWARD */

static bool match_token(m2_parser_s *p,
                         m2_token_t expected_token,
                      m2_tokenset_t set_of_tokens_to_skip_to) {
    m2_tokenset_t set_of_expected_tokens;
    
    // check if lookahead matches expected token
    if (p->lookahead_sym.token == expected_token) {
        
        return true;
    }
    else /* syntax error */ {
        
        // build tokenset from expected token
        set_of_expected_tokens = m2_tokenset_from_list(expected_token, 0);
        
        // report error
        report_mismatch(p, set_of_expected_tokens);
        
        // skip symbols until the lookahead symbol matches skipset
        while (NOT (m2_tokenset_is_element(set_of_tokens_to_skip_to,
                                          p->lookahead_sym.token))) {
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
// the lookahead token matches any of the tokens in <expected_tokens>,  'true'
// is returned.  If it does  not match  any of the tokens,  the parser's error
// counter is incremented,  an error message is printed to stderr,  symbols in
// the input stream are skipped  until the lookahead token  matches one of the
// tokens in <skip_to_tokens> and 'false' is returned.

static bool match_token_in_set(m2_parser_s *p,
                             m2_tokenset_t set_of_expected_tokens,
                             m2_tokenset_t set_of_tokens_to_skip_to) {
    
    // check if lookahead matches any of the expected tokens
    if (m2_tokenset_is_element(set_of_expected_tokens,
                               p->lookahead_sym.token)) {
        
        return true;
    }
    else /* syntax error */ {
        // report error
        report_mismatch(p, set_of_expected_tokens);
        
        // skip symbols until the lookahead symbol matches followset
        while (NOT (m2_tokenset_is_element(set_of_tokens_to_skip_to,
                                          p->lookahead_sym.token))) {
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
// Reports  a mismatch  between the parser's current lookahead symbol  and the
// expected tokens passed in as tokenset iterator <expected_tokens>.

static void report_mismatch(m2_parser_s *p,
                 m2_tokenset_iterator_t set_of_expected_tokens) {
    
    cardinal row = 0, col = 0, index, token_count;
    m2_lexer_status_t status;
    m2_token_t token;
    
    if (set_of_expected_tokens == NULL) return;
    
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


// ---------------------------------------------------------------------------
// private function: fatal_error( )
// ---------------------------------------------------------------------------
//
// Reports fatal error and aborts.

void fatal_error() {
    printf("fatal error: aborted\n");
    exit(1); // FIX ME
} // end fatal_error


// ===========================================================================
// P R O D U C T I O N   R U L E S
// ===========================================================================


// ---------------------------------------------------------------------------
// start symbol
// ---------------------------------------------------------------------------

m2_token_t m2_compilation_unit(m2_parser_s *p); /* FORWARD */

void m2_parse_start_symbol(m2_parser_s *p) {
    m2_token_t token = _lookahead(p);
    
    if (p->source_type == SOURCE_TYPE_MOD) {
        if ((token != TOKEN_IMPLEMENTATION) && (token != TOKEN_MODULE)) {
            // illegal start symbol for source type MOD
            
            // TO DO: report error
            
            fatal_error(); // abort
        } // end if
    }
    else if (p->source_type == SOURCE_TYPE_DEF) {
        if ((token != TOKEN_DEFINITION) && (token != TOKEN_PROTOTYPE)) {
            // illegal start symbol for source type DEF
            
            // TO DO: report error
            
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
        
        // TO DO: report error
        
    } // end if
    
    return;
} // end m2_parse_start_symbol;


// ---------------------------------------------------------------------------
// #1 compilation_unit
// ---------------------------------------------------------------------------
//  prototype |
//  program_module | definition_of_module | implementation_of_module

m2_token_t m2_prototype(m2_parser_s *p); /* FORWARD */
m2_token_t m2_program_module(m2_parser_s *p); /* FORWARD */
m2_token_t m2_definition_of_module(m2_parser_s *p); /* FORWARD */
m2_token_t m2_implementation_of_module(m2_parser_s *p); /* FORWARD */

m2_token_t m2_compilation_unit(m2_parser_s *p) {
    
    //  prototype |
    //  program_module | definition_of_module | implementation_of_module
    switch(_lookahead(p)) {
            
        // alternative: PROTOTYPE
        case TOKEN_PROTOTYPE :
            m2_prototype(p);
            break;
            
        // alternative: MODULE
        case TOKEN_MODULE :
            m2_program_module(p);
            break;
        
        // alternative: DEFINITION
        case TOKEN_DEFINITION :
            m2_definition_of_module(p);
            break;
            
        // alternative: IMPLEMENTATION
        case TOKEN_IMPLEMENTATION :
            m2_implementation_of_module(p);
            break;
            
        // unreachable alternative
        default :
            fatal_error(); // abort
    } // end alternatives
    
    return _lookahead(p);
} // end m2_compilation_unit


// ---------------------------------------------------------------------------
// #2 prototype
// ---------------------------------------------------------------------------
//  PROTOTYPE prototypeId ";"
//  TYPE "=" ( RECORD | OPAQUE RECORD? ( ":=" literalType )? ) ";"
//  ( ASSOCIATIVE ";" )? requiredBinding* END prototypeId "."

m2_token_t m2_required_binding(m2_parser_s *p); /* FORWARD */
m2_token_t m2_literal_type(m2_parser_s *p); /* FORWARD */

m2_token_t m2_prototype(m2_parser_s *p) {
    
    // PROTOTYPE
    _getsym(p);
    
    // prototypeId
    if (match_token(p, TOKEN_IDENTIFIER, SKIP_TO_TYPE_OR_REQ_BINDING)) {
        _getsym(p);
        
        // ";"
        if (match_token(p, TOKEN_SEMICOLON, SKIP_TO_TYPE_OR_REQ_BINDING)) {
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
    while (m2_tokenset_is_element(FIRST_REQUIRED_BINDING, _lookahead(p))) {
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
        // *** TO DO ***
        
    } // end prototypeId
    
    // "."
    if (match_token(p, TOKEN_DOT, SKIP_TO_EOF)) {
        _getsym(p);
        
    } // end "."
    
    return _lookahead(p);
} // end m2_prototype


// ---------------------------------------------------------------------------
// #3 program_module
// ---------------------------------------------------------------------------
//  MODULE moduleId ( "[" constExpression "]" )? ";"
//  importList* block moduleId "."

m2_token_t m2_const_expression(m2_parser_s *p); /* FORWARD */
m2_token_t m2_import_list(m2_parser_s *p); /* FORWARD */
m2_token_t m2_block(m2_parser_s *p); /* FORWARD */

m2_token_t m2_program_module(m2_parser_s *p) {
        
    // MODULE
    _getsym(p);
        
    // moduleId
    if (match_token(p, TOKEN_IDENTIFIER, SKIP_TO_IMPORT_OR_BLOCK)) {
        _getsym(p); // consume moduleId
        
        // store module name
        // *** TO DO ***
        
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
        // *** TO DO ***
        
    } // end moduleId
    
    // "."
    if (match_token(p, TOKEN_DOT, SKIP_TO_EOF)) {
        _getsym(p);

    } // end "."
    
    return _lookahead(p);
} // end m2_program_module


// ---------------------------------------------------------------------------
// #4 definition_of_module
// ---------------------------------------------------------------------------
//  DEFINITION MODULE moduleId ( "[" prototypeId "]" )? ";"
//  importList* definition* END moduleId "."

m2_token_t m2_definition(m2_parser_s *p); /* FORWARD */

m2_token_t m2_definition_of_module(m2_parser_s *p) {
    
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
        // *** TO DO ***
        
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
        // *** TO DO ***
        
    } // end moduleId
    
    // "."
    if (match_token(p, TOKEN_DOT, SKIP_TO_EOF)) {
        _getsym(p);
        
    } // end "."
    
    return _lookahead(p);
} // end m2_definition_of_module


// ---------------------------------------------------------------------------
// #5 implementation_of_module
// ---------------------------------------------------------------------------
//  IMPLEMENTATION programModule

m2_token_t m2_implementation_of_module(m2_parser_s *p) {
    
    // IMPLEMENTATION
    _getsym(p);
    
    // programModule
    if (match_token_in_set(p, FIRST_PROGRAM_MODULE, SKIP_TO_EOF)) {
        m2_program_module(p);
        
    } // end programModule
    
    return _lookahead(p);
} // end m2_implementation_of_module


// ---------------------------------------------------------------------------
// #6 required_binding
// ---------------------------------------------------------------------------
//  ( CONST "[" bindableIdent "] |
//    PROCEDURE "[" ( bindableOperator | bindableIdent ) "]" ) ";"

m2_token_t m2_bindable_operator(m2_parser_s *p); /* FORWARD */

m2_token_t m2_required_binding(m2_parser_s *p) {
    
    // CONST ... | PROCEDURE ...
    switch (_lookahead(p)) {
        
        // alternative: CONST ...
        case TOKEN_CONST :
            _getsym(p);
            
            // "["
            if (match_token(p, TOKEN_LBRACKET, SKIP_TO_IDENT)) {
                _getsym(p);
                
            } // end "["
            
            // bindableIdent
            if (match_token(p, TOKEN_IDENT,
                               SKIP_TO_RBRACKET_OR_CONST_OR_PROC)) {
                _getsym(p);
                
            } // end bindableIdent
            
            // "]"
            if (match_token(p, TOKEN_RBRACKET, SKIP_TO_CONST_OR_PROC)) {
                _getsym(p);
                
            } // end "]"
            break;
        
        // alternative: PROCEDURE ...
        case TOKEN_PROCEDURE :
            _getsym(p);
            
            // "["
            if (match_token(p, TOKEN_LBRACKET, SKIP_TO_IDENT)) {
                _getsym(p);
                
            } // end "["
            
            // bindableOperator | bindableIdent
            if (match_token_in_set(p, FIRST_BINDABLE_OP_OR_IDENT,
                                      SKIP_TO_SEMICOLON)) {
                
                // alternative: bindableOperator
                if (m2_tokenset_is_element(FIRST_BINDABLE_OP,
                                           _lookahead(p))) {
                    m2_bindable_operator(p);
                    
                }
                // alternative: bindableIdent
                else if (_lookahead(p) == TOKEN_IDENTIFIER) {
                    _getsym(p);
                    
                    // check identifier
                    // *** TO DO ***
                }
                // unreachable alternative
                else {
                    fatal_error(); // abort
                } // end alternatives
            
            } // end bindableOperator | bindableIdent
                        
            // "]"
            if (match_token(p, TOKEN_RBRACKET, SKIP_TO_CONST_OR_PROC)) {
                _getsym(p);
                
            } // end "]"
            break;
            
        // unreachable alternative
        default :
            fatal_error(); // abort
    } // switch
    
    // ";"
    if (match_token(p, TOKEN_SEMICOLON, FOLLOW_REQUIRED_BINDING)) {
        _getsym(p);
        
    } // end ";"
    
    return _lookahead(p);
} // end m2_required_binding


// ---------------------------------------------------------------------------
// #7 bindable_operator
// ---------------------------------------------------------------------------
//  DIV | MOD | IN | FOR |
//  ":=" | "?" | "!" | "~" | "+" | "-" | "*" | "/" | "=" | "<" | ">"

m2_token_t m2_bindable_operator(m2_parser_s *p) {
    
    //  DIV | MOD | IN | FOR |
    //  ":=" | "?" | "!" | "~" | "+" | "-" | "*" | "/" | "=" | "<" | ">"
    switch (_lookahead(p)) {
            
        // alternative: DIV
        case TOKEN_DIV :
            _getsym(p);
            break;
            
        // alternative: MOD
        case TOKEN_MOD :
            _getsym(p);
            break;
            
        // alternative: IN
        case TOKEN_IN :
            _getsym(p);
            break;
            
        // alternative: FOR
        case TOKEN_FOR :
            _getsym(p);
            break;
            
        // alternative: ":="
        case TOKEN_ASSIGN_OP :
            _getsym(p);
            break;
            
        // alternative: "?"
        case TOKEN_RETRIEVAL_PSEUDO_OP :
            _getsym(p);
            break;
        
        // alternative: "!"
        case TOKEN_STORAGE_PSEUDO_OP :
            _getsym(p);
            break;
            
        // alternative: "~"
        case TOKEN_REMOVAL_PSEUDO_OP :
            _getsym(p);
            break;
            
        // alternative: "+"
        case TOKEN_PLUS_OP :
            _getsym(p);
            break;
            
        // alternative: "-"
        case TOKEN_MINUS_OP :
            _getsym(p);
            break;
            
        // alternative: "*"
        case TOKEN_ASTERISK_OP :
            _getsym(p);
            break;
            
        // alternative: "/"
        case TOKEN_SLASH_OP :
            _getsym(p);
            break;
            
        // alternative: "="
        case TOKEN_EQUAL_OP :
            _getsym(p);
            break;
            
        // alternative: "<"
        case TOKEN_LESS_OP :
            _getsym(p);
            break;
            
        // alternative: ">"
        case TOKEN_GREATER_OP :
            _getsym(p);
            break;
            
        // unreachable alternative
        default :
            fatal_error(); // abort
    } // end alternatives
    
    return _lookahead(p);
} // end m2_bindable_operator


m2_token_t m2_literal_type(m2_parser_s *p) {
    return _lookahead(p);
} // end m2_literal_type;


// ---------------------------------------------------------------------------
// #8 import_list
// ---------------------------------------------------------------------------
//  ( FROM moduleId IMPORT ( identList | "*" ) |
//    IMPORT ident "+"? ( "," ident "+"? )* ) ";"

m2_token_t m2_import_list(m2_parser_s *p) {
    
    // FROM ... | IMPORT ...
    switch (_lookahead(p)) {
        
        // alternative: FROM ...
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
            
        // alternative: IMPORT ...
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
            
        // unreachable alternative
        default :
            fatal_error(); // abort
    } // end alternatives

    // ";"
    if (match_token(p, TOKEN_SEMICOLON, FOLLOW_IMPORT_LIST)) {
        _getsym(p);
        
    } // end ";"
    
    return _lookahead(p);
} // end m2_import_list


// ---------------------------------------------------------------------------
// #9 block
// ---------------------------------------------------------------------------
//  definition* ( BEGIN statementSequence )? END

m2_token_t m2_statement_sequence(m2_parser_s *p); /* FORWARD */

m2_token_t m2_block(m2_parser_s *p) {
    
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


// ---------------------------------------------------------------------------
// #10 declaration
// ---------------------------------------------------------------------------
//  CONST ( constantDeclaration ";" )* |
//  TYPE ( ident "=" type ";" )* |
//  VAR ( variableDeclaration ";" )* |
//  procedureDeclaration ";"

m2_token_t m2_const_declaration(m2_parser_s *p); /* FORWARD */
m2_token_t m2_type(m2_parser_s *p); /* FORWARD */
m2_token_t m2_variable_declaration(m2_parser_s *p); /* FORWARD */
m2_token_t m2_procedure_declaration(m2_parser_s *p); /* FORWARD */

m2_token_t m2_declaration(m2_parser_s *p) {
    
    // CONST ... | TYPE ... | VAR ... | procedureDeclaration ...
    switch (_lookahead(p)) {
        
        // alternative: CONST ...
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
            
        // alternative: TYPE ...
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
            
        // alternative: VAR ...
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
            
        // alternative: procedureDeclaration ...
        case TOKEN_PROCEDURE :            
            m2_procedure_declaration(p);
            
            // ";"
            if (match_token(p, TOKEN_SEMICOLON, SKIP_TO_SEMICOLON)) {
                _getsym(p);
                
            } // ";"

            break;
        
        // unreachable alternative
        default :
            fatal_error(); // abort
    } // end alternatives
    
    return _lookahead(p);
} // end m2_declaration


// ---------------------------------------------------------------------------
// #11 definition
// ---------------------------------------------------------------------------
//  CONST ( ( "[" ident "]" )? constantDeclaration ";"  )* |
//  TYPE ( ident "=" ( type | OPAQUE recordType? ) ";" )* |
//  VAR ( variableDeclaration ";" )* |
//  procedureHeader ";"
//
// This production is divided into the following sub-productions:
//
// #11-1 definition :
//  CONST constDefinitionTail* |
//  TYPE typeDefinitionTail* |
//  VAR ( variableDeclaration ";" )* |
//  procedureHeader ";"
//
// #11-2 constDefinitionTail :
//  ( "[" ident "]" )? constantDeclaration ";"
//
// #11-3 typeDefinitionTail :
//  ident "=" ( type | OPAQUE recordType? ) ";"


// ---------------------------------------------------------------------------
// #11-1 definition
// ---------------------------------------------------------------------------
//  CONST constDefinitionTail* |
//  TYPE typeDefinitionTail* |
//  VAR ( variableDeclaration ";" )* |
//  procedureHeader ";"

m2_token_t m2_const_definition_tail(m2_parser_s *p); /* FORWARD */
m2_token_t m2_type_definition_tail(m2_parser_s *p); /* FORWARD */
m2_token_t m2_procedure_header(m2_parser_s *p); /* FORWARD */

m2_token_t m2_definition(m2_parser_s *p) {
    
    // CONST ... | TYPE ... | VAR ... | procedureHeader ...
    switch (_lookahead(p)) {
            
        // alternative: CONST
        case TOKEN_CONST :
            _getsym(p);
            
            // constDefinitionTail*
            while (m2_tokenset_is_element(FIRST_CONST_DEFINITION_TAIL,
                                          _lookahead(p))) {
                m2_const_definition_tail(p);
                
            } // end constDefinitionTail*
            
            break;
            
        // alternative: TYPE
        case TOKEN_TYPE :
            _getsym(p);
            
            // typeDefinitionTail*
            while (m2_tokenset_is_element(FIRST_TYPE_DEFINITION_TAIL,
                                          _lookahead(p))) {
                m2_type_definition_tail(p);
                
            } // end typeDefinitionTail*
            
            break;
            
        // alternative: VAR
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
                
            } // ( variableDeclaration ";" )*
            
            break;
            
        // alternative: procedureHeader ...
        case TOKEN_PROCEDURE :
            m2_procedure_header(p);
            
            // ";"
            if (match_token(p, TOKEN_SEMICOLON, SKIP_TO_SEMICOLON)) {
                _getsym(p);
                
            } // ";"
            
            break;
        
        // unreachable alternative
        default :
            fatal_error(); // abort
    } // end alternatives
    
    return _lookahead(p);
} // end m2_definition


// ---------------------------------------------------------------------------
// #11-2 const_definition_tail
// ---------------------------------------------------------------------------
//  ( "[" ident "]" )? constantDeclaration ";"

m2_token_t const_definition_tail(m2_parser_s *p) {
    
    // ( "[" ident "]" )?
    if (_lookahead(p) == TOKEN_LBRACKET) {
        _getsym(p);
        
        // ident
        if (match_token(p, TOKEN_IDENTIFIER, SKIP_TO_RBRACKET)) {
            _getsym(p);
            
        } // end ident
        
        // "]"
        if (match_token(p, TOKEN_RBRACKET, FIRST_CONST_DECLARATION)) {
            _getsym(p);
            
        } // end "]"
        
    } // end ( "[" ident "]" )?
    
    // constantDeclaration
    if (match_token_in_set(p, FIRST_CONST_DECLARATION, SKIP_TO_SEMICOLON)) {
        m2_const_declaration(p);
        
    } // end constDeclaration
    
    // ";"
    if (match_token(p, TOKEN_SEMICOLON, SKIP_TO_SEMICOLON)) {
        _getsym(p);
        
    } // ";"
    
    return _lookahead(p);
} // end const_definition_tail


// ---------------------------------------------------------------------------
// #11-3 type_definition_tail
// ---------------------------------------------------------------------------
//  ident "=" ( type | OPAQUE recordType? ) ";"

m2_token_t m2_record_type(m2_parser_s *p); /* FORWARD */

m2_token_t type_definition_tail(m2_parser_s *p) {
    
    // ident
    if (match_token(p, TOKEN_IDENTIFIER, SKIP_TO_EQUAL)) {
        _getsym(p);
        
    } // end ident
    
    // "="
    if (match_token(p, TOKEN_EQUAL_OP, FIRST_TYPE_OR_OPAQUE)) {
        _getsym(p);
        
    } // end "="
    
    // type | OPAQUE recordType?
    if (match_token_in_set(p, FIRST_TYPE_OR_OPAQUE, SKIP_TO_SEMICOLON)) {
        
        // alternative: type
        if (m2_tokenset_is_element(FIRST_TYPE, _lookahead(p))) {
            m2_type(p);
            
        }
        // alternative: OPAQUE recordType?
        else if () {
           
            // OPAQUE
            if (match_token(p, TOKEN_OPAQUE, FIRST_RECORD_TYPE)) {
                _getsym(p);
                
            } // end OPAQUE
            
            // recordType?
            if (m2_tokenset_is_element(FIRST_RECORD_TYPE, _lookahead(p))) {
                m2_record_type(p);
                
            } // end recordType?
        }
        // unreachable alternative
        else {
            fatal_error(); // abort
        } // end alternatives
        
    } // end type | OPAQUE recordType?
    
    // ";"
    if (match_token(p, TOKEN_SEMICOLON, SKIP_TO_SEMICOLON)) {
        _getsym(p);
        
    } // ";"
    
    return _lookahead(p);
} // end type_definition_tail


// ---------------------------------------------------------------------------
// #12 const_declaration
// ---------------------------------------------------------------------------
//  ident "=" constExpression

m2_token_t m2_const_declaration(m2_parser_s *p) {
    
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


// ---------------------------------------------------------------------------
// #13 type
// ---------------------------------------------------------------------------
//  ( ALIAS | range ) OF namedType | enumerationType |
//  arrayType | recordType | setType | pointerType | procedureType

m2_token_t m2_range(m2_parser_s *p); /* FORWARD */
m2_token_t m2_enumeration_type(m2_parser_s *p); /* FORWARD */
m2_token_t m2_array_type(m2_parser_s *p); /* FORWARD */
m2_token_t m2_record_type(m2_parser_s *p); /* FORWARD */
m2_token_t m2_set_type(m2_parser_s *p); /* FORWARD */
m2_token_t m2_pointer_type(m2_parser_s *p); /* FORWARD */
m2_token_t m2_procedure_type(m2_parser_s *p); /* FORWARD */

m2_token_t m2_type(m2_parser_s *p) {
    
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


// ---------------------------------------------------------------------------
// #14 range
// ---------------------------------------------------------------------------
//  "[" constExpression ".." constExpression "]"

m2_token_t m2_range(m2_parser_s *p) {
    
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


// ---------------------------------------------------------------------------
// #15 enumeration_type
// ---------------------------------------------------------------------------
//  "(" ( ( "+" namedType ) | ident )
//        ( "," ( ( "+" namedType ) | ident ) )* ")"
//
// This production is divided into the following sub-productions:
//
// #15-1 enumerationType :
//  "(" enumerationComponent ( "," enumerationComponent )* ")"
//
// #15-2 enumerationComponent :
//  ( "+" namedType | ident )


// ---------------------------------------------------------------------------
// #15-1 enumeration_type
// ---------------------------------------------------------------------------
//  "(" enumerationComponent ( "," enumerationComponent )* ")"

m2_token_t m2_enumeration_component(m2_parser_s *p); /* FOWARD */

m2_token_t m2_enumeration_type(m2_parser_s *p) {

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


// ---------------------------------------------------------------------------
// #15-2 enumeration_component
// ---------------------------------------------------------------------------
//  ( "+" namedType | ident )

m2_token_t m2_enumeration_component(m2_parser_s *p) {
    
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


// ---------------------------------------------------------------------------
// #16 array_type
// ---------------------------------------------------------------------------
//  ( ARRAY constComponentCount ( "," constComponentCount )* |
//    ASSOCIATIVE ARRAY ) OF namedType

m2_token_t m2_array_type(m2_parser_s *p) {
    
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


// ---------------------------------------------------------------------------
// #17 record_type
// ---------------------------------------------------------------------------
//  RECORD ( "(" baseType ")" )? fieldListSequence? END

m2_token_t m2_field_list_sequence(m2_parser_s *p); /* FORWARD */

m2_token_t m2_record_type(m2_parser_s *p) {
    
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
    if (match_token(p, TOKEN_END, FOLLOW_RECORD_TYPE)) {
        _getsym(p);
        
    } // end END
    
    return _lookahead(p);
} // end m2_record_type


// ---------------------------------------------------------------------------
// #18 field_list_sequence
// ---------------------------------------------------------------------------
//  fieldList ( ";" fieldList )*

m2_token_t m2_field_list(m2_parser_s *p); /* FORWARD */

m2_token_t m2_field_list_sequence(m2_parser_s *p) {
    
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


// ---------------------------------------------------------------------------
// #19 field_list
// ---------------------------------------------------------------------------
//  ident
//  ( ( "," ident )+ ":" namedType |
//    ":" ( ARRAY determinantField OF )? namedType )

m2_token_t m2_field_list(m2_parser_s *p) {
    
    // ident
    _getsym(p);
    
    // ( "," ident )+ ":" namedType
    if (_lookahead(p) == TOKEN_COMMA) {
        _getsym(p);
        
        // ident
        if (match_token(p, TOKEN_IDENTIFIER, SKIP_TO_COMMA_OR_IDENT)) {
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
            _getsym(p);
            
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


// ---------------------------------------------------------------------------
// #20 set_type
// ---------------------------------------------------------------------------
//  SET OF ( namedEnumType | "(" identList ")" )

m2_token_t m2_ident_list(m2_parser_s *p); /* FORWARD */

m2_token_t m2_set_type(m2_parser_s *p) {
    
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


// ---------------------------------------------------------------------------
// #21 pointer_type
// ---------------------------------------------------------------------------
//  POINTER TO CONST? namedType

m2_token_t m2_pointer_type(m2_parser_s *p) {
    
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


// ---------------------------------------------------------------------------
// #22 procedure_type
// ---------------------------------------------------------------------------
//  PROCEDURE ( "(" formalTypeList ")" )? ( ":" returnedType )?

m2_token_t m2_formal_type_list(m2_parser_s *p); /* FORWARD */

m2_token_t m2_procedure_type(m2_parser_s *p) {
    
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


// ---------------------------------------------------------------------------
// #23 formal_type_list
// ---------------------------------------------------------------------------
//  formalType ( "," formalType )*

m2_token_t m2_formal_type(m2_parser_s *p); /* FORWARD */

m2_token_t m2_formal_type_list(m2_parser_s *p) {
    
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


// ---------------------------------------------------------------------------
// #24 formal_type
// ---------------------------------------------------------------------------
//  simpleFormalType | attributedFormalType

m2_token_t m2_simple_formal_type(m2_parser_s *p); /* FORWARD */
m2_token_t m2_attributed_formal_type(m2_parser_s *p); /* FORWARD */

m2_token_t m2_formal_type(m2_parser_s *p) {
    m2_token_t token = _lookahead(p);

    // alternative: simpleFormalType
    if (m2_tokenset_is_element(FIRST_SIMPLE_FORMAL_TYPE, token)) {
        m2_simple_formal_type(p);
    }
    // alternative: attributedFormalType
    else if (m2_tokenset_is_element(FIRST_ATTRIBUTED_FORMAL_TYPE, token)) {
        m2_attributed_formal_type(p);
    }
    // unreachable alternative
    else {
        fatal_error(); // abort
    } // end alternatives
    
    return _lookahead(p);
} // end m2_formal_type


// ---------------------------------------------------------------------------
// #25 simple_formal_type
// ---------------------------------------------------------------------------
//  ( CAST? ARRAY OF )? namedType

m2_token_t m2_simple_formal_type(m2_parser_s *p) {
    
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


// ---------------------------------------------------------------------------
// #25 attributed_formal_type
// ---------------------------------------------------------------------------
//  ( CONST | VAR ) ( VARIADIC OF )? simpleFormalType |
//  VARIADIC OF ( simpleFormalType | '(' nonVariadicFormalTypeList ')' )
//
// This production is divided into the following sub-productions:
//
// #25-1 attributedFormalType :
//  constVarPrefixedFormalType | variadicPrefixedFormalType
//
// #25-2 constVarPrefixedFormalType :
//  ( CONST | VAR ) ( VARIADIC OF )? simpleFormalType
//
// #25-3 variadicPrefixedFormalType :
//  VARIADIC OF ( simpleFormalType | '(' nonVariadicFormalTypeList ')' )


// ---------------------------------------------------------------------------
// #25-1 attributed_formal_type
// ---------------------------------------------------------------------------
//  constVarPrefixedFormalType | variadicPrefixedFormalType

m2_token_t m2_const_var_prefixed_formal_type(m2_parser_s *p); /* FORWARD */
m2_token_t m2_variadic_prefixed_formal_type(m2_parser_s *p); /* FORWARD */

m2_token_t m2_attributed_formal_type(m2_parser_s *p) {
    m2_token_t token = _lookahead(p);
    
    // alternative: constVarPrefixedFormalType
    if ((token == TOKEN_CONST) || (token == TOKEN_VAR)) {
        m2_const_var_prefixed_formal_type(p);
    }
    // alternative: variadicPrefixedFormalType
    else if (token == TOKEN_VARIADIC) {
        m2_variadic_prefixed_formal_type(p);
    }
    // unreachable alternative
    else {
        fatal_error(); // abort
    } // end alternatives
    
    return _lookahead(p);
} // end m2_attributed_formal_type


// ---------------------------------------------------------------------------
// #25-2 const_var_prefixed_formal_type
// ---------------------------------------------------------------------------
//  ( CONST | VAR ) ( VARIADIC OF )? simpleFormalType

m2_token_t m2_const_var_prefixed_formal_type(m2_parser_s *p) {
    
    // ( CONST | VAR )?
    if (_lookahead(p) == TOKEN_CONST) {
        _getsym(p);
        
    }
    else if (_lookahead(p) == TOKEN_VAR) {
        _getsym(p);
        
    } // end ( CONST | VAR )?
    
    // ( VARIADIC OF )?
    if (_lookahead(p) == TOKEN_VARIADIC) {
        _getsym(p);

        // OF
        if (match_token(p, TOKEN_OF, FIRST_SIMPLE_FORMAL_TYPE)) {
            _getsym(p);
            
        } // end OF
        
    } // end ( VARIADIC OF )?
    
    // simpleFormalType
    if (match_token_in_set(p, FIRST_SIMPLE_FORMAL_TYPE,
                              FOLLOW_SIMPLE_FORMAL_TYPE)) {
        m2_simple_formal_type(p);
        
    } // end simpleFormalType
    
    return _lookahead(p);
} // end m2_const_var_prefixed_formal_type


// ---------------------------------------------------------------------------
// #25-3 variadic_prefixed_formal_type
// ---------------------------------------------------------------------------
//  VARIADIC OF ( simpleFormalType | "(" nonVariadicFormalTypeList ")" )

m2_token_t m2_non_variadic_formal_type_list(m2_parser_s *p); /* FORWARD */

m2_token_t m2_variadic_prefixed_formal_type(m2_parser_s *p) {
    
    // VARIADIC
    _getsym(p);
    
    // OF
    if (match_token(p, TOKEN_OF, FIRST_SIMPLE_FORMAL_TYPE_OR_LPAREN)) {
        _getsym(p);
        
    } // end OF
    
    // ( simpleFormalType | "(" nonVariadicFormalTypeList ")" )
    if (match_token_in_set(p, FIRST_SIMPLE_FORMAL_TYPE_OR_LPAREN,
                              FOLLOW_ATTRIBUTED_FORMAL_TYPE)) {
        
        // alternative: simpleFormalType
        if () {
            m2_simple_formal_type(p);
        }
        // alternative: "(" nonVariadicFormalTypeList ")"
        else if (token == TOKEN_LPAREN) {
            
            // "("
            _getsym(p);
                
            // nonVariadicFormalTypeList
            if (match_token_in_set(p, FIRST_NON_VARIADIC_FORMAL_TYPE_LIST,
                                      SKIP_TO_RPAREN)) {
                m2_non_variadic_formal_type_list(p);
                
            } // end nonVariadicFormalTypeList
            
            // ")"
            if (match_token(p, TOKEN_RPAREN, FOLLOW_ATTRIBUTED_FORMAL_TYPE)) {
                _getsym(p);
                
            } // end ")"
        }
        // unreachable alternative
        else {
            fatal_error(); // abort
        } // end alternatives
        
    } // end ( simpleFormalType | "(" nonVariadicFormalTypeList ")" )
    
    return _lookahead(p);
} // end m2_variadic_prefixed_formal_type


// ---------------------------------------------------------------------------
// #27 non_variadic_formal_type_list
// ---------------------------------------------------------------------------
//  nonVariadicFormalType ( ',' nonVariadicFormalType )*

m2_token_t m2_non_variadic_formal_type(m2_parser_s *p); /* FORWARD */

m2_token_t m2_non_variadic_formal_type_list(m2_parser_s *p) {
    
    // nonVariadicFormalType
    m2_non_variadic_formal_type(p);
    
    // ( ',' nonVariadicFormalType )*
    while (_lookahead(p) == TOKEN_COMMA) {
        _getsym(p);
        
        // nonVariadicFormalType
        if (match_token_in_set(p, FIRST_NON_VARIADIC_FORMAL_TYPE,
                                  FOLLOW_NON_VARIADIC_FORMAL_TYPE)) {
            m2_non_variadic_formal_type(p);
            
        } // end nonVariadicFormalType
        
    } // end ( ',' nonVariadicFormalType )*
    
    return _lookahead(p);
} // end m2_non_variadic_formal_type_list


// ---------------------------------------------------------------------------
// #28 non_variadic_formal_type
// ---------------------------------------------------------------------------
//  ( CONST | VAR )? simpleFormalType

m2_token_t m2_non_variadic_formal_type(m2_parser_s *p) {
    
    // ( CONST | VAR )?
    
    // alternative: CONST
    if (_lookahead(p) == TOKEN_CONST) {
        _getsym(p);
        
    }
    // alternative: VAR
    else if (_lookahead(p) == TOKEN_VAR) {
        _getsym(p);
        
    }
    // unreachable alternative
    else {
        fatal_error(); // abort
    } // end alternatives
    
    // simpleFormalType
    if (match_token_in_set(p, FIRST_SIMPLE_FORMAL_TYPE,
                              FOLLOW_SIMPLE_FORMAL_TYPE)) {
        m2_simple_formal_type(p);
        
    } // end simpleFormalType
    
    return _lookahead(p);
} // end m2_attributed_formal_type


// ---------------------------------------------------------------------------
// #29 variable_declaration
// ---------------------------------------------------------------------------
//  ident ( "[" machineAddress "]" | "," identList )?
//  ":" ( ARRAY constComponentCount OF )? namedType

m2_token_t m2_variable_declaration(m2_parser_s *p) {
    
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
            if (match_token(p, TOKEN_RBRACKET, SKIP_TO_COLON)) {
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
    if (match_token(p, TOKEN_COLON,
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


// ---------------------------------------------------------------------------
// #30 procedure_declaration
// ---------------------------------------------------------------------------
//  procedureHeader ";" block ident

m2_token_t m2_procedure_declaration(m2_parser_s *p) {
    
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


// ---------------------------------------------------------------------------
// #31 procedure_header
// ---------------------------------------------------------------------------
//  PROCEDURE ( "[" ( "::" | bindableOperator | bindableIdent ) "]" )?
//  ident ( "(" formalParamList ")" )? ( ":" returnedType )?

m2_token_t m2_formal_param_list(m2_parser_s *p); /* FORWARD */

m2_token_t m2_procedure_header(m2_parser_s *p) {
    
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
            
        } // end returnedType
        
    } // end ( ":" returnedType )?
    
    return _lookahead(p);
} // end m2_procedure_header


// ---------------------------------------------------------------------------
// #32 formal_param_list
// ---------------------------------------------------------------------------
//  formalParams ( ";" formalParams )*

m2_token_t m2_formal_params(m2_parser_s *p); /* FORWARD */

m2_token_t m2_formal_param_list(m2_parser_s *p) {
    
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


// ---------------------------------------------------------------------------
// #33 formal_params
// ---------------------------------------------------------------------------
//  ( CONST | VAR ) identList ":" variadicAttribute? simpleFormalType |
//  identList ":"
//  ( simpleFormalType | variadicAttribute
//    ( simpleFormalType | "(" nonVariadicFormalParamList ")" ) )
//
// This production is divided into the following sub-productions:
//
// #33-1 formalParams :
//  constVarPrefixedFormalParams | nonPrefixedFormalParams
//
// #33-2 constVarPrefixedFormalParams :
//  ( CONST | VAR ) identList ":" variadicAttribute? simpleFormalType 
//
// #33-3 nonPrefixedFormalParams :
//  identList ":" nonPrefixedFormalParamsTail
//
// #33-4 nonPrefixedFormalParamsTail :
//  simpleFormalType | variadicAttribute variadicFormalParamsTail
//
// #33-5 variadicFormalParamsTail :
//    simpleFormalType | "(" nonVariadicFormalParamList ")"


// ---------------------------------------------------------------------------
// #33-1 formalParams :
// ---------------------------------------------------------------------------
//  constVarPrefixedFormalParams | nonPrefixedFormalParams

m2_token_t m2_const_var_prefixed_formal_params(m2_parser_s *p); /* FORWARD */
m2_token_t m2_non_prefixed_formal_params(m2_parser_s *p); /* FORWARD */

m2_token_t m2_formal_params(m2_parser_s *p) {
    m2_token_t token = _lookahead(p);
    
    // constVarPrefixedFormalParams | nonPrefixedFormalParams
    
    // alternative: constVarPrefixedFormalParams
    if (m2_tokenset_is_element(
            FIRST_CONST_VAR_PREFIXED_FORMAL_PARAMS, token)) {
        m2_const_var_prefixed_formal_params(p);
    }
    // alternative: nonPrefixedFormalParams
    else if (m2_tokenset_is_element(
                FIRST_NON_PREFIXED_FORMAL_PARAMS, token)) {
        m2_non_prefixed_formal_params(p);
    }
    // unreachable alternative
    else {
        fatal_error(); // abort
    } // end alternatives
    
    return _lookahead(p);
} // end m2_formal_params


// ---------------------------------------------------------------------------
// #33-2 constVarPrefixedFormalParams :
// ---------------------------------------------------------------------------
//  ( CONST | VAR ) identList ":" variadicAttribute? simpleFormalType 

m2_token_t m2_variadic_attribute(m2_parser_s *p); /* FORWARD */

m2_token_t m2_const_var_prefixed_formal_params(m2_parser_s *p) {
    m2_token token = _lookahead(p);
    
    // ( CONST | VAR )
    
    // alternative: CONST
    if (token == TOKEN_CONST) {
        _getsym(p);
        
    }
    // alternative: VAR
    else if (token == TOKEN_VAR) {
        _getsym(p);
        
    }
    // unreachable alternative
    else {
        fatal_error(); // abort
    } // end alternatives
    
    // identList
    if (match_token_in_set(p, FIRST_IDENT_LIST, FOLLOW_IDENT_LIST)) {
        m2_ident_list(p);
        
    } // end identList
    
    // ":"
    if (match_token(p, TOKEN_COLON, FIRST_VARIADIC_ATTRIBUTE)) {
        _getsym(p);
        
    } // end ":"
    
    // variadicAttribute?
    if (m2_tokenset_is_element(FIRST_VARIADIC_ATTRIBUTE, _lookahead(p))) {
        m2_variadic_attribute(p);
        
    } // end variadicAttribute?
    
    // simpleFormalType
    if (match_token_in_set(p, FIRST_SIMPLE_FORMAL_TYPE,
                              FOLLOW_SIMPLE_FORMAL_TYPE)) {
        m2_simple_formal_type(p);
        
    } // end simpleFormalType
    
    return _lookahead(p);
} // end m2_const_var_prefixed_formal_params


// ---------------------------------------------------------------------------
// #33-3 nonPrefixedFormalParams :
// ---------------------------------------------------------------------------
//  identList ":" nonPrefixedFormalParamsTail

m2_token_t m2_non_prefixed_formal_params_tail(m2_parser_s *p); /* FORWARD */

m2_token_t m2_non_prefixed_formal_params(m2_parser_s *p) {
    
    // identList
    m2_ident_list(p);
    
    // ":"
    if (match_token(p, TOKEN_COLON, FIRST_NON_PREFIXED_FORMAL_PARAMS_TAIL)) {
        _getsym(p);
        
    } // end ":"
    
    // nonPrefixedFormalParamsTail
    if (match_token_in_set(p, FIRST_NON_PREFIXED_FORMAL_PARAMS_TAIL,
                              FOLLOW_NON_PREFIXED_FORMAL_PARAMS_TAIL)) {
        m2_non_prefixed_formal_params_tail(p);
        
    } // end nonPrefixedFormalParamsTail
    
    return _lookahead(p);
} // end m2_non_prefixed_formal_params


// ---------------------------------------------------------------------------
// #33-4 non_prefixed_formal_params_tail :
// ---------------------------------------------------------------------------
//  simpleFormalType | variadicAttribute variadicFormalParamsTail

m2_token_t m2_variadic_formal_params_tail(m2_parser_s *p); /* FORWARD */

m2_token_t m2_non_prefixed_formal_params_tail(m2_parser_s *p) {
    m2_token token = _lookahead(p);

    // simpleFormalType | variadicAttribute
    
    // alternative: simpleFormalType
    if (m2_tokenset_is_element(FIRST_SIMPLE_FORMAL_TYPE, token)) {
        m2_simple_formal_type(p);
        
    }
    // alternative: variadicAttribute
    else if (m2_tokenset_is_element(FIRST_VARIADIC_ATTRIBUTE, token)) {
        m2_variadic_attribute(p);
   
        // variadicFormalParamsTail
        if (match_token_in_set(p, FIRST_VARIADIC_FORMAL_PARAMS_TAIL,
                                  FOLLOW_VARIADIC_FORMAL_PARAMS_TAIL)) {
            m2_variadic_formal_params_tail(p);
            
        } // end variadicFormalParamsTail
        
    }
    // unreachable alternative
    else {
        fatal_error(); // abort
    } // end alternatives
    
    return _lookahead(p);
} // end m2_non_prefixed_formal_params_tail


// ---------------------------------------------------------------------------
// #33-5 variadic_formal_params_tail
// ---------------------------------------------------------------------------
//    simpleFormalType | "(" nonVariadicFormalParamList ")"

m2_token_t m2_non_variadic_formal_param_list(m2_parser_s *p); /* FORWARD */

m2_token_t m2_variadic_formal_params_tail(m2_parser_s *p) {
    m2_token token = _lookahead(p);

    // simpleFormalType | "("
    
    // alternative: simpleFormalType
    if (m2_tokenset_is_element(FIRST_SIMPLE_FORMAL_TYPE, token)) {
        m2_simple_formal_type(p);
        
    }
    // alternative: "("
    else if (token == TOKEN_LPAREN) {
    
        // nonVariadicFormalParamList
        if (match_token_in_set(p, FIRST_NON_VARIADIC_FORMAL_PARAM_LIST,
                                  FOLLOW_NON_VARIADIC_FORMAL_PARAM_LIST)) {
            m2_non_variadic_formal_param_list(p);
            
        } // end variadicFormalParamsTail
        
        // ")"
        if (match_token(p, TOKEN_RPAREN,
                           FOLLOW_NON_VARIADIC_FORMAL_PARAMS_TAIL)) {
            _getsym(p);
            
        } // end ")"
        
    }
    // unreachable alternative
    else {
        fatal_error(); // abort
    } // end alternatives
    
    return _lookahead(p);
} // end m2_variadic_formal_params_tail


// ---------------------------------------------------------------------------
// #34 variadic_attribute
// ---------------------------------------------------------------------------
//  VARIADIC ( variadicCounter | "[" variadicTerminator "]" )? OF

m2_token_t m2_variadic_attribute(m2_parser_s *p) {
    
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
    if (match_token(p, TOKEN_OF, FOLLOW_VARIADIC_ATTRIBUTE)) {
        _getsym(p);
        
    } // end OF
    
    return _lookahead(p);
} // end m2_variadic_attribute


// ---------------------------------------------------------------------------
// #35 non_variadic_formal_param_list
// ---------------------------------------------------------------------------
//  nonVariadicFormalParams ( ';' nonVariadicFormalParams )*

m2_token_t m2_non_variadic_formal_params(m2_parser_s *p); /* FORWARD */

m2_token_t m2_non_variadic_formal_param_list(m2_parser_s *p) {
    
    // nonVariadicFormalParams
    m2_non_variadic_formal_params(p);
    
    // ( ';' nonVariadicFormalParams )*
    while (_lookahead(p) == TOKEN_SEMICOLON) {
        _getsym(p);
        
        // nonVariadicFormalParams
        if (match_token_in_set(p, FIRST_NON_VARIADIC_FORMAL_PARAMS,
                                  FOLLOW_NON_VARIADIC_FORMAL_PARAMS)) {
            m2_non_variadic_formal_params(p);
            
        } // end variadicTerminator
        
    } // end ( ';' nonVariadicFormalParams )*
    
    return _lookahead(p);
} // end m2_non_variadic_formal_param_list


// ---------------------------------------------------------------------------
// #36 non_variadic_formal_params
// ---------------------------------------------------------------------------
//  ( CONST | VAR )? identList ':' simpleFormalType

m2_token_t m2_non_variadic_formal_params(m2_parser_s *p) {
    m2_token token = _lookahead(p);

    // ( CONST | VAR )?
    if (token == TOKEN_CONST) {
        _getsym(p);
        
    }
    else if (token == TOKEN_VAR) {
        _getsym(p);
        
    } // end ( CONST | VAR )?
    
    // identList
    if (match_token_in_set(p, FIRST_IDENT_LIST, FOLLOW_IDENT_LIST)) {
        m2_ident_list(p);
        
    } // end identList
    
    // ":"
    if (match_token(p, TOKEN_COLON, FIRST_VARIADIC_ATTRIBUTE)) {
        _getsym(p);
        
    } // end ":"
    
    // variadicAttribute?
    if (m2_tokenset_is_element(FIRST_VARIADIC_ATTRIBUTE, _lookahead(p))) {
        m2_variadic_attribute(p);
        
    } // end variadicAttribute?
    
    // simpleFormalType
    if (match_token_in_set(p, FIRST_SIMPLE_FORMAL_TYPE,
                              FOLLOW_SIMPLE_FORMAL_TYPE)) {
        m2_simple_formal_type(p);
        
    } // end simpleFormalType
    
    return _lookahead(p);
} // end m2_non_variadic_formal_params


// ---------------------------------------------------------------------------
// #37 statement
// ---------------------------------------------------------------------------
//  assignmentOrProcedureCall | ifStatement | caseStatement |
//  whileStatement | repeatStatement | loopStatement |
//  forStatement | RETURN expression? | EXIT 

m2_token_t m2_assignment_or_procedure_call(m2_parser_s *p); /* FORWARD */
m2_token_t m2_if_statement(m2_parser_s *p); /* FORWARD */
m2_token_t m2_case_statement(m2_parser_s *p); /* FORWARD */
m2_token_t m2_while_statement(m2_parser_s *p); /* FORWARD */
m2_token_t m2_repeat_statement(m2_parser_s *p); /* FORWARD */
m2_token_t m2_loop_statement(m2_parser_s *p); /* FORWARD */
m2_token_t m2_for_statement(m2_parser_s *p); /* FORWARD */
m2_token_t m2_expression(m2_parser_s *p); /* FORWARD */

m2_token_t m2_statement(m2_parser_s *p) {
    
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


// ---------------------------------------------------------------------------
// #38 statement_sequence
// ---------------------------------------------------------------------------
//  statement ( ";" statement )*

m2_token_t m2_statement_sequence(m2_parser_s *p) {
    
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


// ---------------------------------------------------------------------------
// #39 assignment_or_procedure_call
// ---------------------------------------------------------------------------
//  designator ( ":=" expression | "++" | "--" | actualParameters )?

m2_token_t m2_designator(m2_parser_s *p); /* FORWARD */
m2_token_t m2_actual_params(m2_parser_s *p); /* FORWARD */

m2_token_t m2_assignment_or_procedure_call(m2_parser_s *p) {
    
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


// ---------------------------------------------------------------------------
// #40 if_statement
// ---------------------------------------------------------------------------
//  IF expression THEN statementSequence
//  ( ELSIF expression THEN statementSequence )*
//  ( ELSE statementSequence )?
//  END

m2_token_t m2_if_statement(m2_parser_s *p) {
    
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


// ---------------------------------------------------------------------------
// #41 case_statement
// ---------------------------------------------------------------------------
//  CASE expression OF case ( "|" case )* ( ELSE statementSequence )? END

m2_token_t m2_case(m2_parser_s *p); /* FORWARD */

m2_token_t m2_case_statement(m2_parser_s *p) {
    
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
        _getsym(p);
        
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


// ---------------------------------------------------------------------------
// #42 case
// ---------------------------------------------------------------------------
//  caseLabels ( "," caseLabels )* ":" statementSequence

m2_token_t m2_case_labels(m2_parser_s *p); /* FORWARD */

m2_token_t m2_case(m2_parser_s *p) {
    
    // caseLabels
    m2_case_labels(p);
    
    // ( "," caseLabels )*
    while (_lookahead(p) == TOKEN_COMMA) {
        _getsym(p);
        
        // caseLabels
        if (match_token_in_set(p, FIRST_CASE_LABELS, FOLLOW_CASE_LABELS)) {
            m2_case_labels(p);
            
        } // end caseLabels
        
    } // end ( "," caseLabels )*
    
    return _lookahead(p);
} // end m2_case


// ---------------------------------------------------------------------------
// #43 case_labels
// ---------------------------------------------------------------------------
//  constExpression ( ".." constExpression )?

m2_token_t m2_case_labels(m2_parser_s *p) {
    
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


// ---------------------------------------------------------------------------
// #44 while_statement
// ---------------------------------------------------------------------------
//  WHILE expression DO statementSequence END

m2_token_t m2_while_statement(m2_parser_s *p) {
    
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


// ---------------------------------------------------------------------------
// #45 repeat_statement
// ---------------------------------------------------------------------------
//  REPEAT statementSequence UNTIL expression

m2_token_t m2_repeat_statement(m2_parser_s *p) {
    
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


// ---------------------------------------------------------------------------
// #46 loop_statement
// ---------------------------------------------------------------------------
//  LOOP statementSequence END

m2_token_t m2_loop_statement(m2_parser_s *p) {
    
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


// ---------------------------------------------------------------------------
// #47 for_statement
// ---------------------------------------------------------------------------
//  FOR controlVariable DESCENDING? IN ( expression | range OF namedType )

m2_token_t m2_for_statement(m2_parser_s *p) {
    
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


// ---------------------------------------------------------------------------
// #48 const_expression
// ---------------------------------------------------------------------------
//  boolConstTerm ( OR boolConstTerm )*

m2_token_t m2_bool_const_term(m2_parser_s *p); /* FORWARD */

m2_token_t m2_const_expression(m2_parser_s *p) {
    
    // boolConstTerm
    m2_bool_const_term(p);
    
    // ( OR boolConstTerm )*
    while (_lookahead(p) == TOKEN_OR) {
        _getsym(p);
        
        // boolConstTerm
        if (match_token_in_set(p, FIRST_BOOL_CONST_TERM,
                                  FOLLOW_BOOL_CONST_TERM)) {
            m2_bool_const_term(p);

        } // end boolConstTerm
        
    } // end ( OR boolConstTerm )*
    
    return _lookahead(p);
} // end m2_const_expression


// ---------------------------------------------------------------------------
// #49 bool_const_term
// ---------------------------------------------------------------------------
//  boolConstFactor ( AND boolConstFactor )*

m2_token_t m2_bool_const_factor(m2_parser_s *p); /* FORWARD */

m2_token_t m2_bool_const_term(m2_parser_s *p) {
    
    // boolConstFactor
    m2_bool_const_factor(p);

    // ( AND boolConstFactor )*
    while (_lookahead(p) == TOKEN_AND) {
        _getsym(p);
        
        // boolConstFactor
        if (match_token_in_set(p, FIRST_BOOL_CONST_FACTOR,
                                  FOLLOW_BOOL_CONST_FACTOR)) {
            m2_bool_const_factor(p);
            
        } // end boolConstFactor
        
    } // end ( AND boolConstFactor )*
    
    return _lookahead(p);
} // end m2_bool_const_term


// ---------------------------------------------------------------------------
// #50 bool_const_factor
// ---------------------------------------------------------------------------
//  NOT? relConstExpr

m2_token_t m2_rel_const_expression(m2_parser_s *p); /* FORWARD */

m2_token_t m2_bool_const_factor(m2_parser_s *p) {
    
    // NOT?
    if (_lookahead(p) == TOKEN_NOT) {
        _getsym(p);
        
    } // end NOT?
    
    // relConstExpr
    if (match_token_in_set(p, FIRST_REL_CONST_EXPRESSION,
                              FOLLOW_REL_CONST_EXPRESSION)) {
        m2_rel_const_expression(p);
        
    } // end relConstExpr
    
    return _lookahead(p);
} // end m2_bool_const_factor


// ---------------------------------------------------------------------------
// #51 rel_const_expression
// ---------------------------------------------------------------------------
//  simpleConstExpr ( relation simpleConstExpr )?

m2_token_t m2_relation(m2_parser_s *p); /* FORWARD */
m2_token_t m2_simple_const_expression(m2_parser_s *p); /* FORWARD */

m2_token_t m2_rel_const_expression(m2_parser_s *p) {
    
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


// ---------------------------------------------------------------------------
// #52 relation
// ---------------------------------------------------------------------------
//  "=" | "#" | "<" | "<=" | ">" | ">=" | "IN"

m2_token_t m2_relation(m2_parser_s *p) {
    
    // "=" | "#" | "<" | "<=" | ">" | ">=" | "IN"
    switch (_lookahead(p)) {
        
        // alternative: "="
        case TOKEN_EQUAL_OP :
            _getsym(p);
            
            break;
            
        // alternative: "#"
        case TOKEN_NOT_EQUAL_OP :
            _getsym(p);
            
            break;
            
        // alternative: "<"
        case TOKEN_LESS_OP :
            _getsym(p);
            
            break;
            
        // alternative: "<="
        case TOKEN_LESS_OR_EQUAL_OP :
            _getsym(p);
            
            break;
        
        // alternative: ">"
        case TOKEN_GREATER_OP :
            _getsym(p);
            
            break;

        // alternative: ">="
        case TOKEN_GREATER_OR_EQUAL_OP :
            _getsym(p);
            
            break;
        
        // alternative: "IN"
        case TOKEN_IN :
            _getsym(p);
            
            break;
        
        // unreachable alternative
        default :
            fatal_error(); // abort
        
    } // end alternatives
    
    return _lookahead(p);
} // end m2_relation


// ---------------------------------------------------------------------------
// #53 simple_const_expression
// ---------------------------------------------------------------------------
//  unaryAddOperator? constTerm ( addOperator constTerm )*

m2_token_t m2_const_term(m2_parser_s *p); /* FORWARD */
m2_token_t m2_add_operator(m2_parser_s *p); /* FORWARD */

m2_token_t m2_simple_const_expression(m2_parser_s *p) {
    
    // unaryAddOperator?
    if (_lookahead(p) == TOKEN_PLUS_OP) {
        _getsym(p);
        
    }
    else if (_lookahead(p) == TOKEN_MINUS_OP) {
        _getsym(p);
        
    } // end unaryAddOperator?
    
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


// ---------------------------------------------------------------------------
// #54 add_operator
// ---------------------------------------------------------------------------
//  "+" | "-"

m2_token_t m2_add_operator(m2_parser_s *p) {
    
    // "+" | "-"
    switch (_lookahead(p)) {
        
        // alternative: "+"
        case TOKEN_PLUS_OP :
            _getsym(p);
            
            break;
            
        // alternative: "-"
        case TOKEN_MINUS_OP :
            _getsym(p);
            
            break;
            
        // unreachable alternative
        default :
            fatal_error(); // abort
    } // end switch
    
    return _lookahead(p);
} // end m2_add_operator


// ---------------------------------------------------------------------------
// #55 const_term
// ---------------------------------------------------------------------------
//  constFactor ( mulOperator constFactor )*

m2_token_t m2_const_factor(m2_parser_s *p); /* FORWARD */
m2_token_t m2_mul_operator(m2_parser_s *p); /* FORWARD */

m2_token_t m2_const_term(m2_parser_s *p) {
    
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


// ---------------------------------------------------------------------------
// #56 mul_operator
// ---------------------------------------------------------------------------
//  "*" | "/" | DIV | MOD

m2_token_t m2_mul_operator(m2_parser_s *p) {
    
    // "*" | "/" | DIV | MOD
    switch (_lookahead(p)) {
            
        // alternative: "*"
        case TOKEN_ASTERISK_OP :
            _getsym(p);
            
            break;
            
        // alternative: "/"
        case TOKEN_SLASH_OP :
            _getsym(p);
            
            break;
            
        // alternative: DIV
        case TOKEN_DIV :
            _getsym(p);
            
            break;
            
        // alternative: MOD
        case TOKEN_MOD :
            _getsym(p);
            
            break;
            
        // unreachable alternative
        default :
            fatal_error(); // abort
    } // end alternatives

    return _lookahead(p);
} // end m2_mul_operator


// ---------------------------------------------------------------------------
// #57 const_factor
// ---------------------------------------------------------------------------
//  ( Number | String | constQualident | constStructuredValue |
//    "(" constExpression ")" | CAST "(" namedType "," constExpression ")" )
//  ( "::" namedType )?

m2_token_t m2_qualident(m2_parser_s *p); /* FORWARD */
m2_token_t m2_const_structured_value(m2_parser_s *p); /* FORWARD */

m2_token_t m2_const_factor(m2_parser_s *p) {
    
    // ( Number | String | constQualident | constStructuredValue |
    //   "(" constExpression ")" | CAST "(" namedType "," constExpression ")" )
    // ( "::" namedType )?
    switch (_lookahead(p)) {
    
        // alternative: Number
        case TOKEN_NUM_LITERAL :
            _getsym(p);
            
            break;
            
        // alternative: String
        case TOKEN_STR_LITERAL :
            _getsym(p);
            
            break;
                
        // alternative: constQualident
        case TOKEN_IDENTIFIER :
            m2_qualident(p);
            
            break;
            
        // alternative: constStructuredValue
        case TOKEN_LBRACE :
            m2_const_structured_value(p);
            
            break;
            
        // alternative: "(" constExpression ")"
        case TOKEN_LPAREN :
            _getsym(p);
            
            // constExpression
            if (match_token_in_set(p, FIRST_CONST_EXPRESSION,
                                      FOLLOW_CONST_EXPRESSION)) {
                m2_const_expression(p);
            } // constExpression
            
            // ")"
            if (match_token(p, TOKEN_RPAREN, FOLLOW_CONST_FACTOR)) {
                _getsym(p);
                
            } // end ")"
            
            break;
            
        // alternative: CAST "(" namedType "," constExpression ")"
        case TOKEN_CAST :
            _getsym(p);
            
            // "("
            if (match_token(p, TOKEN_LPAREN, SKIP_TO_IDENT)) {
                _getsym(p);
                
            } // end "("
                
            // namedType
            if (match_token(p, TOKEN_IDENTIFIER, SKIP_TO_COMMA)) {
                _getsym(p);
                
            } // end namedType
                
            // ","
            if (match_token(p, TOKEN_COMMA, FIRST_CONST_EXPRESSION)) {
                _getsym(p);
                
            } // end ","
            
            // constExpression
            if (match_token_in_set(p, FIRST_CONST_EXPRESSION,
                                      FOLLOW_CONST_EXPRESSION)) {
                m2_const_expression(p);
            } // constExpression
            
            // ")"
            if (match_token(p, TOKEN_RPAREN, FOLLOW_CONST_FACTOR)) {
                _getsym(p);
                
            } // end ")"
            
            break;
                
        // unreachable alternative
        default :
            fatal_error(); // abort
    } // end alternatives
        
    // ( "::" namedType )?
    if (_lookahead(p) == TOKEN_TYPE_CONVERSION_OP) {
        _getsym(p);
        
        // namedType
        if (match_token(p, TOKEN_IDENTIFIER, FOLLOW_CONST_FACTOR)) {
            _getsym(p);
            
        } // end namedType
        
    } // end ( "::" namedType )?
    
    return _lookahead(p);
} // end m2_const_factor


// ---------------------------------------------------------------------------
// #58 designator
// ---------------------------------------------------------------------------
//  qualident designatorTail?

m2_token_t m2_designator_tail(m2_parser_s *p); /* FORWARD */

m2_token_t m2_designator(m2_parser_s *p) {
    
    // qualident
    m2_qualident(p);
    
    // designatorTail?
    if (m2_tokenset_is_element(FIRST_DESIGNATOR_TAIL, _lookahead(p))) {
        m2_designator_tail(p);
        
    } // end  designatorTail?
    
    return _lookahead(p);
} // end m2_designator


// ---------------------------------------------------------------------------
// #59 designator_tail
// ---------------------------------------------------------------------------
//  ( ( "[" expressionList "]" | "^" ) ( "." ident )* )+

m2_token_t m2_expression_list(m2_parser_s *p); /* FORWARD */

m2_token_t m2_designator_tail(m2_parser_s *p) {
    
    do {
        
        // ( "[" expressionList "]" | "^" )
        if (_lookahead(p) == TOKEN_LBRACKET) {
            _getsym(p);
            
            // expressionList
            if (match_token_in_set(p, FIRST_EXPRESSION_LIST,
                                      SKIP_TO_RBRACKET)) {
                m2_expression_list(p);
            } // end expressionList
            
            // "]"
            if (match_token(p, TOKEN_RBRACKET, FOLLOW_DESIGNATOR_TAIL)) {
                _getsym(p);
                
            } // end "]"
        }
        // "^"
        else if (_lookahead(p) == TOKEN_POINTER_DEREF_OP) {
            _getsym(p);
            
        }
        // unreachable code
        else {
            fatal_error(); // abort
            
        } // end if
        
        // ( "." ident )*
        while (_lookahead(p) == TOKEN_DOT) {
            _getsym(p);
            
            // ident
            if (match_token(p, TOKEN_IDENTIFIER, FOLLOW_DESIGNATOR_TAIL)) {
                _getsym(p);
                
            } // end ident
            
        } // end ( "." ident )*
        
    } while ((_lookahead(p) == TOKEN_LBRACKET) ||
             (_lookahead(p) == TOKEN_POINTER_DEREF_OP));
    
    return _lookahead(p);
} // end m2_designator_tail


// ---------------------------------------------------------------------------
// #60 expression_list
// ---------------------------------------------------------------------------
//  expression ( "," expression )*

m2_token_t m2_expression_list(m2_parser_s *p) {
    
    // expression
    m2_expression(p);
    
    // ( "," expression )*
    while (_lookahead(p) == TOKEN_COMMA) {
        _getsym(p);
        
        // expression
        if (match_token_in_set(p, FIRST_EXPRESSION, FOLLOW_EXPRESSION)) {
            m2_expression(p);
            
        } // end expression
        
    } // end ( "," expression )*
    
    return _lookahead(p);
} // end m2_expression_list


// ---------------------------------------------------------------------------
// #61 expression
// ---------------------------------------------------------------------------
//  boolTerm ( OR boolTerm )*

m2_token_t m2_bool_term(m2_parser_s *p); /* FORWARD */

m2_token_t m2_expression(m2_parser_s *p) {
    
    // boolTerm
    m2_bool_term(p);
    
    // ( OR boolTerm )*
    while (_lookahead(p) == TOKEN_OR) {
        _getsym(p);
        
        // boolConstTerm
        if (match_token_in_set(p, FIRST_BOOL_TERM,
                                  FOLLOW_BOOL_TERM)) {
            m2_bool_term(p);
            
        } // end boolTerm
        
    } // end ( OR boolTerm )*
    
    return _lookahead(p);
} // end m2_expression


// ---------------------------------------------------------------------------
// #62 bool_term
// ---------------------------------------------------------------------------
//  boolFactor ( AND boolFactor )*

m2_token_t m2_bool_factor(m2_parser_s *p); /* FORWARD */

m2_token_t m2_bool_term(m2_parser_s *p) {
    
    // boolFactor
    m2_bool_factor(p);
    
    // ( AND boolFactor )*
    while (_lookahead(p) == TOKEN_AND) {
        _getsym(p);
        
        // boolFactor
        if (match_token_in_set(p, FIRST_BOOL_FACTOR,
                                  FOLLOW_BOOL_FACTOR)) {
            m2_bool_factor(p);
            
        } // end boolFactor
        
    } // end ( AND boolFactor )*
    
    return _lookahead(p);
} // end m2_bool_term


// ---------------------------------------------------------------------------
// #63 bool_factor
// ---------------------------------------------------------------------------
//  NOT? relExpression

m2_token_t m2_rel_expression(m2_parser_s *p); /* FORWARD */

m2_token_t m2_bool_factor(m2_parser_s *p) {
    
    // NOT?
    if (_lookahead(p) == TOKEN_NOT) {
        _getsym(p);
        
    } // end NOT?
    
    // relExpression
    if (match_token_in_set(p, FIRST_REL_EXPRESSION,
                              FOLLOW_REL_EXPRESSION)) {
        m2_rel_expression(p);
        
    } // end relExpression
    
    return _lookahead(p);
} // end m2_bool_factor


// ---------------------------------------------------------------------------
// #64 rel_expression
// ---------------------------------------------------------------------------
//  simpleExpression ( relation simpleExpression )?

m2_token_t m2_simple_expression(m2_parser_s *p); /* FORWARD */

m2_token_t m2_expression(m2_parser_s *p) {
    
    // simpleExpression
    m2_simple_expression(p);
    
    // ( relation simpleExpression )?
    if (m2_tokenset_is_element(FIRST_RELATION, _lookahead(p))) {
        m2_relation(p);
        
        // simpleExpression
        if (match_token_in_set(p, FIRST_SIMPLE_EXPRESSION,
                                  FOLLOW_SIMPLE_EXPRESSION)) {
            m2_simple_expression(p);
            
        } // end simpleExpression
        
    } // end ( relation simpleExpression )?
    
    return _lookahead(p);
} // end m2_expression


// ---------------------------------------------------------------------------
// #65 simple_expression
// ---------------------------------------------------------------------------
//  UnaryAddOperator? term ( addOperator term )*

m2_token_t m2_term(m2_parser_s *p); /* FORWARD */

m2_token_t m2_simple_expression(m2_parser_s *p) {
    
    // UnaryAddOperator?
    if (_lookahead(p) == TOKEN_PLUS_OP) {
        _getsym(p);
        
    }
    else if (_lookahead(p) == TOKEN_MINUS_OP) {
        _getsym(p);
        
    } // end UnaryAddOperator?
    
    // term
    if (match_token_in_set(p, FIRST_TERM, FOLLOW_TERM)) {
        m2_term(p);
        
    } // end term
    
    // ( addOperator term )*
    while (m2_tokenset_is_element(FIRST_ADD_OPERATOR, _lookahead(p))) {
        m2_add_operator(p);
        
        // term
        if (match_token_in_set(p, FIRST_TERM, FOLLOW_TERM)) {
            m2_term(p);
            
        } // end term
        
    } // end ( addOperator term )*
    
    return _lookahead(p);
} // end m2_simple_expression


// ---------------------------------------------------------------------------
// #66 term
// ---------------------------------------------------------------------------
//  factor ( mulOperator factor )*

m2_token_t m2_factor(m2_parser_s *p); /* FORWARD */

m2_token_t m2_term(m2_parser_s *p) {
    
    // factor
    m2_factor(p);
    
    // ( mulOperator factor )*
    while (m2_tokenset_is_element(FIRST_MUL_OPERATOR, _lookahead(p))) {
        m2_mul_operator(p);
        
        // factor
        if (match_token_in_set(p, FIRST_FACTOR, FOLLOW_FACTOR)) {
            m2_factor(p);
            
        } // end factor
        
    } // end ( mulOperator factor )*
    
    return _lookahead(p);
} // end m2_term


// ---------------------------------------------------------------------------
// #67 factor
// ---------------------------------------------------------------------------
//  ( Number | String | structuredValue | designatorOrProcedureCall |
//    "(" constExpression ")" | CAST "(" namedType "," constExpression ")" )
//  ( "::" namedType )?

m2_token_t m2_structured_value(m2_parser_s *p); /* FORWARD */
m2_token_t m2_designator_or_procedure_call(m2_parser_s *p); /* FORWARD */

m2_token_t m2_factor(m2_parser_s *p) {
    
    // ( Number | String | structuredValue | designatorOrProcedureCall |
    //   "(" constExpression ")" | CAST "(" namedType "," constExpression ")" )
    // ( "::" namedType )?
    switch (_lookahead(p)) {
            
        // alternative: Number
        case TOKEN_NUM_LITERAL :
            _getsym(p);
            
            break;
            
        // alternative: String
        case TOKEN_STR_LITERAL :
            _getsym(p);
            
            break;
            
        // alternative: structuredValue
        case TOKEN_LBRACE :
            m2_structured_value(p);
            
            break;
            
        // alternative: designatorOrProcedureCall
        case TOKEN_IDENTIFIER :
            m2_designator_or_procedure_call(p);
            
            break;
            
        // alternative: "(" expression ")"
        case TOKEN_LPAREN :
            _getsym(p);
            
            // expression
            if (match_token_in_set(p, FIRST_EXPRESSION,
                                      FOLLOW_EXPRESSION)) {
                m2_expression(p);
            } // expression
            
            // ")"
            if (match_token(p, TOKEN_RPAREN, FOLLOW_FACTOR)) {
                _getsym(p);
                
            } // end ")"
            
            break;
            
        // alternative: CAST "(" namedType "," expression ")"
        case TOKEN_CAST :
            _getsym(p);
            
            // "("
            if (match_token(p, TOKEN_LPAREN, SKIP_TO_IDENT)) {
                _getsym(p);
                
            } // end "("
            
            // namedType
            if (match_token(p, TOKEN_IDENTIFIER, SKIP_TO_COMMA)) {
                _getsym(p);
                
            } // end namedType
            
            // ","
            if (match_token(p, TOKEN_COMMA, FIRST_EXPRESSION)) {
                _getsym(p);
                
            } // end ","
            
            // expression
            if (match_token_in_set(p, FIRST_EXPRESSION,
                                      FOLLOW_EXPRESSION)) {
                m2_expression(p);
            } // expression
            
            // ")"
            if (match_token(p, TOKEN_RPAREN, FOLLOW_FACTOR)) {
                _getsym(p);
                
            } // end ")"
            
            break;
        
        // unreachable alternative
        default :
            fatal_error(); // abort
    } // end switch
    
    // ( "::" namedType )?
    if (_lookahead(p) == TOKEN_TYPE_CONVERSION_OP) {
        _getsym(p);
        
        // namedType
        if (match_token(p, TOKEN_IDENTIFIER, FOLLOW_CONST_FACTOR)) {
            _getsym(p);
            
        } // end namedType
        
    } // end ( "::" namedType )?
    
    return _lookahead(p);
} // end m2_factor


// ---------------------------------------------------------------------------
// #68 designator_or_procedure_call
// ---------------------------------------------------------------------------
//  qualident designatorTail? actualParameters?

m2_token_t m2_designator_or_procedure_call(m2_parser_s *p) {
    
    // qualident
    m2_qualident(p);
    
    // designatorTail?
    if (m2_tokenset_is_element(FIRST_DESIGNATOR_TAIL, _lookahead(p))) {
        m2_designator_tail(p);
        
    } // end designatorTail?
    
    // actualParameters?
    if (_lookahead(p) == TOKEN_LPAREN) {
        m2_actual_params(p);
        
    } // end actualParameters?
    
    return _lookahead(p);
} // end m2_designator_or_procedure_call


// ---------------------------------------------------------------------------
// #69 actual_params
// ---------------------------------------------------------------------------
//  "(" expressionList? ")"

m2_token_t m2_actual_params(m2_parser_s *p) {
    
    // "("
    _getsym(p);
    
    // expressionList?
    if (m2_tokenset_is_element(FIRST_EXPRESSION_LIST, _lookahead(p))) {
        m2_expression_list(p);
        
    } // end expressionList?
    
    // ")"
    if (_lookahead(p) == TOKEN_RPAREN) {
        _getsym(p);
        
    } // end ")"
    
    return _lookahead(p);
} // end m2_actual_params


// ---------------------------------------------------------------------------
// #70 const_structured_value
// ---------------------------------------------------------------------------
//  "{" ( constValueComponent ( "," constValueComponent )* )? "}"

m2_token_t m2_const_value_component(m2_parser_s *p); /* FORWARD */

m2_token_t m2_const_structured_value(m2_parser_s *p) {
    
    // "{"
    _getsym(p);
    
    if (m2_tokenset_is_element(FIRST_CONST_VALUE_COMPONENT, _lookahead(p))) {
        m2_const_value_component(p);
        
        // ( "," constValueComponent )*
        while (_lookahead(p) == TOKEN_COMMA) {
            _getsym(p);
            
            // constValueComponent
            if (match_token_in_set(p, FIRST_CONST_VALUE_COMPONENT,
                                      FOLLOW_CONST_VALUE_COMPONENT)) {
                m2_const_value_component(p);
            } // end constValueComponent

        } // end ( "," constValueComponent )*
    } // end
    
    // "}"
    if (match_token(p, TOKEN_RBRACE, FOLLOW_CONST_VALUE_COMPONENT)) {
        _getsym(p);
        
    } // end "}"
    
    return _lookahead(p);
} // end m2_const_structured_value


// ---------------------------------------------------------------------------
// #71 const_value_component
// ---------------------------------------------------------------------------
//  constExpression ( ( BY | ".." ) constExpression )?

m2_token_t m2_const_value_component(m2_parser_s *p) {
    
    // constExpression
    m2_const_expression(p);
    
    // ( ( BY | ".." ) constExpression )?
    if ((_lookahead(p) == TOKEN_BY) || (_lookahead(p) == TOKEN_RANGE_OP)) {
        _getsym(p);
        
        // constExpression
        if (match_token_in_set(p, FIRST_CONST_EXPRESSION,
                                  FOLLOW_CONST_EXPRESSION)) {
            m2_const_expression(p);
            
        } // end constExpression
        
    } // end ( ( BY | ".." ) constExpression )?
    
    return _lookahead(p);
} // end m2_const_value_component


// ---------------------------------------------------------------------------
// #72 structured_value
// ---------------------------------------------------------------------------
//  "{" ( valueComponent ( "," valueComponent )* )? "}"

m2_token_t m2_value_component(m2_parser_s *p); /* FORWARD */

m2_token_t m2_structured_value(m2_parser_s *p) {
    
    // "{"
    _getsym(p);
    
    if (m2_tokenset_is_element(FIRST_VALUE_COMPONENT, _lookahead(p))) {
        m2_value_component(p);
        
        // ( "," valueComponent )*
        while (_lookahead(p) == TOKEN_COMMA) {
            _getsym(p);
            
            // valueComponent
            if (match_token_in_set(p, FIRST_VALUE_COMPONENT,
                                      FOLLOW_VALUE_COMPONENT)) {
                m2_value_component(p);
            } // end valueComponent
            
        } // end ( "," valueComponent )*
    } // end
    
    // "}"
    if (match_token(p, TOKEN_RBRACE, FOLLOW_VALUE_COMPONENT)) {
        _getsym(p);
        
    } // end "}"
    
    return _lookahead(p);
} // end m2_structured_value


// ---------------------------------------------------------------------------
// #73 value_component
// ---------------------------------------------------------------------------
//  expression ( ( BY | ".." ) constExpression )?

m2_token_t m2_value_component(m2_parser_s *p) {
    
    // expression
    m2_expression(p);
    
    // ( ( BY | ".." ) constExpression )?
    if ((_lookahead(p) == TOKEN_BY) || (_lookahead(p) == TOKEN_RANGE_OP)) {
        _getsym(p);
        
        // constExpression
        if (match_token_in_set(p, FIRST_CONST_EXPRESSION,
                                  FOLLOW_CONST_EXPRESSION)) {
            m2_const_expression(p);
            
        } // end constExpression
        
    } // end ( ( BY | ".." ) constExpression )?
    
    return _lookahead(p);
} // end m2_value_component


// ---------------------------------------------------------------------------
// #74 qualident
// ---------------------------------------------------------------------------
//  ident ( "." ident )*

m2_token_t m2_qualident(m2_parser_s *p) {

    // ident
    _getsym(p);
    
    // ( "." ident )*
    while (_lookahead(p) == TOKEN_DOT) {
        _getsym(p);
        
        // ident
        if (match_token(p, TOKEN_IDENTIFIER, FOLLOW_QUALIDENT)) {
            _getsym(p);
            
        } // end ident
        
    } // end ( "." ident )*
    
    return _lookahead(p);
} // end m2_qualident


// ---------------------------------------------------------------------------
// #75 ident_list
// ---------------------------------------------------------------------------
//  ident ( "," ident )*

m2_token_t m2_ident_list(m2_parser_s *p) {
    
    // ident
    _getsym(p);
    
    // ( "," ident )*
    while (_lookahead(p) == TOKEN_COMMA) {
        _getsym(p);
        
        // ident
        if (match_token(p, TOKEN_IDENTIFIER, FOLLOW_IDENT_LIST)) {
            _getsym(p);
            
        } // end ident
        
    } // end ( "," ident )*
    
    return _lookahead(p);
} // end m2_ident_list


// END OF FILE
