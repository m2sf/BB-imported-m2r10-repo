/*! Modula-2 R10 Compiler (m2r10c)
 *
 *  @file  m2_lexer.c
 *  @brief Lexer implementation
 *
 *  @b Author: Benjamin Kowarsch, Roel Messiant
 *
 *  @b Copyright: (C) 2010 B.Kowarsch, R.Messiant. All rights reserved.
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


// ---------------------------------------------------------------------------
// C standard library imports
// ---------------------------------------------------------------------------

#include <stdlib.h>

// ---------------------------------------------------------------------------
// Project imports
// ---------------------------------------------------------------------------

#include "hash.h"
#include "ASCII.h"
#include "alloc.h"
#include "m2_build_params.h"
#include "m2_reserved_words.h"
#include "m2_lexer.h"


// ---------------------------------------------------------------------------
// Lexeme buffer size
// ---------------------------------------------------------------------------
//
// Buffer size required to hold the largest type of lexeme
//
// Calculation makes adjustments for extra characters:
//  o  one extra character for internally stored type designator appended to
//     quoted string literals ('A' or 'U') and real number literals ('R').
//  o  two extra characters for quotes (' or ") delimiting strings.
//  o  one extra character for C string terminator (ASCII NUL).

#define M2_MAX_LEXEME_LENGTH \
    (MAX3(M2_MAX_IDENT_LENGTH, \
          M2_MAX_NUM_LENGTH + 1, \
          M2_MAX_STRING_LENGTH + 3) + 1)


// ---------------------------------------------------------------------------
// Lexeme buffer type
// ---------------------------------------------------------------------------

typedef struct /* lexbuf_t */ {
    cardinal length;
    char string[M2_MAX_LEXEME_LENGTH];
} lexbuf_t;


// ---------------------------------------------------------------------------
// Lexer state descriptor
// ---------------------------------------------------------------------------

typedef /* m2_lexer_s */ struct {
    
    // configuration parameters
    m2_file_t source_file;              // source file
    kvs_table_t lextab;                 // lexeme table
    m2_notification_f notify;           // notification handler
    
    // return values
    m2_token_t token;                   // token to be returned
    cardinal lexkey;                    // lexeme key to be returned
    m2_lexer_status_t status;           // status to be returned
    
    // counters
    m2_file_pos_t token_pos;            // position of current symbol
    uint16_t paren_nesting_level;       // current parenthesis nesting level
    uint16_t bracket_nesting_level;     // current bracket nesting level
    uint16_t brace_nesting_level;       // current brace nesting level
    uint16_t symbol_error_count;        // number of errors in current symbol
    
    // lexeme buffer
    lexbuf_t lexeme;
    
    // offending character
    char offending_char;
    m2_file_pos_t offending_char_pos;   // position of offending character
} m2_lexer_s;

#define NOT_EOF(_lexer) (m2_fileio_eof(_lexer->source_file) == false)
#define EOF_REACHED(_lexer) (m2_fileio_eof(_lexer->source_file) == true)


// ===========================================================================
// P R I V A T E   F U N C T I O N   P R O T O T Y P E S
// ===========================================================================

static fmacro uchar_t get_ident(m2_lexer_s *lexer); /* FORWARD */

static fmacro uchar_t get_numeric_literal(m2_lexer_s *lexer); /* FORWARD */

static uchar_t get_prefixed_number(m2_lexer_s *lexer); /* FORWARD */

static uchar_t get_real_number(m2_lexer_s *lexer); /* FORWARD */

static fmacro uchar_t get_quoted_literal(m2_lexer_s *lexer); /* FORWARD */

static fmacro bool is_escaped_char(uchar_t ch); /* FORWARD */

static fmacro void add_lexeme_to_lextab(m2_lexer_s *lexer); /* FORWARD */

static fmacro uchar_t skip_multiline_comment(m2_lexer_s *lexer); /* FORWARD */

static fmacro uchar_t skip_past_end_of_line(m2_lexer_s *lexer); /* FORWARD */

static fmacro uchar_t skip_pragma(m2_lexer_s *lexer); /* FORWARD */


// ===========================================================================
// P U B L I C   F U N C T I O N   I M P L E M E N T A T I O N S
// ===========================================================================

#define readchar(v) m2_fileio_read(this_lexer->source_file) // v = void
#define nextchar(v) m2_fileio_lookahead(this_lexer->source_file) // v = void

// ---------------------------------------------------------------------------
// function:  m2_new_lexer(infile, lextab, status)
// ---------------------------------------------------------------------------
//
// Creates  and  returns  a  new  lexer object  associated  with  source  file 
// <infile> and lexeme table <lextab>.  The status of the operation  is passed
// back in <status> unless NULL is passed in for <status>.
//
// Returns NULL if the lexer object could not be created.

m2_lexer_t m2_new_lexer(m2_file_t infile,
                      kvs_table_t lextab,
                m2_notification_f handler,
                m2_lexer_status_t *status) {
    
    m2_lexer_s *new_lexer;    
    
    // assert pre-conditions
    
    if (infile == NULL) {
        ASSIGN_BY_REF(status, M2_LEXER_STATUS_INVALID_REFERENCE);
        return NULL;
    } // end if
    
    if (lextab == NULL) {
        ASSIGN_BY_REF(status, M2_LEXER_STATUS_INVALID_REFERENCE);
        return NULL;
    } // end if
    
    // bail out if handler is NULL
    if (handler == NULL) {
        ASSIGN_BY_REF(status, M2_LEXER_STATUS_INVALID_HANDLER);
        return NULL;
    } // end if
    
    // allocate a new lexer object
    new_lexer = ALLOCATE(sizeof(m2_lexer_s));
    
    if (new_lexer == NULL) {
        ASSIGN_BY_REF(status, M2_LEXER_STATUS_ALLOCATION_FAILED);
        return NULL;
    } // end if

    // initialise the new lexer object
    
    // configuration parameters
    new_lexer->source_file = infile;
    new_lexer->lextab = lextab;
    new_lexer->notify = handler;
    
    // return values
    new_lexer->token = 0;
    new_lexer->lexkey = 0;
    
    // counters
    m2_fileio_getpos(new_lexer->source_file, &new_lexer->token_pos);
    new_lexer->paren_nesting_level = 0;
    new_lexer->bracket_nesting_level = 0;
    new_lexer->brace_nesting_level = 0;
    new_lexer->symbol_error_count = 0;
    
    // input buffer
    new_lexer->lexeme.length = 0;
    new_lexer->lexeme.string[0] = CSTRING_TERMINATOR;
    
    // offending character
    new_lexer->offending_char = 0;
    SET_FPOS(new_lexer->offending_char_pos, 0, 0);
    
    // return the initialised lexer object
    ASSIGN_BY_REF(status, M2_LEXER_STATUS_SUCCESS);
    return (m2_lexer_t) new_lexer;
} // end m2_new_lexer;


// ---------------------------------------------------------------------------
// function:  m2_lexer_getsym(lexer, lexeme, status)
// ---------------------------------------------------------------------------
//
// Reads one symbol from the input stream of lexer <lexer>, returns its token,
// and passes a key for its lexeme back in <lexeme> unless  NULL  is passed in
// for <lexeme>.  The  status  of  the  operation  is  passed back in <status>
// unless NULL is passed in for <status>.

m2_token_t m2_lexer_getsym(m2_lexer_t lexer,
                             cardinal *lexeme,
                    m2_lexer_status_t *status) {
    
    register m2_lexer_s *this_lexer = (m2_lexer_s *) lexer;
    register uchar_t ch;
    bool ignore_token;
    
    // assert pre-condition
    if (lexer == NULL) {
        ASSIGN_BY_REF(lexeme, 0);
        ASSIGN_BY_REF(status, M2_LEXER_STATUS_INVALID_REFERENCE);
        return TOKEN_ILLEGAL_CHARACTER;
    } // end if
    
    // clear lexeme
    this_lexer->lexkey = 0;
    this_lexer->lexeme.length = 0;
    this_lexer->lexeme.string[0] = CSTRING_TERMINATOR;
    
    // clear error counter
    this_lexer->symbol_error_count = 0;
    
    ch = nextchar();
    repeat {
        
        // skip all whitespace, tab and EOL characters
        while ((NOT_EOF(this_lexer)) &&
               ((ch == WHITESPACE) || (ch == TAB) || (ch == EOL))) {
            // skip the current character
            readchar();
            // take a peek at the next one
            ch = nextchar();
        } // end while;
        
        // remember position at the start of the symbol
        m2_fileio_getpos(this_lexer->source_file, &this_lexer->token_pos);
        
        // start optimistically
        this_lexer->status = M2_LEXER_STATUS_SUCCESS;
        ignore_token = false;
        
        // check for end-of-file
        if (EOF_REACHED(this_lexer)) {
            this_lexer->token = TOKEN_EOF_MARKER;
        } // end eof check
        
        // check for identifier or reserved word
        else if ((ch == UNDERSCORE) || (ch == DOLLAR) || (IS_LETTER(ch))) {
            // found identifier or reserved word
            ch = get_ident(this_lexer);
            if (this_lexer->status == M2_LEXER_STATUS_SUCCESS)
                add_lexeme_to_lextab(this_lexer);                
        } // end identifier/reserved word check
        
        // check for numeric literal
        else if (IS_DIGIT(ch)) {
            // found numeric literal
            ch = get_numeric_literal(this_lexer);
            if (this_lexer->status == M2_LEXER_STATUS_SUCCESS)
                add_lexeme_to_lextab(this_lexer);                
        } // end numeric literal check
        
        else switch (ch) {
            case SINGLE_QUOTE :
            case DOUBLE_QUOTE :
                ch = get_quoted_literal(this_lexer);
                if (this_lexer->status == M2_LEXER_STATUS_SUCCESS)
                    add_lexeme_to_lextab(this_lexer);                
                break;
            case DOT :
                ch = readchar();
                ch = nextchar();
                if (ch != DOT) // found '.'
                    this_lexer->token = TOKEN_DOT;
                else { // found '..'
                    ch = readchar();
                    ch = nextchar();
                    this_lexer->token = TOKEN_RANGE_OP;
                } // end range-operator check
                break;
            case COMMA :
                ch = readchar();
                ch = nextchar();
                this_lexer->token = TOKEN_COMMA;
                break;
            case COLON :
                ch = readchar();
                ch = nextchar();
                switch (ch) {
                    case EQUAL_SIGN : // found ':='
                        ch = readchar();
                        ch = nextchar();
                        this_lexer->token = TOKEN_ASSIGN_OP;
                        break;
                    case COLON : // found '::'
                        ch = readchar();
                        ch = nextchar();
                        this_lexer->token = TOKEN_TYPE_CONVERSION_OP;
                        break;
                    default : // found ':'
                        this_lexer->token = TOKEN_COLON;
                } // end switch
                break;                    
            case SEMICOLON :
                ch = readchar();
                ch = nextchar();
                this_lexer->token = TOKEN_SEMICOLON;
                break;
            case OPENING_PARENTHESIS :
                ch = readchar();
                ch = nextchar();
                if (ch != ASTERISK) { // found '('
                    this_lexer->token = TOKEN_OPENING_PARENTHESIS;
                    this_lexer->paren_nesting_level++;
                }
                else { // found '(*'
                    ch = skip_multiline_comment(this_lexer);
                    if (this_lexer->status == M2_LEXER_STATUS_SUCCESS)
                        ignore_token = true;
                } // end if
                break;
            case CLOSING_PARENTHESIS :
                ch = readchar();
                ch = nextchar();
                this_lexer->token = TOKEN_CLOSING_PARENTHESIS;
                this_lexer->paren_nesting_level--;
                break;
            case OPENING_BRACKET :
                ch = readchar();
                ch = nextchar();
                this_lexer->token = TOKEN_OPENING_BRACKET;
                this_lexer->bracket_nesting_level++;
                break;
            case CLOSING_BRACKET :
                ch = readchar();
                ch = nextchar();
                this_lexer->token = TOKEN_CLOSING_BRACKET;
                this_lexer->bracket_nesting_level--;
                break;
            case OPENING_BRACE :
                ch = readchar();
                ch = nextchar();
                this_lexer->token = TOKEN_OPENING_BRACE;
                this_lexer->brace_nesting_level++;
                break;
            case CLOSING_BRACE :
                ch = readchar();
                ch = nextchar();
                this_lexer->token = TOKEN_CLOSING_BRACE;
                this_lexer->brace_nesting_level--;
                break;
            case EXCLAMATION :
                ch = readchar();
                ch = nextchar();
                this_lexer->token = TOKEN_STORAGE_PSEUDO_OP;
                break;
            case TILDE :
                ch = readchar();
                ch = nextchar();
                this_lexer->token = TOKEN_REMOVAL_PSEUDO_OP;
                break;
            case QUESTION_MARK :
                ch = readchar();
                ch = nextchar();
                this_lexer->token = TOKEN_RETRIEVAL_PSEUDO_OP;
                break;
            case EQUAL_SIGN :
                ch = readchar();
                ch = nextchar();
                this_lexer->token = TOKEN_EQUAL_OP;
                break;
            case NUMBER_SIGN :
                ch = readchar();
                ch = nextchar();
                this_lexer->token = TOKEN_NOT_EQUAL_OP;
                break;
            case GREATER_THAN :
                ch = readchar();
                ch = nextchar();
                if (ch != EQUAL_SIGN) // found '>'
                    this_lexer->token = TOKEN_GREATER_OP;
                else { // found '>='
                    ch = readchar();
                    ch = nextchar();
                    this_lexer->token = TOKEN_GREATER_OR_EQUAL_OP;
                } // end if
                break;
            case LESS_THAN :
                ch = readchar();
                ch = nextchar();
                switch (ch) {
                    case EQUAL_SIGN : // found '<='
                        ch = readchar();
                        ch = nextchar();
                        this_lexer->token = TOKEN_LESS_OR_EQUAL_OP;
                        break;
                    case ASTERISK : // found '<*'
                        #ifdef M2LEXER_SKIP_PRAGMAS
                        ch = skip_pragma(this_lexer);
                        ignore_token = true;
                        #else
                        ch = readchar();
                        ch = nextchar();
                        this_lexer->token = TOKEN_START_PRAGMA;
                        #endif
                        break;
                    default : // found '<'
                        this_lexer->token = TOKEN_LESS_OP;
                } // end switch
                break;
            case PLUS :
                ch = readchar();
                ch = nextchar();
                if (ch != PLUS) // found '+'
                    this_lexer->token = TOKEN_PLUS_OP;
                else { // found '++'
                    ch = readchar();
                    ch = nextchar();
                    this_lexer->token = TOKEN_INCREMENT_OP;
                } // end if
                break;
            case MINUS :
                ch = readchar();
                ch = nextchar();
                if (ch != MINUS) // found '-'
                    this_lexer->token = TOKEN_MINUS_OP;
                else { // found '--'
                    ch = readchar();
                    ch = nextchar();
                    this_lexer->token = TOKEN_DECREMENT_OP;
                } // end if
                break;
            case ASTERISK :
                ch = readchar();
                ch = nextchar();
                if (ch != GREATER_THAN) // found '*'
                    this_lexer->token = TOKEN_ASTERISK_OP;
                else { // found '*>'
                    ch = readchar();
                    ch = nextchar();
                    this_lexer->token = TOKEN_END_PRAGMA;
                } // end if
                break;
            case FORWARD_SLASH :
                ch = readchar();
                ch = nextchar();
                if (ch != FORWARD_SLASH) // found '/'
                    this_lexer->token = TOKEN_SLASH_OP;
                else { // found '//'
                    ch = skip_past_end_of_line(this_lexer);
                    ignore_token = true;
                } // end if
                break;
            case CARET :
                ch = readchar();
                ch = nextchar();
                this_lexer->token = TOKEN_POINTER_DEREF_OP;
                break;
            case VERTICAL_BAR :
                ch = readchar();
                ch = nextchar();
                this_lexer->token = TOKEN_CASE_LABEL_SEPARATOR;
                break;
            default : // found illegal character
                this_lexer->offending_char = readchar();
                m2_fileio_getpos(this_lexer->source_file,
                                 &this_lexer->offending_char_pos);
                ch = nextchar();
                this_lexer->token = TOKEN_ILLEGAL_CHARACTER;
        } // end if
    } until (ignore_token == false);
    
    // pass back lexeme key and status
    ASSIGN_BY_REF(lexeme, this_lexer->lexkey);
    ASSIGN_BY_REF(status, this_lexer->status);
    
    // return the token
    return this_lexer->token;
} // end m2_lexer_getsym;


// ---------------------------------------------------------------------------
// function:  m2_lexer_getpos(lexer, row, col, status)
// ---------------------------------------------------------------------------
//
// Obtains the position of the last symbol read from the input stream.  Passes
// the row back in <row>  unless NULL is passed in for <row>,  and the coloumn
// back in <col>  unless  NULL  is  passed  in  for <col>.  The status  of the
// operation is passed back in <status> unless NULL is passed in for <status>.

void m2_lexer_getpos(m2_lexer_t lexer,
                       cardinal *row,
                       cardinal *col,
              m2_lexer_status_t *status) {
    
    m2_lexer_s *this_lexer = (m2_lexer_s *) lexer;
    
    if (lexer == NULL) {
        ASSIGN_BY_REF(status, M2_LEXER_STATUS_INVALID_REFERENCE);
	}
    else {
        ASSIGN_BY_REF(row, this_lexer->token_pos.line);
        ASSIGN_BY_REF(col, this_lexer->token_pos.col);
        ASSIGN_BY_REF(status, M2_LEXER_STATUS_SUCCESS);
    } // end if
    
    return;
} // end m2_lexer_getpos


// ---------------------------------------------------------------------------
// function:  m2_offending_char(lexer, row, col, status)
// ---------------------------------------------------------------------------
//
// Returns the offending character of the last read operation  and  passes its
// position  back  in  <row>  and <col>.  If no error occurred during the last
// read operation then ASCII NUL is returned  and zero is passed pack in <row>
// and <col>.  This function should only be called  after a preceeding call to
// function m2_lexer_getsym()  returned an error indicating that an illegal or
// unexcpected character was found. The status of the operation is passed back
// in <status> unless NULL is passed in for <status>.

char m2_offending_char(m2_lexer_t lexer,
                         cardinal *row,
                         cardinal *col,
                m2_lexer_status_t *status) {
    
    m2_lexer_s *this_lexer = (m2_lexer_s *) lexer;
    
    if (lexer == NULL) {
        ASSIGN_BY_REF(row, 0);
        ASSIGN_BY_REF(col, 0);
        ASSIGN_BY_REF(status, M2_LEXER_STATUS_INVALID_REFERENCE);
        return ASCII_NUL;
    }
    else if (this_lexer->status != M2_LEXER_STATUS_ILLEGAL_CHARACTER) {
        ASSIGN_BY_REF(row, 0);
        ASSIGN_BY_REF(col, 0);
        ASSIGN_BY_REF(status, M2_LEXER_STATUS_INVALID_REFERENCE);
        return ASCII_NUL;
    }
    else {
        ASSIGN_BY_REF(row, this_lexer->offending_char_pos.line);
        ASSIGN_BY_REF(col, this_lexer->offending_char_pos.col);
        ASSIGN_BY_REF(status, M2_LEXER_STATUS_SUCCESS);
        return this_lexer->offending_char;
    } // end if
} // end m2_offending_char


// ---------------------------------------------------------------------------
// function:  m2_dispose_lexer(lexer, status)
// ---------------------------------------------------------------------------
//
// Deallocates  lexer object <lexer>.  The function does  not  close the input
// stream  and  it  does  not  deallocate the lexeme table associated with the
// lexer object.  The  status  of  the  operation  is  passed back in <status>
// unless NULL is passed in for <status>.

void m2_dispose_lexer(m2_lexer_t lexer,
               m2_lexer_status_t *status) {
    
    m2_lexer_s *this_lexer = (m2_lexer_s *) lexer;
    
    if (lexer == NULL) {
        ASSIGN_BY_REF(status, M2_LEXER_STATUS_INVALID_REFERENCE);
        return;
    } // end if
    
    DEALLOCATE(this_lexer);
    
    ASSIGN_BY_REF(status, M2_LEXER_STATUS_SUCCESS);
    return;
} // end m2_dispose_lexer;

#undef readchar
#undef nextchar


// ===========================================================================
// P R I V A T E   F U N C T I O N   I M P L E M E N T A T I O N S
// ===========================================================================

// ---------------------------------------------------------------------------
// macros for private functions
// ---------------------------------------------------------------------------

#define readchar(v) m2_fileio_read(lexer->source_file) // v = void
#define nextchar(v) m2_fileio_lookahead(lexer->source_file) // v = void


// ---------------------------------------------------------------------------
// private function:  get_ident(lexer)
// ---------------------------------------------------------------------------
//
// Reads  an  identifier  from  the  input stream of <lexer>  and  returns the
// character following the identifier.
//
// This function accepts input conforming to the following syntax:
//
//  identifier := ( "_" | "$" | letter ) ( "_" | "$" | letter | digit )*
//  letter := "a" .. "z" | "A" .. "Z"
//  digit := "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
//
// pre-conditions:
//  o  lexer is an initialised lexer object
//  o  the current lookahead character is the first character at the beginning
//     of the identifier.
//  o  the length of the identifier does not exceed M2_MAX_IDENT_LENGTH.
//
// post-conditions:
//  o  lexer->lexeme.string contains the identifier,
//     followed by a C string terminator (ASCII NUL).
//  o  lexer->lexeme.length contains the length of lexer->lexeme.string.
//  o  lexer->token contains the respective token value
//     - TOKEN_<reserved_word> if the identifier is a reserved word
//     - TOKEN_IDENTIFIER if the identifier is a non-reserved word identifier
//  o  lexer->lexkey contains the key for the lexeme table.
//  o  lexer->status contains M2_LEXER_STATUS_SUCCESS.
//  o  the new lookahead character is the character following the identifier.
//
// error-conditions:
//  if the identifier exceeds M2_MAX_IDENT_LENGTH
//  o  lexer->lexeme.string contains the significant characters only,
//     followed by a C string terminator (ASCII NUL).
//  o  otherwise, post-conditions apply.

static fmacro uchar_t get_ident(m2_lexer_s *lexer) {
    bool is_all_uppercase = true;
    uchar_t ch;
    
#ifndef PRIV_FUNCS_DONT_CHECK_NULL_PARAMS
    if (lexer == NULL) return (uchar_t)0;
#endif
    
    lexer->lexeme.length = 0;
    lexer->lexkey = HASH_INITIAL;
    
    do {
        ch = readchar();
        if (IS_NOT_UPPERCASE(ch))
            is_all_uppercase = false;
        lexer->lexeme.string[lexer->lexeme.length] = ch;
        lexer->lexkey = HASH_NEXT_CHAR(lexer->lexkey, ch);
        lexer->lexeme.length++;
        ch = nextchar();
    } while (((IS_ALPHANUM(ch)) || (ch == UNDERSCORE) || (ch == DOLLAR)) &&
             ((lexer->lexeme.length < (M2_MAX_IDENT_LENGTH)) &&
              (NOT_EOF(lexer))));
    
    // check if maximum identifier length has been reached
    if (lexer->lexeme.length == (M2_MAX_IDENT_LENGTH)) {
        
        // any further characters are not significant, skip them
        while (((IS_ALPHANUM(ch)) || (ch == UNDERSCORE) || (ch == DOLLAR)) &&
               (NOT_EOF(lexer))) {
            ch = readchar();
            ch = nextchar();
        } // end while
    } // end if
    
    // terminate the lexeme
    lexer->lexeme.string[lexer->lexeme.length] = CSTRING_TERMINATOR;
    lexer->lexkey = HASH_FINAL(lexer->lexkey);
    
    // determine if lexeme is reserved word or identifier
    if (is_all_uppercase) // may be a reserved word
        lexer->token = m2_token_for_reserved_word_hash(lexer->lexkey);
    else // could not possibly be a reserved word
        lexer->token = TOKEN_IDENTIFIER;
    
    return ch;
} // end get_ident


// ---------------------------------------------------------------------------
// private function:  get_numeric_literal(lexer)
// ---------------------------------------------------------------------------
//
// Reads a  numeric literal  from the input stream of <lexer>  and returns the
// character following the literal.
//
// This function accepts input conforming to the following syntax:
//
//  number := integer | real
//  integer := binary-integer | decimal-integer | base-16-integer
//  binary-integer := ( "0" | "1" )+ "B"
//  decimal-integer := digit+
//  base-16-integer := digit base-16-digit* ( "H" | "U" )
//  real := digit+ "." digit+ | digit "." digit+ "E" ( "+" | "-")? digit+
//  digit := "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
//  base-16-digit := digit | "A" | "B" | "C" | "D" | "E" | "F"
//
// pre-conditions:
//  o  lexer is an initialised lexer object
//  o  the current lookahead character is the first digit  at the beginning of
//     the literal.
//  o  the literal is well-formed, conforming to the syntax given above.
//
// post-conditions:
//  o  lexer->lexeme.string contains the literal,
//     followed by a type designator ('B', 'H', 'U' or 'R'),
//     followed by a C string terminator (ASCII NUL).
//  o  lexer->lexeme.length contains the length of lexer->lexeme.string.
//  o  lexer->token contains TOKEN_NUMERIC_LITERAL.
//  o  lexer->lexkey contains the key for the lexeme table.
//  o  lexer->status contains M2_LEXER_STATUS_SUCCESS.
//  o  the new lookahead character is the character following the literal.
//
// error-conditions:
//  o  lexer->lexeme.string contains the part of the literal before the
//     offending character, followed by a C string terminator (ASCII NUL).
//  o  lexer->lexeme.length contains the length of lexer->lexeme.string.
//  o  lexer->token contains TOKEN_ILLEGAL_CHARACTER.
//  o  lexer->lexkey contains 0.
//  o  lexer->offending_char contains the offending character.
//  o  lexer->offending_char_pos contains the position of the offending char.
//  o  lexer->status contains
//     - M2_LEXER_STATUS_LITERAL_TOO_LONG if maximum length is exceeded,
//     - M2_LEXER_STATUS_MALFORMED_NUMBER if illegal characters are found.
//  o  the new lookahead character is the offending character.

static fmacro uchar_t get_numeric_literal(m2_lexer_s *lexer) {
    uchar_t ch;
    uchar_t last_digit;
    cardinal non_decimal_digits = 0;
    cardinal non_binary_digits = 0;
    
    lexer->status = M2_LEXER_STATUS_SUCCESS;
    
    lexer->lexeme.length = 0;
    lexer->lexkey = HASH_INITIAL;
    
    // read the leading sequence of digits
    ch = nextchar();
    while (IS_UPPERHEX(ch)) {
        ch = readchar();
        if (lexer->lexeme.length < M2_MAX_NUM_LENGTH) {
            lexer->lexeme.string[lexer->lexeme.length] = ch;
            lexer->lexkey = HASH_NEXT_CHAR(lexer->lexkey, ch);
        } // end if
        lexer->lexeme.length++;
        
        if (!IS_DIGIT(ch)) {
            non_decimal_digits++;
        }
        if (!IS_BINARY(ch)) {
            non_binary_digits++;
        }
        
        last_digit = ch;
        ch = nextchar();
    } // end while
    
    // classify the sequence of digits
    switch (ch) {
        case LOWERCASE_B:
        case LOWERCASE_U:
        case LOWERCASE_X:
            if ((lexer->lexeme.length == 1) && (last_digit == DIGIT_ZERO)) {
                // prefixed number
                ch = get_prefixed_number(lexer);
            }
            else if (non_decimal_digits != 0) {
                // hexadecimal without suffix
                lexer->status = M2_LEXER_STATUS_MALFORMED_NUMBER;
            } // end if
            break;
        
        case DOT:
            if (non_decimal_digits == 0) {
                // real number
                ch = get_real_number(lexer);
            }
            else {
                // hexadecimal without suffix
                lexer->status = M2_LEXER_STATUS_MALFORMED_NUMBER;
            } // end if
            break;
        
        case UPPERCASE_H:
        case UPPERCASE_U:
            // suffixed hexadecimal
            ch = readchar();
            if (lexer->lexeme.length < M2_MAX_NUM_LENGTH) {
                lexer->lexeme.string[lexer->lexeme.length] = ch;
                lexer->lexkey = HASH_NEXT_CHAR(lexer->lexkey, ch);
            } // end if
            lexer->lexeme.length++;
            
            ch = nextchar();
            break;
        
        default:
            if ((non_decimal_digits != 0) &&
                !((non_binary_digits == 1) && (last_digit == UPPERCASE_B))) {
                // hexadecimal without suffix
                lexer->status = M2_LEXER_STATUS_MALFORMED_NUMBER;
            } // end if
    }
    
    // numeric literal length checks
    if (lexer->lexeme.length > M2_MAX_NUM_LENGTH + 1) {
        // long binary, decimal, hexadecimal or real
        lexer->status = M2_LEXER_STATUS_LITERAL_TOO_LONG;
    }
    else if ((lexer->lexeme.length == M2_MAX_NUM_LENGTH + 1) &&
        (lexer->lexeme.string[M2_MAX_NUM_LENGTH] != UPPERCASE_R)) {
        // long binary, decimal or hexadecimal
        lexer->status = M2_LEXER_STATUS_LITERAL_TOO_LONG;
    }
    
    if (lexer->status == M2_LEXER_STATUS_SUCCESS) {
        // good binary, decimal or hexadecimal
        lexer->lexeme.string[lexer->lexeme.length] = CSTRING_TERMINATOR;
        lexer->lexkey = HASH_FINAL(lexer->lexkey);
        lexer->token = TOKEN_NUMERIC_LITERAL;
    }
    else {
        lexer->lexkey = 0;
        lexer->token = TOKEN_ILLEGAL_CHARACTER;
    }
    
    return ch;
} // end get_numeric_literal


// ---------------------------------------------------------------------------
// private function:  get_prefixed_number(lexer)
// ---------------------------------------------------------------------------
//
// Reads a  prefixed numeric literal  from the input stream of <lexer> and re-
// turns the character following the literal.
//
// This function accepts input conforming to the following syntax:
//
//  prefixedNumber := prefixedBase2Number | prefixedBase16Number ;
//  prefixedBase2Number := '0b' ( '0' | '1' )+ ;
//  prefixedBase16Number := ( '0x' | '0u' ) lowercaseBase16Digit+ ;
//  lowercaseBase16Digit := digit | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' ;
//  digit := '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ;
//
// pre-conditions:
//
//  o  lexer is an initialised lexer object.
//  o  the value of lexer->lexeme.string[0] is '0'.
//  o  the value of lexer->lexeme.length is 1.
//  o  the current lookahead character is 'b' or 'x' or 'u'.
//  o  the literal is well-formed, conforming to the syntax given above.
//
// post-conditions:
//
//  o  lexer->lexeme.string contains the prefixed numeric literal.
//  o  lexer->lexeme.length contains the length of lexer->lexeme.string.
//  o  lexer->lexkey contains the key for the lexeme table.
//  o  lexer->status contains M2_LEXER_STATUS_SUCCESS.
//  o  the new lookahead character is the character following the literal.

static uchar_t get_prefixed_number(m2_lexer_s *lexer) {
    uchar_t ch;
    
    // read the prefix
    ch = readchar();
    if (lexer->lexeme.length < M2_MAX_NUM_LENGTH) {
        lexer->lexeme.string[lexer->lexeme.length] = ch;
        lexer->lexkey = HASH_NEXT_CHAR(lexer->lexkey, ch);
    } // end if
    lexer->lexeme.length++;
    
    if (ch == LOWERCASE_B) {
        // prefixed binary
        ch = nextchar();
        while (IS_BINARY(ch)) {
            ch = readchar();
            if (lexer->lexeme.length < M2_MAX_NUM_LENGTH) {
                lexer->lexeme.string[lexer->lexeme.length] = ch;
                lexer->lexkey = HASH_NEXT_CHAR(lexer->lexkey, ch);
            } // end if
            lexer->lexeme.length++;
            
            ch = nextchar();
        } // end while
    }
    else {
        // prefixed hexadecimal
        ch = nextchar();
        while (IS_LOWERHEX(ch)) {
            ch = readchar();
            if (lexer->lexeme.length < M2_MAX_NUM_LENGTH) {
                lexer->lexeme.string[lexer->lexeme.length] = ch;
                lexer->lexkey = HASH_NEXT_CHAR(lexer->lexkey, ch);
            } // end if
            lexer->lexeme.length++;
            
            ch = nextchar();
        } // end while
    } // end if
    
    if (lexer->lexeme.length <= 2) {
        // only a prefix is present
        lexer->status = M2_LEXER_STATUS_MALFORMED_NUMBER;
    } // end if
    
    return ch;
} // end get_prefixed_number


// ---------------------------------------------------------------------------
// private function:  get_real_number(lexer)
// ---------------------------------------------------------------------------
//
// Reads a real number literal from the input stream of <lexer>  and returns
// the character following the literal.
//
// This function accepts input conforming to the following syntax:
//
//  real := digit+ "." digit+ | digit "." digit+ "E" ( "+" | "-")? digit+
//  digit := "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
//
// pre-conditions:
//  o  lexer is an initialised lexer object.
//  o  the value of lexer->lexeme.string is a sequence of decimal digits.
//  o  the value of lexer->lexeme.length is greater than 0.
//  o  the current lookahead character is '.'.
//  o  the literal is well-formed, conforming to the syntax given above.
//
// post-conditions:
//  o  lexer->lexeme.string contains the real number literal,
//     followed by type designator 'R'.
//  o  lexer->lexeme.length contains the length of lexer->lexeme.string.
//  o  lexer->lexkey contains the key for the lexeme table.
//  o  lexer->status contains M2_LEXER_STATUS_SUCCESS.
//  o  the new lookahead character is the character following the literal.

static uchar_t get_real_number(m2_lexer_s *lexer) {
    uchar_t ch;
    cardinal fraction_offset;
    cardinal fraction_size;
    cardinal exponent_offset = 0;
    
    // read the decimal separator
    ch = readchar();
    if (lexer->lexeme.length < M2_MAX_NUM_LENGTH) {
        lexer->lexeme.string[lexer->lexeme.length] = ch;
        lexer->lexkey = HASH_NEXT_CHAR(lexer->lexkey, ch);
    } // end if
    lexer->lexeme.length++;
    
    fraction_offset = lexer->lexeme.length;
    
    ch = nextchar();
    while (IS_DIGIT(ch)) {
        ch = readchar();
        if (lexer->lexeme.length < M2_MAX_NUM_LENGTH) {
            lexer->lexeme.string[lexer->lexeme.length] = ch;
            lexer->lexkey = HASH_NEXT_CHAR(lexer->lexkey, ch);
        } // end if
        lexer->lexeme.length++;
        
        ch = nextchar();
    } // end while
    
    fraction_size = lexer->lexeme.length - fraction_offset;
    
    if ((ch == LOWERCASE_E) || (ch == UPPERCASE_E)) {
        ch = readchar();
        if (lexer->lexeme.length < M2_MAX_NUM_LENGTH) {
            lexer->lexeme.string[lexer->lexeme.length] = ch;
            lexer->lexkey = HASH_NEXT_CHAR(lexer->lexkey, ch);
        } // end if
        lexer->lexeme.length++;
        
        ch = nextchar();
        if ((ch == PLUS) || (ch == MINUS)) {
            ch = readchar();
            if (lexer->lexeme.length < M2_MAX_NUM_LENGTH) {
                lexer->lexeme.string[lexer->lexeme.length] = ch;
                lexer->lexkey = HASH_NEXT_CHAR(lexer->lexkey, ch);
            } // end if
            lexer->lexeme.length++;
            
            ch = nextchar();
        } // end if
        
        exponent_offset = lexer->lexeme.length;
        
        while (IS_DIGIT(ch)) {
            ch = readchar();
            if (lexer->lexeme.length < M2_MAX_NUM_LENGTH) {
                lexer->lexeme.string[lexer->lexeme.length] = ch;
                lexer->lexkey = HASH_NEXT_CHAR(lexer->lexkey, ch);
            } // end if
            lexer->lexeme.length++;
            
            ch = nextchar();
        } // end while
    } // end if
    
    // append 'R' designator
    if (lexer->lexeme.length < M2_MAX_NUM_LENGTH + 1) {
        lexer->lexeme.string[lexer->lexeme.length] = UPPERCASE_R;
        lexer->lexkey = HASH_NEXT_CHAR(lexer->lexkey, UPPERCASE_R);
    } // end if
    lexer->lexeme.length++;
    
    if (fraction_size == 0) {
        // short fractional part
        lexer->status = M2_LEXER_STATUS_MALFORMED_NUMBER;
    }
    else if (exponent_offset == lexer->lexeme.length - 1) {
        // short exponent part
        lexer->status = M2_LEXER_STATUS_MALFORMED_NUMBER;
    }
    else if ((fraction_offset > 2) && (exponent_offset != 0)) {
        // long integral part with exponent
        lexer->status = M2_LEXER_STATUS_MALFORMED_NUMBER;
    } // end if
    
    return ch;
} // end get_real_number


// ---------------------------------------------------------------------------
// private function:  get_quoted_literal(lexer)
// ---------------------------------------------------------------------------
//
// Reads a  quoted literal  from the input stream of <lexer>  and  returns the
// character following the literal.
//
// pre-conditions:
//  o  lexer is an initialised lexer object.
//  o  the current lookahead character is the delimiting quotation mark at the
//     beginning of the literal.
//  o  the literal is properly delimited with matching opening and closing
//     quotation marks, does not exceed the maximum string length and does
//     not contain any control characters.
//
// post-conditions:
//  o  lexer->lexeme.string contains the literal including delimiters,
//     followed by a type designator ('A' for ASCII, 'U' for Unicode),
//     followed by a C string terminator (ASCII NUL).
//  o  lexer->lexeme.length contains the length of lexer->lexeme.string.
//  o  lexer->token contains TOKEN_STRING_LITERAL.
//  o  lexer->lexkey contains the key for the lexeme table.
//  o  lexer->status contains M2_LEXER_STATUS_SUCCESS.
//  o  the new lookahead character is the character following the literal.
//
// error-conditions:
//  o  lexer->lexeme.string contains the part of the literal before the
//     offending character, followed by a C string terminator (ASCII NUL).
//  o  lexer->lexeme.length contains the length of lexer->lexeme.string.
//  o  lexer->token contains TOKEN_ILLEGAL_CHARACTER.
//  o  lexer->lexkey contains 0.
//  o  lexer->offending_char contains the offending character.
//  o  lexer->offending_char_pos contains the position of the offending char.
//  o  lexer->status contains
//     - M2_LEXER_STATUS_LITERAL_TOO_LONG if maximum length is exceeded,
//     - M2_LEXER_STATUS_STRING_NOT_DELIMITED if EOF is reached,
//     - M2_LEXER_STATUS_ILLEGAL_CHARACTER if illegal characters are found.
//  o  characters in the input stream are skipped until a matching closing
//     quotation mark delimiter or EOF is found.
//  o  the new lookahead character is the character following the literal.

static fmacro uchar_t get_quoted_literal(m2_lexer_s *lexer) {
    uchar_t ch, delimiter_ch;
    bool all_7bit_ascii = true;
    
#ifndef PRIV_FUNCS_DONT_CHECK_NULL_PARAMS
    if (lexer == NULL) return (uchar_t)0;
#endif
    
    lexer->lexeme.length = 0;
    lexer->lexkey = HASH_INITIAL;
    
    // copy opening delimiter
    ch = readchar();
    delimiter_ch = ch;
    lexer->lexeme.string[lexer->lexeme.length] = ch;
    lexer->lexkey = HASH_NEXT_CHAR(lexer->lexkey, ch);
    lexer->lexeme.length++;
    ch = nextchar();
    
    while ((ch != delimiter_ch) && (IS_NOT_CONTROL(ch)) &&
           ( lexer->lexeme.length < M2_MAX_STRING_LENGTH) && (NOT_EOF(lexer))) {
        
        // remember occurrence of non-ASCII chars
        if (IS_NOT_7BIT_ASCII(ch))
            all_7bit_ascii = false;
        
        // consume the character
        ch = readchar();
        
        // copy the char into the lexeme string
        lexer->lexeme.string[lexer->lexeme.length] = ch;
        lexer->lexkey = HASH_NEXT_CHAR(lexer->lexkey, ch);
        lexer->lexeme.length++;
        
        // check for escape sequence
        if (ch == BACKSLASH) {
            
            if ((lexer->lexeme.length < M2_MAX_STRING_LENGTH) &&
                (is_escaped_char(nextchar()))) {
                // valid escape sequence found
                
                // consume escaped character
                ch = readchar();
                
                // copy escaped character into the lexeme string
                lexer->lexeme.string[lexer->lexeme.length] = ch;
                lexer->lexkey = HASH_NEXT_CHAR(lexer->lexkey, ch);
                lexer->lexeme.length++;
            }
            else {
                // error: sole backslash found
                
                m2_fileio_getpos(lexer->source_file,
                                 &lexer->offending_char_pos);
                
                // notify offending character and position
                lexer->notify(M2_NOTIFY_ILLEGAL_CHAR_IN_STRING_LITERAL,
                              NULL, // TO DO : obtain and pass filename
                              lexer->offending_char_pos,
                              M2_NOTIFIER_LEXER,
                              &lexer->offending_char);
                
                // increment error count
                lexer->symbol_error_count++;
                
            } // end if
            
        } // end if
                
        // prepare for next
        ch = nextchar();
    } // end while    
    
    if ((ch == delimiter_ch) && (lexer->symbol_error_count == 0)) {
        // copy closing delimiter
        ch = readchar();
        lexer->lexeme.string[lexer->lexeme.length] = ch;
        lexer->lexkey = HASH_NEXT_CHAR(lexer->lexkey, ch);
        lexer->lexeme.length++;
        
        // append type designator
        if (all_7bit_ascii) ch = 'A'; else ch = 'U';
        lexer->lexeme.string[lexer->lexeme.length] = ch;
        lexer->lexkey = HASH_NEXT_CHAR(lexer->lexkey, ch);
        lexer->lexeme.length++;
        
        // terminate lexeme string
        lexer->lexeme.string[lexer->lexeme.length] = CSTRING_TERMINATOR;
        lexer->lexkey = HASH_FINAL(lexer->lexkey);
        
        // pass back token and lexeme key
        lexer->token = TOKEN_STRING_LITERAL;
        
        // set status
        if (lexer->status != KVS_STATUS_ALLOCATION_FAILED)
            lexer->status = M2_LEXER_STATUS_SUCCESS;
        else
            lexer->status = M2_LEXER_STATUS_ALLOCATION_FAILED;
        
        // return lookahead char
        return nextchar();
    }
    else { // error
        lexer->token = TOKEN_ILLEGAL_CHARACTER;
        lexer->lexkey = 0;
        lexer->offending_char = ch;
        m2_fileio_getpos(lexer->source_file, &lexer->offending_char_pos);
        
        // terminate lexeme string
        lexer->lexeme.string[lexer->lexeme.length] = CSTRING_TERMINATOR;
        
        // set status
        if (lexer->lexeme.length >= M2_MAX_STRING_LENGTH)
            lexer->status = M2_LEXER_STATUS_LITERAL_TOO_LONG;
        else if (EOF_REACHED(lexer))
            lexer->status = M2_LEXER_STATUS_STRING_NOT_DELIMITED;
        else
            lexer->status = M2_LEXER_STATUS_ILLEGAL_CHARACTER;
        
        // skip past string literal
        while ((ch != delimiter_ch) && (NOT_EOF(lexer))) {
            // eat the character
            ch = readchar();
            
            // check for escaped characters
            if (ch == BACKSLASH) {
                // get the possibly escaped character
                ch = nextchar();
                
                // consume the escaped character
                if (is_escaped_char(ch))
                    ch = readchar();
            } // endif
            
            // prepare for next
            ch = nextchar();
        } // end while
        
        // consume the closing delimiter and get the next
        ch = readchar();
        ch = nextchar();
        
        // return lookahead char
        return ch;
    } // end if
} // end get_quoted_literal


// ---------------------------------------------------------------------------
// private function:  is_escaped_char(ch)
// ---------------------------------------------------------------------------
//
// Determines if character <ch> is a valid escape character.
//
// return-value:
// o true if the character is a valid escape character, false otherwise.

static fmacro bool is_escaped_char(uchar_t ch) {
    
    switch (ch) {
        // valid escape characters
        case DOUBLE_QUOTE :
        case SINGLE_QUOTE :
        case DIGIT_ZERO :
        case LOWERCASE_N :
        case LOWERCASE_R :
        case LOWERCASE_T :
        case BACKSLASH :
            return true;
            break;
        // any other characters
        default :
            return false;
    } // end switch
    
} // end is_escaped_char


// ---------------------------------------------------------------------------
// private function:  skip_c_comment(lexer)
// ---------------------------------------------------------------------------
//
// Adds the lexer's current lexeme to its accociated lexeme table.
//
// pre-conditions:
//  o  lexer is an initialised lexer object.
//  o  lexer->lextab is an initialised kvs table object.
//  o  lexer->lexeme.string is a properly terminated C string containing the
//     lexeme of the current symbol.
//  o  lexer->lexeme.length contains the length of lexer->lexeme.string.
//  o  lexer->lexkey contains the key for lexer->lexeme.string.
//
// post-conditions:
//  o  lexer->lexeme.string has been entered into lexer->lextab.
//  o  lexer->status contains M2_LEXER_STATUS_SUCCESS.
//
// error-conditions:
//  if memmory allocation failed:
//  o  no entry has been added to lexer->lextab.
//  o  lexer->status contains M2_LEXER_STATUS_ALLOCATION_FAILED.

static fmacro void add_lexeme_to_lextab(m2_lexer_s *lexer) {
    kvs_status_t status;
    
#ifndef PRIV_FUNCS_DONT_CHECK_NULL_PARAMS
    if ((lexer == NULL) || (lexer->lextab == NULL)) return;
#endif
    
    kvs_store_value(lexer->lextab,
                    lexer->lexkey,
                    lexer->lexeme.string,
                    lexer->lexeme.length, true, &status);
    
    if (status == KVS_STATUS_ALLOCATION_FAILED)
        lexer->status = status;
    
} // end add_lexeme_to_lextab


// ---------------------------------------------------------------------------
// private function:  skip_multiline_comment(lexer)
// ---------------------------------------------------------------------------
//
// Skips the  current multi-line comment  including any nested comments in the
// input stream of <lexer>  and  returns  the character  following the closing 
// comment delimiter.
//
// pre-condition:
//  o  lexer is an initialised lexer object.
//  o  the lookahead character is the asterisk of the opening comment
//     delimiter at the beginning of the comment.
//
// post-conditions:
//  o  the new lookahead character is the character following the closing
//     comment delimiter at the end of the comment.
//  o  the lexer's line and coloumn counters have been updated.
//  o  lexer->status contains M2_LEXER_STATUS_SUCCESS.
//
// error-conditions:
//  if the number of open comments exceeded the maximum of 10
//  o  all characters in the input stream have been skipped.
//  o  the new lookahead character is the end-of-file marker.
//  o  lexer->offending_char contains the opening parenthesis of the offending
//     opening comment delimiter that exceeded the maximum nesting level.
//  o  lexer->offending_char contains the position of the offending character.
//  o  the lexer's line and coloumn counters have been updated.
//  o  lexer->status contains M2_LEXER_STATUS_COMMENT_NESTING_LIMIT_REACHED.

static fmacro uchar_t skip_multiline_comment(m2_lexer_s *lexer) {
    uchar_t ch, nextch;
    cardinal open_comment_count = 1;
    
#ifndef PRIV_FUNCS_DONT_CHECK_NULL_PARAMS
    if (lexer == NULL) return (uchar_t)0;
#endif
    
    // consume the opening comment delimiter
    readchar();
    
    // skip until opening and closing comment delimiters are balanced
    // or until comment nesting limit or end-of-file is reached
    while (NOT_EOF(lexer) &&
           (open_comment_count > 0) && (open_comment_count <= 10)) {
        
        // read next char
        ch = readchar();
        nextch = nextchar();
        
        // count up if new comment is opened
        if ((ch == OPENING_PARENTHESIS) && (nextch == ASTERISK)) {
            open_comment_count++;
            readchar(); // consume '*'
        }
        // count down if a comment is closed
        else if ((ch == ASTERISK) && (nextch == CLOSING_PARENTHESIS)) {
            open_comment_count--;
            readchar(); // consume ')'
        } // end if
        
    } // end while
    
    // determine status
    if (open_comment_count == 0) {
        lexer->status = M2_LEXER_STATUS_SUCCESS;
    }
    else if (open_comment_count > 10) {
        lexer->status = M2_LEXER_STATUS_COMMENT_NESTING_LIMIT_REACHED;
        lexer->offending_char = ch;
        m2_fileio_getpos(lexer->source_file, &lexer->offending_char_pos);
    }
    else {
        lexer->status = M2_LEXER_STATUS_EOF_REACHED_WITHIN_COMMENT;
        lexer->offending_char = ch;
        m2_fileio_getpos(lexer->source_file, &lexer->offending_char_pos);
    } // end if
    
    // return the lookahead character
    return nextchar();
} // end skip_multiline_comment


// ---------------------------------------------------------------------------
// private function:  skip_past_end_of_line(lexer)
// ---------------------------------------------------------------------------
//
// Skips past the next end-of-line in the input stream of <lexer>  and returns
// the character following end-of-line.
//
// pre-condition:
//  o  lexer is an initialised lexer object.
//
// post-conditions:
//  o  the new lookahead character is  the character following the end-of-line
//     marker.
//  o  the lexer's line and coloumn counters have been updated.

static fmacro uchar_t skip_past_end_of_line(m2_lexer_s *lexer) {
    
#ifndef PRIV_FUNCS_DONT_CHECK_NULL_PARAMS
    if (lexer == NULL) return (uchar_t)0;
#endif
    
    // consume the lookahead character
    readchar();
    
    // skip all characters until end-of-line marker is found
    while ((NOT_EOF(lexer)) && (nextchar() != EOL))
        readchar();
    
    // skip past the end-of-line marker
    readchar();
    
    // return the lookahead character
    return nextchar();
} // end skip_past_end_of_line


// ---------------------------------------------------------------------------
// private function:  skip_pragma(lexer)
// ---------------------------------------------------------------------------
//
// Skips the  current pragma  in the input stream of <lexer>  and  returns the
// character following the closing pragma delimiter.
//
// pre-condition:
//  o  lexer is an initialised lexer object.
//  o  the lookahead character is the asterisk of the opening pragma
//     delimiter at the beginning of the pragma.
//
// post-conditions:
//  o  the new lookahead character is the character following the closing
//     pragma delimiter at the end of the comment.
//  o  the lexer's line and coloumn counters have been updated.
//  o  lexer->status contains M2_LEXER_STATUS_SUCCESS.
//
// error-conditions:
//  if end-of-file was reached within the pragma
//  o  the new lookahead character is the end-of-file marker.
//  o  lexer->offending_char contains the last character in the input.
//  o  lexer->offending_char contains the position of the offending character.
//  o  the lexer's line and coloumn counters have been updated.
//  o  lexer->status contains M2_LEXER_STATUS_EOF_REACHED_WITHIN_COMMENT.

static fmacro uchar_t skip_pragma(m2_lexer_s *lexer) {
    uchar_t ch, nextch, quote;
    
#ifndef PRIV_FUNCS_DONT_CHECK_NULL_PARAMS
    if (lexer == NULL) return (uchar_t)0;
#endif
    
    // consume the opening pragma delimiter
    readchar();
    
    // skip until closing pragma delimiter is found
    // or end-of-file is reached
    while (NOT_EOF(lexer)) {
        
        // read next char
        ch = readchar();
        nextch = nextchar();
        
        if ((ch == ASTERISK) && (nextch == GREATER_THAN)) {
            
            // consume '>'
            readchar();
            break;
        }
        else if ((ch == SINGLE_QUOTE) || (ch == DOUBLE_QUOTE)) {
            
            quote = ch;
            ch = readchar();
            nextch = nextchar();
            
            // skip all characters until end of string
            while ((ch != quote) && (NOT_EOF(lexer))) {
                
                // skip any escaping backslashes
                if (ch == BACKSLASH) {
                    ch = readchar();
                } // end if
                
                // skip character
                ch = readchar();
                nextch = nextchar();
                
            } // end while
                        
        } // end if
        
    } // end while
        
    // determine status
    if (EOF_REACHED(lexer)) {
        lexer->status = M2_LEXER_STATUS_EOF_REACHED_WITHIN_COMMENT;
        lexer->offending_char = ch;
        m2_fileio_getpos(lexer->source_file, &lexer->offending_char_pos);
    } // end if
    
    // return the lookahead character
    return nextchar();
} // end skip_pragma


// END OF FILE
