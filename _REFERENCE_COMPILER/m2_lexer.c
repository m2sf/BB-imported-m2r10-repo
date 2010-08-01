/* Modula-2 R10 Compiler (m2r10c)
 *
 *  m2_lexer.c
 *  Lexer implementation
 *
 *  Author: Benjamin Kowarsch, Roel Messiant
 *
 *  Copyright (C) 2010 B.Kowarsch, R.Messiant. All rights reserved.
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


// ---------------------------------------------------------------------------
// Standard library imports
// ---------------------------------------------------------------------------

#include <stdio.h>
#include <errno.h>
#include <stdlib.h>

// ---------------------------------------------------------------------------
// ObjM2 project imports
// ---------------------------------------------------------------------------

#include "hash.h"
#include "ASCII.h"
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
    FILE *sourcefile;                   // source file
    kvs_table_t lextab;                 // lexeme table
    
    // return values
    m2_token_t token;                   // token to be returned
    cardinal lexkey;                    // lexeme key to be returned
    m2_lexer_status_t status;           // status to be returned
    
    // counters
    file_pos_t token_pos;               // position of current symbol
    file_pos_t current_pos;             // position of current character
    uint16_t paren_nesting_level;       // current parenthesis nesting level
    uint16_t bracket_nesting_level;     // current bracket nesting level
    uint16_t brace_nesting_level;       // current brace nesting level
    
    // flags
    bool end_of_file;                   // end-of-file has been reached
    
    // lexeme buffer
    lexbuf_t lexeme;
    
    // offending character
    char offending_char;
    file_pos_t offending_char_pos;
} m2_lexer_s;

#define NOT_EOF(_lexer) (_lexer->end_of_file == false)
#define EOF_REACHED(_lexer) (_lexer->end_of_file == true)


// ==========================================================================
// P R I V A T E   F U N C T I O N   P R O T O T Y P E S
// ==========================================================================

static fmacro uchar_t _readchar(m2_lexer_s *lexer);

static fmacro uchar_t _nextchar(m2_lexer_s *lexer);

static fmacro uchar_t get_ident(m2_lexer_s *lexer);

static fmacro uchar_t get_numeric_literal(m2_lexer_s *lexer);

static uchar_t get_prefixed_number(m2_lexer_s *lexer);

static uchar_t get_suffixed_number(m2_lexer_s *lexer);

static uchar_t get_digits(m2_lexer_s *lexer,
                                 cardinal *non_binary_digits,
                                 cardinal *non_decimal_digits);

static uchar_t get_decimal_digits(m2_lexer_s *lexer);

static uchar_t get_scale_factor(m2_lexer_s *lexer);

static fmacro uchar_t get_quoted_literal(m2_lexer_s *lexer);

static uchar_t get_escaped_char(m2_lexer_s *lexer);

static fmacro void add_lexeme_to_lextab(m2_lexer_s *lexer);

static fmacro uchar_t skip_multiline_comment(m2_lexer_s *lexer);

static fmacro uchar_t skip_past_end_of_line(m2_lexer_s *lexer);


// ==========================================================================
// P U B L I C   F U N C T I O N   I M P L E M E N T A T I O N S
// ==========================================================================

#define readchar(v) _readchar(this_lexer) /* v = void */
#define nextchar(v) _nextchar(this_lexer) /* v = void */

// --------------------------------------------------------------------------
// function:  m2_new_lexer(infile, lextab, status)
// --------------------------------------------------------------------------
//
// Creates  and  returns  a  new  lexer object  associated  with  source file 
// <infile> and lexeme table <lextab>.  The status of the operation is passed
// back in <status> unless NULL is passed in for <status>.
//
// Returns NULL if the lexer object could not be created.

m2_lexer_t m2_new_lexer(FILE *infile,
                        kvs_table_t lextab,
                        m2_lexer_status_t *status) {
    
    m2_lexer_s *new_lexer;    
    
    // assert pre-conditions
    
    if (infile == NULL) {
        ASSIGN_BY_REF(status, M2_LEXER_STATUS_INVALID_REFERENCE);
        return NULL;
    } // end if
    
    errno = 0;
    rewind(infile);
    if (errno != 0) {
        ASSIGN_BY_REF(status, M2_LEXER_STATUS_INVALID_REFERENCE);
        return NULL;
    } // end if
    
    if (lextab == NULL) {
        ASSIGN_BY_REF(status, M2_LEXER_STATUS_INVALID_REFERENCE);
        return NULL;
    } // end if
    
    // allocate a new lexer object
    new_lexer = (m2_lexer_s*) malloc(sizeof(m2_lexer_s));
    
    if (new_lexer == NULL) {
        ASSIGN_BY_REF(status, M2_LEXER_STATUS_ALLOCATION_FAILED);
        return NULL;
    } // end if
    
    // initialise the new lexer object
    
    // configuration parameters
    new_lexer->sourcefile = infile;
    new_lexer->lextab = lextab;
    
    // return values
    new_lexer->token = 0;
    new_lexer->lexkey = 0;
    
    // counters
    SET_FPOS(new_lexer->token_pos, 1, 1);
    SET_FPOS(new_lexer->current_pos, 1, 1);
    new_lexer->paren_nesting_level = 0;
    new_lexer->bracket_nesting_level = 0;
    new_lexer->brace_nesting_level = 0;
    
    // lexer flags
    new_lexer->end_of_file = false;
    
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
        this_lexer->token_pos = this_lexer->current_pos;
        
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
                        ch = readchar();
                        ch = nextchar();
                        this_lexer->token = TOKEN_START_PRAGMA;
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
                this_lexer->offending_char_pos = this_lexer->current_pos;
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
        ASSIGN_BY_REF(row, this_lexer->current_pos.line);
        ASSIGN_BY_REF(col, this_lexer->current_pos.col);
        ASSIGN_BY_REF(status, M2_LEXER_STATUS_SUCCESS);
        return this_lexer->offending_char;
    } // end if
} // end m2_offending_char


// ---------------------------------------------------------------------------
// function:  m2_dispose_lexer(lexer)
// ---------------------------------------------------------------------------
//
// Disposes of lexer object <lexer>  and  closes its sourcefile if it is open. 
// The  symbol table  used  by  the lexer is  NOT  disposed of.  The status of
// the operation is passed back in <status>.

void m2_dispose_lexer(m2_lexer_t lexer,
                      m2_lexer_status_t *status) {
    
    m2_lexer_s *this_lexer = (m2_lexer_s *) lexer;
    
    if (lexer == NULL) {
        ASSIGN_BY_REF(status, M2_LEXER_STATUS_INVALID_REFERENCE);
        return;
    } // end if
    
    fclose(this_lexer->sourcefile);
    free(this_lexer);
    
    ASSIGN_BY_REF(status, M2_LEXER_STATUS_SUCCESS);
    return;
} // end m2_dispose_lexer;

#undef readchar
#undef nextchar


// ==========================================================================
// P R I V A T E   F U N C T I O N   I M P L E M E N T A T I O N S
// ==========================================================================

// ---------------------------------------------------------------------------
// macros for private functions
// ---------------------------------------------------------------------------

#define readchar(v) _readchar(lexer) /* v = void */
#define nextchar(v) _nextchar(lexer) /* v = void */
#define IS_METHOD_SIGNATURE_CONTEXT(lexer) \
((lexer->seen_method && !(lexer->seen_open_paren_since_method)) || \
(lexer->bracket_nesting_level > 0) || (lexer->seen_backquote))


// ---------------------------------------------------------------------------
// private function:  _readchar(lexer)
// ---------------------------------------------------------------------------
//
// Reads one character from the input stream of <lexer>  and  returns it.  The
// lexer's coloumn counter is incremented.  Returns linefeed (ASCII LF) if any
// of linefeed (ASCII LF)  or carriage return (ASCII CR)  or  a combination of
// CR and LF (CRLF or LFCR) is read.  If LF is returned,  the  lexer's coloumn
// counter is reset and its line counter is incremented.
//
// pre-conditions:
//  o  lexer is an initialised lexer object
//
// post-conditions:
//  o  new current character is the character read (consumed)
//  o  new lookahead character is the character following the character read
//  o  position counters are updated accordingly
//
// return-value:
//  o  read (consumed) character is returned

static fmacro uchar_t _readchar(m2_lexer_s *lexer) {
    register int c;
    
#ifndef PRIV_FUNCS_DONT_CHECK_NULL_PARAMS
    if (lexer == NULL) return (uchar_t)0;
#endif
    
    // read one character from source file
    c = getc(lexer->sourcefile);
    
    // handle LF style end-of-line
    if (c == ASCII_LF) {
        lexer->current_pos.col = 1;
        lexer->current_pos.line++;
    }
    // handle CRLF and CR style end-of-line
    else if (c == ASCII_CR) {
        lexer->current_pos.col = 1;
        lexer->current_pos.line++;
        c = getc(lexer->sourcefile);
        if (c != NEWLINE) {
            ungetc(c, lexer->sourcefile);
        } // end if
        c = NEWLINE;
    }
    // handle end-of-file
    else if (c == EOF) {
        // set end-of-file flag if end-of-file reached
        lexer->end_of_file = (feof(lexer->sourcefile) == true);
        c = 0;
    }
    else /* any other characters */ {
        // increment row counter
        lexer->current_pos.col++;
    } // end if
    
    if (((uchar_t) c == 255) || (c == 0)) {
        //printf("");
        ;
    } // end if
    
    // return character
    return (uchar_t) c;
} // end _readchar


// ---------------------------------------------------------------------------
// private function:  _nextchar(lexer)
// ---------------------------------------------------------------------------
//
// Returns the lookahead character in the input stream of <lexer>  and returns
// it without incrementing the file pointer  and  without changing the lexer's
// coloumn and line counters.
//
// pre-conditions:
//  o  lexer is an initialised lexer object
//
// post-conditions:
//  o  position counters remain unchanged
//
// return-value:
//  o  lookahead character is returned

static fmacro uchar_t _nextchar(m2_lexer_s *lexer) {
    register int status;
    register int c;
    
#ifndef PRIV_FUNCS_DONT_CHECK_NULL_PARAMS
    if (lexer == NULL) return (uchar_t)0;
#endif
    
    c = getc(lexer->sourcefile);
    
    status = ungetc(c, lexer->sourcefile);
    if (status != EOF) {
        lexer->end_of_file = false;
    }
    else {
        lexer->end_of_file = true;
        c = 0;
    } // end if
    
    return (uchar_t) c;
} // end _nextchar


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
//  sbase-16-integer := digit base-16-digit+ ( "H" | "U" )
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
    uchar_t ch, first_ch;
    
    lexer->lexeme.length = 0;
    lexer->lexkey = HASH_INITIAL;
    
    // get the first character
    first_ch = readchar();
    lexer->lexeme.string[lexer->lexeme.length] = first_ch;
    lexer->lexkey = HASH_NEXT_CHAR(lexer->lexkey, first_ch);
    lexer->lexeme.length++;
    ch = nextchar();
    
    // handle prefixed literal
    if ((first_ch == DIGIT_ZERO) &&
        ((ch == LOWERCASE_X) || (ch == LOWERCASE_U) || (ch == LOWERCASE_B))) {
        ch = get_prefixed_number(lexer);
    }
    // handle non-prefixed literal
    else {
        ch = get_suffixed_number(lexer);
    } // end if
    
    // return the lokkahead character
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
//  o  lexer->lexeme.string contains the literal,
//     followed by a C string terminator (ASCII NUL).
//  o  lexer->lexeme.length contains the length of lexer->lexeme.string.
//  o  lexer->token contains TOKEN_NUMERIC_LITERAL.
//  o  lexer->lexkey contains the key for the lexeme table.
//  o  lexer->status contains M2_LEXER_STATUS_SUCCESS.
//  o  the new lookahead character is the character following the literal.

static fmacro uchar_t get_prefixed_number(m2_lexer_s *lexer) {
    uchar_t ch, prefix;
    
    // consume prefix
    prefix = readchar();
    
    // add prefix to lexeme buffer
    lexer->lexeme.string[lexer->lexeme.length] = prefix;
    lexer->lexkey = HASH_NEXT_CHAR(lexer->lexkey, prefix);
    lexer->lexeme.length++;
    
    // peek at first character following prefix
    ch = nextchar();
    
    // get any remaining digits
    if ((prefix == LOWERCASE_X) || (prefix == LOWERCASE_U)) {
        // base-16 digits only
        while (((IS_DIGIT(ch)) || (IS_LOWERHEX(ch))) &&
               (lexer->lexeme.length <= M2_MAX_NUM_LENGTH) &&
               (NOT_EOF(lexer))) {
            ch = readchar();
            lexer->lexeme.string[lexer->lexeme.length] = ch;
            lexer->lexkey = HASH_NEXT_CHAR(lexer->lexkey, ch);
            lexer->lexeme.length++;
            ch = nextchar();
        } // end while
    }
    else if (prefix == LOWERCASE_B) {
        // base-2 digits only
        while (((ch == DIGIT_ZERO) || (ch == DIGIT_ONE)) &&
               (lexer->lexeme.length <= M2_MAX_NUM_LENGTH) &&
               (NOT_EOF(lexer))) {
            ch = readchar();
            lexer->lexeme.string[lexer->lexeme.length] = ch;
            lexer->lexkey = HASH_NEXT_CHAR(lexer->lexkey, ch);
            lexer->lexeme.length++;
            ch = nextchar();
        } // end while
    } // end if
    
    // check literal length
    if ((lexer->lexeme.length > 2) &&
        (lexer->lexeme.length <= M2_MAX_NUM_LENGTH)) {
        // literal is valid
        lexer->token = TOKEN_NUMERIC_LITERAL;
        lexer->status = M2_LEXER_STATUS_SUCCESS;
        lexer->lexeme.string[lexer->lexeme.length] = CSTRING_TERMINATOR;
        return ch;
    } // end if
    
    // FAILURE
    
    if (lexer->lexeme.length > M2_MAX_NUM_LENGTH) {
        // literal exceeds maximum length
        lexer->token = TOKEN_ILLEGAL_CHARACTER;
        lexer->status = M2_LEXER_STATUS_LITERAL_TOO_LONG;
    }
    else if (lexer->lexeme.length <= 2) {
        // literal has no digits
        lexer->token = TOKEN_ILLEGAL_CHARACTER;
        lexer->status = M2_LEXER_STATUS_MALFORMED_NUMBER;
    } // end if
    
    lexer->offending_char = ch;
    lexer->offending_char_pos = lexer->current_pos;
    lexer->lexkey = 0;
    return ch;
} // end get_prefixed_number


// ---------------------------------------------------------------------------
// private function:  get_suffixed_number(lexer)
// ---------------------------------------------------------------------------
//
// Reads a  numeric literal  from the input stream of <lexer>  and returns the
// character following the literal.
//
// This function accepts input conforming to the following syntax:
//
//  number := integer | real ;
//  integer := digit+ | suffixedBase2Integer | suffixedBase16Integer ;
//  suffixedBase2Integer := ( "0" | "1" )+ "B" ;
//  suffixedBase16Integer := uppercaseBase16Digit+ ( "H" | "U" ) ;
//  real := digit+ "." digit+ | digit "." digit+ "E" ( "+" | "-" )? digit+ ;
//  digit := "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
//  uppercaseBase16Digit := digit | "A" | "B" | "C" | "D" | "E" | "F" ;
//
// pre-conditions:
//
//  o  lexer is an initialised lexer object.
//  o  the value of lexer->lexeme.string[0] is a digit.
//  o  the value of lexer->lexeme.length is 1.
//  o  the literal is well-formed, conforming to the syntax given above.
//
// post-conditions:
//
//  o  lexer->lexeme.string contains the literal,
//     followed by a C string terminator (ASCII NUL).
//  o  lexer->lexeme.length contains the length of lexer->lexeme.string.
//  o  lexer->token contains TOKEN_NUMERIC_LITERAL.
//  o  lexer->lexkey contains the key for the lexeme table.
//  o  lexer->status contains M2_LEXER_STATUS_SUCCESS.
//  o  the new lookahead character is the character following the literal.

static fmacro uchar_t get_suffixed_number(m2_lexer_s *lexer) {
    
    cardinal non_binary_digit_count = 0;
    cardinal non_decimal_digit_count = 0;
    cardinal digits_before_decimal_point = 0;
    bool found_H_or_U = false;
    bool well_formed = false;
    uchar_t final_ch;
    uchar_t ch;
    
#ifndef PRIV_FUNCS_DONT_CHECK_NULL_PARAMS
    if (lexer == NULL) return (uchar_t)0;
#endif
    
    // get all digits until the first non-digit is found or length is exceeded
    ch = get_digits(lexer, &non_binary_digit_count, &non_decimal_digit_count);
    
    // check for 'H' and 'U' designator
    if ((lexer->lexeme.length < M2_MAX_NUM_LENGTH) &&
        ((ch == UPPERCASE_H) || (ch == UPPERCASE_U))) {
        found_H_or_U = true;
        ch = readchar();
        lexer->lexeme.string[lexer->lexeme.length] = ch;
        lexer->lexkey = HASH_NEXT_CHAR(lexer->lexkey, ch);
        lexer->lexeme.length++;
        ch = nextchar();
    } // end if
    
    // check length
    if (lexer->lexeme.length > M2_MAX_NUM_LENGTH) {
        // error: maximum length exceeded
        lexer->status = M2_LEXER_STATUS_LITERAL_TOO_LONG;
        lexer->offending_char = ch;
        lexer->offending_char_pos = lexer->current_pos;
        lexer->lexeme.string[lexer->lexeme.length] = CSTRING_TERMINATOR;
        lexer->token = TOKEN_ILLEGAL_CHARACTER;
        lexer->lexkey = 0;
        return ch;
    } // end if
    
    // the digit sequence is an integer literal if:
    // o  it contains any non-decimal digits, or
    // o  it is followed by a 'H' designator, or
    // o  it is not followed by a decimal point
    
    if ((non_decimal_digit_count > 0) || (found_H_or_U) || (ch != DOT)) {
        
        // terminate the lexeme
        lexer->lexeme.string[lexer->lexeme.length] = CSTRING_TERMINATOR;
        
        // check well-formedness
        final_ch = lexer->lexeme.string[lexer->lexeme.length - 1];
        if (IS_DIGIT(final_ch))
            // well formed if there is no designator and all digits are decimal
            well_formed = (non_decimal_digit_count == 0);
        else switch (final_ch) {
            case UPPERCASE_B :
                // well formed if designator is 'B' and all digits are binary
                well_formed = (non_binary_digit_count <= 1);
                break;
            case UPPERCASE_U :
                // well formed if designator is 'U'
                well_formed = true;
                break;
            case UPPERCASE_H :
                // well formed if designator is 'H'
                well_formed = true;
                break;
            default :
                // malformed in any other case
                well_formed = false;
        } // end if
        
        // if well formed, return token, lexeme key and status
        if (well_formed) {
            lexer->token = TOKEN_NUMERIC_LITERAL;
            lexer->lexkey = HASH_FINAL(lexer->lexkey);
            lexer->status = M2_LEXER_STATUS_SUCCESS;
            return ch;
        }
        // if malformed, return offending char, null-token, null-key, status
        else /* malformed */ {
            lexer->status = M2_LEXER_STATUS_MALFORMED_NUMBER;
            lexer->offending_char = ch;
            lexer->offending_char_pos = lexer->current_pos;
            lexer->token = TOKEN_ILLEGAL_CHARACTER;
            lexer->lexkey = 0;
            return ch;
        } // end if
    } // end if
    
    // at this point the digit sequence is part of a real number literal
    
    // get decimal point
    if (lexer->lexeme.length < M2_MAX_NUM_LENGTH) {
        ch = readchar();
        digits_before_decimal_point = lexer->lexeme.length;
        lexer->lexeme.string[lexer->lexeme.length] = ch;
        lexer->lexkey = HASH_NEXT_CHAR(lexer->lexkey, ch);
        lexer->lexeme.length++;
        ch = nextchar();
    } // end if
    
    // check length
    if (lexer->lexeme.length > M2_MAX_NUM_LENGTH) {
        // error: maximum length exceeded
        lexer->status = M2_LEXER_STATUS_LITERAL_TOO_LONG;
        lexer->offending_char = ch;
        lexer->offending_char_pos = lexer->current_pos;
        lexer->lexeme.string[lexer->lexeme.length] = CSTRING_TERMINATOR;
        lexer->token = TOKEN_ILLEGAL_CHARACTER;
        lexer->lexkey = 0;
        return ch;
    } // end if
    
    // get digits following decimal point
    if (IS_DIGIT(ch))
        ch = get_decimal_digits(lexer);
    else {
        // error: missing digits after decimal point
        lexer->status = M2_LEXER_STATUS_MALFORMED_NUMBER;
        lexer->offending_char = ch;
        lexer->offending_char_pos = lexer->current_pos;
        lexer->lexeme.string[lexer->lexeme.length] = CSTRING_TERMINATOR;
        lexer->token = TOKEN_ILLEGAL_CHARACTER;
        lexer->lexkey = 0;
        return ch;
    } // end if
    
    // check length
    if (lexer->lexeme.length > M2_MAX_NUM_LENGTH) {
        // error: maximum length exceeded
        lexer->status = M2_LEXER_STATUS_LITERAL_TOO_LONG;
        lexer->offending_char = ch;
        lexer->offending_char_pos = lexer->current_pos;
        lexer->lexeme.string[lexer->lexeme.length] = CSTRING_TERMINATOR;
        lexer->token = TOKEN_ILLEGAL_CHARACTER;
        lexer->lexkey = 0;
        return ch;
    } // end if
    
    // a real number literal may have a scale factor *only*
    // if it has exactly one digit before the decimal point
    
    if ((digits_before_decimal_point == 1) &&
        ((ch == UPPERCASE_E) || (ch == LOWERCASE_E))) {
        ch = get_scale_factor(lexer);
        
        // check length
        if (lexer->lexeme.length > M2_MAX_NUM_LENGTH) {
            // error: maximum length exceeded
            lexer->status = M2_LEXER_STATUS_LITERAL_TOO_LONG;
            lexer->offending_char = ch;
            lexer->offending_char_pos = lexer->current_pos;
            lexer->lexeme.string[lexer->lexeme.length] = CSTRING_TERMINATOR;
            lexer->token = TOKEN_ILLEGAL_CHARACTER;
            lexer->lexkey = 0;
            return ch;
        } // end if
    } // end if
    
    // terminate the lexeme
    
    // Get the last character in the lexeme.
    final_ch = lexer->lexeme.string[lexer->lexeme.length - 1];
    
    // if well formed, return token, lexeme key and status
    if (IS_DIGIT(final_ch)) {
        lexer->status = M2_LEXER_STATUS_SUCCESS;
        
        // append type designator 'R'
        ch = 'R';
        lexer->lexeme.string[lexer->lexeme.length] = ch;
        lexer->lexkey = HASH_NEXT_CHAR(lexer->lexkey, ch);
        lexer->lexeme.length++;
        lexer->token = TOKEN_NUMERIC_LITERAL;
        lexer->lexeme.string[lexer->lexeme.length] = CSTRING_TERMINATOR;        
        lexer->lexkey = HASH_FINAL(lexer->lexkey);
        return ch;
    }
    // if malformed, return offending char, null-token, null-key, status
    else /* malformed */ {
        lexer->status = M2_LEXER_STATUS_MALFORMED_NUMBER;
        lexer->offending_char = ch;
        lexer->offending_char_pos = lexer->current_pos;
        lexer->token = TOKEN_ILLEGAL_CHARACTER;
        lexer->lexeme.string[lexer->lexeme.length] = CSTRING_TERMINATOR;        
        lexer->lexkey = 0;
        return ch;
    } // end if
    
} // end get_numeric_literal


// ---------------------------------------------------------------------------
// private function:  get_digits(lexer, non_binary_digits, non_decimal_digits)
// ---------------------------------------------------------------------------
//
// Reads a sequence of digits from the input stream of <lexer> and returns the
// character following the digit sequence.  The number of non-binary digits is
// passed back in <non_binary_digits>.  The  number  of  non-decimal digits is
// passed back in <non_decimal_digits>.
//
// This function accepts input conforming to the following syntax:
//
//  digit-sequence := ( digit | "A" | "B" | "C" | "D" | "E" | "F" )*
//  digit : = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"

static fmacro uchar_t get_digits(m2_lexer_s *lexer,
                                 cardinal *non_binary_digits,
                                 cardinal *non_decimal_digits) {
    uchar_t ch;
    
#ifndef PRIV_FUNCS_DONT_CHECK_NULL_PARAMS
    if (lexer == NULL) return (uchar_t)0;
#endif
    
    *non_binary_digits = 0;
    *non_decimal_digits = 0;
    
    ch = nextchar();
    while ((IS_UPPERHEX(ch)) &&
           (lexer->lexeme.length < M2_MAX_NUM_LENGTH) && (NOT_EOF(lexer))) {
        if (ch >= UPPERCASE_A)
            (*non_decimal_digits)++;
        if (ch >= DIGIT_TWO)
            (*non_binary_digits)++;
        ch = readchar();
        lexer->lexeme.string[lexer->lexeme.length] = ch;
        lexer->lexeme.length++;
        lexer->lexkey = HASH_NEXT_CHAR(lexer->lexkey, ch);
        ch = nextchar();
    } // end while
    
    return ch;
} // end get_digits


// ---------------------------------------------------------------------------
// private function:  get_decimal_digits(lexer)
// ---------------------------------------------------------------------------
//
// Reads  a  sequence of decimal digits  from the input stream of <lexer>  and
// returns the character following the decimal digit sequence.
//
// This function accepts input conforming to the following syntax:
//
//  decimal-digit-sequence := digit*
//  digit : = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"

static fmacro uchar_t get_decimal_digits(m2_lexer_s *lexer) {
    uchar_t ch;
    
#ifndef PRIV_FUNCS_DONT_CHECK_NULL_PARAMS
    if (lexer == NULL) return (uchar_t)0;
#endif
    
    ch = nextchar();
    while ((IS_DIGIT(ch)) &&
           (lexer->lexeme.length < M2_MAX_NUM_LENGTH) && (NOT_EOF(lexer))) {
        ch = readchar();
        lexer->lexeme.string[lexer->lexeme.length] = ch;
        lexer->lexeme.length++;
        lexer->lexkey = HASH_NEXT_CHAR(lexer->lexkey, ch);
        ch = nextchar();
    } // end while
    
    return ch;
} // end get_decimal_digits


// ---------------------------------------------------------------------------
// private function:  get_scale_factor(lexer)
// ---------------------------------------------------------------------------
//
// Reads the  scale factor  of a real number literal  from the input stream of
// <lexer>  and  returns the character following the scale factor.
//
// This function accepts input conforming to the following syntax:
//
//  scale-factor := "E" ( "+" | "-" )? digit*
//  digit : = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"

static fmacro uchar_t get_scale_factor(m2_lexer_s *lexer) {
    uchar_t ch;
    
#ifndef PRIV_FUNCS_DONT_CHECK_NULL_PARAMS
    if (lexer == NULL) return (uchar_t)0;
#endif
    
    // get 'E' designator
    ch = readchar();
    lexer->lexeme.string[lexer->lexeme.length] = ch;
    lexer->lexkey = HASH_NEXT_CHAR(lexer->lexkey, ch);
    lexer->lexeme.length++;
    ch = nextchar();
    
    // get exponent sign, if present
    if (((ch == PLUS) || (ch == MINUS)) &&
        (lexer->lexeme.length < M2_MAX_NUM_LENGTH) && (NOT_EOF(lexer))) {
        ch = readchar();
        lexer->lexeme.string[lexer->lexeme.length] = ch;
        lexer->lexkey = HASH_NEXT_CHAR(lexer->lexkey, ch);
        lexer->lexeme.length++;
        ch = nextchar();
    } // end if
    
    // get exponent digits
    while ((IS_DIGIT(ch)) &&
           (lexer->lexeme.length < M2_MAX_NUM_LENGTH) && (NOT_EOF(lexer))) {
        ch = readchar();
        lexer->lexeme.string[lexer->lexeme.length] = ch;
        lexer->lexkey = HASH_NEXT_CHAR(lexer->lexkey, ch);
        lexer->lexeme.length++;
        ch = nextchar();
    } // while exponent digits
    
    return ch;
} // end get_scale_factor


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
        
        // check for escaped chars
        if (ch != BACKSLASH)
            ch = readchar();
        else // backslash escaped char
            ch = get_escaped_char(lexer);
        
        // copy the char into lexeme string
        lexer->lexeme.string[lexer->lexeme.length] = ch;
        lexer->lexkey = HASH_NEXT_CHAR(lexer->lexkey, ch);
        lexer->lexeme.length++;
        
        // prepare for next
        ch = nextchar();
    } // end while    
    
    if (ch == delimiter_ch) {
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
        lexer->offending_char_pos = lexer->current_pos;
        
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
            if (ch != BACKSLASH)
                ch = readchar();
            else // backslash escaped char
                ch = get_escaped_char(lexer);
            ch = nextchar();
        } // end while
        
        // return lookahead char
        return ch;
    } // end if
} // end get_quoted_literal


// ---------------------------------------------------------------------------
// private function:  get_escaped_char(lexer)
// ---------------------------------------------------------------------------
//
// Reads an  escaped character sequence  from the input stream of <lexer>  and
// returns the character represented by the escape sequence.
//
// pre-conditions:
//  o  lexer is an initialised lexer object
//  o  current character is *assumed* to be backslash
//
// post-conditions:
//  if the assumed backslash starts an escape sequence
//  o  the current character is the last character in the escape sequence
//  o  the lookahead character is the character following the escape sequence
//  o  line and coloumn counters are updated
//
//  if the assumed backslash does not start an escape sequence
//  o  current character, lookahead character, line and coloumn counter
//     remain unchanged
//
// return-value:
//  if the assumed backslash starts an escape sequence
//  o  the escaped character is returned
//
//  if the assumed backslash does not start an escape sequence
//  o  a backslash is returned

static fmacro uchar_t get_escaped_char(m2_lexer_s *lexer) {
    uchar_t ch, nextch;
    bool escape_sequence_found = false;
    
#ifndef PRIV_FUNCS_DONT_CHECK_NULL_PARAMS
    if (lexer == NULL) return (uchar_t)0;
#endif
    
    // must NOT consume current character
    // simply assume that it is backslash
    ch = BACKSLASH;
    
    // get the lookahead character
    nextch = nextchar();
    
    switch (nextch) {
        case DOUBLE_QUOTE :
        case SINGLE_QUOTE :
            escape_sequence_found = true;
            ch = nextchar();
            break;
        case DIGIT_ZERO :
            escape_sequence_found = true;
            ch = ASCII_NUL;
            break;
        case LOWERCASE_N :
            escape_sequence_found = true;
            ch = LINEFEED;
            break;
        case LOWERCASE_R :
            escape_sequence_found = true;
            ch = CARRIAGE_RETURN;
            break;
        case LOWERCASE_T :
            escape_sequence_found = true;
            ch = TAB;
            break;
        case BACKSLASH :
            escape_sequence_found = true;
            ch = BACKSLASH;
    } // end switch
    
    // consume current character only if escape sequence was found
    if (escape_sequence_found)
        readchar();
    
    return ch;
} // end get_escaped_char


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
        lexer->offending_char_pos = lexer->current_pos;
    }
    else {
        lexer->status = M2_LEXER_STATUS_EOF_REACHED_WITHIN_COMMENT;
        lexer->offending_char = ch;
        lexer->offending_char_pos = lexer->current_pos;
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


// END OF FILE
